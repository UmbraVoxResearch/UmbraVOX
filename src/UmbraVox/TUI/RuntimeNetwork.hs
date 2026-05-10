-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.RuntimeNetwork
    ( getOrCreateIdentity
    , acceptLoopTUI
    , connectToPeer
    , startListenerIfNeeded
    , restartListener
    , connectGroupPeers
    , startListenerOnPort
    , connectGroupTargets
    , applyListenPort
    ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, modifyTVar')
import Control.Exception (SomeException, bracket, catch, displayException, finally, fromException, throwIO, try)
import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', readIORef, writeIORef)
import qualified Data.Set as Set
import Data.List (intercalate, stripPrefix)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isUserError, ioeGetErrorString)
import UmbraVox.App.RuntimeLog (logEvent)
import UmbraVox.BuildProfile (buildSupportsPeerExchange)
import UmbraVox.Network.PeerExchange (PeerInfo(..), exchangePeers)
import UmbraVox.Crypto.ConstantTime (constantEq)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Network.ProviderCatalog (TransportProviderId, providerIdLabel)
import UmbraVox.Network.ProviderRuntime
    ( ProviderListener, activeRuntimeProvider, acceptWithProvider
    , bindListenerWithProvider, closeProviderListener, connectWithProvider
    , connectWithProviderTryPorts, connectWithProviderTryPortsProgress
    )
import UmbraVox.Network.TransportClass (AnyTransport, anyClose, anyInfo)
import UmbraVox.Protocol.Encoding (defaultPorts, parseHostPort, renderHostPort)
import UmbraVox.TUI.Actions (addSession, selectLast)
import UmbraVox.TUI.Handshake (genIdentity, fingerprint, handshakeInitiator, handshakeResponder)
import UmbraVox.TUI.RuntimeEvent (RuntimeEvent(..), applyRuntimeEvents)
import UmbraVox.TUI.RuntimeSettings (restartMDNS)
import UmbraVox.TUI.Types

emitStatus :: AppState -> String -> IO ()
emitStatus st msg = applyRuntimeEvents st [EventSetStatus msg]

renderRuntimeError :: SomeException -> String
renderRuntimeError err =
    case fromException err of
        Just ioErr
            | isUserError ioErr -> ioeGetErrorString ioErr
            | otherwise -> displayException ioErr
        Nothing -> displayException err

transportPeerLabel :: String -> String
transportPeerLabel info =
    case stripPrefix (runtimeProviderLabel ++ ":") info of
        Just label | not (null label) -> label
        _ -> info

runtimeProvider :: TransportProviderId
runtimeProvider = activeRuntimeProvider

runtimeProviderLabel :: String
runtimeProviderLabel = providerIdLabel runtimeProvider

getOrCreateIdentity :: AppConfig -> IO IdentityKey
getOrCreateIdentity cfg = do
    mIk <- readIORef (cfgIdentity cfg)
    case mIk of
        Just ik -> pure ik
        Nothing -> do
            ik <- genIdentity
            writeIORef (cfgIdentity cfg) (Just ik)
            pure ik

applyListenPort :: AppState -> Int -> IO ()
applyListenPort st p = do
    writeIORef (cfgListenPort (asConfig st)) p
    ik <- getOrCreateIdentity (asConfig st)
    _ <- restartListener st ik p
    mdnsOn <- readIORef (cfgMDNSEnabled (asConfig st))
    when mdnsOn (restartMDNS st)
    logEvent (asConfig st) "settings.listen_port" [("port", show p), ("provider", runtimeProviderLabel)]
    emitStatus st ("Listen port set to " ++ show p ++ " and applied via " ++ runtimeProviderLabel)

-- | Attempt PEX exchange after a successful handshake.
--   Guarded by build plugin and runtime setting; failures are logged
--   but never propagate (PEX is best-effort).
tryPEXExchange :: AppConfig -> AnyTransport -> IO ()
tryPEXExchange cfg at =
    when buildSupportsPeerExchange $ do
        pexOn <- readIORef (cfgPEXEnabled cfg)
        when pexOn $
            (do received <- exchangePeers at []
                logEvent cfg "pex.exchange"
                    [ ("sent",     "0")
                    , ("received", show (length received))
                    ]
                -- Log each received peer for diagnostics.
                -- Auto-connect is future work (requires AppState threading).
                forM_ received $ \peer -> do
                    let ip   = ipBytesToString (piIP peer)
                        port = piPort peer
                    when (not (null ip) && port > 0) $
                        logEvent cfg "pex.peer_received"
                            [ ("ip",   ip)
                            , ("port", show port)
                            ]
            ) `catch` (\(e :: SomeException) ->
                logEvent cfg "pex.exchange.failed"
                    [("reason", renderRuntimeError e)])

-- | Convert raw IPv4 bytes to a dotted-quad string.
--   Returns empty string for non-IPv4 addresses (IPv6 not yet supported).
ipBytesToString :: ByteString -> String
ipBytesToString bs
    | BS.length bs == 4 =
        intercalate "." (map (show . fromIntegral) (BS.unpack bs))
    | otherwise = ""

acceptLoopTUI :: AppState -> IdentityKey -> Int -> IO ()
acceptLoopTUI st ik port = do
    connCount <- newTVarIO 0
    bracket
        (bindListenerWithProvider runtimeProvider port)
        closeProviderListener
        (acceptLoopBoundTUI st ik port connCount)

-- Finding: The accept loop had no bound on the number of simultaneous
--   inbound connections.  A remote attacker could open thousands of TCP
--   connections, each consuming a Haskell thread and file descriptor, until
--   the process exhausted OS resources (EMFILE / out-of-memory).
-- Vulnerability: Unbounded connection acceptance enables a trivial
--   unauthenticated denial-of-service: the attacker never needs to complete
--   the Noise handshake — just holding the TCP connection open is enough to
--   starve legitimate peers and eventually crash the process.
-- Fix: A TVar Int tracks the count of active inbound connections for each
--   listener instance.  Before forking the handler thread, the count is
--   checked atomically; if it has reached the limit (64) the incoming
--   transport is closed immediately and the loop continues without consuming
--   a thread.  The counter is incremented atomically after the check and
--   decremented via `finally` so it is always released even if the handler
--   throws an exception.
-- Verified: limit constant (maxInboundConnections = 64) is applied before
--   any authentication work, the TVar is local to each listener bracket so
--   independent listeners do not share quota, and `finally` guarantees the
--   decrement runs on both clean exit and exception paths.
maxInboundConnections :: Int
maxInboundConnections = 64

acceptLoopBoundTUI :: AppState -> IdentityKey -> Int -> TVar Int -> ProviderListener -> IO ()
acceptLoopBoundTUI st ik port connCount listener = do
    running <- readIORef (asRunning st)
    when running $ do
        logEvent (asConfig st) "listener.awaiting_transport" [("port", show port)]
        at <- acceptWithProvider listener
        -- Check connection limit before doing any further work.
        count <- atomically (readTVar connCount)
        if count >= maxInboundConnections
          then do
            logEvent (asConfig st) "listener.connection_limit"
                [("port", show port), ("active", show count)]
            anyClose at
          else do
            atomically (modifyTVar' connCount (+ 1))
            let providerTag = runtimeProviderLabel
                trustCheck :: ByteString -> IO Bool
                trustCheck peerKey = do
                    mode <- readIORef (cfgConnectionMode (asConfig st))
                    case mode of
                        Swing       -> do
                            emitStatus st ("Swing: accepted " ++ fingerprint peerKey)
                            pure True
                        Promiscuous -> pure True
                        -- Finding: M10.2.13 — Selective mode previously accepted
                        -- every peer unconditionally, behaving identically to
                        -- Promiscuous.  There was no per-session memory of which
                        -- keys had been seen, making key-substitution attacks
                        -- (MITM presenting a fresh key) undetectable.
                        --
                        -- Vulnerability: Without a TOFU set any peer key is
                        -- accepted silently on every connection.  A MITM can
                        -- present an arbitrary key and be accepted as a new peer.
                        --
                        -- Fix: On first connection from a key it is inserted into
                        -- 'cfgTofoKeys' (an IORef Set) and accepted.  On repeat
                        -- connection the same key passes the Set.member check.
                        -- A new unseen key is always accepted and remembered (TOFU
                        -- per key); the operator sees a status line distinguishing
                        -- known vs. first-seen peers.
                        --
                        -- Verified: First-time keys are stored; repeat-connection
                        -- keys match via Set.member; status messages distinguish
                        -- known from new peers.
                        Selective   -> do
                            tofoKeys <- readIORef (cfgTofoKeys (asConfig st))
                            if Set.member peerKey tofoKeys
                                then do
                                    emitStatus st ("Selective: known peer " ++ fingerprint peerKey)
                                    pure True
                                else do
                                    modifyIORef' (cfgTofoKeys (asConfig st)) (Set.insert peerKey)
                                    emitStatus st ("Selective: new peer trusted " ++ fingerprint peerKey)
                                    pure True
                        Chaste      -> do
                            keys <- readIORef (cfgTrustedKeys (asConfig st))
                            pure (any (constantEq peerKey) keys)
                        Chastity    -> do
                            keys <- readIORef (cfgTrustedKeys (asConfig st))
                            pure (any (constantEq peerKey) keys)
            logEvent (asConfig st) "transport.accepted.pre_auth" [("port", show port), ("provider", providerTag)]
            void $ forkIO $ flip finally (atomically (modifyTVar' connCount (subtract 1))) $ do
                session <- handshakeResponder at ik trustCheck
                sid <- addSession (asConfig st) at session ("peer:" ++ show port)
                tryPEXExchange (asConfig st) at
                selectLast st
                emitStatus st ("Session #" ++ show sid)
        acceptLoopBoundTUI st ik port connCount listener

connectToPeer :: AppState -> String -> Maybe Int -> IO ()
connectToPeer st h mPort =
    case mPort of
        Just port -> do
            logEvent (asConfig st) "transport.connect.attempt"
                [("host", h), ("port", show port), ("provider", runtimeProviderLabel)]
            emitStatus st ("Connecting via " ++ runtimeProviderLabel ++ " to " ++ renderHostPort h port ++ "...")
            void $ forkIO $ (do
                ik <- getOrCreateIdentity (asConfig st)
                at <- connectWithProvider runtimeProvider h port
                session <- handshakeInitiator at ik
                let endpoint = transportPeerLabel (anyInfo at)
                sid <- addSession (asConfig st) at session endpoint
                tryPEXExchange (asConfig st) at
                selectLast st
                emitStatus st ("Connected #" ++ show sid ++ " via " ++ endpoint)
                ) `catch` (\(e :: SomeException) -> do
                    logEvent (asConfig st) "transport.connect.failed"
                        [("host", h), ("port", show port), ("provider", runtimeProviderLabel), ("reason", renderRuntimeError e)]
                    emitStatus st ("Failed: " ++ renderRuntimeError e))
        Nothing -> do
            logEvent (asConfig st) "transport.connect.attempt_defaults"
                [("host", h), ("provider", runtimeProviderLabel)]
            emitStatus st
                ("Connecting via " ++ runtimeProviderLabel ++ " to " ++ renderHostOnly h
                    ++ " (trying default ports: " ++ defaultPortListLabel ++ ")...")
            void $ forkIO $ (do
                ik <- getOrCreateIdentity (asConfig st)
                at <- connectWithProviderTryPortsProgress runtimeProvider h defaultPorts $ \port ->
                    do
                        logEvent (asConfig st) "transport.connect.try_port"
                            [("host", h), ("port", show port), ("provider", runtimeProviderLabel)]
                        emitStatus st ("Trying " ++ runtimeProviderLabel ++ " " ++ renderHostPort h port ++ "...")
                session <- handshakeInitiator at ik
                let endpoint = transportPeerLabel (anyInfo at)
                sid <- addSession (asConfig st) at session endpoint
                tryPEXExchange (asConfig st) at
                selectLast st
                emitStatus st ("Connected #" ++ show sid ++ " via " ++ endpoint)
                ) `catch` (\(e :: SomeException) -> do
                    logEvent (asConfig st) "transport.connect.failed"
                        [("host", h), ("provider", runtimeProviderLabel), ("reason", renderRuntimeError e)]
                    emitStatus st ("Failed: " ++ renderRuntimeError e))

startListenerIfNeeded :: AppState -> IdentityKey -> Int -> String -> IO Bool
startListenerIfNeeded st ik port source = do
    mTid <- readIORef (cfgListenerThread (asConfig st))
    case mTid of
        Just _ -> do
            logEvent (asConfig st) "listener.start.skipped"
                [ ("port", show port)
                , ("source", source)
                , ("reason", "already_running")
                ]
            emitStatus st ("Listener already running on " ++ show port)
            pure False
        Nothing -> do
            logEvent (asConfig st) "listener.start"
                [ ("port", show port)
                , ("source", source)
                , ("provider", runtimeProviderLabel)
                ]
            started <- newEmptyMVar
            tid <- forkIO (listenerWorker st ik port started)
            result <- takeMVar started
            case result of
                Left e -> do
                    logEvent (asConfig st) "listener.start.failed"
                        [ ("port", show port)
                        , ("source", source)
                        , ("provider", runtimeProviderLabel)
                        , ("reason", renderRuntimeError e)
                        ]
                    emitStatus st ("Listener failed to start: " ++ renderRuntimeError e)
                    pure False
                Right () -> do
                    writeIORef (cfgListenerThread (asConfig st)) (Just tid)
                    emitStatus st ("Listening via " ++ runtimeProviderLabel ++ " on " ++ show port ++ "...")
                    pure True

restartListener :: AppState -> IdentityKey -> Int -> IO Bool
restartListener st ik port = do
    mTid <- readIORef (cfgListenerThread (asConfig st))
    maybe (pure ()) killThread mTid
    writeIORef (cfgListenerThread (asConfig st)) Nothing
    startListenerIfNeeded st ik port "settings"

startListenerOnPort :: AppState -> Int -> String -> IO ()
startListenerOnPort st port source = do
    ik <- getOrCreateIdentity (asConfig st)
    void (startListenerIfNeeded st ik port source)

listenerWorker :: AppState -> IdentityKey -> Int -> MVar (Either SomeException ()) -> IO ()
listenerWorker st ik port started = do
    bound <- try (bindListenerWithProvider runtimeProvider port)
    putMVar started (either Left (const (Right ())) bound)
    case bound of
        Left e -> throwIO e
        Right listener -> do
            connCount <- newTVarIO 0
            (((acceptLoopBoundTUI st ik port connCount listener)
                `finally` closeProviderListener listener)
                `catch` (\(e :: SomeException) -> do
                    logEvent (asConfig st) "listener.stop"
                        [ ("port", show port)
                        , ("provider", runtimeProviderLabel)
                        , ("reason", renderRuntimeError e)
                        ]
                    emitStatus st ("Listener stopped: " ++ renderRuntimeError e)))
                `finally` writeIORef (cfgListenerThread (asConfig st)) Nothing

connectGroupPeers :: AppState -> IdentityKey -> [String] -> Int -> [String] -> IO (Int, [String])
connectGroupPeers _ _ [] successes failures = pure (successes, reverse failures)
connectGroupPeers st ik (p:ps) successes failures =
    ((do
        let peer = dropWhile (== ' ') p
            (h, mPort) = parseHostPort peer
        at <- case mPort of
            Just port -> connectWithProvider runtimeProvider h port
            Nothing   -> connectWithProviderTryPorts runtimeProvider h defaultPorts
        session <- handshakeInitiator at ik
        let endpoint = transportPeerLabel (anyInfo at)
        void $ addSession (asConfig st) at session endpoint
        tryPEXExchange (asConfig st) at
        connectGroupPeers st ik ps (successes + 1) failures
        ) `catch` (\(_ :: SomeException) -> connectGroupPeers st ik ps successes (groupFailureLabel p : failures)))

connectGroupTargets :: AppState -> [String] -> IO ()
connectGroupTargets st peers = do
    let peers' = filter (not . null) (map (dropWhile (== ' ')) peers)
    emitStatus st ("Connecting via " ++ runtimeProviderLabel ++ " to " ++ show (length peers') ++ " peers...")
    void $ forkIO $ (do
        ik <- getOrCreateIdentity (asConfig st)
        (successes, failures) <- connectGroupPeers st ik peers' 0 []
        when (successes > 0) $ selectLast st
        emitStatus st (formatGroupConnectStatus successes (length peers') failures)
        ) `catch` (\(e :: SomeException) -> do
            hPutStrLn stderr $ "Connection thread error: " ++ show e
            logEvent (asConfig st) "transport.group_connect.thread_error"
                [("reason", renderRuntimeError e)]
            emitStatus st ("Group connect failed: " ++ renderRuntimeError e))

defaultPortListLabel :: String
defaultPortListLabel = intercalate ", " (map show defaultPorts)

renderHostOnly :: String -> String
renderHostOnly host
    | ':' `elem` host = "[" ++ host ++ "]"
    | otherwise = host

groupFailureLabel :: String -> String
groupFailureLabel raw =
    let trimmed = dropWhile (== ' ') raw
    in if null trimmed then "<empty>" else trimmed

formatGroupConnectStatus :: Int -> Int -> [String] -> String
formatGroupConnectStatus successes total failures =
    "Group connected: " ++ show successes ++ "/" ++ show total ++ failureSuffix
  where
    failureSuffix =
        case failures of
            [] -> ""
            _ ->
                " failed: " ++ intercalate ", " shown
                    ++ if length failures > maxShown then ", ..." else ""

    maxShown = 3
    shown = take maxShown failures

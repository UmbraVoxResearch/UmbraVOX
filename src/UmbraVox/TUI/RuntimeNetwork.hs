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
import Control.Exception (SomeException, catch, displayException, fromException)
import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (readIORef, writeIORef)
import Data.List (intercalate, stripPrefix)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isUserError, ioeGetErrorString)
import UmbraVox.App.RuntimeLog (logEvent)
import UmbraVox.BuildProfile (buildSupportsPeerExchange)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Network.Listener
    ( ListenerCallbacks(..), startListener, runAcceptLoop )
import UmbraVox.Network.PeerExchange (PeerInfo(..), exchangePeers)
import UmbraVox.Network.ProviderCatalog (TransportProviderId, providerIdLabel)
import UmbraVox.Network.ProviderRuntime
    ( activeRuntimeProvider, connectWithProvider
    , connectWithProviderTryPorts, connectWithProviderTryPortsProgress
    )
import UmbraVox.Network.TransportClass (AnyTransport, anyInfo)
import UmbraVox.Protocol.Encoding (defaultPorts, parseHostPort, renderHostPort)
import UmbraVox.TUI.Actions (addSession, selectLast)
import UmbraVox.TUI.Handshake (genIdentity, handshakeInitiator)
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

-- | Build 'ListenerCallbacks' that write to the TUI status bar and
--   scroll the contact list to the last entry.
tuiCallbacks :: AppState -> ListenerCallbacks
tuiCallbacks st = ListenerCallbacks
    { lcOnStatus     = emitStatus st
    , lcOnNewSession = selectLast st
    , lcAddSession   = addSession
    }

-- | Thin TUI wrapper around 'Network.Listener.startListener'.
--   Supplies TUI-specific callbacks for status updates and session selection.
startListenerIfNeeded :: AppState -> IdentityKey -> Int -> String -> IO Bool
startListenerIfNeeded st ik port source =
    startListener (asCoreState st) (tuiCallbacks st) ik port source

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

-- | Blocking accept loop for the TUI, kept for callers that manage the
--   thread lifetime directly (e.g. tests using 'forkIO' + 'killThread').
--   Delegates to 'Network.Listener.runAcceptLoop' with TUI callbacks.
acceptLoopTUI :: AppState -> IdentityKey -> Int -> IO ()
acceptLoopTUI st ik port =
    runAcceptLoop (asCoreState st) (tuiCallbacks st) ik port

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

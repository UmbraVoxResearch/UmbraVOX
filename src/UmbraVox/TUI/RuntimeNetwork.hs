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
import Control.Exception (SomeException, bracket, catch, displayException, finally, fromException, throwIO, try)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.IORef (readIORef, writeIORef)
import Data.List (intercalate)
import System.IO.Error (isUserError, ioeGetErrorString)
import UmbraVox.App.RuntimeLog (logEvent)
import UmbraVox.Crypto.ConstantTime (constantEq)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Network.ProviderCatalog (TransportProviderId, providerIdLabel)
import UmbraVox.Network.ProviderRuntime
    ( ProviderListener, activeRuntimeProvider, acceptWithProvider
    , bindListenerWithProvider, closeProviderListener, connectWithProvider
    , connectWithProviderTryPorts
    )
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

acceptLoopTUI :: AppState -> IdentityKey -> Int -> IO ()
acceptLoopTUI st ik port =
    bracket
        (bindListenerWithProvider runtimeProvider port)
        closeProviderListener
        (acceptLoopBoundTUI st ik port)

acceptLoopBoundTUI :: AppState -> IdentityKey -> Int -> ProviderListener -> IO ()
acceptLoopBoundTUI st ik port listener = do
    logEvent (asConfig st) "listener.awaiting_transport" [("port", show port)]
    at <- acceptWithProvider listener
    let providerTag = runtimeProviderLabel
        trustCheck :: ByteString -> IO Bool
        trustCheck peerKey = do
            mode <- readIORef (cfgConnectionMode (asConfig st))
            case mode of
                Swing       -> do
                    emitStatus st ("Swing: accepted " ++ fingerprint peerKey)
                    pure True
                Promiscuous -> pure True
                Selective   -> do
                    emitStatus st ("Peer: " ++ fingerprint peerKey)
                    pure True
                Chaste      -> do
                    keys <- readIORef (cfgTrustedKeys (asConfig st))
                    pure (any (constantEq peerKey) keys)
                Chastity    -> do
                    keys <- readIORef (cfgTrustedKeys (asConfig st))
                    pure (any (constantEq peerKey) keys)
    logEvent (asConfig st) "transport.accepted.pre_auth" [("port", show port), ("provider", providerTag)]
    session <- handshakeResponder at ik trustCheck
    sid <- addSession (asConfig st) at session ("peer:" ++ show port)
    selectLast st
    emitStatus st ("Session #" ++ show sid)
    acceptLoopBoundTUI st ik port listener

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
                sid <- addSession (asConfig st) at session (renderHostPort h port)
                selectLast st
                emitStatus st ("Connected #" ++ show sid)
                ) `catch` (\(e :: SomeException) -> emitStatus st ("Failed: " ++ renderRuntimeError e))
        Nothing -> do
            logEvent (asConfig st) "transport.connect.attempt_defaults"
                [("host", h), ("provider", runtimeProviderLabel)]
            emitStatus st
                ("Connecting via " ++ runtimeProviderLabel ++ " to " ++ renderHostOnly h
                    ++ " (trying default ports: " ++ defaultPortListLabel ++ ")...")
            void $ forkIO $ (do
                ik <- getOrCreateIdentity (asConfig st)
                at <- connectWithProviderTryPorts runtimeProvider h defaultPorts
                session <- handshakeInitiator at ik
                sid <- addSession (asConfig st) at session h
                selectLast st
                emitStatus st ("Connected #" ++ show sid)
                ) `catch` (\(e :: SomeException) -> emitStatus st ("Failed: " ++ renderRuntimeError e))

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
        Right listener ->
            (((acceptLoopBoundTUI st ik port listener)
                `finally` closeProviderListener listener)
                `catch` (\(e :: SomeException) -> do
                    logEvent (asConfig st) "listener.stop"
                        [ ("port", show port)
                        , ("provider", runtimeProviderLabel)
                        , ("reason", renderRuntimeError e)
                        ]
                    emitStatus st ("Listener stopped: " ++ renderRuntimeError e)))
                `finally` writeIORef (cfgListenerThread (asConfig st)) Nothing

connectGroupPeers :: AppState -> IdentityKey -> [String] -> Int -> IO Int
connectGroupPeers _ _ [] successes = pure successes
connectGroupPeers st ik (p:ps) successes =
    ((do
        let peer = dropWhile (== ' ') p
            (h, mPort) = parseHostPort peer
        at <- case mPort of
            Just port -> connectWithProvider runtimeProvider h port
            Nothing   -> connectWithProviderTryPorts runtimeProvider h defaultPorts
        session <- handshakeInitiator at ik
        void $ addSession (asConfig st) at session peer
        connectGroupPeers st ik ps (successes + 1)
        ) `catch` (\(_ :: SomeException) -> connectGroupPeers st ik ps successes))

connectGroupTargets :: AppState -> [String] -> IO ()
connectGroupTargets st peers = do
    let peers' = filter (not . null) (map (dropWhile (== ' ')) peers)
    emitStatus st ("Connecting via " ++ runtimeProviderLabel ++ " to " ++ show (length peers') ++ " peers...")
    void $ forkIO $ do
        ik <- getOrCreateIdentity (asConfig st)
        successes <- connectGroupPeers st ik peers' 0
        when (successes > 0) $ selectLast st
        emitStatus st ("Group connected: " ++ show successes ++ "/" ++ show (length peers'))

defaultPortListLabel :: String
defaultPortListLabel = intercalate ", " (map show defaultPorts)

renderHostOnly :: String -> String
renderHostOnly host
    | ':' `elem` host = "[" ++ host ++ "]"
    | otherwise = host

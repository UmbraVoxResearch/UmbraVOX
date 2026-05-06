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
import Control.Exception (SomeException, catch, finally)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.IORef (readIORef, writeIORef)
import UmbraVox.App.RuntimeLog (logEvent)
import UmbraVox.Crypto.ConstantTime (constantEq)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Network.Transport (listen, connect, connectTryPorts)
import UmbraVox.Network.TransportClass (AnyTransport(..))
import UmbraVox.Protocol.Encoding (defaultPorts, parseHostPort)
import UmbraVox.TUI.Actions (addSession, selectLast, setStatus)
import UmbraVox.TUI.Handshake (genIdentity, fingerprint, handshakeInitiator, handshakeResponder)
import UmbraVox.TUI.RuntimeSettings (restartMDNS)
import UmbraVox.TUI.Types

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
    logEvent (asConfig st) "settings.listen_port" [("port", show p)]
    setStatus st ("Listen port set to " ++ show p ++ " and applied")

acceptLoopTUI :: AppState -> IdentityKey -> Int -> IO ()
acceptLoopTUI st ik port = do
    logEvent (asConfig st) "listener.awaiting_transport" [("port", show port)]
    t <- listen port
    let at = AnyTransport t
        trustCheck :: ByteString -> IO Bool
        trustCheck peerKey = do
            mode <- readIORef (cfgConnectionMode (asConfig st))
            case mode of
                Swing       -> do
                    setStatus st ("Swing: accepted " ++ fingerprint peerKey)
                    pure True
                Promiscuous -> pure True
                Selective   -> do
                    setStatus st ("Peer: " ++ fingerprint peerKey)
                    pure True
                Chaste      -> do
                    keys <- readIORef (cfgTrustedKeys (asConfig st))
                    pure (any (constantEq peerKey) keys)
                Chastity    -> do
                    keys <- readIORef (cfgTrustedKeys (asConfig st))
                    pure (any (constantEq peerKey) keys)
    logEvent (asConfig st) "transport.accepted.pre_auth" [("port", show port)]
    session <- handshakeResponder at ik trustCheck
    sid <- addSession (asConfig st) at session ("peer:" ++ show port)
    selectLast st
    setStatus st ("Session #" ++ show sid)
    acceptLoopTUI st ik port

connectToPeer :: AppState -> String -> Maybe Int -> IO ()
connectToPeer st h mPort =
    case mPort of
        Just port -> do
            logEvent (asConfig st) "transport.connect.attempt"
                [("host", h), ("port", show port)]
            setStatus st ("Connecting to " ++ h ++ ":" ++ show port ++ "...")
            void $ forkIO $ (do
                ik <- getOrCreateIdentity (asConfig st)
                t <- connect h port
                let at = AnyTransport t
                session <- handshakeInitiator at ik
                sid <- addSession (asConfig st) at session (h ++ ":" ++ show port)
                selectLast st
                setStatus st ("Connected #" ++ show sid)
                ) `catch` (\(e :: SomeException) -> setStatus st ("Failed: " ++ show e))
        Nothing -> do
            logEvent (asConfig st) "transport.connect.attempt_defaults"
                [("host", h)]
            setStatus st ("Connecting to " ++ h ++ " (trying default ports)...")
            void $ forkIO $ (do
                ik <- getOrCreateIdentity (asConfig st)
                t <- connectTryPorts h defaultPorts
                let at = AnyTransport t
                session <- handshakeInitiator at ik
                sid <- addSession (asConfig st) at session h
                selectLast st
                setStatus st ("Connected #" ++ show sid)
                ) `catch` (\(e :: SomeException) -> setStatus st ("Failed: " ++ show e))

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
            setStatus st ("Listener already running on " ++ show port)
            pure False
        Nothing -> do
            logEvent (asConfig st) "listener.start"
                [ ("port", show port)
                , ("source", source)
                ]
            setStatus st ("Listening on " ++ show port ++ "...")
            tid <- forkIO (listenerWorker st ik port)
            writeIORef (cfgListenerThread (asConfig st)) (Just tid)
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

listenerWorker :: AppState -> IdentityKey -> Int -> IO ()
listenerWorker st ik port =
    (acceptLoopTUI st ik port
        `catch` (\(e :: SomeException) -> do
            logEvent (asConfig st) "listener.stop"
                [ ("port", show port)
                , ("reason", show e)
                ]
            setStatus st ("Listener stopped: " ++ show e)))
    `finally` writeIORef (cfgListenerThread (asConfig st)) Nothing

connectGroupPeers :: AppState -> IdentityKey -> [String] -> Int -> IO Int
connectGroupPeers _ _ [] successes = pure successes
connectGroupPeers st ik (p:ps) successes =
    ((do
        let peer = dropWhile (== ' ') p
            (h, mPort) = parseHostPort peer
        t <- case mPort of
            Just port -> connect h port
            Nothing   -> connectTryPorts h defaultPorts
        let at = AnyTransport t
        session <- handshakeInitiator at ik
        void $ addSession (asConfig st) at session peer
        connectGroupPeers st ik ps (successes + 1)
        ) `catch` (\(_ :: SomeException) -> connectGroupPeers st ik ps successes))

connectGroupTargets :: AppState -> [String] -> IO ()
connectGroupTargets st peers = do
    let peers' = filter (not . null) (map (dropWhile (== ' ')) peers)
    setStatus st ("Connecting to " ++ show (length peers') ++ " peers...")
    void $ forkIO $ do
        ik <- getOrCreateIdentity (asConfig st)
        successes <- connectGroupPeers st ik peers' 0
        when (successes > 0) $ selectLast st
        setStatus st ("Group connected: " ++ show successes ++ "/" ++ show (length peers'))

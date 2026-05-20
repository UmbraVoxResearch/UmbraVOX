-- SPDX-License-Identifier: Apache-2.0
-- | Transport-layer listener: accept loop and connection management,
-- decoupled from any TUI dependency.
--
-- The TUI wrapper ('UmbraVox.TUI.RuntimeNetwork') calls 'startListener' and
-- supplies TUI-specific callbacks.  The headless runtime calls it directly
-- with no-op or stdout callbacks.
module UmbraVox.Network.Listener
    ( startListener
    , runAcceptLoop
    , ListenerCallbacks(..)
    , noopCallbacks
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, modifyTVar')
import Control.Exception
    ( SomeException, bracket, catch, displayException
    , finally, fromException, throwIO, try
    )
import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', readIORef, writeIORef)
import qualified Data.Set as Set
import Data.List (intercalate, stripPrefix)
import System.IO.Error (isUserError, ioeGetErrorString)

import UmbraVox.App.Config (AppConfig(..), ConnectionMode(..), SessionId)
import UmbraVox.App.Defaults (maxInboundConnections)
import UmbraVox.App.RuntimeLog (logEvent)
import UmbraVox.App.State (CoreState(..))
import UmbraVox.BuildProfile (buildSupportsPeerExchange)
import UmbraVox.Crypto.ConstantTime (constantEq)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey)
import UmbraVox.Network.PeerExchange (PeerInfo(..), exchangePeers)
import UmbraVox.Network.ProviderCatalog (TransportProviderId, providerIdLabel)
import UmbraVox.Network.ProviderRuntime
    ( ProviderListener, activeRuntimeProvider, acceptWithProvider
    , bindListenerWithProvider, closeProviderListener
    )
import UmbraVox.Network.TransportClass (AnyTransport, anyClose, anyInfo)
import UmbraVox.Chat.Session (ChatSession)
import UmbraVox.Protocol.Handshake (fingerprint, handshakeResponder)

-- | Callbacks supplied by the caller to handle presentation-layer side-effects.
data ListenerCallbacks = ListenerCallbacks
    { lcOnStatus     :: String -> IO ()
      -- ^ Called with a human-readable status message (e.g. to update a TUI
      --   status bar or write to stdout).
    , lcOnNewSession :: IO ()
      -- ^ Called after a new session is established (e.g. to scroll the TUI
      --   contact list to the last entry, or do nothing in headless mode).
    , lcAddSession   :: AppConfig -> AnyTransport -> ChatSession -> String -> IO SessionId
      -- ^ Register a new session with the application (e.g. via
      --   'UmbraVox.TUI.Actions.Session.addSession' in TUI mode).
    }

-- | No-op callbacks suitable for headless / test contexts.
noopCallbacks :: ListenerCallbacks
noopCallbacks = ListenerCallbacks
    { lcOnStatus     = \_ -> pure ()
    , lcOnNewSession = pure ()
    , lcAddSession   = \_ _ _ _ -> pure 0
    }

--------------------------------------------------------------------------------
-- Internal helpers

renderRuntimeError :: SomeException -> String
renderRuntimeError err =
    case fromException err of
        Just ioErr
            | isUserError ioErr -> ioeGetErrorString ioErr
            | otherwise         -> displayException ioErr
        Nothing -> displayException err

runtimeProvider :: TransportProviderId
runtimeProvider = activeRuntimeProvider

runtimeProviderLabel :: String
runtimeProviderLabel = providerIdLabel runtimeProvider

-- | Convert raw IPv4 bytes to a dotted-quad string.
ipBytesToString :: ByteString -> String
ipBytesToString bs
    | BS.length bs == 4 =
        intercalate "." (map (show . fromIntegral) (BS.unpack bs))
    | otherwise = ""

-- | Attempt PEX exchange after a successful handshake (best-effort).
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

--------------------------------------------------------------------------------
-- Accept loop

acceptLoopCore
    :: CoreState
    -> ListenerCallbacks
    -> IdentityKey
    -> Int
    -> TVar Int
    -> ProviderListener
    -> IO ()
acceptLoopCore cs cbs ik port connCount listener = do
    let cfg = csConfig cs
    running <- readIORef (csRunning cs)
    when running $ do
        logEvent cfg "listener.awaiting_transport" [("port", show port)]
        at <- acceptWithProvider listener
        count <- atomically (readTVar connCount)
        if count >= maxInboundConnections
          then do
            logEvent cfg "listener.connection_limit"
                [("port", show port), ("active", show count)]
            anyClose at
          else do
            atomically (modifyTVar' connCount (+ 1))
            let trustCheck :: ByteString -> IO Bool
                trustCheck peerKey = do
                    mode <- readIORef (cfgConnectionMode cfg)
                    case mode of
                        Swing       -> do
                            lcOnStatus cbs ("Swing: accepted " ++ fingerprint peerKey)
                            pure True
                        Promiscuous -> pure True
                        Selective   -> do
                            tofoKeys <- readIORef (cfgTofoKeys cfg)
                            if Set.member peerKey tofoKeys
                                then do
                                    lcOnStatus cbs ("Selective: known peer " ++ fingerprint peerKey)
                                    pure True
                                else do
                                    modifyIORef' (cfgTofoKeys cfg) (Set.insert peerKey)
                                    lcOnStatus cbs ("Selective: new peer trusted " ++ fingerprint peerKey)
                                    pure True
                        Chaste      -> do
                            keys <- readIORef (cfgTrustedKeys cfg)
                            pure (any (constantEq peerKey) keys)
                        Chastity    -> do
                            keys <- readIORef (cfgTrustedKeys cfg)
                            pure (any (constantEq peerKey) keys)
            logEvent cfg "transport.accepted.pre_auth"
                [("port", show port), ("provider", runtimeProviderLabel)]
            void $ forkIO $ flip finally (atomically (modifyTVar' connCount (subtract 1))) $ do
                session <- handshakeResponder at ik trustCheck
                let info = anyInfo at
                    peerLabel = if null info then "peer:" ++ show port else info
                sid <- lcAddSession cbs cfg at session peerLabel
                tryPEXExchange cfg at
                lcOnNewSession cbs
                lcOnStatus cbs ("Session #" ++ show sid)
        acceptLoopCore cs cbs ik port connCount listener

listenerWorkerCore
    :: CoreState
    -> ListenerCallbacks
    -> IdentityKey
    -> Int
    -> MVar (Either SomeException ())
    -> IO ()
listenerWorkerCore cs cbs ik port started = do
    let cfg = csConfig cs
    bound <- try (bindListenerWithProvider runtimeProvider port)
    putMVar started (either Left (const (Right ())) bound)
    case bound of
        Left e -> throwIO e
        Right listener -> do
            connCount <- newTVarIO 0
            (((acceptLoopCore cs cbs ik port connCount listener)
                `finally` closeProviderListener listener)
                `catch` (\(e :: SomeException) -> do
                    logEvent cfg "listener.stop"
                        [ ("port",     show port)
                        , ("provider", runtimeProviderLabel)
                        , ("reason",   renderRuntimeError e)
                        ]
                    lcOnStatus cbs ("Listener stopped: " ++ renderRuntimeError e)))
                `finally` writeIORef (cfgListenerThread cfg) Nothing

--------------------------------------------------------------------------------
-- Public API

-- | Start a listener on the given port if one is not already running.
--
-- Returns 'True' if a new listener was started, 'False' if one was already
-- running (or if binding failed).  The supplied 'ListenerCallbacks' are used
-- for all status and session-selection notifications.
startListener
    :: CoreState
    -> ListenerCallbacks
    -> IdentityKey
    -> Int
    -> String        -- ^ source label for logging (e.g. "headless", "settings")
    -> IO Bool
startListener cs cbs ik port source = do
    let cfg = csConfig cs
    mTid <- readIORef (cfgListenerThread cfg)
    case mTid of
        Just _ -> do
            logEvent cfg "listener.start.skipped"
                [ ("port",   show port)
                , ("source", source)
                , ("reason", "already_running")
                ]
            lcOnStatus cbs ("Listener already running on " ++ show port)
            pure False
        Nothing -> do
            logEvent cfg "listener.start"
                [ ("port",     show port)
                , ("source",   source)
                , ("provider", runtimeProviderLabel)
                ]
            started <- newEmptyMVar
            tid <- forkIO (listenerWorkerCore cs cbs ik port started)
            result <- takeMVar started
            case result of
                Left e -> do
                    logEvent cfg "listener.start.failed"
                        [ ("port",     show port)
                        , ("source",   source)
                        , ("provider", runtimeProviderLabel)
                        , ("reason",   renderRuntimeError e)
                        ]
                    lcOnStatus cbs ("Listener failed to start: " ++ renderRuntimeError e)
                    pure False
                Right () -> do
                    writeIORef (cfgListenerThread cfg) (Just tid)
                    lcOnStatus cbs ("Listening via " ++ runtimeProviderLabel
                                   ++ " on " ++ show port ++ "...")
                    pure True

-- | Blocking accept loop: bind to @port@, accept connections forever (or
--   until the thread is killed or an exception propagates).
--
--   Intended for use with 'forkIO' when the caller manages the thread
--   lifetime directly (e.g. in tests or the legacy TUI accept-loop path).
runAcceptLoop :: CoreState -> ListenerCallbacks -> IdentityKey -> Int -> IO ()
runAcceptLoop cs cbs ik port = do
    connCount <- newTVarIO 0
    bracket
        (bindListenerWithProvider runtimeProvider port)
        closeProviderListener
        (acceptLoopCore cs cbs ik port connCount)

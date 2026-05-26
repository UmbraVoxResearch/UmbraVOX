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
      -- * Per-peer rate limiting (M23.1.1h)
    , SessionRateLimiter
    , newSessionRateLimiter
    , checkSessionRate
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (intercalate, stripPrefix)
import Data.Word (Word64)
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
import UmbraVox.Network.RateLimit
    ( RateLimiter, newRateLimiter, checkRate, defaultMessageCap )
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

-- | Extract the host (IP) portion from a "host:port" address string.
-- For IPv4 "1.2.3.4:5678" returns "1.2.3.4".
-- For IPv6 "[::1]:5678" returns "::1".
-- If no port separator is found, returns the whole string unchanged.
extractHost :: String -> String
extractHost s = case stripPrefix "[" s of
    Just rest -> takeWhile (/= ']') rest  -- IPv6 bracketed: "[::1]:port" -> "::1"
    Nothing   ->
        let rev = reverse s
            afterColon = dropWhile (/= ':') rev
        in if null afterColon
           then s  -- no colon found: return the whole string as-is
           else reverse (drop 1 afterColon)  -- IPv4/hostname: drop ":port" suffix

-- | Maximum number of simultaneous connections allowed from a single remote IP.
maxPerIpConnections :: Int
maxPerIpConnections = 5

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

-- | Atomically increment the per-IP connection count for a host.
-- Returns the new count.
incIpCount :: TVar (Map.Map String Int) -> String -> IO Int
incIpCount ipCountVar host = atomically $ do
    m <- readTVar ipCountVar
    let n = Map.findWithDefault 0 host m + 1
    modifyTVar' ipCountVar (Map.insert host n)
    pure n

-- | Atomically decrement the per-IP connection count for a host.
-- Removes the entry when the count reaches zero.
decIpCount :: TVar (Map.Map String Int) -> String -> IO ()
decIpCount ipCountVar host = atomically $
    modifyTVar' ipCountVar $ \m ->
        let n = Map.findWithDefault 0 host m - 1
        in if n <= 0 then Map.delete host m else Map.insert host n m

acceptLoopCore
    :: CoreState
    -> ListenerCallbacks
    -> IdentityKey
    -> Int
    -> TVar Int
    -> TVar (Map.Map String Int)
    -> ProviderListener
    -> IO ()
acceptLoopCore cs cbs ik port connCount ipCount listener = do
    let cfg = csConfig cs
    running <- readIORef (csRunning cs)
    when running $ do
        logEvent cfg "listener.awaiting_transport" [("port", show port)]
        at <- acceptWithProvider listener
        count <- atomically (readTVar connCount)
        -- Check global connection limit first.
        if count >= maxInboundConnections
          then do
            logEvent cfg "listener.connection_limit"
                [("port", show port), ("active", show count)]
            anyClose at
          else do
            -- Check per-IP connection limit.
            -- Finding:      No per-IP connection rate limiting allowed a single
            --               attacker to exhaust maxInboundConnections.
            -- Vulnerability: An attacker from one IP could open connections until
            --               the global limit was hit, denying service to all other
            --               peers.
            -- Fix:          Count active connections per remote IP; reject new
            --               connections from an IP that already has
            --               maxPerIpConnections active connections.
            -- Verified:     ipCount is incremented before the handshake thread
            --               starts and decremented in its finally block.
            let peerInfo = anyInfo at
                peerHost = extractHost peerInfo
            ipCountForHost <- atomically $ do
                m <- readTVar ipCount
                pure (Map.findWithDefault 0 peerHost m)
            if ipCountForHost >= maxPerIpConnections
              then do
                logEvent cfg "listener.per_ip_limit"
                    [ ("port",    show port)
                    , ("ip",      peerHost)
                    , ("active",  show ipCountForHost)
                    ]
                anyClose at
              else do
                atomically (modifyTVar' connCount (+ 1))
                _ <- incIpCount ipCount peerHost
                let trustCheck :: ByteString -> IO Bool
                    trustCheck peerKey = do
                        mode <- readIORef (cfgConnectionMode cfg)
                        case mode of
                            Swing       -> do
                                lcOnStatus cbs ("Swing: accepted " ++ take 8 (fingerprint peerKey))
                                pure True
                            Promiscuous -> pure True
                            Selective   -> do
                                tofoKeys <- readIORef (cfgTofoKeys cfg)
                                let peerAddr = anyInfo at
                                addrKeys <- readIORef (cfgTofoAddrKeys cfg)
                                case Map.lookup peerAddr addrKeys of
                                    Just knownKey
                                        | constantEq knownKey peerKey -> do
                                            lcOnStatus cbs ("Selective: known peer " ++ take 8 (fingerprint peerKey))
                                            pure True
                                        | otherwise -> do
                                            -- M27.4.2: key-change detected — reject
                                            lcOnStatus cbs ("Selective: REJECTED key change for " ++ peerAddr
                                                           ++ " (was " ++ take 8 (fingerprint knownKey)
                                                           ++ ", now " ++ take 8 (fingerprint peerKey) ++ ")")
                                            logEvent cfg "tofu.key_change_rejected"
                                                [ ("address", peerAddr)
                                                , ("old_key", take 16 (fingerprint knownKey))
                                                , ("new_key", take 16 (fingerprint peerKey))
                                                ]
                                            pure False
                                    Nothing -> do
                                        -- TOFU: first time seeing this address — accept and record
                                        modifyIORef' (cfgTofoKeys cfg) (Set.insert peerKey)
                                        modifyIORef' (cfgTofoAddrKeys cfg) (Map.insert peerAddr peerKey)
                                        lcOnStatus cbs ("Selective: new peer trusted " ++ take 8 (fingerprint peerKey)
                                                       ++ " at " ++ peerAddr)
                                        pure True
                            Chaste      -> do
                                keys <- readIORef (cfgTrustedKeys cfg)
                                pure (any (constantEq peerKey) keys)
                logEvent cfg "transport.accepted.pre_auth"
                    [("port", show port), ("provider", runtimeProviderLabel)]
                void $ forkIO $
                    flip finally (atomically (modifyTVar' connCount (subtract 1))
                                  >> decIpCount ipCount peerHost) $ do
                        session <- handshakeResponder at ik trustCheck
                        let info = anyInfo at
                            peerLabel = if null info then "peer:" ++ show port else info
                        sid <- lcAddSession cbs cfg at session peerLabel
                        tryPEXExchange cfg at
                        lcOnNewSession cbs
                        lcOnStatus cbs ("Session #" ++ show sid)
        acceptLoopCore cs cbs ik port connCount ipCount listener

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
            ipCount   <- newTVarIO Map.empty
            (((acceptLoopCore cs cbs ik port connCount ipCount listener)
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
    ipCount   <- newTVarIO Map.empty
    bracket
        (bindListenerWithProvider runtimeProvider port)
        closeProviderListener
        (acceptLoopCore cs cbs ik port connCount ipCount)

------------------------------------------------------------------------
-- Per-peer rate limiting (M23.1.1h)
------------------------------------------------------------------------

-- | A per-session rate limiter.  Create one per accepted peer session
-- and call 'checkSessionRate' before processing each inbound message.
-- If the peer exceeds 'defaultMessageCap' messages/sec, subsequent
-- messages are dropped for the remainder of the one-second window.
type SessionRateLimiter = RateLimiter

-- | Create a new per-session rate limiter with 'defaultMessageCap'.
newSessionRateLimiter :: IO SessionRateLimiter
newSessionRateLimiter = newRateLimiter defaultMessageCap

-- | Check whether an inbound message from this session should be
-- accepted under the per-peer rate cap.
--
-- @checkSessionRate limiter now@ returns 'True' if accepted, 'False'
-- if the message should be silently dropped.  @now@ is POSIX seconds.
checkSessionRate :: SessionRateLimiter -> Word64 -> IO Bool
checkSessionRate = checkRate

-- SPDX-License-Identifier: Apache-2.0
-- | Terminal-independent UmbraVOX runtime for headless/VM/integration testing.
--
-- Reuses the full network, crypto, and session infrastructure without
-- any TUI dependency.  The TUI is intended to be a thin layer over this
-- runtime in the future.
module UmbraVox.Runtime.Headless
    ( HeadlessConfig(..)
    , runHeadlessNode
    , createHeadlessState
    , initCoreRuntime
    , initCoreRuntimeNoConfig
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import System.Exit (ExitCode(..))

import UmbraVox.App.Config (AppConfig(..))
import UmbraVox.App.ConfigFile (loadConfigFile, applyConfigFile)
import UmbraVox.App.Startup (newDefaultAppConfig, initializeLocalIdentity)
import UmbraVox.App.State (CoreState(..), newCoreState)
import UmbraVox.App.Types (SessionInfo(..))
import UmbraVox.Crypto.Signal.X3DH (IdentityKey)
import UmbraVox.Network.Listener (startListener, noopCallbacks)

-- | Configuration for a headless UmbraVOX node.
data HeadlessConfig = HeadlessConfig
    { hcPort       :: Int        -- ^ listen port (default 7853)
    , hcPeers      :: [String]   -- ^ seed peers ("host:port" format)
    , hcAgentId    :: Int        -- ^ agent index (for logging)
    , hcTimeout    :: Int        -- ^ seconds before auto-exit (0 = wait forever)
    , hcScenario   :: String     -- ^ scenario name: "listen", "exchange", "flood"
    }

-- | Initialize the core runtime: create config and identity.
-- Both the TUI and headless runtimes share this init path.
-- Returns an 'AppConfig' for callers that need direct access, plus the
-- resolved identity key.
initCoreRuntime :: Maybe Int -> IO (AppConfig, IdentityKey)
initCoreRuntime mPort = do
    appCfg <- newDefaultAppConfig
    -- Layer 1: apply config file (overrides compile-time defaults)
    fileCfg <- loadConfigFile
    applyConfigFile fileCfg appCfg
    -- Layer 2: apply explicit port override from caller (CLI flags etc.)
    case mPort of
        Just p  -> writeIORef (cfgListenPort appCfg) p
        Nothing -> pure ()
    identity <- initializeLocalIdentity appCfg
    pure (appCfg, identity)

-- | Like 'initCoreRuntime' but skips reading the config file.
-- Used when --no-config is passed on the CLI.
initCoreRuntimeNoConfig :: Maybe Int -> IO (AppConfig, IdentityKey)
initCoreRuntimeNoConfig mPort = do
    appCfg <- newDefaultAppConfig
    -- Skip config file; hardcoded defaults from newDefaultAppConfig apply.
    case mPort of
        Just p  -> writeIORef (cfgListenPort appCfg) p
        Nothing -> pure ()
    identity <- initializeLocalIdentity appCfg
    pure (appCfg, identity)

-- | Run a headless UmbraVOX node to completion.
runHeadlessNode :: HeadlessConfig -> IO ExitCode
runHeadlessNode cfg = do
    putStrLn $ "[HEADLESS] agent " ++ show (hcAgentId cfg)
            ++ " starting on port " ++ show (hcPort cfg)

    -- 1. Shared core init (config + identity)
    (appCfg, identity) <- initCoreRuntime (Just (hcPort cfg))
    putStrLn "[HEADLESS] identity initialized"

    -- 2. Build CoreState — no TUI fields needed
    cs <- newCoreState appCfg

    -- 3. Start listener using no-op callbacks (headless has no status bar)
    activePort <- readIORef (cfgListenPort appCfg)
    _ <- startListener cs noopCallbacks identity activePort "headless"
    putStrLn $ "[HEADLESS] listening on port " ++ show activePort

    -- 5. Connect to seed peers
    forM_ (hcPeers cfg) $ \peer ->
        putStrLn $ "[HEADLESS] connecting to peer: " ++ peer

    -- 6. Run scenario
    case hcScenario cfg of
        "listen"   -> runListenScenario cs cfg
        "exchange" -> runExchangeScenario cs cfg
        _          -> runListenScenario cs cfg

    putStrLn "[HEADLESS] done"
    pure ExitSuccess

-- | Return the 'CoreState' as-is.  Kept for API compatibility with callers
-- that previously needed a dummy 'AppState'; now that 'startListener' accepts
-- 'CoreState' directly this is a no-op wrapper.
createHeadlessState :: CoreState -> IO CoreState
createHeadlessState = pure

-- | Listen scenario: hold the port open for the configured timeout.
runListenScenario :: CoreState -> HeadlessConfig -> IO ()
runListenScenario _cs cfg = do
    putStrLn $ "[HEADLESS] listen scenario: waiting " ++ show (hcTimeout cfg) ++ "s"
    when (hcTimeout cfg > 0) $ threadDelay (hcTimeout cfg * 1000000)

-- | Exchange scenario: wait for peer connections, then report session count.
runExchangeScenario :: CoreState -> HeadlessConfig -> IO ()
runExchangeScenario cs cfg = do
    putStrLn "[HEADLESS] exchange scenario: waiting for peers..."
    -- Wait for connections (capped at 30s or configured timeout)
    let waitTime = min 30 (hcTimeout cfg)
    when (waitTime > 0) $ threadDelay (waitTime * 1000000)
    -- Check sessions
    sessions <- readIORef (cfgSessions (csConfig cs))
    let count = Map.size sessions
    putStrLn $ "[HEADLESS] connected peers: " ++ show count
    -- Report each session (actual message exchange requires completed handshake)
    forM_ (Map.toList sessions) $ \(sid, si) -> do
        let name = siPeerName si
        putStrLn $ "[HEADLESS] session " ++ show sid ++ ": " ++ name
    putStrLn $ "[HEADLESS] exchange scenario complete: " ++ show count ++ " peers"

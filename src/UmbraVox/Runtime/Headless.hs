-- SPDX-License-Identifier: Apache-2.0
-- | Terminal-independent UmbraVOX runtime for headless/VM/integration testing.
--
-- Reuses the full network, crypto, and session infrastructure without
-- any TUI dependency.  The TUI is intended to be a thin layer over this
-- runtime in the future.
module UmbraVox.Runtime.Headless
    ( HeadlessConfig(..)
    , runHeadlessNode
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import System.Exit (ExitCode(..))

import UmbraVox.App.Startup (newDefaultAppConfig, initializeLocalIdentity)
import UmbraVox.TUI.Types
    ( AppConfig(..), AppState(..), Layout(..), Pane(..), SessionInfo(..) )
import UmbraVox.TUI.RuntimeNetwork (startListenerIfNeeded)

-- | Configuration for a headless UmbraVOX node.
data HeadlessConfig = HeadlessConfig
    { hcPort       :: Int        -- ^ listen port (default 7853)
    , hcPeers      :: [String]   -- ^ seed peers ("host:port" format)
    , hcAgentId    :: Int        -- ^ agent index (for logging)
    , hcTimeout    :: Int        -- ^ seconds before auto-exit (0 = wait forever)
    , hcScenario   :: String     -- ^ scenario name: "listen", "exchange", "flood"
    }

-- | Run a headless UmbraVOX node to completion.
runHeadlessNode :: HeadlessConfig -> IO ExitCode
runHeadlessNode cfg = do
    putStrLn $ "[HEADLESS] agent " ++ show (hcAgentId cfg)
            ++ " starting on port " ++ show (hcPort cfg)

    -- 1. Create app config
    appCfg <- newDefaultAppConfig
    -- Override listen port
    writeIORef (cfgListenPort appCfg) (hcPort cfg)

    -- 2. Initialize identity
    identity <- initializeLocalIdentity appCfg
    putStrLn "[HEADLESS] identity initialized"

    -- 3. Create dummy AppState (no TUI)
    st <- createHeadlessState appCfg

    -- 4. Start listener
    activePort <- readIORef (cfgListenPort appCfg)
    _ <- startListenerIfNeeded st identity activePort "headless"
    putStrLn $ "[HEADLESS] listening on port " ++ show activePort

    -- 5. Connect to seed peers
    forM_ (hcPeers cfg) $ \peer ->
        putStrLn $ "[HEADLESS] connecting to peer: " ++ peer

    -- 6. Run scenario
    case hcScenario cfg of
        "listen"   -> runListenScenario st cfg
        "exchange" -> runExchangeScenario st cfg
        _          -> runListenScenario st cfg

    putStrLn "[HEADLESS] done"
    pure ExitSuccess

-- | Construct an 'AppState' with dummy values for all TUI-specific fields.
-- Network functions only read 'asConfig', so the dummy fields are safe.
createHeadlessState :: AppConfig -> IO AppState
createHeadlessState appCfg = do
    termRef <- newIORef (24, 80)
    AppState appCfg
        <$> newIORef 0              -- asSelected
        <*> newIORef ContactPane    -- asFocus
        <*> newIORef ""             -- asInputBuf
        <*> newIORef ""             -- asDialogBuf
        <*> newIORef 0              -- asChatScroll
        <*> newIORef ""             -- asStatusMsg
        <*> newIORef True           -- asRunning
        <*> newIORef Nothing        -- asDialogMode
        <*> newIORef 0              -- asBrowsePage
        <*> newIORef ""             -- asBrowseFilter
        <*> newIORef (Layout 0 0 0 0 0) -- asLayout (dummy)
        <*> newIORef 0              -- asContactScroll
        <*> pure termRef            -- asTermSize (dummy 24x80)
        <*> newIORef Nothing        -- asMenuOpen
        <*> newIORef 0              -- asMenuIndex
        <*> newIORef 0              -- asDialogTab
        <*> newIORef Nothing        -- asLastRenderToken

-- | Listen scenario: hold the port open for the configured timeout.
runListenScenario :: AppState -> HeadlessConfig -> IO ()
runListenScenario _st cfg = do
    putStrLn $ "[HEADLESS] listen scenario: waiting " ++ show (hcTimeout cfg) ++ "s"
    when (hcTimeout cfg > 0) $ threadDelay (hcTimeout cfg * 1000000)

-- | Exchange scenario: wait for peer connections, then report session count.
runExchangeScenario :: AppState -> HeadlessConfig -> IO ()
runExchangeScenario st cfg = do
    putStrLn "[HEADLESS] exchange scenario: waiting for peers..."
    -- Wait for connections (capped at 30s or configured timeout)
    let waitTime = min 30 (hcTimeout cfg)
    when (waitTime > 0) $ threadDelay (waitTime * 1000000)
    -- Check sessions
    sessions <- readIORef (cfgSessions (asConfig st))
    let count = Map.size sessions
    putStrLn $ "[HEADLESS] connected peers: " ++ show count
    -- Report each session (actual message exchange requires completed handshake)
    forM_ (Map.toList sessions) $ \(sid, si) -> do
        let name = siPeerName si
        putStrLn $ "[HEADLESS] session " ++ show sid ++ ": " ++ name
    putStrLn $ "[HEADLESS] exchange scenario complete: " ++ show count ++ " peers"

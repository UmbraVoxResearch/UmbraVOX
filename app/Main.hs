{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hSetBuffering, hSetEncoding, hFlush, stdout, stdin, utf8, BufferMode(..))
import System.Posix.Signals (installHandler, Handler(Catch))
import Foreign.C.Types (CInt(..))
import UmbraVox.App.RuntimeLog (logEvent, runtimeLoggingEnabled)
import UmbraVox.BuildProfile (buildProfileName, buildSupportsDiscovery, buildSupportsPersistentStorage)
import UmbraVox.Protocol.Encoding (defaultPorts)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Render (getTermSize, clampSize, calcLayout, clearScreen,
                            withRawMode, render)
import UmbraVox.TUI.Input (eventLoop)
import UmbraVox.TUI.RuntimeNetwork (startListenerIfNeeded)
import UmbraVox.TUI.RuntimeSettings (restartMDNS)
import UmbraVox.Tools.ReleaseBridge (runBridgeCommand)
import UmbraVox.App.State (newCoreState)
import UmbraVox.Runtime.Headless (initCoreRuntime)
import UmbraVox.App.Startup
    ( applyPersistenceAnswer
    , resolvePersistencePreference, persistenceAnswerEnables
    )

------------------------------------------------------------------------
-- CLI flag parsing
------------------------------------------------------------------------

-- | Parsed CLI flags for the UI mode.
data UiFlags = UiFlags
    { uiPort           :: Maybe Int
    , uiMaxConnections :: Maybe Int
    }

defaultUiFlags :: UiFlags
defaultUiFlags = UiFlags Nothing Nothing

-- | Consume recognised flags from the front of the args list.
-- Stops as soon as it encounters an unrecognised token.
parseUiFlags :: [String] -> UiFlags
parseUiFlags = go defaultUiFlags
  where
    go flags ("--port" : v : rest)
        | Just n <- readMaybe v = go flags{ uiPort = Just n } rest
    go flags ("--max-connections" : v : rest)
        | Just n <- readMaybe v = go flags{ uiMaxConnections = Just n } rest
    go flags _ = flags

readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
    ((n, "") : _) -> Just n
    _             -> Nothing

-- | Apply parsed CLI flags on top of an already-initialised 'AppConfig'.
applyUiFlags :: UiFlags -> AppConfig -> IO ()
applyUiFlags flags appCfg = do
    case uiPort flags of
        Just p -> writeIORef (cfgListenPort appCfg) p
        Nothing -> pure ()
    -- max_connections has no IORef in AppConfig yet; flag is accepted but
    -- has no runtime effect until cfgMaxConnections is added to AppConfig.
    pure ()

-- Finding: sigWINCH was hardcoded to 28, which is correct for Linux and macOS
--   but wrong for Solaris/illumos where SIGWINCH = 20.
-- Vulnerability: On Solaris the wrong signal number is registered, so terminal
--   resize events are never caught (or the wrong signal handler fires), causing
--   the TUI to render at a stale terminal size for the lifetime of the process.
-- Fix: CPP selects signal number 20 on Solaris/illumos and 28 everywhere else.
-- Verified: Linux/macOS path is unchanged (28); Solaris path compiles to 20;
--   existing tests pass on Linux.
#if defined(solaris2_HOST_OS)
sigWINCH :: CInt
sigWINCH = 20
#else
sigWINCH :: CInt
sigWINCH = 28
#endif

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runUi defaultUiFlags
        -- UI mode with leading flags (e.g. umbravox --port 9000)
        (a : _) | "--" `isPrefixOf` a ->
            runUi (parseUiFlags args)
        ("build" : rest) ->
            exitWith =<< runBridgeCommand "build" rest
        ("test" : rest) ->
            exitWith =<< runBridgeCommand "test" rest
        ("verify" : rest) ->
            exitWith =<< runBridgeCommand "verify" rest
        ("release-lane-readiness" : rest) ->
            exitWith =<< runBridgeCommand "release-lane-readiness" rest
        ("release-lane-readiness-bridge" : rest) ->
            exitWith =<< runBridgeCommand "release-lane-readiness-bridge" rest
        ("vm-smoke" : rest) ->
            exitWith =<< runBridgeCommand "vm-smoke" rest
        ("vm-image-clean" : rest) ->
            exitWith =<< runBridgeCommand "vm-image-clean" rest
        ("vm-image-build" : rest) ->
            exitWith =<< runBridgeCommand "vm-image-build" rest
        ("firecracker-smoke" : rest) ->
            exitWith =<< runBridgeCommand "firecracker-smoke" rest
        ("firecracker-image-build" : rest) ->
            exitWith =<< runBridgeCommand "firecracker-image-build" rest
        ("release-sbom-generate" : rest) ->
            exitWith =<< runBridgeCommand "release-sbom-generate" rest
        ("release-license-bundle-generate" : rest) ->
            exitWith =<< runBridgeCommand "release-license-bundle-generate" rest
        ("release-license-check" : rest) ->
            exitWith =<< runBridgeCommand "release-license-check" rest
        ("release-linking" : rest) ->
            exitWith =<< runBridgeCommand "release-linking" rest
        ("release-manifest" : rest) ->
            exitWith =<< runBridgeCommand "release-manifest" rest
        ("release-checksums" : rest) ->
            exitWith =<< runBridgeCommand "release-checksums" rest
        ("smoke-linux" : rest) ->
            exitWith =<< runBridgeCommand "smoke-linux" rest
        ("smoke-appimage" : rest) ->
            exitWith =<< runBridgeCommand "smoke-appimage" rest
        ("lane-qemu" : rest) ->
            exitWith =<< runBridgeCommand "lane-qemu" rest
        ("lane-firecracker" : rest) ->
            exitWith =<< runBridgeCommand "lane-firecracker" rest
        ("gate-assurance" : rest) ->
            exitWith =<< runBridgeCommand "gate-assurance" rest
        ("vm-integration-test" : rest) ->
            exitWith =<< runBridgeCommand "vm-integration-test" rest
        ("verify-traffic" : rest) ->
            exitWith =<< runBridgeCommand "verify-traffic" rest
        ("vm-forensics" : rest) ->
            exitWith =<< runBridgeCommand "vm-forensics" rest
        ("fstar-report" : rest) ->
            exitWith =<< runBridgeCommand "fstar-report" rest
        ("release-bridge" : cmd : rest) ->
            exitWith =<< runBridgeCommand cmd rest
        (cmd : _) -> do
            putStrLn $ "Unknown command: " ++ cmd
            putStrLn "Try: build, test, verify, fstar-report, release-lane-readiness, vm-smoke, vm-image-build, vm-image-clean, firecracker-smoke, firecracker-image-build, release-sbom-generate, release-license-bundle-generate, release-license-check, release-linking, release-manifest, release-checksums, smoke-linux, smoke-appimage, lane-qemu, lane-firecracker, gate-assurance, vm-integration-test, verify-traffic, vm-forensics"
            exitWith (ExitFailure 64)

runUi :: UiFlags -> IO ()
runUi flags = do
    -- Ensure stdout handles UTF-8 for Unicode box-drawing characters
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    hSetBuffering stdout (BlockBuffering (Just 8192))
    -- Shared core init (config + identity) via headless runtime
    -- initCoreRuntime already applies: defaults → config file
    (cfg, identity) <- initCoreRuntime Nothing
    -- Layer 3: CLI flags override config file values
    applyUiFlags flags cfg
    debugLogging <- runtimeLoggingEnabled cfg
    (initRows, initCols) <- getTermSize
    let (r0, c0) = clampSize initRows initCols
    termRef <- newIORef (initRows, initCols)
    coreState <- newCoreState cfg
    st <- AppState coreState <$> newIORef 0 <*> newIORef ContactPane
                             <*> newIORef "" <*> newIORef ""
                             <*> newIORef 0 <*> newIORef ""
                             <*> newIORef Nothing  -- asDialogMode
                             <*> newIORef 0
                             <*> newIORef ""
                             <*> newIORef (calcLayout r0 c0)
                             <*> newIORef 0
                             <*> pure termRef
                             <*> newIORef Nothing  -- asMenuOpen
                             <*> newIORef 0        -- asMenuIndex
                             <*> newIORef 0        -- asDialogTab
                             <*> newIORef Nothing  -- asLastRenderToken
                             <*> newIORef False    -- asRegenCheckbox
                             <*> newIORef 0        -- asDialogScroll
    -- identity already initialized by initCoreRuntime above
    when debugLogging $ logEvent cfg "app.start" []
    activeListenPort <- readIORef (cfgListenPort cfg)
    putStrLn ""
    putStrLn $ "  Network: starting tcp listener on " ++ show activeListenPort ++ " before storage selection"
    when (activeListenPort /= head defaultPorts) $
        putStrLn $ "  Network: primary port " ++ show (head defaultPorts)
            ++ " unavailable; using " ++ show activeListenPort
    _ <- startListenerIfNeeded st identity activeListenPort "startup"
    mdnsOn <- readIORef (cfgMDNSEnabled cfg)
    when (buildSupportsDiscovery && mdnsOn) (restartMDNS st)
    -- Ask user about persistence mode before attempting DB download
    putStrLn ""
    putStrLn "  UmbraVOX - Post-Quantum Encrypted Messaging"
    putStrLn ""
    putStrLn $ "  Build profile: " ++ buildProfileName
    if not buildSupportsPersistentStorage
        then putStrLn "  Storage: ephemeral mode (compile-time locked)"
        else do
            persistedPreference <- resolvePersistencePreference cfg
            case persistedPreference of
                Just True -> do
                    putStrLn "  Initializing database..."
                    nRestored <- applyPersistenceAnswer cfg "yes"
                    dbEnabled <- readIORef (cfgDBEnabled cfg)
                    if dbEnabled
                        then putStrLn $ "  Storage: persistent mode (" ++ show nRestored ++ " conversations restored)"
                        else putStrLn "  Storage: ephemeral mode (DB unavailable)"
                Just False -> do
                    _ <- applyPersistenceAnswer cfg "no"
                    putStrLn "  Storage: ephemeral mode"
                Nothing -> do
                    putStr "  Enable persistent storage? (requires sqlite3 from nix-shell) [y/N]: "
                    hFlush stdout
                    answer <- getLine
                    if persistenceAnswerEnables answer
                        then do
                            putStrLn "  Initializing database..."
                            nRestored <- applyPersistenceAnswer cfg answer
                            dbEnabled <- readIORef (cfgDBEnabled cfg)
                            if dbEnabled
                                then putStrLn $ "  Storage: persistent mode (" ++ show nRestored ++ " conversations restored)"
                                else putStrLn "  Storage: ephemeral mode (DB unavailable)"
                        else do
                            _ <- applyPersistenceAnswer cfg answer
                            putStrLn "  Storage: ephemeral mode"
    _ <- installHandler sigWINCH (Catch $ getTermSize >>= writeIORef (asTermSize st)) Nothing
    withRawMode $ clearScreen >> render st >> eventLoop st

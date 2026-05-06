module Main (main) where

import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
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
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.App.Startup
    ( newDefaultAppConfig, initializeLocalIdentity, applyPersistenceAnswer
    , resolvePersistencePreference, persistenceAnswerEnables
    )

-- SIGWINCH = 28 on Linux/macOS (not exported by all versions of System.Posix.Signals)
sigWINCH :: CInt
sigWINCH = 28

main :: IO ()
main = do
    -- Ensure stdout handles UTF-8 for Unicode box-drawing characters
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    hSetBuffering stdout (BlockBuffering (Just 8192))
    cfg <- newDefaultAppConfig
    debugLogging <- runtimeLoggingEnabled cfg
    (initRows, initCols) <- getTermSize
    let (r0, c0) = clampSize initRows initCols
    termRef <- newIORef (initRows, initCols)
    st <- AppState cfg <$> newIORef 0 <*> newIORef ContactPane
                       <*> newIORef "" <*> newIORef ""
                       <*> newIORef 0 <*> newIORef ""
                       <*> newIORef True <*> newIORef Nothing
                       <*> newIORef 0
                       <*> newIORef ""
                       <*> newIORef (calcLayout r0 c0)
                       <*> newIORef 0
                       <*> pure termRef
                       <*> newIORef Nothing  -- asMenuOpen
                       <*> newIORef 0        -- asMenuIndex
                       <*> newIORef 0        -- asDialogTab
                       <*> newIORef Nothing  -- asLastRenderToken
    identity <- initializeLocalIdentity cfg
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

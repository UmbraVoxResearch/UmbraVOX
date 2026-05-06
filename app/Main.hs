module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (readMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (when, forever)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO (hSetBuffering, hSetEncoding, hFlush, stdout, stdin, utf8, BufferMode(..))
import System.Posix.Signals (installHandler, Handler(Catch))
import Foreign.C.Types (CInt(..))
import UmbraVox.App.RuntimeLog (logEvent, runtimeLoggingEnabled)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Render (getTermSize, clampSize, calcLayout, clearScreen,
                            withRawMode, render)
import UmbraVox.TUI.Input (eventLoop)
import UmbraVox.TUI.RuntimeNetwork (startListenerIfNeeded)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Network.MDNS (startMDNS)
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
    -- Wire mDNS discovery if enabled (graceful if multicast unavailable)
    mdnsOn <- readIORef (cfgMDNSEnabled cfg)
    when mdnsOn $ (do
        port <- readIORef (cfgListenPort cfg)
        name <- readIORef (cfgDisplayName cfg)
        mIk <- readIORef (cfgIdentity cfg)
        let pubkey = maybe BS.empty ikX25519Public mIk
        (peersRef, tid) <- startMDNS port name pubkey
        writeIORef (cfgMDNSThread cfg) (Just tid)
        logEvent cfg "mdns.start" [("port", show port)]
        -- Poll mDNS peer list into cfgMDNSPeers every 5 seconds
        _ <- forkIO $ forever $ do
            threadDelay 5000000  -- 5 seconds
            peers <- readMVar peersRef
            writeIORef (cfgMDNSPeers cfg) peers
        pure ()
        ) `catch` (\(_ :: SomeException) -> logEvent cfg "mdns.unavailable" [])  -- mDNS unavailable
    -- Ask user about persistence mode before attempting DB download
    putStrLn ""
    putStrLn "  UmbraVOX - Post-Quantum Encrypted Messaging"
    putStrLn ""
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
    -- Auto-start listening for incoming connections in background
    activeListenPort <- readIORef (cfgListenPort cfg)
    _ <- startListenerIfNeeded st identity activeListenPort "startup"
    withRawMode $ clearScreen >> render st >> eventLoop st

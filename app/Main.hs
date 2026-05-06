module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (readMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (when, forever, forM_, void)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (hSetBuffering, hSetEncoding, hFlush, stdout, utf8, BufferMode(..))
import System.Posix.Signals (installHandler, Handler(Catch))
import Foreign.C.Types (CInt(..))
import UmbraVox.TUI.Types
import UmbraVox.TUI.Render (getTermSize, clampSize, calcLayout, clearScreen,
                            withRawMode, render)
import UmbraVox.TUI.Input (eventLoop)
import UmbraVox.Crypto.BIP39 (generatePassphrase)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Network.MDNS (startMDNS)
import qualified Network.Socket as NS
import Control.Monad (forM_)
import UmbraVox.Protocol.Encoding (defaultPorts)
import UmbraVox.Storage.Anthony (openDB, loadConversations, loadMessages)
import UmbraVox.Chat.Session (initChatSession)
import UmbraVox.Crypto.Random (randomBytes)

-- SIGWINCH = 28 on Linux/macOS (not exported by all versions of System.Posix.Signals)
sigWINCH :: CInt
sigWINCH = 28

main :: IO ()
main = do
    -- Ensure stdout handles UTF-8 for Unicode box-drawing characters
    hSetEncoding stdout utf8
    hSetBuffering stdout (BlockBuffering (Just 8192))
    -- Generate random display name from BIP39 wordlist
    randomName <- generatePassphrase 1
    -- Find available listen port
    listenPort <- findAvailablePort defaultPorts
    cfg <- AppConfig
        <$> newIORef listenPort  -- listen port (first available)
        <*> newIORef randomName  -- display name (random BIP39 word)
        <*> newIORef Nothing     -- identity key
        <*> newIORef Map.empty   -- sessions
        <*> newIORef 1           -- next session ID
        -- Discovery settings (off by default)
        <*> newIORef True        -- mDNS enabled by default
        <*> newIORef False       -- PEX disabled
        <*> newIORef True        -- DB persistence enabled
        <*> newIORef "~/.umbravox/umbravox.db"  -- DB path
        -- Discovery state
        <*> newIORef Nothing     -- mDNS thread
        <*> newIORef []          -- mDNS discovered peers
        -- Retention settings
        <*> newIORef 30          -- retention days (0 = forever)
        <*> newIORef True        -- auto-save messages to DB
        <*> newIORef Nothing     -- Anthony DB handle
        <*> newIORef False       -- trusted contacts only (off by default)
    (initRows, initCols) <- getTermSize
    let (r0, c0) = clampSize initRows initCols
    termRef <- newIORef (initRows, initCols)
    st <- AppState cfg <$> newIORef 0 <*> newIORef ContactPane
                       <*> newIORef "" <*> newIORef ""
                       <*> newIORef 0 <*> newIORef ""
                       <*> newIORef True <*> newIORef (Just DlgWelcome)
                       <*> newIORef (calcLayout r0 c0)
                       <*> newIORef 0
                       <*> pure termRef
                       <*> newIORef Nothing  -- asMenuOpen
                       <*> newIORef 0        -- asMenuIndex
    -- Wire mDNS discovery if enabled (graceful if multicast unavailable)
    mdnsOn <- readIORef (cfgMDNSEnabled cfg)
    when mdnsOn $ (do
        port <- readIORef (cfgListenPort cfg)
        mIk <- readIORef (cfgIdentity cfg)
        let pubkey = maybe BS.empty ikX25519Public mIk
        (peersRef, tid) <- startMDNS port pubkey
        writeIORef (cfgMDNSThread cfg) (Just tid)
        -- Poll mDNS peer list into cfgMDNSPeers every 5 seconds
        _ <- forkIO $ forever $ do
            threadDelay 5000000  -- 5 seconds
            peers <- readMVar peersRef
            writeIORef (cfgMDNSPeers cfg) peers
        pure ()
        ) `catch` (\(_ :: SomeException) -> pure ())  -- mDNS unavailable
    -- Ask user about persistence mode before attempting DB download
    putStrLn ""
    putStrLn "  UmbraVOX - Post-Quantum Encrypted Messaging"
    putStrLn ""
    putStr "  Enable persistent storage? (requires 'anthony' DB tool) [y/N]: "
    hFlush stdout
    answer <- getLine
    if answer `elem` ["y", "Y", "yes", "Yes", "YES"]
        then (do
            dbPath <- readIORef (cfgDBPath cfg)
            home <- getHomeDirectory
            let path = if "~/" `isPrefixOf` dbPath
                       then home ++ drop 1 dbPath else dbPath
            createDirectoryIfMissing True (takeDirectory path)
            putStrLn "  Initializing database..."
            db <- openDB path
            writeIORef (cfgAnthonyDB cfg) (Just db)
            -- Restore saved conversations and their messages
            convs <- loadConversations db
            forM_ convs $ \(convId, _pubkey, name, _created) -> do
                -- Create a loopback session for each saved conversation
                chatSec <- randomBytes 32
                dhSec   <- randomBytes 32
                dhPub   <- randomBytes 32
                session <- initChatSession chatSec dhSec dhPub
                sessRef <- newIORef session
                histRef <- newIORef []
                statRef <- newIORef Offline
                let si = SessionInfo
                        { siTransport = Nothing
                        , siSession   = sessRef
                        , siRecvTid   = Nothing
                        , siPeerName  = name
                        , siHistory   = histRef
                        , siStatus    = statRef
                        }
                -- Load saved messages into history
                msgs <- loadMessages db convId 500
                let formatted = map (\(sender, content, _ts) ->
                        sender ++ ": " ++ content) msgs
                writeIORef histRef formatted
                -- Add to sessions map
                sid <- readIORef (cfgNextId cfg)
                writeIORef (cfgNextId cfg) (sid + 1)
                modifyIORef' (cfgSessions cfg) (Map.insert sid si)
            nRestored <- Map.size <$> readIORef (cfgSessions cfg)
            putStrLn $ "  Storage: persistent mode (" ++ show nRestored ++ " conversations restored)"
            ) `catch` (\(_ :: SomeException) -> do
                putStrLn "  Storage: ephemeral mode (DB unavailable)"
                writeIORef (cfgDBEnabled cfg) False
                )
        else do
            putStrLn "  Storage: ephemeral mode"
            writeIORef (cfgDBEnabled cfg) False
    _ <- installHandler sigWINCH (Catch $ getTermSize >>= writeIORef (asTermSize st)) Nothing
    withRawMode $ clearScreen >> render st >> eventLoop st

-- | Try each port in order, return the first one that can be bound.
-- Tests by briefly binding and closing a TCP socket.
findAvailablePort :: [Int] -> IO Int
findAvailablePort [] = pure 1111  -- fallback
findAvailablePort (p:ps) = do
    ok <- tryBindPort p
    if ok then pure p else findAvailablePort ps

tryBindPort :: Int -> IO Bool
tryBindPort port = (do
    let hints = NS.defaultHints { NS.addrFlags = [NS.AI_PASSIVE]
                                , NS.addrSocketType = NS.Stream
                                , NS.addrFamily = NS.AF_INET }
    addr : _ <- NS.getAddrInfo (Just hints) (Just "0.0.0.0") (Just (show port))
    sock <- NS.openSocket addr
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress addr)
    NS.close sock
    pure True
    ) `catch` (\(_ :: SomeException) -> pure False)

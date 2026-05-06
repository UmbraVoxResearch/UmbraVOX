module Main (main) where

import Control.Exception (SomeException, catch)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (hSetBuffering, stdout, BufferMode(..))
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
import UmbraVox.Storage.Anthony (openDB)

-- SIGWINCH = 28 on Linux/macOS (not exported by all versions of System.Posix.Signals)
sigWINCH :: CInt
sigWINCH = 28

main :: IO ()
main = do
    hSetBuffering stdout (BlockBuffering (Just 8192))
    -- Generate random display name from BIP39 wordlist
    randomName <- generatePassphrase 1
    -- Find available listen port
    listenPort <- findAvailablePort [1111, 2222, 4747, 8383, 3838, 3008]
    cfg <- AppConfig
        <$> newIORef listenPort  -- listen port (first available)
        <*> newIORef randomName  -- display name (random BIP39 word)
        <*> newIORef Nothing     -- identity key
        <*> newIORef Map.empty   -- sessions
        <*> newIORef 1           -- next session ID
        -- Discovery settings (off by default)
        <*> newIORef False       -- mDNS disabled
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
    (initRows, initCols) <- getTermSize
    let (r0, c0) = clampSize initRows initCols
    termRef <- newIORef (initRows, initCols)
    st <- AppState cfg <$> newIORef 0 <*> newIORef ContactPane
                       <*> newIORef "" <*> newIORef ""
                       <*> newIORef 0 <*> newIORef ""
                       <*> newIORef True <*> newIORef Nothing
                       <*> newIORef (calcLayout r0 c0)
                       <*> newIORef 0
                       <*> pure termRef
    -- Wire mDNS discovery if enabled (graceful if multicast unavailable)
    mdnsOn <- readIORef (cfgMDNSEnabled cfg)
    when mdnsOn $ (do
        port <- readIORef (cfgListenPort cfg)
        mIk <- readIORef (cfgIdentity cfg)
        let pubkey = maybe BS.empty ikX25519Public mIk
        (_peersRef, tid) <- startMDNS port pubkey
        writeIORef (cfgMDNSThread cfg) (Just tid)
        ) `catch` (\(_ :: SomeException) -> pure ())  -- mDNS unavailable
    -- Wire Anthony DB persistence if enabled (graceful fallback if anthony unavailable)
    dbOn <- readIORef (cfgDBEnabled cfg)
    when dbOn $ (do
        dbPath <- readIORef (cfgDBPath cfg)
        home <- getHomeDirectory
        let path = if "~/" `isPrefixOf` dbPath
                   then home ++ drop 1 dbPath else dbPath
        createDirectoryIfMissing True (takeDirectory path)
        db <- openDB path
        writeIORef (cfgAnthonyDB cfg) (Just db)
        ) `catch` (\(_ :: SomeException) -> pure ())  -- DB unavailable, continue without
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

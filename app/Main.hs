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
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Network.MDNS (startMDNS)
import UmbraVox.Storage.Anthony (openDB)

-- SIGWINCH = 28 on Linux/macOS (not exported by all versions of System.Posix.Signals)
sigWINCH :: CInt
sigWINCH = 28

main :: IO ()
main = do
    hSetBuffering stdout (BlockBuffering (Just 8192))
    cfg <- AppConfig
        <$> newIORef 9999        -- listen port
        <*> newIORef "User"      -- display name
        <*> newIORef Nothing     -- identity key
        <*> newIORef Map.empty   -- sessions
        <*> newIORef 1           -- next session ID
        -- Discovery settings
        <*> newIORef True        -- mDNS enabled
        <*> newIORef True        -- PEX enabled
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
    -- Wire mDNS discovery if enabled
    mdnsOn <- readIORef (cfgMDNSEnabled cfg)
    when mdnsOn $ do
        port <- readIORef (cfgListenPort cfg)
        mIk <- readIORef (cfgIdentity cfg)
        let pubkey = maybe BS.empty ikX25519Public mIk
        (_peersRef, tid) <- startMDNS port pubkey
        writeIORef (cfgMDNSThread cfg) (Just tid)
        writeIORef (cfgMDNSPeers cfg) []
    -- Wire Anthony DB persistence if enabled
    dbOn <- readIORef (cfgDBEnabled cfg)
    when dbOn $ do
        dbPath <- readIORef (cfgDBPath cfg)
        home <- getHomeDirectory
        let path = if "~/" `isPrefixOf` dbPath
                   then home ++ drop 1 dbPath else dbPath
        db <- openDB path `catch` (\(_ :: SomeException) -> do
            createDirectoryIfMissing True (takeDirectory path)
            openDB path)
        writeIORef (cfgAnthonyDB cfg) (Just db)
    _ <- installHandler sigWINCH (Catch $ getTermSize >>= writeIORef (asTermSize st)) Nothing
    withRawMode $ clearScreen >> render st >> eventLoop st

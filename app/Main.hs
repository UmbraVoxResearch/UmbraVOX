module Main (main) where

import Data.IORef (newIORef, writeIORef)
import qualified Data.Map.Strict as Map
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Posix.Signals (installHandler, Handler(Catch))
import Foreign.C.Types (CInt(..))
import UmbraVox.TUI.Types
import UmbraVox.TUI.Render (getTermSize, clampSize, calcLayout, clearScreen,
                            withRawMode, render)
import UmbraVox.TUI.Input (eventLoop)

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
    _ <- installHandler sigWINCH (Catch $ getTermSize >>= writeIORef (asTermSize st)) Nothing
    withRawMode $ clearScreen >> render st >> eventLoop st

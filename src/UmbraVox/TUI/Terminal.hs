-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Terminal
    ( -- * ANSI helpers
      esc, csi, goto, clearScreen
    , hideCursor, showCursor
    , setFg, resetSGR, bold, inverse
    , padR, isPfx
    , withRawMode
      -- * Terminal size
    , getTermSize
    ) where

import Control.Exception (catch, SomeException, throw)
import Control.Monad (void)
import System.IO (hFlush, stdout, stdin, hSetBuffering, BufferMode(..),
                  hSetEcho)
import System.Process (readProcess)
import UmbraVox.TUI.Constants (defaultTermRows, defaultTermCols)
import UmbraVox.TUI.Text (padRight)

-- ANSI / helpers ----------------------------------------------------------
esc :: String -> String; esc code = "\ESC[" ++ code
csi :: String -> IO (); csi s = putStr (esc s)
goto :: Int -> Int -> IO (); goto r c = csi (show r ++ ";" ++ show c ++ "H")
clearScreen :: IO (); clearScreen = csi "2J" >> csi "H" >> hFlush stdout
hideCursor, showCursor :: IO ()
hideCursor = putStr "\ESC[?25l"; showCursor = putStr "\ESC[?25h"
setFg :: Int -> IO (); setFg c = csi (show c ++ "m")
resetSGR :: IO (); resetSGR = csi "0m"
bold :: IO (); bold = csi "1m"
inverse :: IO (); inverse = csi "7m"
padR :: Int -> String -> String; padR = padRight
isPfx :: String -> String -> Bool
isPfx [] _ = True; isPfx _ [] = False
isPfx (x:xs) (y:ys) = x == y && isPfx xs ys

withRawMode :: IO a -> IO a
withRawMode act = do
    -- Disable XON/XOFF BEFORE entering raw mode (uses /dev/tty explicitly)
    void (readProcess "stty" ["-F", "/dev/tty", "-ixon", "-ixoff", "raw", "-echo"] "" `catch`
          (\(_ :: SomeException) -> pure ""))
    hideCursor
    -- Enable xterm mouse reporting when supported:
    -- 1000 = click press/release, 1006 = SGR extended coordinates.
    putStr "\ESC[?1000h\ESC[?1006h"
    hFlush stdout
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    result <- act `catch` (\(e :: SomeException) -> do
        cleanup
        throw e)
    cleanup
    pure result
  where
    cleanup = do
        -- Disable mouse reporting before restoring line mode.
        putStr "\ESC[?1006l\ESC[?1000l"
        showCursor
        putStr "\ESC[?25h"  -- ensure cursor visible
        hFlush stdout
        -- Restore terminal FIRST via stty before changing Haskell buffering
        void (readProcess "stty" ["-F", "/dev/tty", "sane", "echo", "ixon"] "" `catch`
              (\(_ :: SomeException) -> pure ""))
        hSetBuffering stdin LineBuffering
        hSetEcho stdin True

-- Terminal size detection -------------------------------------------------
getTermSize :: IO (Int, Int)  -- (rows, cols)
getTermSize = do
    result <- readProcess "stty" ["-F", "/dev/tty", "size"] ""
              `catch` (\(_ :: SomeException) ->
                  pure (show defaultTermRows ++ " " ++ show defaultTermCols))
    let cleaned = filter (\ch -> (ch >= '0' && ch <= '9') || ch == ' ') result
        ws = words cleaned
    case ws of
        [r, c'] -> pure (read r, read c')
        _       -> pure (defaultTermRows, defaultTermCols)

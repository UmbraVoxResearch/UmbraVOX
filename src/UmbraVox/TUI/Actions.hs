-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Actions
    ( -- * Re-exports from submodules
      module UmbraVox.TUI.Actions.Session
    , module UmbraVox.TUI.Actions.Export
      -- * Dialog openers
    , startNewConn, showHelp, showAbout, startSettings, startBrowse
    , startVerify, startKeysView
      -- * Contact management
    , renameContact, selectLast, adjustContactScroll
      -- * App lifecycle
    , quitApp, setStatus
    ) where

import Control.Concurrent (killThread)
import Control.Exception (catch, SomeException)
import Control.Monad (when, unless, forM_, void)
import Data.IORef (readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import System.IO (hFlush, stdout, stdin, hSetBuffering, hSetEcho, BufferMode(..))
import System.Process (readProcess)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Render (clearScreen, goto, showCursor)
import UmbraVox.TUI.Handshake (genIdentity)
import UmbraVox.TUI.RuntimeEvent (RuntimeEvent(..), applyRuntimeEvents)
import UmbraVox.Network.TransportClass (anyClose)

-- Re-export submodules for backward compatibility
import UmbraVox.TUI.Actions.Session
import UmbraVox.TUI.Actions.Export

-- Dialog openers ----------------------------------------------------------

startNewConn :: AppState -> IO ()
startNewConn st = applyRuntimeEvents st [EventSetDialog (Just DlgNewConn)]

showHelp :: AppState -> IO ()
showHelp st = applyRuntimeEvents st [EventSetDialog (Just DlgHelp)]

showAbout :: AppState -> IO ()
showAbout st = applyRuntimeEvents st [EventSetDialog (Just DlgAbout)]

startSettings :: AppState -> IO ()
startSettings st =
    applyRuntimeEvents st [EventSetDialogTab 0, EventSetDialog (Just DlgSettings)]

startBrowse :: AppState -> IO ()
startBrowse st =
    applyRuntimeEvents st [EventResetBrowse, EventSetDialog (Just DlgBrowse)]

startVerify :: AppState -> IO ()
startVerify st = applyRuntimeEvents st [EventSetDialog (Just DlgVerify)]

startKeysView :: AppState -> IO ()
startKeysView st = do
    applyRuntimeEvents st [EventSetDialog (Just DlgKeys)]
    mIk <- readIORef (cfgIdentity (asConfig st))
    case mIk of
        Nothing -> do
            ik <- genIdentity
            writeIORef (cfgIdentity (asConfig st)) (Just ik)
        Just _  -> pure ()

-- Contact management ------------------------------------------------------

renameContact :: AppState -> IO ()
renameContact st = do
    sessions <- readIORef (cfgSessions (asConfig st))
    sel <- readIORef (asSelected st)
    let entries = Map.toList sessions
    when (sel < length entries) $ do
        let (sid, si) = entries !! sel
        applyRuntimeEvents st
            [ EventSetInput ""
            , EventSetDialog (Just (DlgPrompt ("Rename: " ++ siPeerName si) $ \val ->
                unless (null val) $ do
                    let si' = si { siPeerName = val }
                    modifyIORef' (cfgSessions (asConfig st)) (Map.insert sid si')
                    setStatus st ("Renamed to: " ++ val)
                ))
            ]

selectLast :: AppState -> IO ()
selectLast st = do
    n <- Map.size <$> readIORef (cfgSessions (asConfig st))
    writeIORef (asSelected st) (max 0 (n-1))
    writeIORef (asFocus st) ChatPane
    writeIORef (asChatScroll st) 0

adjustContactScroll :: AppState -> Int -> IO ()
adjustContactScroll st visRows = do
    sel <- readIORef (asSelected st)
    cScroll <- readIORef (asContactScroll st)
    when (sel < cScroll) $ writeIORef (asContactScroll st) sel
    when (sel >= cScroll + visRows) $
        writeIORef (asContactScroll st) (sel - visRows + 1)

-- App lifecycle -----------------------------------------------------------

setStatus :: AppState -> String -> IO ()
setStatus st msg = writeIORef (asStatusMsg st) msg

quitApp :: AppState -> IO ()
quitApp st = do
    writeIORef (asRunning st) False
    mListenerTid <- readIORef (cfgListenerThread (asConfig st))
    maybe (pure ()) killThread mListenerTid
    writeIORef (cfgListenerThread (asConfig st)) Nothing
    sessions <- readIORef (cfgSessions (asConfig st))
    forM_ (Map.elems sessions) $ \si -> do
        maybe (pure ()) killThread (siRecvTid si)
        maybe (pure ()) anyClose (siTransport si)
    -- Restore terminal to sane state before exiting
    clearScreen; goto 1 1; showCursor
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    void (readProcess "stty" ["-F", "/dev/tty", "sane"] "" `catch`
          (\(_ :: SomeException) -> pure ""))
    putStrLn "Goodbye."; hFlush stdout

-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Menu
    ( toggleMenu, openMenu, closeMenu
    , handleMenu, moveMenuTab, activateMenuItem, executeMenuItem
    ) where

import Data.IORef (readIORef, writeIORef, modifyIORef')
import UmbraVox.TUI.Types
import UmbraVox.TUI.Actions (startNewConn, startSettings, startExport,
    startImport, startKeysView, startVerify, startBrowse, showHelp,
    renameContact, sendCurrentMessage, quitApp, setStatus)
import UmbraVox.Version (versionFull)

-- Menu handling -----------------------------------------------------------

-- | Toggle a menu tab open/closed
toggleMenu :: AppState -> MenuTab -> IO ()
toggleMenu st tab = do
    cur <- readIORef (asMenuOpen st)
    case cur of
        Just t | t == tab -> closeMenu st
        _                 -> openMenu st tab

openMenu :: AppState -> MenuTab -> IO ()
openMenu st tab = do
    writeIORef (asMenuOpen st) (Just tab)
    writeIORef (asMenuIndex st) 0

closeMenu :: AppState -> IO ()
closeMenu st = writeIORef (asMenuOpen st) Nothing

-- | Handle keys when a dropdown menu is open
handleMenu :: AppState -> InputEvent -> IO ()
handleMenu st key = case key of
    KeyEscape  -> closeMenu st
    KeyLeft    -> moveMenuTab st (-1)
    KeyRight   -> moveMenuTab st 1
    KeyUp      -> modifyIORef' (asMenuIndex st) (\i -> max 0 (i - 1))
    KeyDown    -> do
        tab <- readIORef (asMenuOpen st)
        case tab of
            Just t -> do
                let maxIdx = length (menuTabItems t) - 1
                modifyIORef' (asMenuIndex st) (\i -> min maxIdx (i + 1))
            Nothing -> pure ()
    KeyEnter   -> activateMenuItem st
    -- F-keys switch directly to that tab
    KeyF1      -> openMenu st MenuHelp
    KeyF2      -> openMenu st MenuContacts
    KeyF3      -> openMenu st MenuChat
    KeyF4      -> openMenu st MenuPrefs
    KeyF5      -> pure ()
    -- Ctrl shortcuts pass through
    KeyCtrlQ   -> closeMenu st >> quitApp st
    KeyCtrlD   -> closeMenu st >> quitApp st
    KeyCtrlN   -> closeMenu st >> startNewConn st
    -- All other keys (including KeyChar) are ignored
    _          -> pure ()

-- | Move between menu tabs with left/right arrows
moveMenuTab :: AppState -> Int -> IO ()
moveMenuTab st dir = do
    mTab <- readIORef (asMenuOpen st)
    case mTab of
        Nothing -> pure ()
        Just tab -> do
            let tabs = [minBound .. maxBound] :: [MenuTab]
                idx  = fromEnum tab
                idx' = (idx + dir) `mod` length tabs
                newTab = toEnum idx'
            writeIORef (asMenuOpen st) (Just newTab)
            writeIORef (asMenuIndex st) 0

-- | Activate the currently selected menu item
activateMenuItem :: AppState -> IO ()
activateMenuItem st = do
    mTab <- readIORef (asMenuOpen st)
    idx <- readIORef (asMenuIndex st)
    closeMenu st
    case mTab of
        Nothing -> pure ()
        Just tab -> do
            let items = menuTabItems tab
            if idx < length items
                then executeMenuItem st tab idx
                else pure ()

-- | Execute a menu action based on tab and item index.
-- Every menu item is wired to a real action.
executeMenuItem :: AppState -> MenuTab -> Int -> IO ()
-- Help menu
executeMenuItem st MenuHelp 0     = showHelp st          -- Help
executeMenuItem st MenuHelp 1     =                      -- About
    setStatus st (versionFull ++ " \x2014 Post-Quantum Encrypted Messaging")
-- Contacts menu
executeMenuItem st MenuContacts 0 = startBrowse st       -- Browse
executeMenuItem st MenuContacts 1 = startVerify st       -- Verify
-- Chat menu
executeMenuItem st MenuChat 0     = startNewConn st      -- New
executeMenuItem st MenuChat 1     = renameContact st     -- Rename
executeMenuItem st MenuChat 2     = sendCurrentMessage st -- Send
executeMenuItem st MenuChat 3     = do                   -- Clear Input
    writeIORef (asInputBuf st) ""
    setStatus st "Input cleared"
-- Prefs menu
executeMenuItem st MenuPrefs 0    = startSettings st     -- Settings
executeMenuItem st MenuPrefs 1    = startKeysView st     -- Keys
executeMenuItem st MenuPrefs 2    = do                   -- mDNS Toggle
    modifyIORef' (cfgMDNSEnabled (asConfig st)) not
    on <- readIORef (cfgMDNSEnabled (asConfig st))
    setStatus st ("mDNS: " ++ if on then "ON" else "OFF")
executeMenuItem st MenuPrefs 3    = startExport st       -- Export Chat
executeMenuItem st MenuPrefs 4    = startImport st       -- Import Chat
-- Quit menu
executeMenuItem st MenuQuit 0     = quitApp st           -- Quit
executeMenuItem _  _        _     = pure ()

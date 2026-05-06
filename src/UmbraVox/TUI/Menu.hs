module UmbraVox.TUI.Menu
    ( toggleMenu, openMenu, closeMenu
    , handleMenu, moveMenuTab, activateMenuItem, executeMenuItem
    ) where

import Data.IORef (readIORef, writeIORef, modifyIORef')
import UmbraVox.TUI.Types
import UmbraVox.TUI.Actions (startNewConn, startSettings, startExport,
    startImport, startKeysView, startVerify, startBrowse, showHelp,
    renameContact, sendCurrentMessage, quitApp, setStatus)

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
    KeyF1      -> openMenu st MenuFile
    KeyF2      -> openMenu st MenuContacts
    KeyF3      -> openMenu st MenuChat
    KeyF4      -> openMenu st MenuPrefs
    KeyF5      -> openMenu st MenuHelp
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
-- File menu
executeMenuItem st MenuFile 0     = startExport st       -- Export
executeMenuItem st MenuFile 1     = startImport st       -- Import
executeMenuItem st MenuFile 2     = quitApp st           -- Quit
-- Contacts menu
executeMenuItem st MenuContacts 0 = startNewConn st      -- New
executeMenuItem st MenuContacts 1 = renameContact st     -- Rename
executeMenuItem st MenuContacts 2 = startBrowse st       -- Browse
executeMenuItem st MenuContacts 3 = startVerify st       -- Verify
-- Chat menu
executeMenuItem st MenuChat 0     = sendCurrentMessage st -- Send
executeMenuItem st MenuChat 1     = do                   -- Clear Input
    writeIORef (asInputBuf st) ""
    setStatus st "Input cleared"
-- Prefs menu
executeMenuItem st MenuPrefs 0    = startSettings st     -- Settings
executeMenuItem st MenuPrefs 1    = startKeysView st     -- Keys
executeMenuItem st MenuPrefs 2    = do                   -- mDNS Toggle
    modifyIORef' (cfgMDNSEnabled (asConfig st)) not
    on <- readIORef (cfgMDNSEnabled (asConfig st))
    setStatus st ("mDNS: " ++ if on then "ON" else "OFF")
-- Help menu
executeMenuItem st MenuHelp 0     = showHelp st          -- Help
executeMenuItem st MenuHelp 1     =                      -- About
    setStatus st "UmbraVOX v0.1 — Post-Quantum Encrypted Messaging"
executeMenuItem _  _        _     = pure ()

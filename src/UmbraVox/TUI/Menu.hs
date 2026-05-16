-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Menu
    ( toggleMenu, openMenu, closeMenu
    , handleMenu, moveMenuTab, activateMenuItem, executeMenuItem
    ) where

import Data.IORef (readIORef, writeIORef, modifyIORef')
import UmbraVox.TUI.Types
import UmbraVox.TUI.Actions (setStatus)
import UmbraVox.TUI.RuntimeCommand (commandForMenuItem, runRuntimeCommand, RuntimeCommand(CmdOpenNewConversation, CmdQuit))

-- Menu handling -----------------------------------------------------------

clampMenuIndexForTab :: MenuTab -> Int -> Int
clampMenuIndexForTab tab i =
    let maxIdx = max 0 (length (menuTabItems tab) - 1)
    in max 0 (min maxIdx i)

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
    writeIORef (asMenuIndex st) (clampMenuIndexForTab tab 0)

closeMenu :: AppState -> IO ()
closeMenu st = writeIORef (asMenuOpen st) Nothing

-- | Handle keys when a dropdown menu is open
handleMenu :: AppState -> InputEvent -> IO ()
handleMenu st key = case key of
    KeyEscape  -> closeMenu st
    KeyLeft    -> moveMenuTab st (-1)
    KeyRight   -> moveMenuTab st 1
    KeyUp      -> do
        mTab <- readIORef (asMenuOpen st)
        case mTab of
            Just tab -> modifyIORef' (asMenuIndex st) (\i -> clampMenuIndexForTab tab (i - 1))
            Nothing -> pure ()
    KeyDown    -> do
        mTab <- readIORef (asMenuOpen st)
        case mTab of
            Just tab -> modifyIORef' (asMenuIndex st) (\i -> clampMenuIndexForTab tab (i + 1))
            Nothing -> pure ()
    KeyEnter   -> activateMenuItem st
    -- F-keys switch directly to that tab
    KeyF1      -> openMenu st MenuHelp
    KeyF2      -> openMenu st MenuPrefs
    KeyF3      -> openMenu st MenuIdentity
    KeyF4      -> pure ()
    KeyF5      -> pure ()
    -- Ctrl shortcuts pass through
    KeyCtrlQ   -> closeMenu st >> runRuntimeCommand st CmdQuit
    KeyCtrlD   -> closeMenu st >> runRuntimeCommand st CmdQuit
    KeyCtrlN   -> closeMenu st >> runRuntimeCommand st CmdOpenNewConversation
    KeyCtrlG   -> closeMenu st >> setStatus st "Use Ctrl+G from the main screen to start a group chat"
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
            writeIORef (asMenuIndex st) (clampMenuIndexForTab newTab 0)

-- | Activate the currently selected menu item
activateMenuItem :: AppState -> IO ()
activateMenuItem st = do
    mTab <- readIORef (asMenuOpen st)
    idx <- readIORef (asMenuIndex st)
    closeMenu st
    case mTab of
        Nothing -> pure ()
        Just tab -> do
            let idx' = clampMenuIndexForTab tab idx
            if idx' < length (menuTabItems tab)
                then executeMenuItem st tab idx'
                else pure ()

-- | Execute a menu action based on tab and item index.
-- Every menu item is wired to a real action.
executeMenuItem :: AppState -> MenuTab -> Int -> IO ()
executeMenuItem st tab idx =
    case commandForMenuItem tab idx of
        Just cmd -> runRuntimeCommand st cmd
        Nothing  -> pure ()

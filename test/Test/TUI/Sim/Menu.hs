-- SPDX-License-Identifier: Apache-2.0
-- | TUI simulation tests: menu navigation and item execution
module Test.TUI.Sim.Menu (runTests) where

import Data.IORef (readIORef, writeIORef)
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Types
import UmbraVox.TUI.Menu (handleMenu, toggleMenu, openMenu, executeMenuItem)

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Sim.Menu"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testMenuLeftWraps
        , testMenuRightWraps
        , testMenuDownClamps
        , testMenuUpClamps
        , testMenuEnterCloses
        , testMenuEscCloses
        , testMenuToggleSameCloses
        , testMenuSwitchDirect
        , testMenuContactsBrowse
        , testMenuContactsVerify
        , testMenuChatNew
        , testMenuChatRename
        , testMenuPrefsSettings
        , testMenuPrefsKeys
        , testMenuPrefsMDNSToggle
        , testMenuPrefsExportChat
        , testMenuPrefsImportChat
        , testMenuHelpHelp
        , testMenuChatClearInput
        , testMenuQuit
        ]
    pure (and results)

testMenuLeftWraps :: IO Bool
testMenuLeftWraps = do
    st <- mkTestState; openMenu st MenuHelp
    handleMenu st KeyLeft
    m <- readIORef (asMenuOpen st)
    assertEq "menu left wraps" (Just MenuQuit) m

testMenuRightWraps :: IO Bool
testMenuRightWraps = do
    st <- mkTestState; openMenu st MenuQuit
    handleMenu st KeyRight
    m <- readIORef (asMenuOpen st)
    assertEq "menu right wraps" (Just MenuHelp) m

testMenuDownClamps :: IO Bool
testMenuDownClamps = do
    st <- mkTestState; openMenu st MenuPrefs
    handleMenu st KeyDown >> handleMenu st KeyDown >> handleMenu st KeyDown
    handleMenu st KeyDown >> handleMenu st KeyDown >> handleMenu st KeyDown
    idx <- readIORef (asMenuIndex st)
    assertEq "menu down clamps at 4" 4 idx

testMenuUpClamps :: IO Bool
testMenuUpClamps = do
    st <- mkTestState; openMenu st MenuHelp
    handleMenu st KeyUp
    idx <- readIORef (asMenuIndex st)
    assertEq "menu up clamps at 0" 0 idx

testMenuEnterCloses :: IO Bool
testMenuEnterCloses = do
    st <- mkTestState; openMenu st MenuHelp
    handleMenu st KeyEnter
    m <- readIORef (asMenuOpen st)
    assertEq "menu enter closes" Nothing m

testMenuEscCloses :: IO Bool
testMenuEscCloses = do
    st <- mkTestState; openMenu st MenuHelp
    handleMenu st KeyEscape
    m <- readIORef (asMenuOpen st)
    assertEq "menu esc closes" Nothing m

testMenuToggleSameCloses :: IO Bool
testMenuToggleSameCloses = do
    st <- mkTestState; openMenu st MenuHelp
    toggleMenu st MenuHelp
    m <- readIORef (asMenuOpen st)
    assertEq "toggle same closes" Nothing m

testMenuSwitchDirect :: IO Bool
testMenuSwitchDirect = do
    st <- mkTestState; openMenu st MenuHelp
    handleMenu st KeyF3
    m <- readIORef (asMenuOpen st)
    assertEq "F3 switches to Chat" (Just MenuChat) m

testMenuContactsBrowse :: IO Bool
testMenuContactsBrowse = do
    st <- mkTestState; executeMenuItem st MenuContacts 0
    dlg <- readIORef (asDialogMode st)
    assertEq "contacts browse opens DlgBrowse" True (isDlgBrowse dlg)

testMenuContactsVerify :: IO Bool
testMenuContactsVerify = do
    st <- mkTestState; executeMenuItem st MenuContacts 1
    dlg <- readIORef (asDialogMode st)
    assertEq "contacts verify opens DlgVerify" True (isDlgVerify dlg)

testMenuChatNew :: IO Bool
testMenuChatNew = do
    st <- mkTestState; executeMenuItem st MenuChat 0
    dlg <- readIORef (asDialogMode st)
    assertEq "chat new opens DlgNewConn" True (isDlgNewConn dlg)

testMenuChatRename :: IO Bool
testMenuChatRename = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "peer-1"
    executeMenuItem st MenuChat 1
    dlg <- readIORef (asDialogMode st)
    assertEq "chat rename opens prompt" True (isDlgPrompt dlg)

testMenuPrefsSettings :: IO Bool
testMenuPrefsSettings = do
    st <- mkTestState; executeMenuItem st MenuPrefs 0
    dlg <- readIORef (asDialogMode st)
    assertEq "prefs settings opens DlgSettings" True (isDlgSettings dlg)

testMenuPrefsKeys :: IO Bool
testMenuPrefsKeys = do
    st <- mkTestState; executeMenuItem st MenuPrefs 1
    dlg <- readIORef (asDialogMode st)
    assertEq "prefs keys opens DlgKeys" True (isDlgKeys dlg)

testMenuPrefsMDNSToggle :: IO Bool
testMenuPrefsMDNSToggle = do
    st <- mkTestState
    before <- readIORef (cfgMDNSEnabled (asConfig st))
    executeMenuItem st MenuPrefs 2
    after <- readIORef (cfgMDNSEnabled (asConfig st))
    assertEq "prefs mDNS toggles" True (before /= after)

testMenuPrefsExportChat :: IO Bool
testMenuPrefsExportChat = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "peer-1"
    executeMenuItem st MenuPrefs 3
    dlg <- readIORef (asDialogMode st)
    assertEq "prefs export chat opens prompt" True (isDlgPrompt dlg)

testMenuPrefsImportChat :: IO Bool
testMenuPrefsImportChat = do
    st <- mkTestState
    executeMenuItem st MenuPrefs 4
    dlg <- readIORef (asDialogMode st)
    assertEq "prefs import chat opens prompt" True (isDlgPrompt dlg)

testMenuHelpHelp :: IO Bool
testMenuHelpHelp = do
    st <- mkTestState; executeMenuItem st MenuHelp 0
    dlg <- readIORef (asDialogMode st)
    assertEq "help opens DlgHelp" True (isDlgHelp dlg)

testMenuChatClearInput :: IO Bool
testMenuChatClearInput = do
    st <- mkTestState; writeIORef (asInputBuf st) "hello"
    executeMenuItem st MenuChat 3
    buf <- readIORef (asInputBuf st)
    assertEq "chat clear input" "" buf

testMenuQuit :: IO Bool
testMenuQuit = do
    st <- mkTestState
    executeMenuItem st MenuQuit 0
    running <- readIORef (asRunning st)
    assertEq "quit menu stops app" False running

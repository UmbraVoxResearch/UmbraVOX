-- | TUI simulation tests: menu navigation and item execution
module Test.TUI.Sim.Menu (runTests) where

import Data.IORef (readIORef, writeIORef)
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Types
import UmbraVox.TUI.Menu (handleMenu, toggleMenu, openMenu, closeMenu, executeMenuItem)

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
        -- Menu item execution
        , testMenuContactsNew
        , testMenuContactsBrowse
        , testMenuContactsVerify
        , testMenuPrefsSettings
        , testMenuPrefsKeys
        , testMenuPrefsMDNSToggle
        , testMenuHelpHelp
        , testMenuChatClearInput
        ]
    pure (and results)

testMenuLeftWraps :: IO Bool
testMenuLeftWraps = do
    st <- mkTestState; openMenu st MenuFile
    handleMenu st KeyLeft  -- File -> wraps to Help
    m <- readIORef (asMenuOpen st)
    assertEq "menu left wraps" (Just MenuHelp) m

testMenuRightWraps :: IO Bool
testMenuRightWraps = do
    st <- mkTestState; openMenu st MenuHelp
    handleMenu st KeyRight  -- Help -> wraps to File
    m <- readIORef (asMenuOpen st)
    assertEq "menu right wraps" (Just MenuFile) m

testMenuDownClamps :: IO Bool
testMenuDownClamps = do
    st <- mkTestState; openMenu st MenuFile  -- 3 items
    handleMenu st KeyDown >> handleMenu st KeyDown >> handleMenu st KeyDown >> handleMenu st KeyDown
    idx <- readIORef (asMenuIndex st)
    assertEq "menu down clamps at 2" 2 idx

testMenuUpClamps :: IO Bool
testMenuUpClamps = do
    st <- mkTestState; openMenu st MenuFile
    handleMenu st KeyUp
    idx <- readIORef (asMenuIndex st)
    assertEq "menu up clamps at 0" 0 idx

testMenuEnterCloses :: IO Bool
testMenuEnterCloses = do
    st <- mkTestState; openMenu st MenuHelp
    handleMenu st KeyEnter  -- activates item 0 (Help) and closes
    m <- readIORef (asMenuOpen st)
    assertEq "menu enter closes" Nothing m

testMenuEscCloses :: IO Bool
testMenuEscCloses = do
    st <- mkTestState; openMenu st MenuFile
    handleMenu st KeyEscape
    m <- readIORef (asMenuOpen st)
    assertEq "menu esc closes" Nothing m

testMenuToggleSameCloses :: IO Bool
testMenuToggleSameCloses = do
    st <- mkTestState; openMenu st MenuFile
    toggleMenu st MenuFile
    m <- readIORef (asMenuOpen st)
    assertEq "toggle same closes" Nothing m

testMenuSwitchDirect :: IO Bool
testMenuSwitchDirect = do
    st <- mkTestState; openMenu st MenuFile
    handleMenu st KeyF3  -- switch to Chat
    m <- readIORef (asMenuOpen st)
    assertEq "F3 switches to Chat" (Just MenuChat) m

-- Item execution tests

testMenuContactsNew :: IO Bool
testMenuContactsNew = do
    st <- mkTestState; executeMenuItem st MenuContacts 0
    dlg <- readIORef (asDialogMode st)
    assertEq "contacts new opens DlgNewConn" True (isDlgNewConn dlg)

testMenuContactsBrowse :: IO Bool
testMenuContactsBrowse = do
    st <- mkTestState; executeMenuItem st MenuContacts 2
    dlg <- readIORef (asDialogMode st)
    assertEq "contacts browse opens DlgBrowse" True (isDlgBrowse dlg)

testMenuContactsVerify :: IO Bool
testMenuContactsVerify = do
    st <- mkTestState; executeMenuItem st MenuContacts 3
    dlg <- readIORef (asDialogMode st)
    assertEq "contacts verify opens DlgVerify" True (isDlgVerify dlg)

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

testMenuHelpHelp :: IO Bool
testMenuHelpHelp = do
    st <- mkTestState; executeMenuItem st MenuHelp 0
    dlg <- readIORef (asDialogMode st)
    assertEq "help opens DlgHelp" True (isDlgHelp dlg)

testMenuChatClearInput :: IO Bool
testMenuChatClearInput = do
    st <- mkTestState; writeIORef (asInputBuf st) "hello"
    executeMenuItem st MenuChat 1
    buf <- readIORef (asInputBuf st)
    assertEq "chat clear input" "" buf

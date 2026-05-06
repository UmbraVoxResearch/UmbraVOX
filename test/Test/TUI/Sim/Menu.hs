-- SPDX-License-Identifier: Apache-2.0
-- | TUI simulation tests: menu navigation and item execution
module Test.TUI.Sim.Menu (runTests) where

import Data.IORef (readIORef, writeIORef)
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Actions (startBrowse)
import UmbraVox.TUI.Dialog (browseOverlayLines, overlayBounds, settingsOverlayLines)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Menu (handleMenu, toggleMenu, openMenu, executeMenuItem)
import UmbraVox.TUI.Input (handleNormal)

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
        , testMenuCtrlGStatusHint
        , testMenuContactsBrowse
        , testMenuContactsVerify
        , testMenuChatNew
        , testMenuChatRename
        , testMenuPrefsSettings
        , testMenuPrefsKeys
        , testMenuPrefsMDNSToggle
        , testMenuPrefsExportChat
        , testMenuPrefsExportChatNoSelection
        , testMenuPrefsImportChat
        , testMenuPrefsImportChatNoSelection
        , testMenuHelpHelp
        , testMenuHelpAbout
        , testMenuChatClearInput
        , testMenuQuit
        , testMenuMouseClickTab
        , testMenuMouseClickQuitTab
        , testMenuMouseClickDropdownItem
        , testMouseClickPaneFocusAndSelection
        , testMouseClickBrowsePeerSelection
        , testMouseClickSettingsOption
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

testMenuCtrlGStatusHint :: IO Bool
testMenuCtrlGStatusHint = do
    st <- mkTestState; openMenu st MenuHelp
    handleMenu st KeyCtrlG
    m <- readIORef (asMenuOpen st)
    status <- readIORef (asStatusMsg st)
    ok1 <- assertEq "Ctrl+G closes menu" Nothing m
    ok2 <- assertEq "Ctrl+G menu hint status" "Use Ctrl+G from the main screen to start a group chat" status
    pure (ok1 && ok2)

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

testMenuPrefsExportChatNoSelection :: IO Bool
testMenuPrefsExportChatNoSelection = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "peer-1"
    writeIORef (asSelected st) (-1)
    executeMenuItem st MenuPrefs 3
    dlg <- readIORef (asDialogMode st)
    status <- readIORef (asStatusMsg st)
    ok1 <- assertEq "prefs export chat no selection stays closed" True (isDlgNothing dlg)
    ok2 <- assertEq "prefs export chat no selection status" "No contact selected" status
    pure (ok1 && ok2)

testMenuPrefsImportChat :: IO Bool
testMenuPrefsImportChat = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "peer-1"
    executeMenuItem st MenuPrefs 4
    dlg <- readIORef (asDialogMode st)
    assertEq "prefs import chat opens prompt" True (isDlgPrompt dlg)

testMenuPrefsImportChatNoSelection :: IO Bool
testMenuPrefsImportChatNoSelection = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "peer-1"
    writeIORef (asSelected st) (-1)
    executeMenuItem st MenuPrefs 4
    dlg <- readIORef (asDialogMode st)
    status <- readIORef (asStatusMsg st)
    ok1 <- assertEq "prefs import chat no selection stays closed" True (isDlgNothing dlg)
    ok2 <- assertEq "prefs import chat no selection status" "No contact selected" status
    pure (ok1 && ok2)

testMenuHelpHelp :: IO Bool
testMenuHelpHelp = do
    st <- mkTestState; executeMenuItem st MenuHelp 0
    dlg <- readIORef (asDialogMode st)
    assertEq "help opens DlgHelp" True (isDlgHelp dlg)

testMenuHelpAbout :: IO Bool
testMenuHelpAbout = do
    st <- mkTestState; executeMenuItem st MenuHelp 1
    dlg <- readIORef (asDialogMode st)
    assertEq "about opens DlgAbout" True (isDlgAbout dlg)

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

testMenuMouseClickTab :: IO Bool
testMenuMouseClickTab = do
    st <- mkTestState
    -- 40x120 test layout: Help tab label starts at col 66 in row 1.
    handleNormal st (KeyMouseLeft 1 67)
    m <- readIORef (asMenuOpen st)
    assertEq "mouse click tab opens menu" (Just MenuHelp) m

testMenuMouseClickQuitTab :: IO Bool
testMenuMouseClickQuitTab = do
    st <- mkTestState
    handleNormal st (KeyMouseLeft 1 112)
    running <- readIORef (asRunning st)
    m <- readIORef (asMenuOpen st)
    ok1 <- assertEq "mouse click quit tab stops app" False running
    ok2 <- assertEq "mouse click quit tab leaves no menu open" Nothing m
    pure (ok1 && ok2)

testMenuMouseClickDropdownItem :: IO Bool
testMenuMouseClickDropdownItem = do
    st <- mkTestState
    openMenu st MenuPrefs
    -- Prefs dropdown starts near col 100 in row 3 (items begin row 3).
    -- Row 4 is item index 1 => "Keys".
    handleNormal st (KeyMouseLeft 4 101)
    dlg <- readIORef (asDialogMode st)
    m <- readIORef (asMenuOpen st)
    ok1 <- assertEq "mouse dropdown click executes item" True (isDlgKeys dlg)
    ok2 <- assertEq "mouse dropdown click closes menu" Nothing m
    pure (ok1 && ok2)

testMouseClickPaneFocusAndSelection :: IO Bool
testMouseClickPaneFocusAndSelection = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "peer-1"
    _ <- addTestSession (asConfig st) "peer-2"
    handleNormal st (KeyMouseLeft 2 80)
    f1 <- readIORef (asFocus st)
    handleNormal st (KeyMouseLeft 3 3)
    f2 <- readIORef (asFocus st)
    sel <- readIORef (asSelected st)
    ok1 <- assertEq "mouse click chat area focuses chat" ChatPane f1
    ok2 <- assertEq "mouse click contacts focuses contacts" ContactPane f2
    ok3 <- assertEq "mouse click contacts selects row" 1 sel
    pure (ok1 && ok2 && ok3)

testMouseClickBrowsePeerSelection :: IO Bool
testMouseClickBrowsePeerSelection = do
    st <- mkTestState
    seedBrowsePeers st 1
    startBrowse st
    lines' <- browseOverlayLines st
    let (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
        firstPeerContentLine = 5
    handleNormal st (KeyMouseLeft (r0 + 1 + firstPeerContentLine) (c0 + 4))
    dlg <- readIORef (asDialogMode st)
    status <- readIORef (asStatusMsg st)
    ok1 <- assertEq "mouse click browse peer closes dialog" True (isDlgNothing dlg)
    ok2 <- assertEq "mouse click browse peer starts connect" True
        ("Connecting via tcp to 127.0.0.1:3000" `prefixOf` status)
    pure (ok1 && ok2)

testMouseClickSettingsOption :: IO Bool
testMouseClickSettingsOption = do
    st <- mkTestState
    lines' <- settingsOverlayLines st
    let keyLine = findLineIndex "   0. View/regenerate keys" lines'
        (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleNormal st (KeyMouseLeft (r0 + keyLine + 1) (c0 + 6))
    dlg <- readIORef (asDialogMode st)
    assertEq "mouse click settings option opens keys" True (isDlgKeys dlg)

prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

findLineIndex :: String -> [String] -> Int
findLineIndex needle = go 0
  where
    go _ [] = 0
    go n (line:rest)
        | line == needle = n
        | otherwise = go (n + 1) rest

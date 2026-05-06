-- SPDX-License-Identifier: Apache-2.0
-- | TUI simulation tests: dialog open/close lifecycle and prompt handling
module Test.TUI.Sim.Dialogs (runTests) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Types
import UmbraVox.TUI.Input (handleNormal, handleDialog, handleSettingsDlg, handleNewConnDlg)
import qualified Data.Map.Strict as Map

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Sim.Dialogs"
    putStrLn (replicate 40 '-')
    results <- sequence
        -- Dialog lifecycle
        [ testDlgHelpAnyKeyCloses
        , testDlgKeysAnyKeyCloses
        , testDlgVerifyAnyKeyCloses
        , testDlgBrowseAnyKeyCloses
        , testDlgWelcomeAnyKeyCloses
        , testDlgWelcomeEscCloses
        , testDialogBlocksNormalInput
        -- Prompt dialog
        , testPromptTyping
        , testPromptBackspace
        , testPromptBackspaceEmpty
        , testPromptEnterCallback
        , testPromptEnterClears
        , testPromptEnterCloses
        , testPromptEscCloses
        , testPromptMaxLength
        -- Settings dialog
        , testSettingsPortPrompt
        , testSettingsMDNSToggle
        , testSettingsPEXToggle
        , testSettingsAutoSaveToggle
        , testSettingsKeysOpens
        , testSettingsUnknownCloses
        -- NewConn dialog
        , testNewConnSecureNotes
        , testNewConnSingleOpensPrompt
        , testNewConnGroupOpensPrompt
        , testNewConnUnknownCloses
        ]
    pure (and results)

-- Dialog lifecycle --------------------------------------------------------

testDlgHelpAnyKeyCloses :: IO Bool
testDlgHelpAnyKeyCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgHelp)
    handleDialog st (KeyChar 'x')
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgHelp any key closes" True (isDlgNothing dlg)

testDlgKeysAnyKeyCloses :: IO Bool
testDlgKeysAnyKeyCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgKeys)
    handleDialog st (KeyChar 'x')
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgKeys any key closes" True (isDlgNothing dlg)

testDlgVerifyAnyKeyCloses :: IO Bool
testDlgVerifyAnyKeyCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgVerify)
    handleDialog st (KeyChar 'x')
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgVerify any key closes" True (isDlgNothing dlg)

testDlgBrowseAnyKeyCloses :: IO Bool
testDlgBrowseAnyKeyCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgBrowse)
    handleDialog st (KeyChar 'x')
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgBrowse any key closes" True (isDlgNothing dlg)

testDlgWelcomeAnyKeyCloses :: IO Bool
testDlgWelcomeAnyKeyCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgWelcome)
    handleDialog st (KeyChar 'x')
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgWelcome any key closes" True (isDlgNothing dlg)

testDlgWelcomeEscCloses :: IO Bool
testDlgWelcomeEscCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgWelcome)
    handleDialog st KeyEscape
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgWelcome esc closes" True (isDlgNothing dlg)

testDialogBlocksNormalInput :: IO Bool
testDialogBlocksNormalInput = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgHelp)
    let origFocus = ContactPane
    writeIORef (asFocus st) origFocus
    handleDialog st KeyTab  -- dialog should intercept, not toggle focus
    f <- readIORef (asFocus st)
    assertEq "dialog blocks Tab" origFocus f

-- Prompt dialog -----------------------------------------------------------

testPromptTyping :: IO Bool
testPromptTyping = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    mapM_ (\c -> handleDialog st (KeyChar c)) ("test" :: String)
    buf <- readIORef (asDialogBuf st)
    assertEq "prompt typing" "test" buf

testPromptBackspace :: IO Bool
testPromptBackspace = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    writeIORef (asDialogBuf st) "abc"
    handleDialog st KeyBackspace
    buf <- readIORef (asDialogBuf st)
    assertEq "prompt backspace" "ab" buf

testPromptBackspaceEmpty :: IO Bool
testPromptBackspaceEmpty = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    handleDialog st KeyBackspace
    buf <- readIORef (asDialogBuf st)
    assertEq "prompt backspace empty" "" buf

testPromptEnterCallback :: IO Bool
testPromptEnterCallback = do
    st <- mkTestState
    result <- newIORef ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\v -> writeIORef result v)))
    writeIORef (asDialogBuf st) "val"
    handleDialog st KeyEnter
    v <- readIORef result
    assertEq "prompt enter callback" "val" v

testPromptEnterClears :: IO Bool
testPromptEnterClears = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    writeIORef (asDialogBuf st) "val"
    handleDialog st KeyEnter
    buf <- readIORef (asDialogBuf st)
    assertEq "prompt enter clears buf" "" buf

testPromptEnterCloses :: IO Bool
testPromptEnterCloses = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    handleDialog st KeyEnter
    dlg <- readIORef (asDialogMode st)
    assertEq "prompt enter closes" True (isDlgNothing dlg)

testPromptEscCloses :: IO Bool
testPromptEscCloses = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    handleDialog st KeyEscape
    dlg <- readIORef (asDialogMode st)
    assertEq "prompt esc closes" True (isDlgNothing dlg)

testPromptMaxLength :: IO Bool
testPromptMaxLength = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    writeIORef (asDialogBuf st) (replicate 4096 'x')
    handleDialog st (KeyChar 'y')
    buf <- readIORef (asDialogBuf st)
    assertEq "prompt max length" 4096 (length buf)

-- Settings dialog ---------------------------------------------------------

testSettingsPortPrompt :: IO Bool
testSettingsPortPrompt = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '1')
    dlg <- readIORef (asDialogMode st)
    assertEq "settings '1' opens port prompt" True (isDlgPrompt dlg)

testSettingsMDNSToggle :: IO Bool
testSettingsMDNSToggle = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    before <- readIORef (cfgMDNSEnabled (asConfig st))
    handleSettingsDlg st (KeyChar '3')
    after <- readIORef (cfgMDNSEnabled (asConfig st))
    assertEq "settings '3' toggles mDNS" True (before /= after)

testSettingsPEXToggle :: IO Bool
testSettingsPEXToggle = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    before <- readIORef (cfgPEXEnabled (asConfig st))
    handleSettingsDlg st (KeyChar '4')
    after <- readIORef (cfgPEXEnabled (asConfig st))
    assertEq "settings '4' toggles PEX" True (before /= after)

testSettingsAutoSaveToggle :: IO Bool
testSettingsAutoSaveToggle = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    before <- readIORef (cfgAutoSaveMessages (asConfig st))
    handleSettingsDlg st (KeyChar '8')
    after <- readIORef (cfgAutoSaveMessages (asConfig st))
    assertEq "settings '8' toggles auto-save" True (before /= after)

testSettingsKeysOpens :: IO Bool
testSettingsKeysOpens = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '0')
    dlg <- readIORef (asDialogMode st)
    assertEq "settings '0' opens keys" True (isDlgKeys dlg)

testSettingsUnknownCloses :: IO Bool
testSettingsUnknownCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar 'z')
    dlg <- readIORef (asDialogMode st)
    assertEq "settings unknown closes" True (isDlgNothing dlg)

-- NewConn dialog ----------------------------------------------------------

testNewConnSecureNotes :: IO Bool
testNewConnSecureNotes = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNewConnDlg st (KeyChar '1')
    sessions <- readIORef (cfgSessions (asConfig st))
    assertEq "newconn '1' creates session" True (Map.size sessions > 0)

testNewConnSingleOpensPrompt :: IO Bool
testNewConnSingleOpensPrompt = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNewConnDlg st (KeyChar '2')
    dlg <- readIORef (asDialogMode st)
    assertEq "newconn '2' opens prompt" True (isDlgPromptWithSubstring "host" dlg)

testNewConnGroupOpensPrompt :: IO Bool
testNewConnGroupOpensPrompt = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNewConnDlg st (KeyChar '3')
    dlg <- readIORef (asDialogMode st)
    assertEq "newconn '3' opens group prompt" True (isDlgPromptWithSubstring "Group" dlg)

testNewConnUnknownCloses :: IO Bool
testNewConnUnknownCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNewConnDlg st (KeyChar 'z')
    dlg <- readIORef (asDialogMode st)
    assertEq "newconn unknown closes" True (isDlgNothing dlg)

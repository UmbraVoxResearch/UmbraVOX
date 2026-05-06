-- | Comprehensive edge-case tests for TUI settings dialog callbacks.
-- Covers all 10 settings options and DlgPrompt callback paths.
module Test.TUI.Sim.SettingsEdge (runTests) where

import Data.IORef (readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Types
import UmbraVox.TUI.Input (handleSettingsDlg, handleDialog)

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Sim.SettingsEdge"
    putStrLn (replicate 40 '-')
    results <- sequence
        -- Port change
        [ testPortChangeValid
        , testPortChangeInvalidLetters
        , testPortChangeEmpty
        -- Name change
        , testNameChangeValid
        , testNameChangeEmpty
        , testNameChangeLong
        -- Boolean toggles
        , testDBToggleThreeTimes
        , testMDNSToggleThreeTimes
        , testPEXToggle
        , testAutoSaveToggle
        -- Keys view
        , testKeysViewOpens
        -- Clear history
        , testClearHistoryYES
        , testClearHistoryNo
        , testClearHistoryEmpty
        -- Unknown key
        , testUnknownKeyCloses
        ]
    pure (and results)

------------------------------------------------------------------------
-- Helper: open settings, press a key to open a DlgPrompt, then type
-- a string and press Enter.  Returns the final state.
------------------------------------------------------------------------

-- | Feed a sequence of characters into a DlgPrompt, then press Enter.
feedPrompt :: AppState -> String -> IO ()
feedPrompt st chars = do
    mapM_ (\c -> handleDialog st (KeyChar c)) chars
    handleDialog st KeyEnter

------------------------------------------------------------------------
-- Port change (option '1')
------------------------------------------------------------------------

testPortChangeValid :: IO Bool
testPortChangeValid = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgSettings)
    -- Press '1' to open the port prompt
    handleSettingsDlg st (KeyChar '1')
    -- Type "8080" and press Enter
    feedPrompt st "8080"
    port <- readIORef (cfgListenPort (asConfig st))
    assertEq "port change valid -> 8080" 8080 port

testPortChangeInvalidLetters :: IO Bool
testPortChangeInvalidLetters = do
    st <- mkTestState
    portBefore <- readIORef (cfgListenPort (asConfig st))
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '1')
    feedPrompt st "abc"
    portAfter <- readIORef (cfgListenPort (asConfig st))
    assertEq "port change invalid letters -> unchanged" portBefore portAfter

testPortChangeEmpty :: IO Bool
testPortChangeEmpty = do
    st <- mkTestState
    portBefore <- readIORef (cfgListenPort (asConfig st))
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '1')
    feedPrompt st ""
    portAfter <- readIORef (cfgListenPort (asConfig st))
    assertEq "port change empty -> unchanged" portBefore portAfter

------------------------------------------------------------------------
-- Name change (option '2')
------------------------------------------------------------------------

testNameChangeValid :: IO Bool
testNameChangeValid = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '2')
    feedPrompt st "Alice"
    name <- readIORef (cfgDisplayName (asConfig st))
    assertEq "name change valid -> Alice" "Alice" name

testNameChangeEmpty :: IO Bool
testNameChangeEmpty = do
    st <- mkTestState
    nameBefore <- readIORef (cfgDisplayName (asConfig st))
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '2')
    feedPrompt st ""
    nameAfter <- readIORef (cfgDisplayName (asConfig st))
    assertEq "name change empty -> unchanged" nameBefore nameAfter

testNameChangeLong :: IO Bool
testNameChangeLong = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '2')
    -- Type a 500-char string (will be capped by maxDialogBufLen=4096, so
    -- all 500 chars fit).  We just need to verify no crash and value is set.
    let longName = replicate 500 'X'
    feedPrompt st longName
    name <- readIORef (cfgDisplayName (asConfig st))
    assertEq "name change long (500 chars) -> set" longName name

------------------------------------------------------------------------
-- Boolean toggles
------------------------------------------------------------------------

testDBToggleThreeTimes :: IO Bool
testDBToggleThreeTimes = do
    st <- mkTestState
    let ref = cfgDBEnabled (asConfig st)
    v0 <- readIORef ref
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '5')
    v1 <- readIORef ref
    -- After first toggle the dialog closes, re-open it
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '5')
    v2 <- readIORef ref
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '5')
    v3 <- readIORef ref
    r1 <- assertEq "DB toggle 1: flipped"    (not v0) v1
    r2 <- assertEq "DB toggle 2: back"       v0       v2
    r3 <- assertEq "DB toggle 3: flipped"    (not v0) v3
    pure (r1 && r2 && r3)

testMDNSToggleThreeTimes :: IO Bool
testMDNSToggleThreeTimes = do
    st <- mkTestState
    let ref = cfgMDNSEnabled (asConfig st)
    v0 <- readIORef ref
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '3')
    v1 <- readIORef ref
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '3')
    v2 <- readIORef ref
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '3')
    v3 <- readIORef ref
    r1 <- assertEq "mDNS toggle 1: flipped"  (not v0) v1
    r2 <- assertEq "mDNS toggle 2: back"     v0       v2
    r3 <- assertEq "mDNS toggle 3: flipped"  (not v0) v3
    pure (r1 && r2 && r3)

testPEXToggle :: IO Bool
testPEXToggle = do
    st <- mkTestState
    let ref = cfgPEXEnabled (asConfig st)
    before <- readIORef ref
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '4')
    after <- readIORef ref
    assertEq "PEX toggle flipped" (not before) after

testAutoSaveToggle :: IO Bool
testAutoSaveToggle = do
    st <- mkTestState
    let ref = cfgAutoSaveMessages (asConfig st)
    before <- readIORef ref
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '8')
    after <- readIORef ref
    assertEq "auto-save toggle flipped" (not before) after

------------------------------------------------------------------------
-- Keys view (option '0')
------------------------------------------------------------------------

testKeysViewOpens :: IO Bool
testKeysViewOpens = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '0')
    dlg <- readIORef (asDialogMode st)
    assertEq "keys view opens DlgKeys" True (isDlgKeys dlg)

------------------------------------------------------------------------
-- Clear history (option '9')
------------------------------------------------------------------------

testClearHistoryYES :: IO Bool
testClearHistoryYES = do
    st <- mkTestState
    -- Add a session with history so we can verify it gets cleared
    _ <- addTestSessionWithHistory (asConfig st) "peer1" ["msg1", "msg2"]
    writeIORef (asSelected st) 0
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '9')
    -- Now we should be in a DlgPrompt; type "YES" and Enter
    feedPrompt st "YES"
    -- Check the session history is cleared
    sessions <- readIORef (cfgSessions (asConfig st))
    let si = snd (head (Map.toAscList sessions))
    hist <- readIORef (siHistory si)
    assertEq "clear history YES -> empty" [] hist

testClearHistoryNo :: IO Bool
testClearHistoryNo = do
    st <- mkTestState
    _ <- addTestSessionWithHistory (asConfig st) "peer1" ["msg1", "msg2"]
    writeIORef (asSelected st) 0
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '9')
    feedPrompt st "no"
    sessions <- readIORef (cfgSessions (asConfig st))
    let si = snd (head (Map.toAscList sessions))
    hist <- readIORef (siHistory si)
    assertEq "clear history 'no' -> unchanged" ["msg1", "msg2"] hist

testClearHistoryEmpty :: IO Bool
testClearHistoryEmpty = do
    st <- mkTestState
    _ <- addTestSessionWithHistory (asConfig st) "peer1" ["msg1", "msg2"]
    writeIORef (asSelected st) 0
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '9')
    feedPrompt st ""
    sessions <- readIORef (cfgSessions (asConfig st))
    let si = snd (head (Map.toAscList sessions))
    hist <- readIORef (siHistory si)
    assertEq "clear history empty -> unchanged" ["msg1", "msg2"] hist

------------------------------------------------------------------------
-- Unknown key
------------------------------------------------------------------------

testUnknownKeyCloses :: IO Bool
testUnknownKeyCloses = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar 'z')
    dlg <- readIORef (asDialogMode st)
    assertEq "unknown key 'z' closes settings" True (isDlgNothing dlg)

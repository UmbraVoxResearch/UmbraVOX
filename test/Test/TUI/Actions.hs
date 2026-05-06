-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.TUI.Actions
-- Most functions in Actions are IO-heavy (session management, terminal control).
-- We test the isPfx-based /file command detection logic and other testable behaviors.
module Test.TUI.Actions (runTests) where

import Data.IORef (readIORef, writeIORef)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import Test.Util (assertEq)
import Test.TUI.Sim.Util (mkTestState, addTestSession, addTestSessionWithHistory, isDlgNothing, isDlgPrompt)
import UmbraVox.TUI.Render (isPfx)
import UmbraVox.TUI.Actions.Export (startExport, exportToPath, startImport)
import UmbraVox.TUI.Types (asConfig, asDialogMode, asSelected, asStatusMsg, cfgSessions, siHistory, DialogMode(..))
import qualified Data.Map.Strict as Map

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Actions"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testFileCommandDetection
        , testFileCommandNoMatch
        , testFileCommandEmpty
        , testFileCommandPartial
        , testFileCommandExact
        , testStartExportRejectsNegativeSelection
        , testExportToPathRejectsNegativeSelectionCallback
        , testStartImportRejectsNegativeSelection
        , testExportImportUtf8RoundTrip
        ]
    pure (and results)

-- sendCurrentMessage uses isPfx "/file " to detect file sends.
-- We test that logic here since it's the core pure decision in Actions.

testFileCommandDetection :: IO Bool
testFileCommandDetection = assertEq "isPfx /file detects file command"
    True (isPfx "/file " "/file /tmp/test.txt")

testFileCommandNoMatch :: IO Bool
testFileCommandNoMatch = assertEq "isPfx /file rejects normal message"
    False (isPfx "/file " "hello world")

testFileCommandEmpty :: IO Bool
testFileCommandEmpty = assertEq "isPfx /file rejects empty"
    False (isPfx "/file " "")

testFileCommandPartial :: IO Bool
testFileCommandPartial = assertEq "isPfx /file rejects partial"
    False (isPfx "/file " "/file")

testFileCommandExact :: IO Bool
testFileCommandExact = assertEq "isPfx /file with just space"
    True (isPfx "/file " "/file ")

testStartExportRejectsNegativeSelection :: IO Bool
testStartExportRejectsNegativeSelection = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "peer"
    writeIORef (asSelected st) (-1)
    startExport st
    status <- readIORef (asStatusMsg st)
    dlg <- readIORef (asDialogMode st)
    a <- assertEq "startExport negative selection status" "No contact selected" status
    b <- assertEq "startExport negative selection leaves no prompt" True (isDlgNothing dlg)
    pure (a && b)

testExportToPathRejectsNegativeSelectionCallback :: IO Bool
testExportToPathRejectsNegativeSelectionCallback = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "peer"
    writeIORef (asSelected st) (-1)
    exportToPath st "build/test-export-negative-selection.enc"
    dlg <- readIORef (asDialogMode st)
    okPrompt <- assertEq "exportToPath opens password prompt" True (isDlgPrompt dlg)
    case dlg of
        Just (DlgPrompt _ onSubmit) -> do
            onSubmit "pw"
            status <- readIORef (asStatusMsg st)
            okStatus <- assertEq "exportToPath negative selection status" "No contact selected" status
            pure (okPrompt && okStatus)
        _ -> pure False

testStartImportRejectsNegativeSelection :: IO Bool
testStartImportRejectsNegativeSelection = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "peer"
    writeIORef (asSelected st) (-1)
    startImport st
    status <- readIORef (asStatusMsg st)
    dlg <- readIORef (asDialogMode st)
    a <- assertEq "startImport negative selection status" "No contact selected" status
    b <- assertEq "startImport negative selection leaves no prompt" True (isDlgNothing dlg)
    pure (a && b)

testExportImportUtf8RoundTrip :: IO Bool
testExportImportUtf8RoundTrip = do
    createDirectoryIfMissing True "build"
    let path = "build/test-export-utf8.enc"
        msgs =
            [ "[09:00] Peer: \x4F60\x597D"
            , "[09:01] You: \x1F44B cafe\x0301"
            ]
        password = "\x5BC6\x7801"
    exists <- doesFileExist path
    if exists then removeFile path else pure ()
    st <- mkTestState
    _ <- addTestSessionWithHistory (asConfig st) "peer" msgs
    writeIORef (asSelected st) 0
    exportToPath st path
    exportDlg <- readIORef (asDialogMode st)
    exportOk <- case exportDlg of
        Just (DlgPrompt _ onSubmit) -> onSubmit password >> pure True
        _ -> pure False
    sessions <- readIORef (cfgSessions (asConfig st))
    let (_, si) = head (Map.toList sessions)
    writeIORef (siHistory si) []
    startImport st
    importDlg1 <- readIORef (asDialogMode st)
    case importDlg1 of
        Just (DlgPrompt _ onPath) -> onPath path
        _ -> pure ()
    importDlg2 <- readIORef (asDialogMode st)
    importOk <- case importDlg2 of
        Just (DlgPrompt _ onPassword) -> onPassword password >> pure True
        _ -> pure False
    hist <- readIORef (siHistory si)
    status <- readIORef (asStatusMsg st)
    finalExists <- doesFileExist path
    if finalExists then removeFile path else pure ()
    a <- assertEq "export/import utf8 prompt flow" True (exportOk && importOk)
    b <- assertEq "export/import utf8 history restored" msgs hist
    c <- assertEq "export/import utf8 status" ("Imported " ++ show (length msgs) ++ " msgs from " ++ path) status
    pure (a && b && c)

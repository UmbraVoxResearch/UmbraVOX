-- | TUI simulation tests: global keyboard shortcuts
module Test.TUI.Sim.Shortcuts (runTests) where

import Data.IORef (readIORef, writeIORef)
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Types
import UmbraVox.TUI.Input (handleNormal)

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Sim.Shortcuts"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testCtrlNOpensNewConn
        , testEscapeClosesDialog
        , testEscapeNoOpWithoutDialog
        , testF1OpensFile
        , testF2OpensContacts
        , testF3OpensChat
        , testF4OpensPrefs
        , testF5OpensHelp
        , testUnknownKeyNoOp
        ]
    pure (and results)

testCtrlNOpensNewConn :: IO Bool
testCtrlNOpensNewConn = do
    st <- mkTestState
    handleNormal st KeyCtrlN
    dlg <- readIORef (asDialogMode st)
    assertEq "Ctrl+N opens NewConn" True (isDlgNewConn dlg)

testEscapeClosesDialog :: IO Bool
testEscapeClosesDialog = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgHelp)
    handleNormal st KeyEscape
    dlg <- readIORef (asDialogMode st)
    assertEq "Escape closes dialog" True (isDlgNothing dlg)

testEscapeNoOpWithoutDialog :: IO Bool
testEscapeNoOpWithoutDialog = do
    st <- mkTestState
    handleNormal st KeyEscape
    dlg <- readIORef (asDialogMode st)
    assertEq "Escape no-op without dialog" True (isDlgNothing dlg)

testF1OpensFile :: IO Bool
testF1OpensFile = do
    st <- mkTestState; handleNormal st KeyF1
    m <- readIORef (asMenuOpen st)
    assertEq "F1 opens File" (Just MenuFile) m

testF2OpensContacts :: IO Bool
testF2OpensContacts = do
    st <- mkTestState; handleNormal st KeyF2
    m <- readIORef (asMenuOpen st)
    assertEq "F2 opens Contacts" (Just MenuContacts) m

testF3OpensChat :: IO Bool
testF3OpensChat = do
    st <- mkTestState; handleNormal st KeyF3
    m <- readIORef (asMenuOpen st)
    assertEq "F3 opens Chat" (Just MenuChat) m

testF4OpensPrefs :: IO Bool
testF4OpensPrefs = do
    st <- mkTestState; handleNormal st KeyF4
    m <- readIORef (asMenuOpen st)
    assertEq "F4 opens Prefs" (Just MenuPrefs) m

testF5OpensHelp :: IO Bool
testF5OpensHelp = do
    st <- mkTestState; handleNormal st KeyF5
    m <- readIORef (asMenuOpen st)
    assertEq "F5 opens Help" (Just MenuHelp) m

testUnknownKeyNoOp :: IO Bool
testUnknownKeyNoOp = do
    st <- mkTestState
    handleNormal st KeyUnknown
    sel <- readIORef (asSelected st)
    f <- readIORef (asFocus st)
    assertEq "unknown key no-op" True (sel == 0 && f == ContactPane)

-- | TUI simulation tests: complete user workflow sequences
module Test.TUI.Sim.Workflows (runTests) where

import Data.IORef (readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Types
import UmbraVox.TUI.Input (handleNormal, handleContact, handleChat, handleDialog,
                           handleNewConnDlg)
import UmbraVox.TUI.Menu (handleMenu, openMenu)

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Sim.Workflows"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testWorkflowCreateSecureNotes
        , testWorkflowTypeAndClear
        , testWorkflowOpenCloseMultipleMenus
        , testWorkflowNavigateContacts
        , testWorkflowEscapeFromNested
        ]
    pure (and results)

-- | F2 → Down → Enter → '1' → session created
testWorkflowCreateSecureNotes :: IO Bool
testWorkflowCreateSecureNotes = do
    st <- mkTestState
    handleNormal st KeyF2         -- open Contacts menu
    handleMenu st KeyEnter        -- activate → opens DlgNewConn
    dlg <- readIORef (asDialogMode st)
    ok1 <- assertEq "workflow: new connection dialog opened" True (isDlgNewConn dlg)
    handleNewConnDlg st (KeyChar '1')  -- create secure notes
    sessions <- readIORef (cfgSessions (asConfig st))
    ok2 <- assertEq "workflow: secure notes created" True (Map.size sessions > 0)
    pure (ok1 && ok2)

-- | Tab to chat → type → F3 → Down → Enter (Clear) → empty
testWorkflowTypeAndClear :: IO Bool
testWorkflowTypeAndClear = do
    st <- mkTestState
    handleNormal st KeyTab         -- → ChatPane
    mapM_ (handleChat st . KeyChar) ("hello world" :: String)
    buf1 <- readIORef (asInputBuf st)
    ok1 <- assertEq "workflow: typed text" "hello world" buf1
    handleNormal st KeyF3          -- open Chat menu
    handleMenu st KeyDown          -- move to "Clear Input" (index 1)
    handleMenu st KeyEnter         -- activate
    buf2 <- readIORef (asInputBuf st)
    ok2 <- assertEq "workflow: cleared" "" buf2
    pure (ok1 && ok2)

-- | F1 → Esc → F2 → Esc → F5 → Esc → all closed
testWorkflowOpenCloseMultipleMenus :: IO Bool
testWorkflowOpenCloseMultipleMenus = do
    st <- mkTestState
    handleNormal st KeyF1; handleMenu st KeyEscape
    handleNormal st KeyF2; handleMenu st KeyEscape
    handleNormal st KeyF5; handleMenu st KeyEscape
    m <- readIORef (asMenuOpen st)
    assertEq "workflow: all menus closed" Nothing m

-- | Add 5 sessions → Down 3x → Enter → verify
testWorkflowNavigateContacts :: IO Bool
testWorkflowNavigateContacts = do
    st <- mkTestState
    mapM_ (\i -> addTestSession (asConfig st) ("peer-" ++ show i)) [1..5 :: Int]
    handleContact st KeyDown >> handleContact st KeyDown >> handleContact st KeyDown
    sel <- readIORef (asSelected st)
    ok1 <- assertEq "workflow: navigated to 3" 3 sel
    handleContact st KeyEnter
    f <- readIORef (asFocus st)
    ok2 <- assertEq "workflow: switched to chat" ChatPane f
    pure (ok1 && ok2)

-- | Open DlgSettings → '1' → DlgPrompt → Esc → Nothing (not back to Settings)
testWorkflowEscapeFromNested :: IO Bool
testWorkflowEscapeFromNested = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleDialog st (KeyChar '1')  -- opens DlgPrompt for port
    dlg1 <- readIORef (asDialogMode st)
    ok1 <- assertEq "workflow: prompt opened" True (isDlgPrompt dlg1)
    handleDialog st KeyEscape      -- closes prompt
    dlg2 <- readIORef (asDialogMode st)
    ok2 <- assertEq "workflow: back to Nothing" True (isDlgNothing dlg2)
    pure (ok1 && ok2)

-- | TUI simulation test orchestrator.
-- Delegates to modular test suites in Test.TUI.Sim.*
module Test.TUI.Paths (runTests) where

import qualified Test.TUI.Sim.Contacts as Contacts
import qualified Test.TUI.Sim.Chat as Chat
import qualified Test.TUI.Sim.Focus as Focus
import qualified Test.TUI.Sim.Shortcuts as Shortcuts
import qualified Test.TUI.Sim.Menu as Menu
import qualified Test.TUI.Sim.Dialogs as Dialogs
import qualified Test.TUI.Sim.Workflows as Workflows

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Paths (simulation orchestrator)"
    putStrLn (replicate 40 '-')
    c <- Contacts.runTests;  putStrLn ""
    h <- Chat.runTests;      putStrLn ""
    f <- Focus.runTests;     putStrLn ""
    s <- Shortcuts.runTests; putStrLn ""
    m <- Menu.runTests;      putStrLn ""
    d <- Dialogs.runTests;   putStrLn ""
    w <- Workflows.runTests
    pure (c && h && f && s && m && d && w)

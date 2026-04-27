-- | Tests for UmbraVox.TUI.Actions
-- Most functions in Actions are IO-heavy (session management, terminal control).
-- We test the isPfx-based /file command detection logic and other testable behaviors.
module Test.TUI.Actions (runTests) where

import Test.Util (assertEq)
import UmbraVox.TUI.Render (isPfx)

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

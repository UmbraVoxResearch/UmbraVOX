-- SPDX-License-Identifier: Apache-2.0
-- | TUI simulation tests: contact pane navigation
module Test.TUI.Sim.Contacts (runTests) where

import Data.IORef (readIORef, writeIORef)
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Types
import UmbraVox.TUI.Input (handleContact)

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Sim.Contacts"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testContactUpEmpty
        , testContactDownEmpty
        , testContactUpWithSessions
        , testContactDownWithSessions
        , testContactEnterSwitchesPane
        , testContactEnterResetsScroll
        , testContactDownClampAtEnd
        ]
    pure (and results)

testContactUpEmpty :: IO Bool
testContactUpEmpty = do
    st <- mkTestState
    handleContact st KeyUp
    sel <- readIORef (asSelected st)
    assertEq "contact up empty no-op" 0 sel

testContactDownEmpty :: IO Bool
testContactDownEmpty = do
    st <- mkTestState
    handleContact st KeyDown
    sel <- readIORef (asSelected st)
    assertEq "contact down empty stays 0" 0 sel

testContactUpWithSessions :: IO Bool
testContactUpWithSessions = do
    st <- mkTestState
    addTestSession (asConfig st) "a" >> addTestSession (asConfig st) "b" >> addTestSession (asConfig st) "c"
    writeIORef (asSelected st) 2
    handleContact st KeyUp
    sel <- readIORef (asSelected st)
    assertEq "contact up: 2 -> 1" 1 sel

testContactDownWithSessions :: IO Bool
testContactDownWithSessions = do
    st <- mkTestState
    addTestSession (asConfig st) "a" >> addTestSession (asConfig st) "b" >> addTestSession (asConfig st) "c"
    writeIORef (asSelected st) 0
    handleContact st KeyDown
    sel <- readIORef (asSelected st)
    assertEq "contact down: 0 -> 1" 1 sel

testContactEnterSwitchesPane :: IO Bool
testContactEnterSwitchesPane = do
    st <- mkTestState
    handleContact st KeyEnter
    f <- readIORef (asFocus st)
    assertEq "contact enter -> ChatPane" ChatPane f

testContactEnterResetsScroll :: IO Bool
testContactEnterResetsScroll = do
    st <- mkTestState
    writeIORef (asChatScroll st) 10
    handleContact st KeyEnter
    s <- readIORef (asChatScroll st)
    assertEq "contact enter resets scroll" 0 s

testContactDownClampAtEnd :: IO Bool
testContactDownClampAtEnd = do
    st <- mkTestState
    addTestSession (asConfig st) "a" >> addTestSession (asConfig st) "b" >> addTestSession (asConfig st) "c"
    writeIORef (asSelected st) 2
    handleContact st KeyDown
    sel <- readIORef (asSelected st)
    assertEq "contact down clamp at 2" 2 sel

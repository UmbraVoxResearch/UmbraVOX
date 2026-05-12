-- SPDX-License-Identifier: Apache-2.0
-- | TUI simulation tests: contact pane navigation
module Test.TUI.Sim.Contacts (runTests) where

import Data.IORef (readIORef, writeIORef)
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Types
import UmbraVox.TUI.Input (handleContact)
import UmbraVox.TUI.Layout (calcLayout)

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
        , testContactPageDownUsesResponsiveVisibleRows
        , testContactPageUpUsesResponsiveVisibleRows
        , testContactPageDownAdjustsContactScroll
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

testContactPageDownUsesResponsiveVisibleRows :: IO Bool
testContactPageDownUsesResponsiveVisibleRows = do
    st <- mkTestState
    mapM_ (\i -> addTestSession (asConfig st) ("peer-" ++ show i)) [1 .. 20 :: Int]
    writeIORef (asLayout st) (calcLayout 24 80)
    writeIORef (asSelected st) 0
    handleContact st KeyPageDown
    sel <- readIORef (asSelected st)
    -- At 24x80: visRows = lChatH - lIdentityH = 13
    assertEq "contact page down moves by responsive page height" 13 sel

testContactPageUpUsesResponsiveVisibleRows :: IO Bool
testContactPageUpUsesResponsiveVisibleRows = do
    st <- mkTestState
    mapM_ (\i -> addTestSession (asConfig st) ("peer-" ++ show i)) [1 .. 20 :: Int]
    writeIORef (asLayout st) (calcLayout 24 80)
    writeIORef (asSelected st) 15
    handleContact st KeyPageUp
    sel <- readIORef (asSelected st)
    assertEq "contact page up moves by responsive page height" 2 sel

testContactPageDownAdjustsContactScroll :: IO Bool
testContactPageDownAdjustsContactScroll = do
    st <- mkTestState
    mapM_ (\i -> addTestSession (asConfig st) ("peer-" ++ show i)) [1 .. 60 :: Int]
    writeIORef (asLayout st) (calcLayout 24 80)
    writeIORef (asSelected st) 0
    writeIORef (asContactScroll st) 0
    handleContact st KeyPageDown
    sel <- readIORef (asSelected st)
    off <- readIORef (asContactScroll st)
    ok1 <- assertEq "contact page down updates selection" 13 sel
    ok2 <- assertEq "contact page down keeps selection visible via scroll" 1 off
    pure (ok1 && ok2)

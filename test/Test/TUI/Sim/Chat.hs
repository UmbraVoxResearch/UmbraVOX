-- | TUI simulation tests: chat pane input and scrolling
module Test.TUI.Sim.Chat (runTests) where

import Data.IORef (readIORef, writeIORef)
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Types
import UmbraVox.TUI.Input (handleChat)
import UmbraVox.TUI.Layout (calcLayout)

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Sim.Chat"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testChatInputAccumulates
        , testChatInputMaxLength
        , testChatBackspaceEmpty
        , testChatBackspaceRemoves
        , testChatScrollUpIncrement
        , testChatScrollDownAtZero
        , testChatPageUpIncrement
        , testChatPageDownClampZero
        ]
    pure (and results)

testChatInputAccumulates :: IO Bool
testChatInputAccumulates = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    mapM_ (handleChat st . KeyChar) ("hello" :: String)
    buf <- readIORef (asInputBuf st)
    assertEq "chat input accumulates" "hello" buf

testChatInputMaxLength :: IO Bool
testChatInputMaxLength = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) (replicate 4096 'x')
    handleChat st (KeyChar 'y')
    buf <- readIORef (asInputBuf st)
    assertEq "chat input max length" 4096 (length buf)

testChatBackspaceEmpty :: IO Bool
testChatBackspaceEmpty = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    handleChat st KeyBackspace
    buf <- readIORef (asInputBuf st)
    assertEq "chat backspace empty" "" buf

testChatBackspaceRemoves :: IO Bool
testChatBackspaceRemoves = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) "abc"
    handleChat st KeyBackspace
    buf <- readIORef (asInputBuf st)
    assertEq "chat backspace removes" "ab" buf

testChatScrollUpIncrement :: IO Bool
testChatScrollUpIncrement = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    handleChat st KeyUp
    s <- readIORef (asChatScroll st)
    assertEq "chat scroll up" 1 s

testChatScrollDownAtZero :: IO Bool
testChatScrollDownAtZero = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    handleChat st KeyDown
    s <- readIORef (asChatScroll st)
    assertEq "chat scroll down at 0" 0 s

testChatPageUpIncrement :: IO Bool
testChatPageUpIncrement = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    lay <- readIORef (asLayout st)
    handleChat st KeyPageUp
    s <- readIORef (asChatScroll st)
    assertEq "chat page up" (lChatH lay) s

testChatPageDownClampZero :: IO Bool
testChatPageDownClampZero = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    writeIORef (asChatScroll st) 3
    handleChat st KeyPageDown
    s <- readIORef (asChatScroll st)
    assertEq "chat page down clamp" 0 s

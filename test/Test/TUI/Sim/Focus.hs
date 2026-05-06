-- | TUI simulation tests: Tab cycling and focus management
module Test.TUI.Sim.Focus (runTests) where

import Data.IORef (readIORef, writeIORef)
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Types
import UmbraVox.TUI.Input (handleNormal, handleContact, handleChat)

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Sim.Focus"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testTabContactToChat
        , testTabChatToContact
        , testTabRoundtrip
        , testTabPreservesSelection
        , testTabPreservesScroll
        , testFocusAffectsContactKeys
        , testFocusAffectsChatKeys
        ]
    pure (and results)

testTabContactToChat :: IO Bool
testTabContactToChat = do
    st <- mkTestState
    handleNormal st KeyTab
    f <- readIORef (asFocus st)
    assertEq "tab: contact -> chat" ChatPane f

testTabChatToContact :: IO Bool
testTabChatToContact = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    handleNormal st KeyTab
    f <- readIORef (asFocus st)
    assertEq "tab: chat -> contact" ContactPane f

testTabRoundtrip :: IO Bool
testTabRoundtrip = do
    st <- mkTestState
    handleNormal st KeyTab >> handleNormal st KeyTab
    f <- readIORef (asFocus st)
    assertEq "tab roundtrip" ContactPane f

testTabPreservesSelection :: IO Bool
testTabPreservesSelection = do
    st <- mkTestState
    addTestSession (asConfig st) "a" >> addTestSession (asConfig st) "b" >> addTestSession (asConfig st) "c"
    writeIORef (asSelected st) 2
    handleNormal st KeyTab >> handleNormal st KeyTab
    sel <- readIORef (asSelected st)
    assertEq "tab preserves selection" 2 sel

testTabPreservesScroll :: IO Bool
testTabPreservesScroll = do
    st <- mkTestState
    writeIORef (asChatScroll st) 5
    handleNormal st KeyTab >> handleNormal st KeyTab
    s <- readIORef (asChatScroll st)
    assertEq "tab preserves scroll" 5 s

testFocusAffectsContactKeys :: IO Bool
testFocusAffectsContactKeys = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    addTestSession (asConfig st) "a" >> addTestSession (asConfig st) "b"
    handleChat st KeyUp  -- in chat mode, Up scrolls chat, not contacts
    sel <- readIORef (asSelected st)
    assertEq "chat KeyUp doesn't affect selection" 0 sel

testFocusAffectsChatKeys :: IO Bool
testFocusAffectsChatKeys = do
    st <- mkTestState  -- focus = ContactPane by default
    handleContact st (KeyChar 'a')  -- unknown key in contact pane
    buf <- readIORef (asInputBuf st)
    assertEq "contact KeyChar doesn't affect input" "" buf

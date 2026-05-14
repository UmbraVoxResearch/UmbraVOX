-- SPDX-License-Identifier: Apache-2.0
-- | TUI simulation tests: chat pane input and scrolling
module Test.TUI.Sim.Chat (runTests) where

import Data.IORef (readIORef, writeIORef)
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.InputBuffer
    ( computeInputBufferLayout
    , cursorScreenOffset
    )
import UmbraVox.TUI.Types
import UmbraVox.TUI.Input (handleChat, handleNormal)
import qualified UmbraVox.TUI.Layout as Layout
import UmbraVox.TUI.InputBuffer (inputPrefix)
import UmbraVox.TUI.Text (displayWidth)

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Sim.Chat"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testChatInputAccumulates
        , testChatInputMaxLength
        , testChatBackspaceEmpty
        , testChatBackspaceRemoves
        , testChatInsertAtCursor
        , testChatMouseClickMovesCursor
        , testChatMouseClickMovesCursorOnWrappedLine
        , testChatVerticalCursorMovementAcrossWrappedLines
        , testChatHomeMovesCursorToStart
        , testChatEndMovesCursorToEnd
        , testRichToolbarToggle
        , testRichToolbarBoldPrompt
        , testRichCursorSkipsMarkdownMarkers
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
    writeIORef (asInputCursor st) 3
    handleChat st KeyBackspace
    buf <- readIORef (asInputBuf st)
    assertEq "chat backspace removes" "ab" buf

testChatInsertAtCursor :: IO Bool
testChatInsertAtCursor = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) "hllo"
    writeIORef (asInputCursor st) 1
    handleChat st (KeyChar 'e')
    buf <- readIORef (asInputBuf st)
    cursor <- readIORef (asInputCursor st)
    a <- assertEq "chat inserts at cursor" "hello" buf
    b <- assertEq "chat cursor advances after insert" 2 cursor
    pure (a && b)

testChatMouseClickMovesCursor :: IO Bool
testChatMouseClickMovesCursor = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) "hello"
    let lay = calcTestLayout
        (inputRow0, inputCol0, _, _) = Layout.inputEntryBounds lay
        prefixW = displayWidth inputPrefix
    handleNormal st (KeyMouseLeft inputRow0 (inputCol0 + prefixW + 2))
    cursor <- readIORef (asInputCursor st)
    handleChat st (KeyChar 'X')
    buf <- readIORef (asInputBuf st)
    a <- assertEq "mouse click places cursor in input area" 2 cursor
    b <- assertEq "chat inserts at clicked cursor" "heXllo" buf
    pure (a && b)

testChatMouseClickMovesCursorOnWrappedLine :: IO Bool
testChatMouseClickMovesCursorOnWrappedLine = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    let lay = calcTestLayout
        bodyW = max 0 (lRightW lay - 1)
        (_, _, _, entryRows) = Layout.inputEntryBounds lay
        buf = replicate 90 'x'
        layout = computeInputBufferLayout bodyW entryRows buf
        (inputRow0, inputCol0, _, _) = Layout.inputEntryBounds lay
        targetCursor = 80
    writeIORef (asInputBuf st) buf
    case cursorScreenOffset layout 0 targetCursor of
        Nothing -> assertEq "wrapped click has visible cursor position" True False
        Just (rowOff, colOff) -> do
            okWrapped <- assertEq "wrapped click target is on continuation row" True (rowOff > 0)
            handleNormal st (KeyMouseLeft (inputRow0 + rowOff) (inputCol0 + colOff))
            cursor <- readIORef (asInputCursor st)
            okCursor <- assertEq "mouse click places cursor on wrapped continuation line" targetCursor cursor
            pure (okWrapped && okCursor)

testChatVerticalCursorMovementAcrossWrappedLines :: IO Bool
testChatVerticalCursorMovementAcrossWrappedLines = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    let buf = replicate 90 'x'
    writeIORef (asInputBuf st) buf
    writeIORef (asInputCursor st) 80
    handleChat st KeyUp
    cursorUp <- readIORef (asInputCursor st)
    handleChat st KeyDown
    cursorDown <- readIORef (asInputCursor st)
    a <- assertEq "chat KeyUp moves cursor to previous wrapped line" True (cursorUp < 80)
    b <- assertEq "chat KeyDown restores cursor to wrapped continuation line" 80 cursorDown
    pure (a && b)

testChatHomeMovesCursorToStart :: IO Bool
testChatHomeMovesCursorToStart = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) "hello"
    writeIORef (asInputCursor st) 3
    handleChat st KeyHome
    cursor <- readIORef (asInputCursor st)
    assertEq "chat Home moves cursor to start" 0 cursor

testChatEndMovesCursorToEnd :: IO Bool
testChatEndMovesCursorToEnd = do
    st <- mkTestState; writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) "hello"
    writeIORef (asInputCursor st) 1
    handleChat st KeyEnd
    cursor <- readIORef (asInputCursor st)
    assertEq "chat End moves cursor to end" 5 cursor

testRichToolbarToggle :: IO Bool
testRichToolbarToggle = do
    st <- mkTestState
    let lay = calcTestLayout
        (toolbarRow0, toolbarCol0, _, _) = Layout.inputToolbarBounds lay
    writeIORef (asRichText st) True
    handleNormal st (KeyMouseLeft toolbarRow0 (toolbarCol0 + 11))
    richEnabled <- readIORef (asRichText st)
    assertEq "plain toolbar button disables rich text" False richEnabled

testRichToolbarBoldPrompt :: IO Bool
testRichToolbarBoldPrompt = do
    st <- mkTestState
    let lay = calcTestLayout
        (toolbarRow0, toolbarCol0, _, _) = Layout.inputToolbarBounds lay
    -- Bold button is "[ Bold ]" at relative offset 20-27 (after "[ Rich* ] [ Plain ] ")
    handleNormal st (KeyMouseLeft toolbarRow0 (toolbarCol0 + 23))
    dlg <- readIORef (asDialogMode st)
    assertEq "bold toolbar button opens prompt" True (isDlgPromptWithSubstring "Bold text" dlg)

testRichCursorSkipsMarkdownMarkers :: IO Bool
testRichCursorSkipsMarkdownMarkers = do
    st <- mkTestState
    writeIORef (asFocus st) ChatPane
    writeIORef (asRichText st) True
    writeIORef (asInputBuf st) "**ab**"
    writeIORef (asInputCursor st) 4
    handleChat st KeyLeft
    cursor1 <- readIORef (asInputCursor st)
    handleChat st KeyLeft
    cursor2 <- readIORef (asInputCursor st)
    handleChat st (KeyChar 'X')
    buf <- readIORef (asInputBuf st)
    a <- assertEq "rich cursor moves across visible markdown glyphs only" 3 cursor1
    b <- assertEq "rich cursor skips opening markers" 2 cursor2
    c <- assertEq "rich insert stays inside markdown span" "**Xab**" buf
    pure (a && b && c)

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

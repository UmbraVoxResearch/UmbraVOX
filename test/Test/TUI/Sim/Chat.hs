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
import UmbraVox.TUI.Input (handleChat, handleNormal, handleDialog)
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
        , testRichToolbarItalicPrompt
        , testRichToolbarColorPrompt
        , testRichToolbarLinkOpensDialog
        , testRichToolbarEmojiOpensDialog
        , testRichCursorSkipsMarkdownMarkers
        , testRichCursorSkipsMarkdownMarkersRight
        , testInsertLinkDialogTabCycles
        , testInsertLinkDialogTyping
        , testInsertLinkDialogSubmit
        , testInsertLinkDialogEscapeCancels
        , testEmojiPickerCategoryTabCycles
        , testEmojiPickerPageNavigation
        , testEmojiPickerEscapeCancels
        , testFormatPromptSubmitsBold
        , testFormatPromptSubmitsItalic
        , testRichCursorSkipsItalicMarkers
        , testRichCursorSkipsLinkMarkers
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
        buf = replicate 200 'x'
        layout = computeInputBufferLayout bodyW entryRows buf
        (inputRow0, inputCol0, _, _) = Layout.inputEntryBounds lay
        targetCursor = 100
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
    let buf = replicate 200 'x'
    writeIORef (asInputBuf st) buf
    writeIORef (asInputCursor st) 100
    handleChat st KeyUp
    cursorUp <- readIORef (asInputCursor st)
    handleChat st KeyDown
    cursorDown <- readIORef (asInputCursor st)
    a <- assertEq "chat KeyUp moves cursor to previous wrapped line" True (cursorUp < 100)
    b <- assertEq "chat KeyDown restores cursor to wrapped continuation line" 100 cursorDown
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
    -- Rich/Plain toggle buttons are now in Prefs (F4 menu), not on the toolbar.
    -- Verify that clicking the toolbar area does NOT toggle rich text (no Rich/Plain buttons).
    st <- mkTestState
    let lay = calcTestLayout
        (toolbarRow0, toolbarCol0, _bodyW, _) = Layout.inputToolbarBounds lay
    writeIORef (asRichText st) True
    -- Click at column 0 relative to toolbar (before any centered button)
    handleNormal st (KeyMouseLeft toolbarRow0 (toolbarCol0 + 0))
    richEnabled <- readIORef (asRichText st)
    assertEq "toolbar click outside buttons does not toggle rich text" True richEnabled

testRichToolbarBoldPrompt :: IO Bool
testRichToolbarBoldPrompt = do
    st <- mkTestState
    let lay = calcTestLayout
        (toolbarRow0, toolbarCol0, bodyW, _) = Layout.inputToolbarBounds lay
        -- Toolbar: "[ Bold ] [ Italic ] [ Color ] [ Link ] [ Emoji ]" (48 chars)
        -- Centered within bodyW: padLeft = (bodyW - 48) / 2
        toolbar :: String
        toolbar = "[ Bold ] [ Italic ] [ Color ] [ Link ] [ Emoji ]"
        padLeft = max 0 ((bodyW - length toolbar) `div` 2)
        boldCol = padLeft + 4  -- middle of "[ Bold ]"
    handleNormal st (KeyMouseLeft toolbarRow0 (toolbarCol0 + boldCol))
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

testRichCursorSkipsMarkdownMarkersRight :: IO Bool
testRichCursorSkipsMarkdownMarkersRight = do
    st <- mkTestState
    writeIORef (asFocus st) ChatPane
    writeIORef (asRichText st) True
    -- Buffer: "**ab**cd" — RichChars: a@2, b@3, then c@6, d@7
    -- Cursor stops: [0, 3, 4, 7, 8]
    writeIORef (asInputBuf st) "**ab**cd"
    writeIORef (asInputCursor st) 3  -- after 'a'
    handleChat st KeyRight
    cursor1 <- readIORef (asInputCursor st)
    handleChat st KeyRight
    cursor2 <- readIORef (asInputCursor st)
    -- From stop 3 -> next stop 4, then from 4 -> next stop 7 (skips closing **)
    a <- assertEq "rich cursor right to next visible char" 4 cursor1
    b <- assertEq "rich cursor right skips closing markers" 7 cursor2
    pure (a && b)

testRichToolbarItalicPrompt :: IO Bool
testRichToolbarItalicPrompt = do
    st <- mkTestState
    let lay = calcTestLayout
        (toolbarRow0, toolbarCol0, bodyW, _) = Layout.inputToolbarBounds lay
        toolbar :: String
        toolbar = "[ Bold ] [ Italic ] [ Color ] [ Link ] [ Emoji ]"
        padLeft = max 0 ((bodyW - length toolbar) `div` 2)
        -- "[ Bold ] " is 10 chars, then "[ Italic ]" starts at 10
        italicCol = padLeft + 10 + 4  -- middle of "[ Italic ]"
    handleNormal st (KeyMouseLeft toolbarRow0 (toolbarCol0 + italicCol))
    dlg <- readIORef (asDialogMode st)
    assertEq "italic toolbar button opens prompt" True (isDlgPromptWithSubstring "Italic text" dlg)

testRichToolbarColorPrompt :: IO Bool
testRichToolbarColorPrompt = do
    st <- mkTestState
    let lay = calcTestLayout
        (toolbarRow0, toolbarCol0, bodyW, _) = Layout.inputToolbarBounds lay
        toolbar :: String
        toolbar = "[ Bold ] [ Italic ] [ Color ] [ Link ] [ Emoji ]"
        padLeft = max 0 ((bodyW - length toolbar) `div` 2)
        -- "[ Bold ] [ Italic ] " is 20 chars, then "[ Color ]" starts at 20
        colorCol = padLeft + 20 + 4  -- middle of "[ Color ]"
    handleNormal st (KeyMouseLeft toolbarRow0 (toolbarCol0 + colorCol))
    dlg <- readIORef (asDialogMode st)
    assertEq "color toolbar button opens prompt" True (isDlgPromptWithSubstring "Color" dlg)

testRichToolbarLinkOpensDialog :: IO Bool
testRichToolbarLinkOpensDialog = do
    st <- mkTestState
    let lay = calcTestLayout
        (toolbarRow0, toolbarCol0, bodyW, _) = Layout.inputToolbarBounds lay
        toolbar :: String
        toolbar = "[ Bold ] [ Italic ] [ Color ] [ Link ] [ Emoji ]"
        padLeft = max 0 ((bodyW - length toolbar) `div` 2)
        -- "[ Bold ] [ Italic ] [ Color ] " is 30 chars, then "[ Link ]" starts at 30
        linkCol = padLeft + 30 + 4  -- middle of "[ Link ]"
    handleNormal st (KeyMouseLeft toolbarRow0 (toolbarCol0 + linkCol))
    dlg <- readIORef (asDialogMode st)
    assertEq "link toolbar button opens insert link dialog" True (isDlgInsertLink dlg)

testRichToolbarEmojiOpensDialog :: IO Bool
testRichToolbarEmojiOpensDialog = do
    st <- mkTestState
    let lay = calcTestLayout
        (toolbarRow0, toolbarCol0, bodyW, _) = Layout.inputToolbarBounds lay
        toolbar :: String
        toolbar = "[ Bold ] [ Italic ] [ Color ] [ Link ] [ Emoji ]"
        padLeft = max 0 ((bodyW - length toolbar) `div` 2)
        -- "[ Bold ] [ Italic ] [ Color ] [ Link ] " is 39 chars, then "[ Emoji ]" starts at 39
        emojiCol = padLeft + 39 + 4  -- middle of "[ Emoji ]"
    handleNormal st (KeyMouseLeft toolbarRow0 (toolbarCol0 + emojiCol))
    dlg <- readIORef (asDialogMode st)
    assertEq "emoji toolbar button opens emoji picker" True (isDlgEmojiPicker dlg)

testInsertLinkDialogTabCycles :: IO Bool
testInsertLinkDialogTabCycles = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgInsertLink)
    writeIORef (asLinkFocus st) 0
    -- Tab through: Text(0) -> URL(1) -> Insert(2) -> Cancel(3) -> Text(0)
    handleDialog st KeyTab
    f1 <- readIORef (asLinkFocus st)
    handleDialog st KeyTab
    f2 <- readIORef (asLinkFocus st)
    handleDialog st KeyTab
    f3 <- readIORef (asLinkFocus st)
    handleDialog st KeyTab
    f4 <- readIORef (asLinkFocus st)
    a <- assertEq "insert link tab to URL" 1 f1
    b <- assertEq "insert link tab to Insert" 2 f2
    c <- assertEq "insert link tab to Cancel" 3 f3
    d <- assertEq "insert link tab wraps to Text" 0 f4
    pure (a && b && c && d)

testInsertLinkDialogTyping :: IO Bool
testInsertLinkDialogTyping = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgInsertLink)
    writeIORef (asLinkFocus st) 0
    writeIORef (asLinkText st) ""
    writeIORef (asLinkUrl st) ""
    -- Type into Text field
    mapM_ (handleDialog st . KeyChar) ("hello" :: String)
    txt <- readIORef (asLinkText st)
    -- Tab to URL field and type
    handleDialog st KeyTab
    mapM_ (handleDialog st . KeyChar) ("http://x" :: String)
    url <- readIORef (asLinkUrl st)
    a <- assertEq "insert link text field" "hello" txt
    b <- assertEq "insert link url field" "http://x" url
    pure (a && b)

testInsertLinkDialogSubmit :: IO Bool
testInsertLinkDialogSubmit = do
    st <- mkTestState
    writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) "prefix"
    writeIORef (asInputCursor st) 6
    writeIORef (asDialogMode st) (Just DlgInsertLink)
    writeIORef (asLinkText st) "click"
    writeIORef (asLinkUrl st) "http://x.com"
    writeIORef (asLinkFocus st) 2  -- Insert button
    handleDialog st KeyEnter
    buf <- readIORef (asInputBuf st)
    dlg <- readIORef (asDialogMode st)
    a <- assertEq "insert link submits markdown" "prefix[click](http://x.com)" buf
    b <- assertEq "insert link closes dialog" True (isDlgNothing dlg)
    pure (a && b)

testInsertLinkDialogEscapeCancels :: IO Bool
testInsertLinkDialogEscapeCancels = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgInsertLink)
    writeIORef (asLinkText st) "text"
    handleDialog st KeyEscape
    dlg <- readIORef (asDialogMode st)
    buf <- readIORef (asInputBuf st)
    a <- assertEq "escape closes insert link dialog" True (isDlgNothing dlg)
    b <- assertEq "escape does not modify input buffer" "" buf
    pure (a && b)

testEmojiPickerCategoryTabCycles :: IO Bool
testEmojiPickerCategoryTabCycles = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgEmojiPicker)
    writeIORef (asEmojiCategory st) 0
    handleDialog st KeyTab
    cat1 <- readIORef (asEmojiCategory st)
    handleDialog st KeyTab
    cat2 <- readIORef (asEmojiCategory st)
    a <- assertEq "emoji tab advances category" 1 cat1
    b <- assertEq "emoji tab advances again" 2 cat2
    pure (a && b)

testEmojiPickerPageNavigation :: IO Bool
testEmojiPickerPageNavigation = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgEmojiPicker)
    writeIORef (asEmojiCategory st) 0
    writeIORef (asEmojiPage st) 0
    handleDialog st KeyRight
    p1 <- readIORef (asEmojiPage st)
    handleDialog st KeyLeft
    p2 <- readIORef (asEmojiPage st)
    a <- assertEq "emoji right advances page" 1 p1
    b <- assertEq "emoji left goes back" 0 p2
    pure (a && b)

testEmojiPickerEscapeCancels :: IO Bool
testEmojiPickerEscapeCancels = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgEmojiPicker)
    handleDialog st KeyEscape
    dlg <- readIORef (asDialogMode st)
    assertEq "escape closes emoji picker" True (isDlgNothing dlg)

testFormatPromptSubmitsBold :: IO Bool
testFormatPromptSubmitsBold = do
    st <- mkTestState
    writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) ""
    writeIORef (asInputCursor st) 0
    -- Click bold toolbar button to open prompt
    let lay = calcTestLayout
        (toolbarRow0, toolbarCol0, bodyW, _) = Layout.inputToolbarBounds lay
        toolbar :: String
        toolbar = "[ Bold ] [ Italic ] [ Color ] [ Link ] [ Emoji ]"
        padLeft = max 0 ((bodyW - length toolbar) `div` 2)
        boldCol = padLeft + 4
    handleNormal st (KeyMouseLeft toolbarRow0 (toolbarCol0 + boldCol))
    -- Type into prompt and submit
    writeIORef (asDialogBuf st) ""
    mapM_ (handleDialog st . KeyChar) ("test" :: String)
    handleDialog st KeyEnter
    buf <- readIORef (asInputBuf st)
    dlg <- readIORef (asDialogMode st)
    a <- assertEq "bold prompt inserts wrapped text" "**test**" buf
    b <- assertEq "bold prompt closes dialog" True (isDlgNothing dlg)
    pure (a && b)

testFormatPromptSubmitsItalic :: IO Bool
testFormatPromptSubmitsItalic = do
    st <- mkTestState
    writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) ""
    writeIORef (asInputCursor st) 0
    let lay = calcTestLayout
        (toolbarRow0, toolbarCol0, bodyW, _) = Layout.inputToolbarBounds lay
        toolbar :: String
        toolbar = "[ Bold ] [ Italic ] [ Color ] [ Link ] [ Emoji ]"
        padLeft = max 0 ((bodyW - length toolbar) `div` 2)
        italicCol = padLeft + 10 + 4
    handleNormal st (KeyMouseLeft toolbarRow0 (toolbarCol0 + italicCol))
    writeIORef (asDialogBuf st) ""
    mapM_ (handleDialog st . KeyChar) ("test" :: String)
    handleDialog st KeyEnter
    buf <- readIORef (asInputBuf st)
    dlg <- readIORef (asDialogMode st)
    a <- assertEq "italic prompt inserts wrapped text" "*test*" buf
    b <- assertEq "italic prompt closes dialog" True (isDlgNothing dlg)
    pure (a && b)

testRichCursorSkipsItalicMarkers :: IO Bool
testRichCursorSkipsItalicMarkers = do
    st <- mkTestState
    writeIORef (asFocus st) ChatPane
    writeIORef (asRichText st) True
    writeIORef (asInputBuf st) "*ab*"
    writeIORef (asInputCursor st) 3
    handleChat st KeyLeft
    cursor1 <- readIORef (asInputCursor st)
    handleChat st KeyLeft
    cursor2 <- readIORef (asInputCursor st)
    a <- assertEq "rich cursor skips italic visible chars" 2 cursor1
    b <- assertEq "rich cursor skips italic opening marker" 1 cursor2
    pure (a && b)

testRichCursorSkipsLinkMarkers :: IO Bool
testRichCursorSkipsLinkMarkers = do
    st <- mkTestState
    writeIORef (asFocus st) ChatPane
    writeIORef (asRichText st) True
    -- Buffer: "[ab](http://x)cd" — link label "ab" is visible, URL is hidden
    -- RichChars: a@1-2, b@2-3 from link, then c@14-15, d@15-16
    -- Stops: [0, 2, 3, 15, 16]
    writeIORef (asInputBuf st) "[ab](http://x)cd"
    writeIORef (asInputCursor st) 2  -- after 'a'
    handleChat st KeyRight
    cursor1 <- readIORef (asInputCursor st)
    handleChat st KeyRight
    cursor2 <- readIORef (asInputCursor st)
    -- From 2 -> 3 (after 'b'), then from 3 -> 15 (skips entire ](http://x) URL)
    a <- assertEq "rich cursor moves through link text" 3 cursor1
    b <- assertEq "rich cursor skips link URL to next visible" 15 cursor2
    pure (a && b)

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

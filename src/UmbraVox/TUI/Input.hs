-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Input
    ( readKey, readCSI, readSS3, drainSeq
    , eventLoop
    , handleNormal, handleContact, handleChat, handleDialog, handleMouseDrag
    , handleSettingsDlg, handleNewConnDlg
    , closeDialog, scrollDialog
    , strip'
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (void, when, unless)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Char (isDigit, toLower, toUpper)
import Data.List (isPrefixOf, isInfixOf)
import Data.IORef (readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import System.IO (stdin, hReady)
import UmbraVox.BuildProfile
    ( BuildPluginId(..), pluginEnabled, pluginUnavailableStatus )
import UmbraVox.TUI.Types
import UmbraVox.TUI.Render (render)
import UmbraVox.TUI.Menu (toggleMenu, handleMenu, openMenu, closeMenu, executeMenuItem)
import UmbraVox.TUI.RuntimeCommand (runRuntimeCommand, RuntimeCommand(..))
import UmbraVox.TUI.PaginatedList (pageItemBySlot, pageMaxIndex, slicePage, psItems, psPage)
import UmbraVox.TUI.Dialog
    ( browseOverlayLines, overlayBounds, overlayButtonAtLine, overlayCloseBounds
    , overlayScrollbarBounds, settingsOverlayLines
    , helpOverlayLines, aboutOverlayLines, newConnOverlayLines
    , verifyOverlayLines, keysOverlayLines, promptOverlayLines, settingsTabLabels
    , regenKeyOverlayLines, exportWarnOverlayLines, exportKeysOverlayLines
    , wrapOverlayLines, insertLinkOverlayLines, emojiPickerOverlayLines
    )
import UmbraVox.TUI.EmojiPicker
    ( emojiCategories, emojiByCategory, searchEmoji
    , emojiPage, emojiPageCount, selectorKeys
    )
import UmbraVox.TUI.Constants (maxInputLen, maxDialogBufLen, minDropdownW)
import UmbraVox.TUI.Actions (addSecureNotes, sendCurrentMessage,
    setStatus, adjustContactScroll, openRegenKeyDialog, regenIdentityKey)
import UmbraVox.TUI.Layout (dropdownCol)
import qualified UmbraVox.TUI.Layout as Layout
import UmbraVox.TUI.InputBuffer
    ( computeInputBufferLayout, clampInputCursor, cursorIndexForScreenOffset
    , cursorIndexForLineColumn, ensureCursorVisible, clampInputScroll, iblShowScrollbar, iblMaxScroll
    , iblBodyW )
import qualified UmbraVox.TUI.InputBuffer as InputBuffer
import UmbraVox.TUI.RichText
    ( computeRichInputLayout, richCursorIndexForLineColumn
    , richCursorIndexForScreenOffset, richCursorScreenOffset
    , richEnsureCursorVisible, richClampCursor
    , richMoveCursorLeft, richMoveCursorRight
    , richVisibleLines, richShowScrollbar, richMaxScroll, richContentW
    , richCursorLineIndex, richCursorTextDisplayCol
    )
import qualified UmbraVox.TUI.RichText as RichText
import UmbraVox.Network.MDNS (MDNSPeer(..))
import UmbraVox.Protocol.Encoding (splitOn, parseHostPort)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))

data GridRegion = RegionContacts | RegionActions | RegionChat | RegionOverlay
    deriving stock (Eq, Show)

toolbarButtonSpecs :: Bool -> [(String, RuntimeCommand)]
toolbarButtonSpecs _richEnabled =
    [ ("[ Bold ]", CmdInsertBold)
    , ("[ Italic ]", CmdInsertItalic)
    , ("[ Color ]", CmdInsertColor)
    , ("[ Link ]", CmdInsertLink)
    , ("[ Emoji ]", CmdInsertEmoji)
    ]

-- Input handling ----------------------------------------------------------
readKey :: IO InputEvent
readKey = do
    c <- getChar
    case c of
        '\n'   -> pure KeyEnter
        '\r'   -> pure KeyEnter
        '\t'   -> pure KeyTab
        '\DEL' -> pure KeyBackspace
        '\x08' -> pure KeyBackspace
        '\x0E' -> pure KeyCtrlN
        '\x07' -> pure KeyCtrlG
        '\x11' -> pure KeyCtrlQ
        '\x04' -> pure KeyCtrlD
        '\x02' -> pure KeyCtrlB
        '\x0B' -> pure KeyCtrlK
        '\x15' -> pure KeyCtrlU
        '\ESC' -> do
            ready <- hReady stdin
            if not ready then pure KeyEscape else do
                c2 <- getChar
                case c2 of
                    '[' -> readCSI; 'O' -> readSS3; _ -> pure KeyEscape
        _ -> pure (KeyChar c)

readCSI :: IO InputEvent
readCSI = do
    c <- getChar
    case c of
        'A' -> pure KeyUp; 'B' -> pure KeyDown
        'C' -> pure KeyRight; 'D' -> pure KeyLeft
        'H' -> pure KeyHome; 'F' -> pure KeyEnd
        '<' -> readMouseCSI
        '5' -> drainTilde >> pure KeyPageUp
        '6' -> drainTilde >> pure KeyPageDown
        '1' -> readCSIExtended
        _ -> drainSeq >> pure KeyUnknown
  where drainTilde = hReady stdin >>= \r -> when r (void getChar)

-- | Read a CSI sequence after seeing '2' (shift modifier).
-- ESC[1;2A=Shift+Up, B=Shift+Down, C=Shift+Right, D=Shift+Left,
-- H=Shift+Home, F=Shift+End.  Others are drained.
readCSIShift :: IO InputEvent
readCSIShift = do
    c <- getChar
    case c of
        'A' -> pure KeyUp
        'B' -> pure KeyDown
        'C' -> pure KeyShiftRight
        'D' -> pure KeyShiftLeft
        'H' -> pure KeyShiftHome
        'F' -> pure KeyShiftEnd
        _ -> drainSeq >> pure KeyUnknown

-- | Handle extended CSI sequences like ESC[1x~ for F-keys and ESC[1;Nm for modifiers.
-- F1=ESC[11~, F2=ESC[12~, F3=ESC[13~, F4=ESC[14~, F5=ESC[15~
-- Shift+Arrow/Home/End = ESC[1;2x
readCSIExtended :: IO InputEvent
readCSIExtended = do
    c2 <- getChar
    case c2 of
        ';' -> do
            -- ESC[1;Nm — modifier sequence
            modChar <- getChar
            case modChar of
                '2' -> readCSIShift  -- shift modifier
                _   -> drainSeq >> pure KeyUnknown
        '1' -> drainTilde' >> pure KeyF1   -- ESC[11~
        '2' -> drainTilde' >> pure KeyF2   -- ESC[12~
        '3' -> drainTilde' >> pure KeyF3   -- ESC[13~
        '4' -> drainTilde' >> pure KeyF4   -- ESC[14~
        '5' -> drainTilde' >> pure KeyF5   -- ESC[15~
        _   -> drainSeq >> pure KeyUnknown
  where drainTilde' = hReady stdin >>= \r -> when r (void getChar)

readSS3 :: IO InputEvent
readSS3 = do
    c <- getChar
    case c of
        'P' -> pure KeyF1; 'Q' -> pure KeyF2
        'R' -> pure KeyF3; 'S' -> pure KeyF4
        _   -> pure KeyUnknown

drainSeq :: IO ()
drainSeq = hReady stdin >>= \r -> when r (void getChar >> drainSeq)

readMouseCSI :: IO InputEvent
readMouseCSI = do
    seqBody <- readMouseBody ""
    pure (parseMouseCSI seqBody)
  where
    readMouseBody acc = do
        ch <- getChar
        let acc' = acc ++ [ch]
        if ch == 'M' || ch == 'm'
            then pure acc'
            else readMouseBody acc'

parseMouseCSI :: String -> InputEvent
parseMouseCSI raw =
    case reverse raw of
        [] -> KeyUnknown
        (term:revBody) ->
            let body = reverse revBody
            in case splitOn ';' body of
                [bS, xS, yS] ->
                    case (reads bS :: [(Int, String)],
                          reads xS :: [(Int, String)],
                          reads yS :: [(Int, String)]) of
                        ([(b, "")], [(x, "")], [(y, "")]) ->
                            let button = b .&. 3
                                isWheel = (b .&. 64) /= 0
                                isMotion = (b .&. 32) /= 0
                                wheelUp = isWheel && button == 0
                                wheelDown = isWheel && button == 1
                            in if isWheel && term == 'M' && wheelUp
                                then KeyMouseScrollUp y x
                            else if isWheel && term == 'M' && wheelDown
                                then KeyMouseScrollDown y x
                            else if term == 'M' && isMotion && button == 0
                                then KeyMouseDrag y x
                            else if term == 'm' && button == 3
                                then KeyMouseRelease y x
                            else if term == 'M' && button == 0 && not isWheel && not isMotion
                                then KeyMouseLeft y x
                                else KeyIgnored
                        _ -> KeyUnknown
                _ -> KeyUnknown

-- Main event loop ---------------------------------------------------------
uiTickMicros :: Int
uiTickMicros = 250000

eventLoop :: AppState -> IO ()
eventLoop st = do
    running <- readIORef (asRunning st)
    when running $ do
        ready <- hReady stdin
        if ready
            then do
                key <- readKey
                case key of
                    KeyIgnored -> pure ()
                    KeyMouseLeft row col -> handleMouseClick st row col
                    KeyMouseDrag row col -> handleMouseDrag st row col
                    KeyMouseRelease _ _ -> pure ()
                    KeyMouseScrollUp row col -> do
                        dlg <- readIORef (asDialogMode st)
                        case dlg of
                            Just _ -> scrollDialog st (-3)
                            Nothing -> handlePaneMouseWheel st row col (-3)
                    KeyMouseScrollDown row col -> do
                        dlg <- readIORef (asDialogMode st)
                        case dlg of
                            Just _ -> scrollDialog st 3
                            Nothing -> handlePaneMouseWheel st row col 3
                    _ -> do
                        dlg <- readIORef (asDialogMode st)
                        case dlg of
                            Just _  -> handleDialog st key
                            Nothing -> do
                                mOpen <- readIORef (asMenuOpen st)
                                case mOpen of
                                    Just _  -> handleMenu st key
                                    Nothing -> handleNormal st key
            else threadDelay uiTickMicros
        readIORef (asRunning st) >>= \r -> when r (render st >> eventLoop st)

-- Normal key handling -----------------------------------------------------
handleNormal :: AppState -> InputEvent -> IO ()
handleNormal st key = do
    focus <- readIORef (asFocus st)
    case key of
        KeyMouseLeft row col -> handleMouseClick st row col
        KeyTab   -> modifyIORef' (asFocus st) (\p -> case p of
            ContactPane  -> ChatPane
            ChatPane     -> IdentityPane
            IdentityPane -> ContactPane)
        KeyCtrlN -> runRuntimeCommand st CmdOpenNewConversation
        KeyCtrlG -> startGroupPrompt st
        KeyCtrlQ -> runRuntimeCommand st CmdQuit
        KeyCtrlD -> runRuntimeCommand st CmdQuit
        KeyF1    -> toggleMenu st MenuHelp
        KeyF2    -> toggleMenu st MenuPrefs
        KeyF3    -> toggleMenu st MenuIdentity
        KeyF4    -> toggleMenu st MenuIdentity
        KeyF5    -> pure ()
        KeyEscape -> do
            dlg <- readIORef (asDialogMode st)
            case dlg of
                Just _  -> closeDialog st
                Nothing -> pure ()
        _ -> case focus of
            ContactPane  -> handleContact st key
            ChatPane     -> handleChat st key
            IdentityPane -> handleIdentity st key

dialogLineCount :: AppState -> DialogMode -> IO Int
dialogLineCount _ DlgHelp = pure (length helpOverlayLines)
dialogLineCount _ DlgAbout = pure (length aboutOverlayLines)
dialogLineCount st DlgSettings = length <$> settingsOverlayLines st
dialogLineCount st DlgVerify = length <$> verifyOverlayLines st
dialogLineCount _ DlgNewConn = pure (length newConnOverlayLines)
dialogLineCount st DlgKeys = length <$> keysOverlayLines st
dialogLineCount st DlgBrowse = length <$> browseOverlayLines st
dialogLineCount st DlgRegenKey = do
    checked <- readIORef (asRegenCheckbox st)
    length <$> regenKeyOverlayLines checked
dialogLineCount _ DlgExportWarn = pure (length (exportWarnOverlayLines False))
dialogLineCount st DlgExportKeys = length <$> exportKeysOverlayLines st
dialogLineCount st DlgInsertLink = do
    txt   <- readIORef (asLinkText st)
    url   <- readIORef (asLinkUrl st)
    focus <- readIORef (asLinkFocus st)
    pure (length (insertLinkOverlayLines txt url focus))
dialogLineCount st DlgEmojiPicker = do
    q    <- readIORef (asEmojiSearch st)
    cat  <- readIORef (asEmojiCategory st)
    page <- readIORef (asEmojiPage st)
    pure (length (emojiPickerOverlayLines q cat page))
dialogLineCount st (DlgPrompt title _) = do
    buf <- readIORef (asDialogBuf st)
    pure (length (promptOverlayLines title buf))

dialogLines :: AppState -> DialogMode -> IO [String]
dialogLines _  DlgHelp = pure helpOverlayLines
dialogLines _  DlgAbout = pure aboutOverlayLines
dialogLines st DlgSettings = settingsOverlayLines st
dialogLines st DlgVerify = verifyOverlayLines st
dialogLines _  DlgNewConn = pure newConnOverlayLines
dialogLines st DlgKeys = keysOverlayLines st
dialogLines st DlgBrowse = browseOverlayLines st
dialogLines st DlgRegenKey = do
    checked <- readIORef (asRegenCheckbox st)
    regenKeyOverlayLines checked
dialogLines st DlgExportWarn = do
    checked <- readIORef (asRegenCheckbox st)
    pure (exportWarnOverlayLines checked)
dialogLines st DlgExportKeys = exportKeysOverlayLines st
dialogLines st DlgInsertLink = do
    txt   <- readIORef (asLinkText st)
    url   <- readIORef (asLinkUrl st)
    focus <- readIORef (asLinkFocus st)
    pure (insertLinkOverlayLines txt url focus)
dialogLines st DlgEmojiPicker = do
    q    <- readIORef (asEmojiSearch st)
    cat  <- readIORef (asEmojiCategory st)
    page <- readIORef (asEmojiPage st)
    pure (emojiPickerOverlayLines q cat page)
dialogLines st (DlgPrompt title _) = do
    buf <- readIORef (asDialogBuf st)
    pure (promptOverlayLines title buf)

dialogRenderedLines :: Layout -> [String] -> [String]
dialogRenderedLines lay rawLines =
    let (_, _, w, _) = overlayBounds lay (length rawLines)
        wrapWidth = max 1 (w - 3)
    in wrapOverlayLines wrapWidth rawLines

gridRegionsFor :: Layout -> Maybe Rect -> [ (GridRegion, Rect) ]
gridRegionsFor lay mOverlay =
    maybe id (\ov -> ((RegionOverlay, ov):)) mOverlay
        [ (RegionContacts, contactsRect)
        , (RegionActions, actionsRect)
        , (RegionChat, chatRect)
        ]
  where
    (contactsRow0, contactsCol0, contactsW, contactsH) = Layout.contactsPaneBounds lay
    (actionsRow0, actionsCol0, actionsW, _) = Layout.actionsPaneBounds lay
    (chatRow0, chatCol0, chatW, chatH) = Layout.chatPaneBounds lay
    (inputRow0, _, _, inputH) = Layout.inputEntryBounds lay
    -- Keep input hit-testing aligned with rendered compact lower-left box:
    -- 3 content rows + 1 closing border row.
    actionsH = 4
    contactsRect = Rect contactsRow0 (contactsRow0 + contactsH - 1) contactsCol0 (contactsCol0 + contactsW - 1)
    actionsRect = Rect actionsRow0 (actionsRow0 + actionsH - 1) actionsCol0 (actionsCol0 + actionsW - 1)
    -- Include the input box bottom border row (where Send/Clear buttons live)
    chatBottom = max (chatRow0 + chatH - 1) (inputRow0 + inputH)
    chatRect = Rect chatRow0 chatBottom chatCol0 (chatCol0 + chatW - 1)

gridRegionAt :: Layout -> Maybe Rect -> Int -> Int -> Maybe GridRegion
gridRegionAt lay mOverlay row col = go (gridRegionsFor lay mOverlay)
  where
    go [] = Nothing
    go ((k, r):rest)
        | rectContains r row col = Just k
        | otherwise = go rest

dropdownRect :: Layout -> MenuTab -> Rect
dropdownRect lay tab =
    Rect itemStartRow itemEndRow dropCol (dropCol + boxW - 1)
  where
    items = menuTabItems tab
    boxW = max minDropdownW (maximum (map length items) + 4)
    dropCol = dropdownCol (lCols lay) tab
    itemStartRow = 3
    itemEndRow = itemStartRow + length items - 1

dropdownRendered :: Layout -> MenuTab -> Bool
dropdownRendered lay tab =
    let items = menuTabItems tab
        boxW = max minDropdownW (maximum (map length items) + 4)
        col = dropdownCol (lCols lay) tab
        startRow = 2
    in startRow + length items + 1 < lRows lay && col + boxW <= lCols lay

dropdownItemIndexAt :: Layout -> MenuTab -> Int -> Int -> Maybe Int
dropdownItemIndexAt lay tab row col =
    let Rect r0 r1 c0 c1 = dropdownRect lay tab
        idx = row - r0
        maxIdx = length (menuTabItems tab) - 1
    in if dropdownRendered lay tab
        && row >= r0 && row <= r1
        && col >= c0 && col <= c1
        then Just (max 0 (min maxIdx idx))
        else Nothing

handleMouseClick :: AppState -> Int -> Int -> IO ()
handleMouseClick st row col = do
    lay <- readIORef (asLayout st)
    dlg <- readIORef (asDialogMode st)
    case dlg of
        Just mode -> do
            rawLines <- dialogLines st mode
            let rows = dialogRenderedLines lay rawLines
            handleDialogMouseClick st lay mode rows rawLines row col
        Nothing -> do
            if row == 1
                then handleMenuBarClick st lay col
                else do
                    mOpen <- readIORef (asMenuOpen st)
                    case mOpen of
                        Just tab -> do
                            let mDrop = if dropdownRendered lay tab then Just (dropdownRect lay tab) else Nothing
                                region = gridRegionAt lay mDrop row col
                            case region of
                                Just RegionOverlay -> do
                                    handled <- handleDropdownClick st lay tab row col
                                    unless handled (closeMenu st)
                                _ -> do
                                    closeMenu st
                                    handlePaneClick st lay row col
                        Nothing -> handlePaneClick st lay row col

handleMouseDrag :: AppState -> Int -> Int -> IO ()
handleMouseDrag st row col = do
    lay <- readIORef (asLayout st)
    dlg <- readIORef (asDialogMode st)
    case dlg of
        Just mode -> do
            rawLines <- dialogLines st mode
            let rows = dialogRenderedLines lay rawLines
            handleDialogDrag st lay rows row col
        Nothing -> handlePaneDrag st lay row col

handleDialogDrag :: AppState -> Layout -> [String] -> Int -> Int -> IO ()
handleDialogDrag st lay rows row col = do
    let lineCount = length rows
    scrollOff <- readIORef (asDialogScroll st)
    let mSb = overlayScrollbarBounds lay lineCount scrollOff
    case mSb of
        Just (sbCol, trackTop, trackBot)
            | col == sbCol && row >= trackTop && row <= trackBot -> do
                let trackH  = trackBot - trackTop + 1
                    maxOff  = max 0 (lineCount - trackH)
                    newOff
                        | row <= trackTop = 0
                        | row >= trackBot = maxOff
                        | otherwise = (row - trackTop) * maxOff `div` max 1 (trackH - 1)
                writeIORef (asDialogScroll st) (max 0 (min maxOff newOff))
            | otherwise -> pure ()
        Nothing -> pure ()

handleDialogMouseClick :: AppState -> Layout -> DialogMode -> [String] -> [String] -> Int -> Int -> IO ()
handleDialogMouseClick st lay dlg rows rawLines row col = do
    let lineCount = length rows
    closedByX <- handleOverlayTopClose st lay lineCount row col
    unless closedByX $ do
        -- Check for a click on the scrollbar track and jump offset if so.
        scrollOff <- readIORef (asDialogScroll st)
        let mSb = overlayScrollbarBounds lay lineCount scrollOff
        sbHandled <- case mSb of
            Just (sbCol, trackTop, trackBot) | col == sbCol && row >= trackTop && row <= trackBot -> do
                let trackH  = trackBot - trackTop + 1
                    maxOff  = max 0 (lineCount - trackH)
                    newOff
                        | row <= trackTop = 0
                        | row >= trackBot = maxOff
                        | otherwise = (row - trackTop) * maxOff `div` max 1 (trackH - 1)
                writeIORef (asDialogScroll st) (max 0 (min maxOff newOff))
                pure True
            _ -> pure False
        unless sbHandled $
            case dlg of
                DlgBrowse        -> handleDlgBrowseClick st lay lineCount rows rawLines row col
                DlgSettings      -> handleDlgSettingsClick st lay lineCount rows rawLines row col
                DlgNewConn       -> handleDlgNewConnClick st lay lineCount rows rawLines row col
                DlgKeys          -> handleDlgKeysClick st lay lineCount rows rawLines row col
                DlgPrompt title cb -> handleDlgPromptClick st lay lineCount rows rawLines row col title cb
                DlgRegenKey      -> handleDlgRegenKeyClick st lay lineCount rows rawLines row col
                DlgExportWarn    -> handleDlgExportWarnClick st lay lineCount rows row col
                DlgExportKeys    -> handleDlgCloseOnly st lay DlgExportKeys lineCount rows rawLines row col
                DlgInsertLink    -> handleDlgInsertLinkClick st lay lineCount rows row col
                DlgEmojiPicker   -> handleDlgEmojiPickerClick st lay lineCount rows rawLines row col
                _                -> handleDlgCloseOnly st lay dlg lineCount rows rawLines row col

handleDlgCloseOnly :: AppState -> Layout -> DialogMode -> Int -> [String] -> [String] -> Int -> Int -> IO ()
handleDlgCloseOnly st lay dlg lineCount _rows rawLines row col = do
    lines' <- closeOnlyLines st dlg
    when (overlayButtonHit lay lineCount row col (lineCount - 1) lines' "close") $
        closeDialog st

closeOnlyLines :: AppState -> DialogMode -> IO [String]
closeOnlyLines _  DlgHelp    = pure helpOverlayLines
closeOnlyLines _  DlgAbout   = pure aboutOverlayLines
closeOnlyLines st DlgKeys    = keysOverlayLines st
closeOnlyLines st DlgVerify  = verifyOverlayLines st
closeOnlyLines st DlgRegenKey = do
    checked <- readIORef (asRegenCheckbox st)
    regenKeyOverlayLines checked
closeOnlyLines _  DlgExportWarn = pure (exportWarnOverlayLines False)
closeOnlyLines st DlgExportKeys = exportKeysOverlayLines st
closeOnlyLines _  _          = pure []

handleDlgBrowseClick :: AppState -> Layout -> Int -> [String] -> [String] -> Int -> Int -> IO ()
handleDlgBrowseClick st lay lineCount rows rawLines row col = do
    let footerIx = lineCount - 1
        btn = overlayButtonHit lay lineCount row col footerIx rows
    case lookup True [ (btn "prev",   stepBrowsePage st (-1))
                     , (btn "next",   stepBrowsePage st 1)
                     , (btn "search", openBrowseSearchPrompt st)
                     , (btn "clear",  clearBrowseSearch st)
                     , (btn "close",  closeDialog st)
                     ] of
        Just action -> action
        Nothing -> case overlayContentLine lay lineCount row col of
            Just lineIx -> do
                let lineText = rows !! lineIx
                case lineOptionKey lineText of
                    Just c -> selectBrowsePeerByDigit st c
                    Nothing
                        | "Search:" `isInfixOf` lineText -> openBrowseSearchPrompt st
                        | otherwise -> pure ()
            Nothing -> pure ()

handleDlgSettingsClick :: AppState -> Layout -> Int -> [String] -> [String] -> Int -> Int -> IO ()
handleDlgSettingsClick st lay lineCount _rows rawLines row col = do
    lines' <- settingsOverlayLines st
    case overlayButtonAtLine lay rawLines 0 row col of
        Just tabLabel -> handleSettingsTabClick st tabLabel
        Nothing -> handleSettingsBodyClick st lay lineCount lines' row col

handleSettingsTabClick :: AppState -> String -> IO ()
handleSettingsTabClick st tabLabel =
    case lookup (map toLower tabLabel) (zip (map (map toLower) settingsTabLabels) [0..]) of
        Just tabIx -> writeIORef (asDialogTab st) tabIx
        Nothing -> pure ()

handleSettingsBodyClick :: AppState -> Layout -> Int -> [String] -> Int -> Int -> IO ()
handleSettingsBodyClick st lay lineCount lines' row col =
    let footerIx = lineCount - 1
    in if overlayButtonHit lay lineCount row col footerIx lines' "close"
        then closeDialog st
        else case overlayContentLine lay lineCount row col of
            Just lineIx ->
                case lineOptionKey (lines' !! lineIx) of
                    Just key -> handleSettingsDlg st (KeyChar key)
                    Nothing -> pure ()
            Nothing -> pure ()

handleDlgNewConnClick :: AppState -> Layout -> Int -> [String] -> [String] -> Int -> Int -> IO ()
handleDlgNewConnClick st lay lineCount rows rawLines row col = do
    let footerIx = lineCount - 1
        btn = overlayButtonHit lay lineCount row col footerIx rows
    case lookup True [ (btn "private", handleNewConnDlg st (KeyChar '1'))
                     , (btn "single",  handleNewConnDlg st (KeyChar '2'))
                     , (btn "group",   handleNewConnDlg st (KeyChar '3'))
                     , (btn "cancel",  closeDialog st)
                     ] of
        Just action -> action
        Nothing -> case overlayContentLine lay lineCount row col of
            Just lineIx ->
                case lineOptionKey (rows !! lineIx) of
                    Just '1' -> handleNewConnDlg st (KeyChar '1')
                    Just '2' -> handleNewConnDlg st (KeyChar '2')
                    Just '3' -> handleNewConnDlg st (KeyChar '3')
                    _ -> pure ()
            _      -> pure ()

handleDlgKeysClick :: AppState -> Layout -> Int -> [String] -> [String] -> Int -> Int -> IO ()
handleDlgKeysClick st lay lineCount _rows rawLines row col = do
    let footerIx = lineCount - 1
        btn = overlayButtonHit lay lineCount row col footerIx _rows
    case lookup True
            [ (btn "regenerate (f5)", openRegenKeyDialog st)
            , (btn "export keys", writeIORef (asRegenCheckbox st) False >> writeIORef (asDialogMode st) (Just DlgExportWarn))
            , (btn "import keys", handleKeysDlg st (KeyChar 'i'))
            , (btn "close", closeDialog st)
            ] of
        Just action -> action
        Nothing -> pure ()

handleDlgPromptClick :: AppState -> Layout -> Int -> [String] -> [String] -> Int -> Int -> String -> (String -> IO ()) -> IO ()
handleDlgPromptClick st lay lineCount _rows rawLines row col title cb = do
    buf <- readIORef (asDialogBuf st)
    let lines' = promptOverlayLines title buf
        footerIx = lineCount - 1
    if overlayButtonHit lay lineCount row col footerIx lines' "ok"
        then submitPrompt st cb
    else if overlayButtonHit lay lineCount row col footerIx lines' "cancel"
        then do
            writeIORef (asDialogBuf st) ""
            closeDialog st
    else pure ()

handleDlgRegenKeyClick :: AppState -> Layout -> Int -> [String] -> [String] -> Int -> Int -> IO ()
handleDlgRegenKeyClick st lay lineCount rows rawLines row col = do
    checked <- readIORef (asRegenCheckbox st)
    lns <- regenKeyOverlayLines checked
    let footerIx = lineCount - 1
        btn = overlayButtonHit lay lineCount row col footerIx lns
    -- Clicking the checkbox line toggles it
    case overlayContentLine lay lineCount row col of
        Just li ->
            let lineText = strip' (rows !! li)
            in if "[ ]" `isPrefixOf` lineText || "[x]" `isPrefixOf` lineText
                then modifyIORef' (asRegenCheckbox st) not
                else pure ()
        _ -> pure ()
    -- Buttons
    case lookup True [ (btn "yes, regenerate", regenIdentityKey st)
                     , (btn "cancel",          closeDialog st)
                     ] of
        Just action -> action
        Nothing     -> pure ()

handleDlgExportWarnClick :: AppState -> Layout -> Int -> [String] -> Int -> Int -> IO ()
handleDlgExportWarnClick st lay lineCount rows row col = do
    checked <- readIORef (asRegenCheckbox st)
    let lns = exportWarnOverlayLines checked
        footerIx = lineCount - 1
        btn = overlayButtonHit lay lineCount row col footerIx lns
    case overlayContentLine lay lineCount row col of
        Just li ->
            let lineText = strip' (rows !! li)
            in if "[ ]" `isPrefixOf` lineText || "[x]" `isPrefixOf` lineText
                then modifyIORef' (asRegenCheckbox st) not
                else pure ()
        _ -> pure ()
    case lookup True [ (btn "continue export", writeIORef (asDialogMode st) (Just DlgExportKeys))
                     , (btn "cancel", closeDialog st)
                     ] of
        Just action -> action
        Nothing -> pure ()

handleDlgInsertLinkClick :: AppState -> Layout -> Int -> [String] -> Int -> Int -> IO ()
handleDlgInsertLinkClick st lay lineCount rows row col = do
    txt   <- readIORef (asLinkText st)
    url   <- readIORef (asLinkUrl st)
    focus <- readIORef (asLinkFocus st)
    let lns = insertLinkOverlayLines txt url focus
        footerIx = lineCount - 1
        btn = overlayButtonHit lay lineCount row col footerIx lns
    case lookup True [ (btn "insert", submitInsertLink st)
                     , (btn "cancel", closeDialog st)
                     ] of
        Just action -> action
        Nothing -> case overlayContentLine lay lineCount row col of
            Just lineIx ->
                -- Line 1 => Text field (0-indexed), line 2 => URL field
                if lineIx == 1 then writeIORef (asLinkFocus st) 0
                else if lineIx == 2 then writeIORef (asLinkFocus st) 1
                else pure ()
            Nothing -> pure ()

handleOverlayTopClose :: AppState -> Layout -> Int -> Int -> Int -> IO Bool
handleOverlayTopClose st lay lineCount row col = do
    let (closeRow, closeStart, closeEnd) = overlayCloseBounds lay lineCount
        hit = row == closeRow && col >= closeStart && col <= closeEnd
    when hit (closeDialog st)
    pure hit

overlayButtonHit :: Layout -> Int -> Int -> Int -> Int -> [String] -> String -> Bool
overlayButtonHit lay _lineCount row col lineIx lines' target =
    case overlayButtonAtLine lay lines' lineIx row col of
        Just label -> map toLower label == map toLower target
        Nothing -> False

submitPrompt :: AppState -> (String -> IO ()) -> IO ()
submitPrompt st cb = do
    b <- readIORef (asDialogBuf st)
    cb b
    writeIORef (asDialogBuf st) ""
    closeDialog st

overlayContentLine :: Layout -> Int -> Int -> Int -> Maybe Int
overlayContentLine lay lineCount row col =
    let (r0, c0, w, h) = overlayBounds lay lineCount
        insideRows = row > r0 && row < r0 + h - 1
        insideCols = col > c0 && col < c0 + w - 1
    in if insideRows && insideCols
        then Just (row - r0 - 1)
        else Nothing

lineOptionKey :: String -> Maybe Char
lineOptionKey line =
    case dropWhile (== ' ') line of
        key:'.':_ -> Just (toLower key)
        _ -> Nothing

handleMenuBarClick :: AppState -> Layout -> Int -> IO ()
handleMenuBarClick st lay col = do
    mOpen <- readIORef (asMenuOpen st)
    case menuTabAtColumn (lCols lay) col of
        Just MenuQuit -> closeMenu st >> runRuntimeCommand st CmdQuit
        Just tab ->
            if mOpen == Just tab
                then closeMenu st
                else openMenu st tab
        Nothing -> pure ()

handleDropdownClick :: AppState -> Layout -> MenuTab -> Int -> Int -> IO Bool
handleDropdownClick st lay tab row col = do
    case dropdownItemIndexAt lay tab row col of
        Just idx -> do
            closeMenu st
            executeMenuItem st tab idx
            pure True
        Nothing ->
            pure False

handlePaneClick :: AppState -> Layout -> Int -> Int -> IO ()
handlePaneClick st lay row col = do
    richEnabled <- readIORef (asRichText st)
    let (contactsRow0, contactsCol0, contactsW, contactsH) = Layout.contactsPaneBounds lay
        (toolbarRow0, _, _, _) = Layout.inputToolbarBounds lay
        contactsBottom = contactsRow0 + contactsH - 1
        identSepRow = contactsBottom + 1
        contentBottom = contactsRow0 + lChatH lay - 1
        leftContentEnd = contactsCol0 + contactsW - 1
    case gridRegionAt lay Nothing row col of
        Just RegionContacts -> do
            lay' <- readIORef (asLayout st)
            let innerW = lLeftW lay' - 2
                allFourW = length ("[ New ] [ Rename ] [ Browse ] [ Verify ]" :: String)
                btnRows = if innerW >= allFourW then 1 else 2
                tbRows = 1 + btnRows  -- 1 separator + button rows
                tbStart = contactsRow0 + max 0 (contactsH - tbRows)
            if col <= leftContentEnd && row >= tbStart && row < contactsRow0 + contactsH
                then handleContactsToolbarClick st lay' row col
                else if col <= leftContentEnd && row < tbStart
                then selectContactByRow st (row - contactsRow0)
                else if col <= leftContentEnd && row > identSepRow && row <= contentBottom
                    then writeIORef (asFocus st) IdentityPane
                    else pure ()
        Just RegionActions -> pure ()  -- actions area is now blank
        Just RegionChat -> do
            writeIORef (asFocus st) ChatPane
            if row == toolbarRow0
                then handleToolbarClick st lay richEnabled col
                else do
                    -- Check if click is on the bottom border Send/Clear buttons
                    let (inputRow0, inputCol0, _, inputH) = Layout.inputEntryBounds lay
                        rw = lRightW lay
                        bottomRow = inputRow0 + inputH  -- the box bottom border row
                        -- Buttons: " [Send] [Clear] " on the right side of bottom border
                        btns :: String
                        btns = " [Send] [Clear] "
                        btnsStart = lLeftW lay + rw - length btns
                    if row == bottomRow && col >= btnsStart && col < btnsStart + 7
                        then runRuntimeCommand st CmdSendCurrentMessage
                        else if row == bottomRow && col >= btnsStart + 8 && col < btnsStart + 15
                        then runRuntimeCommand st CmdClearInput
                        else placeInputCursorIfNeeded st lay row col
        _ -> pure ()

-- | Handle clicks on the contacts toolbar ([ Browse ] [ Verify ]).
-- The toolbar is in the left pane's first input-area row.
handleContactsToolbarClick :: AppState -> Layout -> Int -> Int -> IO ()
handleContactsToolbarClick st lay row col = do
    let (contactsRow0, contactsCol0, contactsW, contactsH) = Layout.contactsPaneBounds lay
        allFour :: String
        allFour = "[ New ] [ Rename ] [ Browse ] [ Verify ]"
        wide = contactsW >= length allFour
        tbRows = if wide then 1 else 2
        tbStart = contactsRow0 + max 0 (contactsH - tbRows)
        relRow = row - tbStart - 1  -- skip the separator row (row 0 of toolbar)
    if wide
        -- Wide: all on row 0: "[ New ] [ Rename ] [ Browse ] [ Verify ]"
        then when (relRow == 0) $ do
            let padLeft = max 0 ((contactsW - length allFour) `div` 2)
                relCol = col - contactsCol0 - padLeft
            if relCol >= 0 && relCol < 7 then runRuntimeCommand st CmdOpenNewConversation
                else if relCol >= 8 && relCol < 18 then runRuntimeCommand st CmdRenameContact
                else if relCol >= 19 && relCol < 29 then runRuntimeCommand st CmdOpenBrowse
                else if relCol >= 30 && relCol < 40 then runRuntimeCommand st CmdOpenVerify
                else pure ()
        -- Narrow: stacked — row 0: "[ New ] [ Rename ]", row 1: "[ Browse ] [ Verify ]"
        else do
            let topTwo :: String
                topTwo = "[ New ] [ Rename ]"
                botTwo :: String
                botTwo = "[ Browse ] [ Verify ]"
            if relRow == 0 then do
                let padLeft = max 0 ((contactsW - length topTwo) `div` 2)
                    relCol = col - contactsCol0 - padLeft
                if relCol >= 0 && relCol < 7 then runRuntimeCommand st CmdOpenNewConversation
                    else if relCol >= 8 && relCol < 18 then runRuntimeCommand st CmdRenameContact
                    else pure ()
            else when (relRow == 1) $ do
                let padLeft = max 0 ((contactsW - length botTwo) `div` 2)
                    relCol = col - contactsCol0 - padLeft
                if relCol >= 0 && relCol < 10 then runRuntimeCommand st CmdOpenBrowse
                    else if relCol >= 11 && relCol < 21 then runRuntimeCommand st CmdOpenVerify
                    else pure ()

handleToolbarClick :: AppState -> Layout -> Bool -> Int -> IO ()
handleToolbarClick st lay richEnabled col =
    case toolbarButtonAtColumn richEnabled bodyW (col - toolbarCol0) of
        Just cmd -> runRuntimeCommand st cmd
        Nothing -> pure ()
  where
    (_, toolbarCol0, bodyW, _) = Layout.inputToolbarBounds lay

-- | Compute the centering offset for toolbar buttons within bodyW.
toolbarCenterOffset :: Int -> Int
toolbarCenterOffset bodyW =
    let specs = toolbarButtonSpecs False
        toolbar = unwords (map fst specs)
        toolbarW = length toolbar
    in max 0 ((bodyW - toolbarW) `div` 2)

toolbarButtonAtColumn :: Bool -> Int -> Int -> Maybe RuntimeCommand
toolbarButtonAtColumn richEnabled bodyW relCol =
    let offset = toolbarCenterOffset bodyW
        adjustedCol = relCol - offset
    in go adjustedCol (toolbarButtonSpecs richEnabled)
  where
    go _ [] = Nothing
    go relC ((label, cmd):rest)
        | relC >= 0 && relC <= length label - 1 = Just cmd
        | otherwise = go (relC - length label - 1) rest

placeInputCursorIfNeeded :: AppState -> Layout -> Int -> Int -> IO ()
placeInputCursorIfNeeded st lay row col = do
    let (inputRow0, inputCol0, _, inputH) = Layout.inputEntryBounds lay
    buf <- readIORef (asInputBuf st)
    richEnabled <- readIORef (asRichText st)
    let inputLayout = currentInputLayout lay buf
        richLayout = currentRichInputLayout lay buf
        contentRight
            | richEnabled = inputCol0 + richContentW' richLayout - 1
            | otherwise = inputCol0 + iblBodyW inputLayout - (if iblShowScrollbar inputLayout then 1 else 0) - 1
    when (row >= inputRow0 && row <= inputRow0 + inputH - 1 && col >= inputCol0 && col <= contentRight) $ do
        scrollOff <- readIORef (asInputScroll st)
        let rowOff = row - inputRow0
            colOff = col - inputCol0
            cursor
                | richEnabled = richCursorIndexForScreenOffset richLayout scrollOff rowOff colOff
                | otherwise = cursorIndexForScreenOffset inputLayout scrollOff rowOff colOff
            clampedCursor
                | richEnabled = richClampCursor richLayout cursor
                | otherwise = clampInputCursor buf cursor
            newScroll
                | richEnabled = richEnsureCursorVisible richLayout scrollOff clampedCursor
                | otherwise = ensureCursorVisible inputLayout scrollOff clampedCursor
        writeIORef (asInputCursor st) clampedCursor
        writeIORef (asInputScroll st) newScroll
        writeIORef (asSelectionStart st) Nothing

selectContactByRow :: AppState -> Int -> IO ()
selectContactByRow st rowOffset = do
    sessions <- readIORef (cfgSessions (asConfig st))
    cScroll <- readIORef (asContactScroll st)
    let n = Map.size sessions
        idx = cScroll + rowOffset
    when (idx >= 0 && idx < n) $ do
        writeIORef (asSelected st) idx
        writeIORef (asFocus st) ContactPane

menuTabAtColumn :: Int -> Int -> Maybe MenuTab
menuTabAtColumn totalW col =
    go starts tabs
  where
    tabs = [minBound..maxBound] :: [MenuTab]
    labels = map menuTabLabel tabs
    tabsContentW = 1 + sum (map (\l -> length l + 1) labels)
    fillW = max 0 (totalW - tabsContentW - 2)
    starts = scanl (+) (3 + fillW) (map (\l -> length l + 1) labels)
    go [] _ = Nothing
    go _ [] = Nothing
    go (s:ss) (t:ts) =
        let w = length (menuTabLabel t)
        in if col >= s && col < s + w then Just t else go ss ts

-- Contact key handling ----------------------------------------------------

handleContact :: AppState -> InputEvent -> IO ()
handleContact st key = do
    n <- Map.size <$> readIORef (cfgSessions (asConfig st))
    lay <- readIORef (asLayout st)
    let visRows = lChatH lay - lIdentityH lay
    case key of
        KeyUp    -> do
            modifyIORef' (asSelected st) (\i -> max 0 (i-1))
            adjustContactScroll st visRows
        KeyDown  -> do
            modifyIORef' (asSelected st) (\i -> min (max 0 (n-1)) (i+1))
            adjustContactScroll st visRows
        KeyPageUp -> do
            modifyIORef' (asSelected st) (\i -> max 0 (i - max 1 visRows))
            adjustContactScroll st visRows
        KeyPageDown -> do
            modifyIORef' (asSelected st) (\i -> min (max 0 (n-1)) (i + max 1 visRows))
            adjustContactScroll st visRows
        KeyEnter -> writeIORef (asFocus st) ChatPane >> writeIORef (asChatScroll st) 0
        _ -> pure ()

handlePaneMouseWheel :: AppState -> Int -> Int -> Int -> IO ()
handlePaneMouseWheel st row col delta = do
    lay <- readIORef (asLayout st)
    sessions <- readIORef (cfgSessions (asConfig st))
    richEnabled <- readIORef (asRichText st)
    let contactsH = lChatH lay - lIdentityH lay
        inRect (Rect r0 r1 c0 c1) = row >= r0 && row <= r1 && col >= c0 && col <= c1
        (chatRow0, chatCol0, chatW, chatH) = Layout.chatPaneBounds lay
        chatRect = Rect chatRow0 (chatRow0 + chatH - 1) chatCol0 (chatCol0 + chatW - 1)
        (inputRow0, inputCol0, inputW, inputH) = Layout.inputEntryBounds lay
        inputRect = Rect inputRow0 (inputRow0 + inputH - 1) inputCol0 (inputCol0 + inputW - 1)
    if gridRegionAt lay Nothing row col == Just RegionContacts
        then do
            let total = Map.size sessions
                maxOff = max 0 (total - contactsH)
            modifyIORef' (asContactScroll st) (\off -> max 0 (min maxOff (off + delta)))
            -- Keep selected contact on-screen when wheel scrolling contacts.
            sel <- readIORef (asSelected st)
            off <- readIORef (asContactScroll st)
            let visTop = off
                visBot = off + max 0 (contactsH - 1)
            when (sel < visTop || sel > visBot) $
                writeIORef (asSelected st) (max 0 (min (max 0 (total - 1)) visTop))
        else if inRect chatRect
            then do
                sel <- readIORef (asSelected st)
                let entries = Map.toList sessions
                    mSi = if sel >= 0 && sel < length entries then Just (snd (entries !! sel)) else Nothing
                total <- maybe (pure 0) (fmap length . readIORef . siHistory) mSi
                let maxOff = max 0 (total - lChatH lay)
                modifyIORef' (asChatScroll st) (\off -> max 0 (min maxOff (off + delta)))
            else if inRect inputRect
                then do
                    buf <- readIORef (asInputBuf st)
                    if richEnabled
                        then do
                            let inputLayout = currentRichInputLayout lay buf
                            modifyIORef' (asInputScroll st) (\off -> max 0 (min (richMaxScroll inputLayout) (off + delta)))
                        else do
                            let inputLayout = currentInputLayout lay buf
                            modifyIORef' (asInputScroll st) (\off -> max 0 (min (iblMaxScroll inputLayout) (off + delta)))
                else pure ()

handlePaneDrag :: AppState -> Layout -> Int -> Int -> IO ()
handlePaneDrag st lay row col = do
    sessions <- readIORef (cfgSessions (asConfig st))
    richEnabled <- readIORef (asRichText st)
    let totalContacts = Map.size sessions
        contactsH = max 1 (lChatH lay - lIdentityH lay)
        contactsMaxOff = max 0 (totalContacts - contactsH)
        contactsShowSb = totalContacts > contactsH
        (contactsRow0, _, _, _) = Layout.contactsPaneBounds lay
        contactsSbCol = lLeftW lay - 1
        chatH = max 1 (lChatH lay)
        chatRow0 = 2
        rightSbCol = lCols lay - 1

    when (contactsShowSb && col == contactsSbCol && row >= contactsRow0 && row <= contactsRow0 + contactsH - 1) $ do
        let trackH = contactsH
            rowOff = row - contactsRow0
            newOff = rowOff * contactsMaxOff `div` max 1 (trackH - 1)
        writeIORef (asContactScroll st) (max 0 (min contactsMaxOff newOff))

    sel <- readIORef (asSelected st)
    let entries = Map.toList sessions
        mSi = if sel >= 0 && sel < length entries then Just (snd (entries !! sel)) else Nothing
    totalChat <- maybe (pure 0) (fmap length . readIORef . siHistory) mSi
    let chatMaxOff = max 0 (totalChat - chatH)
        chatShowSb = totalChat > chatH
    when (chatShowSb && col == rightSbCol && row >= chatRow0 && row <= chatRow0 + chatH - 1) $ do
        let trackH = chatH
            rowOff = row - chatRow0
            offFromTop = rowOff * chatMaxOff `div` max 1 (trackH - 1)
            newOff = chatMaxOff - offFromTop
        writeIORef (asChatScroll st) (max 0 (min chatMaxOff newOff))

    let (inputRow0, _, _, inputH) = Layout.inputEntryBounds lay
    buf <- readIORef (asInputBuf st)
    let inputLayout = currentInputLayout lay buf
        richLayout = currentRichInputLayout lay buf
        inputMaxOff = iblMaxScroll inputLayout
        richInputMaxOff = richMaxScroll richLayout
        inputShowSb = iblShowScrollbar inputLayout
        richInputShowSb = richShowScrollbar richLayout
    when (((not richEnabled && inputShowSb) || (richEnabled && richInputShowSb))
        && col == rightSbCol && row >= inputRow0 && row <= inputRow0 + inputH - 1) $ do
        let trackH = max 1 inputH
            rowOff = row - inputRow0
            maxOff = if richEnabled then richInputMaxOff else inputMaxOff
            offFromTop = rowOff * maxOff `div` max 1 (trackH - 1)
            newOff = maxOff - offFromTop
        writeIORef (asInputScroll st) (max 0 (min (if richEnabled then richInputMaxOff else inputMaxOff) newOff))

handleIdentity :: AppState -> InputEvent -> IO ()
handleIdentity st key = case key of
    KeyEnter -> openRegenKeyDialog st
    _        -> pure ()

currentInputLayout :: Layout -> String -> InputBuffer.InputBufferLayout
currentInputLayout lay buf =
    let (_, _, bodyW, entryRows) = Layout.inputEntryBounds lay
    in computeInputBufferLayout bodyW entryRows buf

currentRichInputLayout :: Layout -> String -> RichTextLayout
currentRichInputLayout lay buf =
    let (_, _, bodyW, entryRows) = Layout.inputEntryBounds lay
    in computeRichInputLayout bodyW entryRows buf

type RichTextLayout = RichText.RichInputLayout

richContentW' :: RichTextLayout -> Int
richContentW' = richContentW

handleChat :: AppState -> InputEvent -> IO ()
handleChat st key = do
    lay <- readIORef (asLayout st)
    let ch = lChatH lay
    buf <- readIORef (asInputBuf st)
    richEnabled <- readIORef (asRichText st)
    cursor0 <- readIORef (asInputCursor st)
    scroll0 <- readIORef (asInputScroll st)
    let inputLayout = currentInputLayout lay buf
        richLayout = currentRichInputLayout lay buf
        cursor = clampInputCursor buf cursor0
        richCursor0 = richClampCursor richLayout cursor0
        scrollOff = clampInputScroll inputLayout scroll0
        richScrollOff = max 0 (min (richMaxScroll richLayout) scroll0)
        activeCursor = if richEnabled then richCursor0 else cursor
        syncScroll newCursor newScroll =
            if richEnabled
                then writeIORef (asInputCursor st) (richClampCursor richLayout newCursor)
                    >> writeIORef (asInputScroll st) (richEnsureCursorVisible richLayout newScroll newCursor)
                else writeIORef (asInputCursor st) newCursor
                    >> writeIORef (asInputScroll st) (ensureCursorVisible inputLayout newScroll newCursor)
        -- Clear selection and move cursor (non-shift movement)
        clearSelAndSync newCursor newScroll = do
            writeIORef (asSelectionStart st) Nothing
            syncScroll newCursor newScroll
        replaceBuffer newBuf newCursor = do
            let clampedCursor = clampInputCursor newBuf newCursor
                newLayout = currentInputLayout lay newBuf
                newRichLayout = currentRichInputLayout lay newBuf
            writeIORef (asInputBuf st) newBuf
            writeIORef (asSelectionStart st) Nothing
            if richEnabled
                then do
                    let richCursor' = richClampCursor newRichLayout clampedCursor
                    writeIORef (asInputCursor st) richCursor'
                    writeIORef (asInputScroll st) (richEnsureCursorVisible newRichLayout 0 richCursor')
                else do
                    writeIORef (asInputCursor st) clampedCursor
                    writeIORef (asInputScroll st) (ensureCursorVisible newLayout 0 clampedCursor)
        -- Extend selection: remember start if not set, then move cursor
        extendSel newCursor newScroll = do
            mSel <- readIORef (asSelectionStart st)
            case mSel of
                Nothing -> writeIORef (asSelectionStart st) (Just activeCursor)
                Just _ -> pure ()
            syncScroll newCursor newScroll
        -- Wrap selected text or insert empty markers at cursor
        insertFormatMarkers open close = do
            mSel <- readIORef (asSelectionStart st)
            case mSel of
                Just selStart -> do
                    let lo = min selStart activeCursor
                        hi = max selStart activeCursor
                        (pre, rest') = splitAt lo buf
                        (mid, post) = splitAt (hi - lo) rest'
                        newBuf = pre ++ open ++ mid ++ close ++ post
                        newCursor = lo + length open + (hi - lo) + length close
                    writeIORef (asRichText st) True
                    replaceBuffer newBuf newCursor
                Nothing -> do
                    let (lhs, rhs) = splitAt activeCursor buf
                        snippet = open ++ close
                        newBuf = lhs ++ snippet ++ rhs
                        newCursor = activeCursor + length open
                    writeIORef (asRichText st) True
                    replaceBuffer newBuf newCursor
    case key of
        KeyChar c
            | length buf >= maxInputLen -> pure ()
            | otherwise -> do
                let (lhs, rhs) = splitAt activeCursor buf
                replaceBuffer (lhs ++ [c] ++ rhs) (activeCursor + 1)
        KeyBackspace
            | richEnabled && richCursor0 <= 0 -> pure ()
            | not richEnabled && cursor <= 0 -> pure ()
            | otherwise -> do
                let (lhs, rhs) = splitAt activeCursor buf
                replaceBuffer (init lhs ++ rhs) (activeCursor - 1)
        KeyEnter     -> sendCurrentMessage st >> writeIORef (asInputScroll st) 0 >> writeIORef (asInputCursor st) 0
                            >> writeIORef (asSelectionStart st) Nothing
        KeyUp
            | richEnabled && length (RichText.rilWrapped richLayout) > 1
            -> writeIORef (asSelectionStart st) Nothing
                >> moveRichInputCursorVertical st lay buf richCursor0 richScrollOff (-1)
            | not richEnabled && length (InputBuffer.iblRendered inputLayout) > 1
            -> writeIORef (asSelectionStart st) Nothing
                >> moveInputCursorVertical st lay buf cursor scrollOff (-1)
            | otherwise
            -> writeIORef (asSelectionStart st) Nothing >> modifyIORef' (asChatScroll st) (+1)
        KeyDown
            | richEnabled && length (RichText.rilWrapped richLayout) > 1
            -> writeIORef (asSelectionStart st) Nothing
                >> moveRichInputCursorVertical st lay buf richCursor0 richScrollOff 1
            | not richEnabled && length (InputBuffer.iblRendered inputLayout) > 1
            -> writeIORef (asSelectionStart st) Nothing
                >> moveInputCursorVertical st lay buf cursor scrollOff 1
            | otherwise
            -> writeIORef (asSelectionStart st) Nothing >> modifyIORef' (asChatScroll st) (\s -> max 0 (s-1))
        KeyPageUp    -> modifyIORef' (asChatScroll st) (+ch)
        KeyPageDown  -> modifyIORef' (asChatScroll st) (\s -> max 0 (s-ch))
        KeyLeft
            | richEnabled && richCursor0 <= 0 -> writeIORef (asSelectionStart st) Nothing
            | richEnabled -> clearSelAndSync (richMoveCursorLeft richLayout richCursor0) richScrollOff
            | cursor <= 0 -> writeIORef (asSelectionStart st) Nothing
            | otherwise -> clearSelAndSync (cursor - 1) scrollOff
        KeyRight
            | richEnabled && richCursor0 >= length buf -> writeIORef (asSelectionStart st) Nothing
            | richEnabled -> clearSelAndSync (richMoveCursorRight richLayout richCursor0) richScrollOff
            | cursor >= length buf -> writeIORef (asSelectionStart st) Nothing
            | otherwise -> clearSelAndSync (cursor + 1) scrollOff
        KeyHome
            | richEnabled -> clearSelAndSync (richClampCursor richLayout 0) richScrollOff
            | otherwise -> clearSelAndSync 0 scrollOff
        KeyEnd
            | richEnabled -> clearSelAndSync (richClampCursor richLayout (length buf)) richScrollOff
            | otherwise -> clearSelAndSync (length buf) scrollOff
        -- Shift+Left/Right extend selection
        KeyShiftLeft
            | richEnabled && richCursor0 <= 0 -> pure ()
            | richEnabled -> extendSel (richMoveCursorLeft richLayout richCursor0) richScrollOff
            | cursor <= 0 -> pure ()
            | otherwise -> extendSel (cursor - 1) scrollOff
        KeyShiftRight
            | richEnabled && richCursor0 >= length buf -> pure ()
            | richEnabled -> extendSel (richMoveCursorRight richLayout richCursor0) richScrollOff
            | cursor >= length buf -> pure ()
            | otherwise -> extendSel (cursor + 1) scrollOff
        KeyShiftHome
            | richEnabled -> extendSel (richClampCursor richLayout 0) richScrollOff
            | otherwise -> extendSel 0 scrollOff
        KeyShiftEnd
            | richEnabled -> extendSel (richClampCursor richLayout (length buf)) richScrollOff
            | otherwise -> extendSel (length buf) scrollOff
        -- Formatting shortcuts
        KeyCtrlB -> insertFormatMarkers "**" "**"
        KeyCtrlI -> insertFormatMarkers "*" "*"
        KeyCtrlU -> insertFormatMarkers "<u>" "</u>"
        KeyCtrlK -> openInsertLinkDialog st buf activeCursor
        _            -> pure ()

moveInputCursorVertical :: AppState -> Layout -> String -> Int -> Int -> Int -> IO ()
moveInputCursorVertical st lay buf cursor scrollOff delta = do
    let inputLayout = currentInputLayout lay buf
        lineIx = InputBuffer.cursorLineIndex inputLayout cursor
        targetLine = max 0 (min (max 0 (length (InputBuffer.iblWrapped inputLayout) - 1)) (lineIx + delta))
        targetCursor = cursorForLineAndColumn inputLayout targetLine
            (InputBuffer.cursorTextDisplayCol inputLayout cursor)
        newScroll = ensureCursorVisible inputLayout scrollOff targetCursor
    writeIORef (asInputCursor st) targetCursor
    writeIORef (asInputScroll st) newScroll

moveRichInputCursorVertical :: AppState -> Layout -> String -> Int -> Int -> Int -> IO ()
moveRichInputCursorVertical st lay buf cursor scrollOff delta = do
    let inputLayout = currentRichInputLayout lay buf
        lineIx = richCursorLineIndex inputLayout cursor
        targetLine = max 0 (min (max 0 (length (RichText.rilWrapped inputLayout) - 1)) (lineIx + delta))
        targetCursor = richCursorIndexForLineColumn inputLayout targetLine
            (richCursorTextDisplayCol inputLayout cursor)
        newScroll = richEnsureCursorVisible inputLayout scrollOff targetCursor
    writeIORef (asInputCursor st) targetCursor
    writeIORef (asInputScroll st) newScroll

cursorForLineAndColumn :: InputBuffer.InputBufferLayout -> Int -> Int -> Int
cursorForLineAndColumn inputLayout lineIx textCol =
    cursorIndexForLineColumn inputLayout lineIx textCol

-- Dialogs -----------------------------------------------------------------

-- | Close the current dialog and reset the dialog scroll position.
closeDialog :: AppState -> IO ()
closeDialog st = do
    writeIORef (asDialogMode st) Nothing
    writeIORef (asDialogScroll st) 0

-- | Scroll the current dialog overlay by a given delta (positive = down).
-- Reads the current dialog's total line count to enforce the upper bound.
scrollDialog :: AppState -> Int -> IO ()
scrollDialog st delta = do
    dlg <- readIORef (asDialogMode st)
    case dlg of
        Nothing -> pure ()
        Just mode -> do
            lay <- readIORef (asLayout st)
            totalLines <- dialogLineCount st mode
            let (_, _, _, h) = overlayBounds lay totalLines
                contentH = h - 2
                maxOff   = max 0 (totalLines - contentH)
            modifyIORef' (asDialogScroll st) (\off -> max 0 (min maxOff (off + delta)))

handleDialog :: AppState -> InputEvent -> IO ()
handleDialog st KeyEscape = closeDialog st
handleDialog st key = do
    dlg <- readIORef (asDialogMode st)
    case dlg of
        -- Read-only dialogs: arrow/page keys scroll; any other key closes.
        Just DlgHelp   -> handleScrollableReadOnly st key DlgHelp
        Just DlgAbout  -> handleScrollableReadOnly st key DlgAbout
        Just DlgKeys   -> handleScrollableInteractive st key (handleKeysDlg st key)
        Just DlgVerify -> handleScrollableReadOnly st key DlgVerify
        -- Interactive dialogs: scroll keys are intercepted for Up/Down only;
        -- PageUp/PageDown are passed through to dialog-specific handlers.
        Just DlgBrowse   -> handleScrollableInteractive st key (handleBrowseDlg st key)
        Just DlgSettings -> handleScrollableInteractive st key (handleSettingsDlg st key)
        Just DlgNewConn  -> handleScrollableInteractive st key (handleNewConnDlg st key)
        Just DlgRegenKey -> handleScrollableInteractive st key (handleRegenKeyDlg st key)
        Just DlgExportWarn -> handleScrollableInteractive st key (handleExportWarnDlg st key)
        Just DlgExportKeys -> handleScrollableReadOnly st key DlgExportKeys
        Just DlgInsertLink   -> handleInsertLinkDlg st key
        Just DlgEmojiPicker  -> handleEmojiPickerDlg st key
        Just (DlgPrompt _ cb) ->
            handleScrollableInteractive st key $ case key of
                KeyEnter     -> submitPrompt st cb
                KeyChar c    -> modifyIORef' (asDialogBuf st) (\s ->
                    if length s >= maxDialogBufLen then s else s ++ [c])
                KeyBackspace -> modifyIORef' (asDialogBuf st) (\s -> if null s then s else init s)
                _            -> pure ()
        _ -> closeDialog st

-- | Handle key input for a read-only scrollable dialog.
-- Scroll keys (Up/Down/PageUp/PageDown) scroll the overlay;
-- any other key closes the dialog.
handleScrollableReadOnly :: AppState -> InputEvent -> DialogMode -> IO ()
handleScrollableReadOnly st key mode = case key of
    KeyUp       -> scrollDialog st (-1)
    KeyDown     -> scrollDialog st 1
    KeyPageUp   -> scrollDialogPage st mode True
    KeyPageDown -> scrollDialogPage st mode False
    _           -> closeDialog st

-- | Handle key input for an interactive dialog.
-- Up/Down scroll the overlay; all other keys are passed to the given action.
handleScrollableInteractive :: AppState -> InputEvent -> IO () -> IO ()
handleScrollableInteractive st key action = case key of
    KeyUp   -> scrollDialog st (-1)
    KeyDown -> scrollDialog st 1
    _       -> action

-- | Scroll an overlay up or down by one full content-height page.
scrollDialogPage :: AppState -> DialogMode -> Bool -> IO ()
scrollDialogPage st mode upward = do
    lay <- readIORef (asLayout st)
    totalLines <- dialogLineCount st mode
    let (_, _, _, h) = overlayBounds lay totalLines
        contentH = h - 2
        delta = if upward then negate contentH else contentH
    scrollDialog st delta

handleRegenKeyDlg :: AppState -> InputEvent -> IO ()
handleRegenKeyDlg st (KeyChar ' ') =
    modifyIORef' (asRegenCheckbox st) not
handleRegenKeyDlg st (KeyChar 'x') =
    modifyIORef' (asRegenCheckbox st) not
handleRegenKeyDlg st (KeyChar 'X') =
    modifyIORef' (asRegenCheckbox st) not
handleRegenKeyDlg st KeyEnter = do
    checked <- readIORef (asRegenCheckbox st)
    when checked (regenIdentityKey st)
handleRegenKeyDlg _ _ = pure ()

handleKeysDlg :: AppState -> InputEvent -> IO ()
handleKeysDlg st (KeyChar 'r') = openRegenKeyDialog st
handleKeysDlg st (KeyChar 'R') = openRegenKeyDialog st
handleKeysDlg st (KeyChar 'e') = writeIORef (asRegenCheckbox st) False >> writeIORef (asDialogMode st) (Just DlgExportWarn)
handleKeysDlg st (KeyChar 'E') = handleKeysDlg st (KeyChar 'e')
handleKeysDlg st (KeyChar 'x') = closeDialog st
handleKeysDlg st (KeyChar 'X') = closeDialog st
handleKeysDlg st (KeyChar 'i') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Import Identity Key (hex payload)" $ \val ->
        case decodeIdentityHex val of
            Just ik -> writeIORef (cfgIdentity (asConfig st)) (Just ik) >> setStatus st "Identity key imported."
            Nothing -> setStatus st "Invalid key payload."))
handleKeysDlg st (KeyChar 'I') = handleKeysDlg st (KeyChar 'i')
handleKeysDlg _ _ = pure ()

handleExportWarnDlg :: AppState -> InputEvent -> IO ()
handleExportWarnDlg st (KeyChar ' ') = modifyIORef' (asRegenCheckbox st) not
handleExportWarnDlg st (KeyChar 'x') = modifyIORef' (asRegenCheckbox st) not
handleExportWarnDlg st (KeyChar 'X') = modifyIORef' (asRegenCheckbox st) not
handleExportWarnDlg st KeyEnter = do
    checked <- readIORef (asRegenCheckbox st)
    when checked (writeIORef (asDialogMode st) (Just DlgExportKeys))
handleExportWarnDlg _ _ = pure ()

-- | Handle key input for the Insert Link modal dialog.
handleInsertLinkDlg :: AppState -> InputEvent -> IO ()
handleInsertLinkDlg st KeyEscape = closeDialog st
handleInsertLinkDlg st KeyTab = do
    modifyIORef' (asLinkFocus st) (\f -> (f + 1) `mod` 4)
handleInsertLinkDlg st KeyEnter = do
    focus <- readIORef (asLinkFocus st)
    case focus of
        2 -> submitInsertLink st
        3 -> closeDialog st
        _ -> modifyIORef' (asLinkFocus st) (\f -> (f + 1) `mod` 4)
handleInsertLinkDlg st KeyBackspace = do
    focus <- readIORef (asLinkFocus st)
    case focus of
        0 -> modifyIORef' (asLinkText st) (\s -> if null s then s else init s)
        1 -> modifyIORef' (asLinkUrl st)  (\s -> if null s then s else init s)
        _ -> pure ()
handleInsertLinkDlg st (KeyChar c) = do
    focus <- readIORef (asLinkFocus st)
    case focus of
        0 -> modifyIORef' (asLinkText st) (++ [c])
        1 -> modifyIORef' (asLinkUrl st)  (++ [c])
        _ -> pure ()
handleInsertLinkDlg _ _ = pure ()

-- | Open the Insert Link modal, pre-filling the Text field from any active selection.
openInsertLinkDialog :: AppState -> String -> Int -> IO ()
openInsertLinkDialog st buf activeCursor = do
    mSel <- readIORef (asSelectionStart st)
    let selText = case mSel of
            Just selStart ->
                let lo = min selStart activeCursor
                    hi = max selStart activeCursor
                in take (hi - lo) (drop lo buf)
            Nothing -> ""
    writeIORef (asLinkText st) selText
    writeIORef (asLinkUrl st) ""
    writeIORef (asLinkFocus st) (if null selText then 0 else 1)
    writeIORef (asDialogMode st) (Just DlgInsertLink)
    writeIORef (asDialogScroll st) 0

submitInsertLink :: AppState -> IO ()
submitInsertLink st = do
    txt <- readIORef (asLinkText st)
    url <- readIORef (asLinkUrl st)
    unless (null url) $ do
        buf    <- readIORef (asInputBuf st)
        cursor <- readIORef (asInputCursor st)
        let snippet = "[" ++ txt ++ "](" ++ url ++ ")"
            (lhs, rhs) = splitAt cursor buf
            newBuf = lhs ++ snippet ++ rhs
            newCursor = cursor + length snippet
        writeIORef (asRichText st) True
        writeIORef (asInputBuf st) newBuf
        writeIORef (asInputCursor st) newCursor
        writeIORef (asSelectionStart st) Nothing
        writeIORef (asInputScroll st) 0
    closeDialog st

-- Emoji picker dialog handling --------------------------------------------

handleEmojiPickerDlg :: AppState -> InputEvent -> IO ()
handleEmojiPickerDlg st KeyEscape = closeDialog st
handleEmojiPickerDlg st KeyTab = do
    -- Tab cycles through categories (wraps around)
    let nCats = length emojiCategories
    modifyIORef' (asEmojiCategory st) (\c -> (c + 1) `mod` nCats)
    writeIORef (asEmojiPage st) 0
handleEmojiPickerDlg st KeyLeft = stepEmojiPage st (-1)
handleEmojiPickerDlg st KeyRight = stepEmojiPage st 1
handleEmojiPickerDlg st KeyPageUp = stepEmojiPage st (-1)
handleEmojiPickerDlg st KeyPageDown = stepEmojiPage st 1
handleEmojiPickerDlg st KeyEnter = pure ()
handleEmojiPickerDlg st KeyBackspace = do
    modifyIORef' (asEmojiSearch st) (\s -> if null s then s else init s)
    writeIORef (asEmojiPage st) 0
handleEmojiPickerDlg st (KeyChar c)
    | c `elem` selectorKeys || (c >= 'a' && c <= 'z') = do
        -- A-Z or a-z: select emoji at that position
        selectEmojiByKey st (toUpper c)
    | otherwise = do
        -- Any other printable char: append to search
        modifyIORef' (asEmojiSearch st) (++ [c])
        writeIORef (asEmojiPage st) 0
handleEmojiPickerDlg _ _ = pure ()

stepEmojiPage :: AppState -> Int -> IO ()
stepEmojiPage st delta = do
    q    <- readIORef (asEmojiSearch st)
    cat  <- readIORef (asEmojiCategory st)
    page <- readIORef (asEmojiPage st)
    let entries = if null q then emojiByCategory cat else searchEmoji q
        total = emojiPageCount entries
    writeIORef (asEmojiPage st) (max 0 (min (total - 1) (page + delta)))

selectEmojiByKey :: AppState -> Char -> IO ()
selectEmojiByKey st key = do
    q    <- readIORef (asEmojiSearch st)
    cat  <- readIORef (asEmojiCategory st)
    page <- readIORef (asEmojiPage st)
    let entries = if null q then emojiByCategory cat else searchEmoji q
        pageEntries = emojiPage page entries
        mIdx = lookup key (zip selectorKeys [0 :: Int ..])
    case mIdx of
        Nothing -> pure ()
        Just idx ->
            if idx < length pageEntries
                then do
                    let (emoji, _) = pageEntries !! idx
                    insertEmojiIntoInput st emoji
                else pure ()

insertEmojiIntoInput :: AppState -> String -> IO ()
insertEmojiIntoInput st emoji = do
    buf    <- readIORef (asInputBuf st)
    cursor <- readIORef (asInputCursor st)
    let (lhs, rhs) = splitAt cursor buf
        newBuf = lhs ++ emoji ++ rhs
        newCursor = cursor + length emoji
    writeIORef (asInputBuf st) newBuf
    writeIORef (asInputCursor st) newCursor
    writeIORef (asSelectionStart st) Nothing
    closeDialog st

-- | Mouse click handler for the emoji picker overlay.
handleDlgEmojiPickerClick :: AppState -> Layout -> Int -> [String] -> [String] -> Int -> Int -> IO ()
handleDlgEmojiPickerClick st lay lineCount rows _rawLines row col = do
    -- Check for Close button
    let footerIx = lineCount - 1
        btn = overlayButtonHit lay lineCount row col footerIx rows
    if btn "close"
        then closeDialog st
        else do
            -- Check for a click on a content line that contains an emoji cell
            case overlayContentLine lay lineCount row col of
                Just _lineIx -> pure ()  -- cell-level click not yet implemented
                Nothing -> pure ()

decodeIdentityHex :: String -> Maybe IdentityKey
decodeIdentityHex input = do
    let compact = filter (/= ' ') input
    hexText <- if all isDigit compact && even (length compact)
        then decodeIdentityNumeric compact
        else Just (map toLower compact)
    bytes <- fromHex hexText
    if BS.length bytes /= 128 then Nothing else
        Just IdentityKey
            { ikEd25519Secret = BS.take 32 bytes
            , ikEd25519Public = BS.take 32 (BS.drop 32 bytes)
            , ikX25519Secret  = BS.take 32 (BS.drop 64 bytes)
            , ikX25519Public  = BS.drop 96 bytes
            }
  where
    fromHex [] = Just BS.empty
    fromHex [_] = Nothing
    fromHex (a:b:rest) = do
        hi <- nibble a
        lo <- nibble b
        tailBs <- fromHex rest
        pure (BS.cons (fromIntegral (hi * 16 + lo)) tailBs)
    nibble c
        | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
        | c >= 'a' && c <= 'f' = Just (10 + fromEnum c - fromEnum 'a')
        | otherwise = Nothing

decodeIdentityNumeric :: String -> Maybe String
decodeIdentityNumeric [] = Just []
decodeIdentityNumeric [_] = Nothing
decodeIdentityNumeric (a:b:rest) = do
    ch <- case a of
        '0' | b >= '0' && b <= '9' -> Just b
        '1' | b >= '0' && b <= '5' -> Just (toEnum (fromEnum 'a' + fromEnum b - fromEnum '0'))
        _ -> Nothing
    tailTxt <- decodeIdentityNumeric rest
    pure (ch : tailTxt)

handleSettingsDlg :: AppState -> InputEvent -> IO ()
handleSettingsDlg st KeyLeft = shiftSettingsTab st (-1)
handleSettingsDlg st KeyRight = shiftSettingsTab st 1
handleSettingsDlg st (KeyChar '1') = do
    tabIx <- readIORef (asDialogTab st)
    if tabIx == 5
        then runRuntimeCommand st (CmdTogglePlugin "key-persistence")
        else do
            writeIORef (asDialogBuf st) ""
            writeIORef (asDialogMode st) (Just (DlgPrompt "Set Port" $ \val ->
                case reads val of
                    [(p,_)] ->
                        runRuntimeCommand st (CmdSetListenPort (p :: Int))
                    _ -> pure ()))
handleSettingsDlg st (KeyChar '2') = do
    tabIx <- readIORef (asDialogTab st)
    if tabIx == 5
        then runRuntimeCommand st (CmdTogglePlugin "message-storage")
        else do
            writeIORef (asDialogBuf st) ""
            writeIORef (asDialogMode st) (Just (DlgPrompt "Set Display Name" $ \val ->
                unless (null val) $
                    runRuntimeCommand st (CmdSetDisplayName val)))
handleSettingsDlg st (KeyChar '3') = do
    tabIx <- readIORef (asDialogTab st)
    if tabIx == 5
        then runRuntimeCommand st (CmdTogglePlugin "ratchet-persistence")
        else runRuntimeCommand st CmdToggleMDNS
handleSettingsDlg st (KeyChar '4') = do
    tabIx <- readIORef (asDialogTab st)
    if tabIx == 5
        then runRuntimeCommand st (CmdTogglePlugin "runtime-logging")
        else runRuntimeCommand st CmdTogglePEX
handleSettingsDlg st (KeyChar '5') = do
    tabIx <- readIORef (asDialogTab st)
    case tabIx of
        0 -> runRuntimeCommand st CmdToggleRichText
        5 -> runRuntimeCommand st (CmdTogglePlugin "full-persistence")
        _ -> runRuntimeCommand st CmdTogglePersistentStorage
handleSettingsDlg st (KeyChar '6') = do
    if not (pluginEnabled PluginPersistentStorage)
        then setStatus st (pluginUnavailableStatus PluginPersistentStorage)
        else do
            writeIORef (asDialogBuf st) ""
            writeIORef (asDialogMode st) (Just (DlgPrompt "Set DB Path" $ \val ->
                unless (null val) $
                    runRuntimeCommand st (CmdSetDBPath val)))
handleSettingsDlg st (KeyChar '7') = do
    if not (pluginEnabled PluginPersistentStorage)
        then setStatus st (pluginUnavailableStatus PluginPersistentStorage)
        else do
            writeIORef (asDialogBuf st) ""
            writeIORef (asDialogMode st) (Just (DlgPrompt "Retention (days, 0=forever)" $ \val ->
                case reads val of
                    [(d,_)] -> do
                        let days = max 0 (d :: Int)
                        runRuntimeCommand st (CmdSetRetentionDays days)
                    _ -> pure () ))
handleSettingsDlg st (KeyChar '8') =
    runRuntimeCommand st CmdToggleAutoSave
handleSettingsDlg st (KeyChar '9') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Clear history? Type YES to confirm" $ \val ->
        when (val == "YES") $
            runRuntimeCommand st CmdClearSelectedHistory))
handleSettingsDlg st (KeyChar 'a') =
    runRuntimeCommand st CmdToggleDebugLogging
handleSettingsDlg st (KeyChar 'A') = handleSettingsDlg st (KeyChar 'a')
handleSettingsDlg st (KeyChar 'b') = do
    if not (pluginEnabled PluginRuntimeLogging)
        then setStatus st (pluginUnavailableStatus PluginRuntimeLogging)
        else do
            writeIORef (asDialogBuf st) ""
            writeIORef (asDialogMode st) (Just (DlgPrompt "Set Log Path" $ \val ->
                unless (null val) $
                    runRuntimeCommand st (CmdSetDebugLogPath val)))
handleSettingsDlg st (KeyChar 'B') = handleSettingsDlg st (KeyChar 'b')
handleSettingsDlg st (KeyChar 'c') = runRuntimeCommand st CmdCycleConnectionMode
handleSettingsDlg st (KeyChar 'C') = handleSettingsDlg st (KeyChar 'c')
handleSettingsDlg _ _ = pure ()

shiftSettingsTab :: AppState -> Int -> IO ()
shiftSettingsTab st delta = do
    let maxIx = length settingsTabLabels - 1
    modifyIORef' (asDialogTab st) (\ix -> max 0 (min maxIx (ix + delta)))

handleBrowseDlg :: AppState -> InputEvent -> IO ()
handleBrowseDlg st key = case key of
    KeyEscape -> closeDialog st
    KeyLeft -> stepBrowsePage st (-1)
    KeyRight -> stepBrowsePage st 1
    KeyPageUp -> stepBrowsePage st (-1)
    KeyPageDown -> stepBrowsePage st 1
    KeyChar '/' -> openBrowseSearchPrompt st
    KeyChar 's' -> openBrowseSearchPrompt st
    KeyChar 'S' -> openBrowseSearchPrompt st
    KeyChar 'c' -> clearBrowseSearch st
    KeyChar 'C' -> clearBrowseSearch st
    KeyChar c
        | isDigit c -> selectBrowsePeerByDigit st c
        | otherwise -> pure ()
    _ -> pure ()

handleNewConnDlg :: AppState -> InputEvent -> IO ()
-- 1 = Private (secure notes, local only)
handleNewConnDlg st (KeyChar '1') = do
    closeDialog st
    addSecureNotes st
-- 2 = Single (connect to one peer via host:port or listen)
handleNewConnDlg st (KeyChar '2') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "host[:port] (or 'listen' or 'listen:PORT')" $ \val ->
        if val == "listen" || "listen:" `isPrefixOf` val then do
            port <- case break (== ':') val of
                (_, ':':p) -> case reads p of
                    [(n, _)] -> pure (n :: Int)
                    _        -> readIORef (cfgListenPort (asConfig st))
                _          -> readIORef (cfgListenPort (asConfig st))
            runRuntimeCommand st (CmdStartListener port)
        else do
            let (h, mPort) = parseHostPort val
            runRuntimeCommand st (CmdConnectPeer h mPort)
        ))
-- 3 = Group (multiple peers)
handleNewConnDlg st (KeyChar '3') = startGroupPrompt st
handleNewConnDlg _ _ = pure ()

startGroupPrompt :: AppState -> IO ()
startGroupPrompt st = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Group: host[:port] (comma-separated)" $ \val -> do
        let peers = filter (not . null) $ splitOn ',' val
        runRuntimeCommand st (CmdConnectGroup peers)
        ))

strip' :: String -> String
strip' = dropWhile (== ' ')

browsePageSize :: Int
browsePageSize = 10

stepBrowsePage :: AppState -> Int -> IO ()
stepBrowsePage st delta = do
    page <- clampedBrowsePage st
    maxPage <- browseMaxPage st
    writeIORef (asBrowsePage st) (max 0 (min maxPage (page + delta)))

openBrowseSearchPrompt :: AppState -> IO ()
openBrowseSearchPrompt st = do
    current <- readIORef (asBrowseFilter st)
    writeIORef (asDialogBuf st) current
    writeIORef (asDialogMode st) (Just (DlgPrompt "Search peers (name and/or pubkey)" $ \val -> do
        writeIORef (asBrowseFilter st) val
        writeIORef (asBrowsePage st) 0
        writeIORef (asDialogMode st) (Just DlgBrowse)))

clearBrowseSearch :: AppState -> IO ()
clearBrowseSearch st = do
    writeIORef (asBrowseFilter st) ""
    writeIORef (asBrowsePage st) 0

selectBrowsePeerByDigit :: AppState -> Char -> IO ()
selectBrowsePeerByDigit st c = do
    let idx = fromEnum c - fromEnum '0'
    peers <- filteredBrowsePeers st
    page <- clampedBrowsePage st
    case pageItemBySlot browsePageSize page idx peers of
        Just peer -> do
            runRuntimeCommand st (CmdConnectPeer (mdnsIP peer) (Just (mdnsPort peer)))
            closeDialog st
        Nothing ->
            setStatus st ("No peer on slot " ++ [c] ++ " for the current page")

currentBrowsePeers :: AppState -> IO [MDNSPeer]
currentBrowsePeers st = do
    peers <- filteredBrowsePeers st
    page <- clampedBrowsePage st
    pure (map snd (psItems (slicePage browsePageSize page peers)))

clampedBrowsePage :: AppState -> IO Int
clampedBrowsePage st = do
    page <- readIORef (asBrowsePage st)
    peers <- filteredBrowsePeers st
    let page' = psPage (slicePage browsePageSize page peers)
    when (page' /= page) $
        writeIORef (asBrowsePage st) page'
    pure page'

browseMaxPage :: AppState -> IO Int
browseMaxPage st = do
    peers <- filteredBrowsePeers st
    pure (pageMaxIndex browsePageSize peers)

filteredBrowsePeers :: AppState -> IO [MDNSPeer]
filteredBrowsePeers st = do
    peers <- readIORef (cfgMDNSPeers (asConfig st))
    needles <- queryTerms <$> readIORef (asBrowseFilter st)
    pure $
        if null needles
            then peers
            else filter (peerMatches needles) peers

peerMatches :: [String] -> MDNSPeer -> Bool
peerMatches needles peer =
    all (\needle -> any (contains needle) haystacks) needles
  where
    haystacks =
        [ map toLower (maybe "" id (mdnsName peer))
        , map toLower (hexLower (mdnsPubkey peer))
        ]
    contains sub s = any (isPrefixOf sub) (tails s)

    tails [] = [[]]
    tails xs@(_:rest) = xs : tails rest

    hexLower = concatMap byteHex . BS.unpack
    byteHex b =
        let digits = "0123456789abcdef"
            hi = digits !! fromIntegral (b `div` 16)
            lo = digits !! fromIntegral (b `mod` 16)
        in [hi, lo]

queryTerms :: String -> [String]
queryTerms =
    words . map normalize
  where
    normalize c
        | c `elem` [',', ':', ';'] = ' '
        | otherwise = toLower c

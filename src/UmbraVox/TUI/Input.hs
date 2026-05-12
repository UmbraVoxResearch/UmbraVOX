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
import Data.Char (isDigit, toLower)
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
    , wrapOverlayLines
    )
import UmbraVox.TUI.Constants (maxInputLen, maxDialogBufLen, minDropdownW)
import UmbraVox.TUI.Actions (addSecureNotes, sendCurrentMessage,
    setStatus, adjustContactScroll, openRegenKeyDialog, regenIdentityKey)
import UmbraVox.TUI.Layout (dropdownCol)
import qualified UmbraVox.TUI.Layout as Layout
import UmbraVox.Network.MDNS (MDNSPeer(..))
import UmbraVox.Protocol.Encoding (splitOn, parseHostPort)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import qualified Data.ByteString as BS

data GridRegion = RegionContacts | RegionActions | RegionChat | RegionOverlay
    deriving stock (Eq, Show)

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
        '<' -> readMouseCSI
        '5' -> drainTilde >> pure KeyPageUp
        '6' -> drainTilde >> pure KeyPageDown
        '1' -> readCSIExtended
        _ -> drainSeq >> pure KeyUnknown
  where drainTilde = hReady stdin >>= \r -> when r (void getChar)

-- | Handle extended CSI sequences like ESC[1x~ for F-keys
-- F1=ESC[11~, F2=ESC[12~, F3=ESC[13~, F4=ESC[14~, F5=ESC[15~
readCSIExtended :: IO InputEvent
readCSIExtended = do
    c2 <- getChar
    case c2 of
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
        KeyF2    -> toggleMenu st MenuContacts
        KeyF3    -> toggleMenu st MenuChat
        KeyF4    -> toggleMenu st MenuPrefs
        KeyF5    -> writeIORef (asDialogMode st) (Just DlgKeys)
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
    (actionsRow0, actionsCol0, actionsW, actionsH) = Layout.actionsPaneBounds lay
    (chatRow0, chatCol0, chatW, chatH) = Layout.chatPaneBounds lay
    contactsRect = Rect contactsRow0 (contactsRow0 + contactsH - 1) contactsCol0 (contactsCol0 + contactsW - 1)
    actionsRect = Rect actionsRow0 (actionsRow0 + actionsH - 1) actionsCol0 (actionsCol0 + actionsW - 1)
    chatRect = Rect chatRow0 (chatRow0 + chatH - 1) chatCol0 (chatCol0 + chatW - 1)

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
        Nothing -> pure ()

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
                    newOff  = (row - trackTop) * maxOff `div` max 1 (trackH - 1)
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
                    -- Map click row to a scroll offset proportionally
                    newOff  = (row - trackTop) * maxOff `div` max 1 (trackH - 1)
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
        btn = overlayButtonHit lay lineCount row col footerIx rawLines
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
        btn = overlayButtonHit lay lineCount row col footerIx rawLines
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
        btn = overlayButtonHit lay lineCount row col footerIx rawLines
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
    let (contactsRow0, _, _, contactsH) = Layout.contactsPaneBounds lay
        (inputTop, _, _, _) = Layout.actionsPaneBounds lay
        contactsBottom = contactsRow0 + contactsH - 1
        identSepRow = contactsBottom + 1
        row1Text :: String
        row1Text = "[ Regenerate (F5) ]  [ Export Keys ]"
        row1W = length row1Text
        row1Start = 2 + max 0 ((lLeftW lay - 2 - row1W) `div` 2)
        regenText :: String
        regenText = "[ Regenerate (F5) ]"
        exportText :: String
        exportText = "[ Export Keys ]"
        importText :: String
        importText = "[ Import Keys ]"
        regenStart = row1Start
        regenEnd = regenStart + length regenText - 1
        exportStart = regenEnd + 3
        exportEnd = exportStart + length exportText - 1
        importW = length importText
        importStart = 2 + max 0 ((lLeftW lay - 2 - importW) `div` 2)
        importEnd = importStart + length importText - 1
    case gridRegionAt lay Nothing row col of
        Just RegionContacts ->
            if row <= contactsBottom
                then selectContactByRow st (row - contactsRow0)
                else if row > identSepRow
                    then writeIORef (asFocus st) IdentityPane
                    else pure ()
        Just RegionActions ->
            if row == inputTop
                then if col >= regenStart && col <= regenEnd
                    then openRegenKeyDialog st
                    else if col >= exportStart && col <= exportEnd
                        then writeIORef (asRegenCheckbox st) False >> writeIORef (asDialogMode st) (Just DlgExportWarn)
                        else pure ()
                else if row == inputTop + 1
                    then if col >= importStart && col <= importEnd
                        then handleKeysDlg st (KeyChar 'i')
                        else pure ()
                else pure ()
        Just RegionChat -> writeIORef (asFocus st) ChatPane
        _ -> pure ()

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
    let contactsH = lChatH lay - lIdentityH lay
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
        else pure ()

handleIdentity :: AppState -> InputEvent -> IO ()
handleIdentity st key = case key of
    KeyEnter -> openRegenKeyDialog st
    _        -> pure ()

handleChat :: AppState -> InputEvent -> IO ()
handleChat st key = do
    lay <- readIORef (asLayout st)
    let ch = lChatH lay
    case key of
        KeyChar c    -> modifyIORef' (asInputBuf st) (\s ->
            if length s >= maxInputLen then s else s ++ [c])
        KeyBackspace -> modifyIORef' (asInputBuf st) (\s -> if null s then s else init s)
        KeyEnter     -> sendCurrentMessage st
        KeyUp        -> modifyIORef' (asChatScroll st) (+1)
        KeyDown      -> modifyIORef' (asChatScroll st) (\s -> max 0 (s-1))
        KeyPageUp    -> modifyIORef' (asChatScroll st) (+ch)
        KeyPageDown  -> modifyIORef' (asChatScroll st) (\s -> max 0 (s-ch))
        _            -> pure ()

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
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Set Port" $ \val ->
        case reads val of
            [(p,_)] ->
                runRuntimeCommand st (CmdSetListenPort (p :: Int))
            _ -> pure ()))
handleSettingsDlg st (KeyChar '2') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Set Display Name" $ \val ->
        unless (null val) $
            runRuntimeCommand st (CmdSetDisplayName val)))
handleSettingsDlg st (KeyChar '3') =
    runRuntimeCommand st CmdToggleMDNS
handleSettingsDlg st (KeyChar '4') =
    runRuntimeCommand st CmdTogglePEX
handleSettingsDlg st (KeyChar '5') =
    runRuntimeCommand st CmdTogglePersistentStorage
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

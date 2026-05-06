-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Input
    ( readKey, readCSI, readSS3, drainSeq
    , eventLoop
    , handleNormal, handleContact, handleChat, handleDialog
    , handleSettingsDlg, handleNewConnDlg
    , strip'
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (void, when, unless)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Char (isDigit, toLower)
import Data.List (isPrefixOf)
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
    ( browseOverlayLines, overlayBounds, overlayButtonAtLine, overlayCloseBounds, settingsOverlayLines
    , helpOverlayLines, aboutOverlayLines, newConnOverlayLines
    , verifyOverlayLines, keysOverlayLines, promptOverlayLines, settingsTabLabels
    )
import UmbraVox.TUI.Constants (maxInputLen, maxDialogBufLen, minDropdownW)
import UmbraVox.TUI.Actions (addSecureNotes, sendCurrentMessage,
    setStatus, adjustContactScroll)
import UmbraVox.TUI.Layout (dropdownCol)
import UmbraVox.Network.MDNS (MDNSPeer(..))
import UmbraVox.Protocol.Encoding (splitOn, parseHostPort)

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
                            in if term == 'M' && button == 0 && not isWheel && not isMotion
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
        KeyTab   -> modifyIORef' (asFocus st) (\p ->
            if p == ContactPane then ChatPane else ContactPane)
        KeyCtrlN -> runRuntimeCommand st CmdOpenNewConversation
        KeyCtrlG -> startGroupPrompt st
        KeyCtrlQ -> runRuntimeCommand st CmdQuit
        KeyCtrlD -> runRuntimeCommand st CmdQuit
        KeyF1    -> toggleMenu st MenuHelp
        KeyF2    -> toggleMenu st MenuContacts
        KeyF3    -> toggleMenu st MenuChat
        KeyF4    -> toggleMenu st MenuPrefs
        KeyF5    -> pure ()
        KeyEscape -> do
            dlg <- readIORef (asDialogMode st)
            case dlg of
                Just _  -> writeIORef (asDialogMode st) Nothing
                Nothing -> pure ()
        _ -> case focus of
            ContactPane -> handleContact st key
            ChatPane    -> handleChat st key

handleMouseClick :: AppState -> Int -> Int -> IO ()
handleMouseClick st row col = do
    lay <- readIORef (asLayout st)
    dlg <- readIORef (asDialogMode st)
    case dlg of
        Just mode -> handleDialogMouseClick st lay mode row col
        Nothing -> do
            if row == 1
                then handleMenuBarClick st lay col
                else do
                    mOpen <- readIORef (asMenuOpen st)
                    case mOpen of
                        Just tab -> do
                            handled <- handleDropdownClick st lay tab row col
                            when (not handled) $ do
                                closeMenu st
                                handlePaneClick st lay row col
                        Nothing -> handlePaneClick st lay row col

handleDialogMouseClick :: AppState -> Layout -> DialogMode -> Int -> Int -> IO ()
handleDialogMouseClick st lay dlg row col = do
    lineCount <- dialogLineCount st dlg
    closedByX <- handleOverlayTopClose st lay lineCount row col
    unless closedByX $
        case dlg of
            DlgHelp ->
                when (overlayButtonHit lay lineCount row col (length helpOverlayLines - 1) helpOverlayLines "close") $
                    writeIORef (asDialogMode st) Nothing
            DlgAbout ->
                when (overlayButtonHit lay lineCount row col (length aboutOverlayLines - 1) aboutOverlayLines "close") $
                    writeIORef (asDialogMode st) Nothing
            DlgKeys -> do
                lines' <- keysOverlayLines st
                when (overlayButtonHit lay lineCount row col (length lines' - 1) lines' "close") $
                    writeIORef (asDialogMode st) Nothing
            DlgVerify -> do
                lines' <- verifyOverlayLines st
                when (overlayButtonHit lay lineCount row col (length lines' - 1) lines' "close") $
                    writeIORef (asDialogMode st) Nothing
            DlgBrowse -> do
                lines' <- browseOverlayLines st
                let footerIx = length lines' - 1
                if overlayButtonHit lay lineCount row col footerIx lines' "prev"
                    then stepBrowsePage st (-1)
                else if overlayButtonHit lay lineCount row col footerIx lines' "next"
                    then stepBrowsePage st 1
                else if overlayButtonHit lay lineCount row col footerIx lines' "search"
                    then openBrowseSearchPrompt st
                else if overlayButtonHit lay lineCount row col footerIx lines' "clear"
                    then clearBrowseSearch st
                else if overlayButtonHit lay lineCount row col footerIx lines' "close"
                    then writeIORef (asDialogMode st) Nothing
                else case overlayContentLine lay lineCount row col of
                    Just 2 -> openBrowseSearchPrompt st
                    Just lineIx -> do
                        visible <- currentBrowsePeers st
                        let peerIx = lineIx - 5
                        when (peerIx >= 0 && peerIx < length visible) $
                            selectBrowsePeerByDigit st (toEnum (fromEnum '0' + peerIx))
                    Nothing -> pure ()
            DlgSettings -> do
                lines' <- settingsOverlayLines st
                let footerIx = length lines' - 1
                case overlayButtonAtLine lay lines' 0 row col of
                    Just tabLabel ->
                        case lookup (map toLower tabLabel) (zip (map (map toLower) settingsTabLabels) [0..]) of
                            Just tabIx -> writeIORef (asDialogTab st) tabIx
                            Nothing -> pure ()
                    Nothing ->
                        if overlayButtonHit lay lineCount row col footerIx lines' "close"
                            then writeIORef (asDialogMode st) Nothing
                            else case overlayContentLine lay lineCount row col of
                                Just lineIx ->
                                    case lineOptionKey (lines' !! lineIx) of
                                        Just key -> handleSettingsDlg st (KeyChar key)
                                        Nothing -> pure ()
                                Nothing -> pure ()
            DlgNewConn -> do
                let lines' = newConnOverlayLines
                    footerIx = length lines' - 1
                if overlayButtonHit lay lineCount row col footerIx lines' "private"
                    then handleNewConnDlg st (KeyChar '1')
                else if overlayButtonHit lay lineCount row col footerIx lines' "single"
                    then handleNewConnDlg st (KeyChar '2')
                else if overlayButtonHit lay lineCount row col footerIx lines' "group"
                    then handleNewConnDlg st (KeyChar '3')
                else if overlayButtonHit lay lineCount row col footerIx lines' "cancel"
                    then writeIORef (asDialogMode st) Nothing
                else case overlayContentLine lay lineCount row col of
                    Just 0 -> handleNewConnDlg st (KeyChar '1')
                    Just 1 -> handleNewConnDlg st (KeyChar '2')
                    Just 2 -> handleNewConnDlg st (KeyChar '3')
                    _ -> pure ()
            DlgPrompt title cb -> do
                buf <- readIORef (asDialogBuf st)
                let lines' = promptOverlayLines title buf
                    footerIx = length lines' - 1
                if overlayButtonHit lay lineCount row col footerIx lines' "ok"
                    then submitPrompt st cb
                else if overlayButtonHit lay lineCount row col footerIx lines' "cancel"
                    then do
                        writeIORef (asDialogBuf st) ""
                        writeIORef (asDialogMode st) Nothing
                else pure ()

dialogLineCount :: AppState -> DialogMode -> IO Int
dialogLineCount _ DlgHelp = pure (length helpOverlayLines)
dialogLineCount _ DlgAbout = pure (length aboutOverlayLines)
dialogLineCount st DlgSettings = length <$> settingsOverlayLines st
dialogLineCount st DlgVerify = length <$> verifyOverlayLines st
dialogLineCount _ DlgNewConn = pure (length newConnOverlayLines)
dialogLineCount st DlgKeys = length <$> keysOverlayLines st
dialogLineCount st DlgBrowse = length <$> browseOverlayLines st
dialogLineCount st (DlgPrompt title _) = do
    buf <- readIORef (asDialogBuf st)
    pure (length (promptOverlayLines title buf))

handleOverlayTopClose :: AppState -> Layout -> Int -> Int -> Int -> IO Bool
handleOverlayTopClose st lay lineCount row col = do
    let (closeRow, closeStart, closeEnd) = overlayCloseBounds lay lineCount
        hit = row == closeRow && col >= closeStart && col <= closeEnd
    when hit (writeIORef (asDialogMode st) Nothing)
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
    writeIORef (asDialogMode st) Nothing

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
    let items = menuTabItems tab
        boxW = max minDropdownW (maximum (map length items) + 4)
        dropCol = dropdownCol (lCols lay) tab
        itemStartRow = 3
        itemEndRow = itemStartRow + length items - 1
        insideCols = col >= dropCol && col < dropCol + boxW
        insideRows = row >= itemStartRow && row <= itemEndRow
    if insideCols && insideRows
        then do
            let idx = row - itemStartRow
            closeMenu st
            executeMenuItem st tab idx
            pure True
        else pure False

handlePaneClick :: AppState -> Layout -> Int -> Int -> IO ()
handlePaneClick st lay row col = do
    let chatTop = 2
        chatBottom = 1 + lChatH lay
        inputRow = lChatH lay + 3
        leftInnerStart = 2
        leftInnerEnd = lLeftW lay - 1
        rightInnerStart = lLeftW lay + 2
    if row >= chatTop && row <= chatBottom
        then if col >= leftInnerStart && col <= leftInnerEnd
            then selectContactByRow st (row - chatTop)
            else when (col >= rightInnerStart) $
                writeIORef (asFocus st) ChatPane
        else when (row == inputRow && col >= rightInnerStart) $
            writeIORef (asFocus st) ChatPane

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
    let visRows = lChatH lay
    case key of
        KeyUp    -> do
            modifyIORef' (asSelected st) (\i -> max 0 (i-1))
            adjustContactScroll st visRows
        KeyDown  -> do
            modifyIORef' (asSelected st) (\i -> min (max 0 (n-1)) (i+1))
            adjustContactScroll st visRows
        KeyEnter -> writeIORef (asFocus st) ChatPane >> writeIORef (asChatScroll st) 0
        _ -> pure ()

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
handleDialog :: AppState -> InputEvent -> IO ()
handleDialog st KeyEscape = writeIORef (asDialogMode st) Nothing
handleDialog st key = do
    dlg <- readIORef (asDialogMode st)
    case dlg of
        Just DlgHelp    -> writeIORef (asDialogMode st) Nothing
        Just DlgAbout   -> writeIORef (asDialogMode st) Nothing
        Just DlgKeys    -> writeIORef (asDialogMode st) Nothing
        Just DlgVerify  -> writeIORef (asDialogMode st) Nothing
        Just DlgBrowse  -> handleBrowseDlg st key
        Just DlgSettings -> handleSettingsDlg st key
        Just DlgNewConn  -> handleNewConnDlg st key
        Just (DlgPrompt _ cb) -> case key of
            KeyEnter -> submitPrompt st cb
            KeyChar c -> modifyIORef' (asDialogBuf st) (\s ->
                if length s >= maxDialogBufLen then s else s ++ [c])
            KeyBackspace -> modifyIORef' (asDialogBuf st) (\s -> if null s then s else init s)
            _ -> pure ()
        _ -> writeIORef (asDialogMode st) Nothing

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
handleSettingsDlg st (KeyChar '0') =
    writeIORef (asDialogMode st) (Just DlgKeys)
handleSettingsDlg _ _ = pure ()

shiftSettingsTab :: AppState -> Int -> IO ()
shiftSettingsTab st delta = do
    let maxIx = length settingsTabLabels - 1
    modifyIORef' (asDialogTab st) (\ix -> max 0 (min maxIx (ix + delta)))

handleBrowseDlg :: AppState -> InputEvent -> IO ()
handleBrowseDlg st key = case key of
    KeyEscape -> writeIORef (asDialogMode st) Nothing
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
    writeIORef (asDialogMode st) Nothing
    addSecureNotes st
-- 2 = Single (connect to one peer via host:port or listen)
handleNewConnDlg st (KeyChar '2') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "host:port (or 'listen' or 'listen:PORT')" $ \val ->
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
    writeIORef (asDialogMode st) (Just (DlgPrompt "Group: host:port (comma-separated)" $ \val -> do
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
            writeIORef (asDialogMode st) Nothing
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

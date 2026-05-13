-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Render
    ( render
    , clearScreen, goto, showCursor, hideCursor, withRawMode
    , getTermSize, clampSize, sizeValid, calcLayout
    , showOverlay, padR, isPfx
    , statusBarConnTag
    ) where

import Control.Monad (forM_, when)
import Data.IORef (readIORef, writeIORef)
import Data.List (stripPrefix)
import qualified Data.Map.Strict as Map
import System.IO (hFlush, stdout)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Terminal (csi, goto, clearScreen, hideCursor, showCursor,
    setFg, resetSGR, bold, padR, getTermSize, withRawMode)
import UmbraVox.TUI.Layout (clampSize, sizeValid, calcLayout)
import qualified UmbraVox.TUI.Layout as Layout
import UmbraVox.TUI.Text (displayWidth, trimToWidth)
import UmbraVox.TUI.Dialog (showOverlay, renderHelpOverlay, renderAboutOverlay, renderKeysOverlay,
    renderSettingsOverlay, renderNewConnOverlay, renderVerifyOverlay,
    renderBrowseOverlay, renderPromptOverlay, renderRegenKeyOverlay, renderDropdown,
    helpOverlayLines, aboutOverlayLines, newConnOverlayLines, keysOverlayLines,
    verifyOverlayLines, promptOverlayLines, settingsOverlayLines, browseOverlayLines,
    regenKeyOverlayLines, exportWarnOverlayLines, exportKeysOverlayLines)
import UmbraVox.Protocol.QRCode (generateSafetyNumber, renderSafetyNumber, renderFingerprint, generateQRCode, renderQRCode)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Version (versionFull)

clampInt :: Int -> Int -> Int -> Int
clampInt lo hi x = max lo (min hi x)

data RenderGrid = RenderGrid
    { gContentTop :: Int
    , gContentBottom :: Int
    , gSepRow :: Int
    , gInputTop :: Int
    , gInputBottom :: Int
    , gBottomBorderRow :: Int
    , gStatusRow :: Int
    }

mkRenderGrid :: Layout -> RenderGrid
mkRenderGrid lay =
    let contentTop = 2
        contentBottom = contentTop + lChatH lay - 1
        sepRow = contentBottom + 1
        inputTop = sepRow + 1
        inputBottom = inputTop + Layout.inputAreaRows - 1
        bottomBorderRow = inputBottom + 1
    in RenderGrid
        { gContentTop = contentTop
        , gContentBottom = contentBottom
        , gSepRow = sepRow
        , gInputTop = inputTop
        , gInputBottom = inputBottom
        , gBottomBorderRow = bottomBorderRow
        , gStatusRow = lRows lay
        }

-- | Prefix check — used by Actions.hs for input parsing.
isPfx :: String -> String -> Bool
isPfx [] _ = True; isPfx _ [] = False
isPfx (x:xs) (y:ys) = x == y && isPfx xs ys

-- Rendering ---------------------------------------------------------------
renderSizeWarning :: Int -> Int -> IO ()
renderSizeWarning rows cols = do
    csi "2J"; csi "H"
    let msg1 = "Terminal size: " ++ show cols ++ "x" ++ show rows ++ "."
        msg2 = "Required: min 80x24, max 300x100. Please resize."
        r1 = max 1 (rows `div` 2 - 1)
        c1a = max 1 ((cols - displayWidth msg1) `div` 2)
        c1b = max 1 ((cols - displayWidth msg2) `div` 2)
    goto r1 c1a; setFg 31; bold; putStr msg1; resetSGR
    goto (r1+1) c1b; setFg 31; bold; putStr msg2; resetSGR
    hFlush stdout

-- | Row 1: tabbed menu bar with separators, edge-to-edge.
renderMenuBar :: Layout -> Maybe MenuTab -> IO ()
renderMenuBar lay mOpen = do
    let totalW = lCols lay
        tabs = [minBound..maxBound] :: [MenuTab]
        tabLabels = map menuTabLabel tabs
        tabsContentW = 1 + sum (map (\l -> length l + 1) tabLabels)
        fillW = max 0 (totalW - tabsContentW - 2)
    goto 1 1; setFg 36; putStr "\x256D"
    resetSGR; setFg 36; putStr (replicate fillW '\x2500')
    putStr "\x2502"
    mapM_ (\tab -> do
        let label = menuTabLabel tab
        if mOpen == Just tab then do
            resetSGR; bold; setFg 37; csi "46m"  -- white on cyan
            renderUnderlinedLabel label (menuTabUnderlineIndex tab)
            resetSGR; setFg 36
        else do
            resetSGR; setFg 36
            renderUnderlinedLabel label (menuTabUnderlineIndex tab)
        putStr "\x2502"
        ) tabs
    putStr "\x256E"; resetSGR

renderUnderlinedLabel :: String -> Maybe Int -> IO ()
renderUnderlinedLabel label mIx =
    mapM_ renderChar (zip [0 :: Int ..] label)
  where
    renderChar (ix, ch)
        | mIx == Just ix = do
            csi "4m"
            putChar ch
            csi "24m"
        | otherwise = putChar ch

-- | Render a single contact row (left pane content)
renderContactCell :: Layout -> [(SessionId, SessionInfo)] -> Int -> Pane -> Int -> Int -> IO ()
renderContactCell lay entries sel focus cScroll row = do
    let lw = lLeftW lay
        chatH' = lChatH lay
        contactsH = chatH' - lIdentityH lay
        total = length entries
        idx = row + cScroll
    let showScrollbar = total > contactsH
        scrollbarChar =
            let trackH = max 1 contactsH
                thumbSz = max 1 (trackH * contactsH `div` max 1 total)
                maxOff = max 0 (total - contactsH)
                thumbPos = if maxOff == 0 then 0 else cScroll * (trackH - thumbSz) `div` max 1 maxOff
            in if row >= thumbPos && row < thumbPos + thumbSz then '\x2588' else '\x2502'
    if idx >= 0 && idx < length entries then do
        let (_, si) = entries !! idx
        tag <- statusTag <$> readIORef (siStatus si)
        let mk = if idx == sel then " \x25B8 " else "   "
            sbW = if showScrollbar then 1 else 0
            nameW = max 0 (lw - 2 - displayWidth mk - displayWidth tag - sbW)
            cell = mk ++ padR nameW (siPeerName si) ++ tag
        when (idx == sel) $ if focus == ContactPane then bold >> setFg 32 else bold
        putStr cell
        resetSGR
        when showScrollbar $ csi "2m" >> setFg 37 >> putChar scrollbarChar >> resetSGR
    else do
        let fillW = lw - (if showScrollbar then 3 else 2)
        putStr (replicate fillW ' ')
        when showScrollbar $ csi "2m" >> setFg 37 >> putChar scrollbarChar >> resetSGR

-- | Build the lines for the inline identity panel given an optional IdentityKey.
-- Returns exactly 'identityH - 1' content lines (the separator row is drawn separately).
-- Layout (top to bottom):
--   1. One QR code centered (derived from combined X25519+Ed25519 safety number)
--   2. Standard header centered under QR
--   3. Safety number rows (4x3 groups)
--   4. Blank line
--   5. Two-column fingerprints: " X25519:" | " Ed25519:", then rows side by side
--   6. One spacer line
--   7. (reserved spacer; key action row is rendered in the lower-left box)
identityPanelLines :: Layout -> Maybe IdentityKey -> Pane -> [String]
identityPanelLines lay mIk _focus =
    let innerW   = lLeftW lay - 2
        qrStdText = "Standard: X3DH safety number"
        available = lIdentityH lay - 1
        noIdLines = padR innerW "No identity yet."
                  : replicate (available - 1) (replicate innerW ' ')
        -- Pin the button to the very last slot; body fills or is trimmed.
        finalize body =
            let bodySlots = available - 1
                fitted = if length body <= bodySlots
                    then body ++ replicate (bodySlots - length body) (replicate innerW ' ')
                    else take bodySlots body
            in fitted ++ [replicate innerW ' ']
    in case mIk of
        Nothing -> noIdLines
        Just ik ->
            let x25519Lns = renderFingerprint (ikX25519Public ik)
                ed25519Lns = renderFingerprint (ikEd25519Public ik)
                -- One combined safety number from both keys
                safetyNum  = generateSafetyNumber (ikX25519Public ik) (ikEd25519Public ik)
                qrLns      = renderQRCode (generateQRCode safetyNum)
                safetyLns  = renderSafetyNumber safetyNum
                body       = identityBody innerW x25519Lns ed25519Lns qrLns qrStdText safetyLns
            in finalize body

-- | Content body for the identity panel (Regenerate button not included).
-- Layout: centered QR code, standard label, safety-number rows, blank,
-- then two-column fingerprint block (X25519 left | Ed25519 right).
identityBody :: Int -> [String] -> [String] -> [String] -> String -> [String] -> [String]
identityBody innerW x25519Lns ed25519Lns qrLns qrStdText safetyLns =
    map centerLine qrLns
    ++ [centerUnderQr innerW qrLns qrStdText]
    ++ map (padR innerW) safetyLns
    ++ [replicate innerW ' ']
    ++ fpTwoColLines innerW x25519Lns ed25519Lns
  where
    centerLine s =
        centerText innerW s

centerUnderQr :: Int -> [String] -> String -> String
centerUnderQr innerW qrLns label =
    let qrW = maximum (1 : map displayWidth qrLns)
        qrPad = max 0 ((innerW - qrW) `div` 2)
        labelW = displayWidth label
        labelPadInQr = max 0 ((qrW - labelW) `div` 2)
        totalPad = max 0 (min (innerW - labelW) (qrPad + labelPadInQr))
    in padR innerW (replicate totalPad ' ' ++ label)

centerText :: Int -> String -> String
centerText innerW s = centerTextOffset innerW 0 s

centerTextOffset :: Int -> Int -> String -> String
centerTextOffset innerW offset s =
        let w = length s
            room = max 0 (innerW - w)
            basePad = room `div` 2
            pad = max 0 (min room (basePad + offset))
        in padR innerW (replicate pad ' ' ++ s)

-- | Two-column fingerprint block: header row then rows zipped from each fingerprint.
fpTwoColLines :: Int -> [String] -> [String] -> [String]
fpTwoColLines innerW x25519Lns ed25519Lns =
    let colW  = (innerW - 2) `div` 2
        sep   = "  "
        hdr   = padR innerW (padR colW " X25519:" ++ sep ++ padR colW " Ed25519:")
        zipRows ls rs =
            let maxLen = max (length ls) (length rs)
                blank  = replicate colW ' '
                padCol col = col ++ replicate (maxLen - length col) blank
            in zipWith (\l r -> padR colW l ++ sep ++ padR colW r)
                       (padCol ls) (padCol rs)
    in hdr : zipRows x25519Lns ed25519Lns

-- | Render one content row: left contact + divider + right chat
renderPaneRow :: Layout -> RenderGrid -> [(SessionId, SessionInfo)] -> Maybe SessionInfo
              -> Int -> Pane -> Int -> Int -> Int -> IO ()
renderPaneRow lay grid entries selSi sel focus scroll' cScroll row = do
    renderPaneRowLeft lay grid entries sel focus cScroll row
    renderPaneRowRight lay grid selSi scroll' row

renderPaneRowLeft :: Layout -> RenderGrid -> [(SessionId, SessionInfo)] -> Int -> Pane -> Int -> Int -> IO ()
renderPaneRowLeft lay grid entries sel focus cScroll row = do
    let contentRow = gContentTop grid + row
        chatH' = lChatH lay
    goto contentRow 1
    -- Left border
    setFg 36; putStr "\x2502"; resetSGR
    -- Contact cell (only in the contacts area — rows above the identity panel)
    let contactsH = chatH' - lIdentityH lay
    if row < contactsH
        then renderContactCell lay entries sel focus cScroll row
        else putStr (replicate (lLeftW lay - 2) ' ')
    -- Divider
    setFg 36; putStr "\x2502"; resetSGR

renderPaneRowRight :: Layout -> RenderGrid -> Maybe SessionInfo -> Int -> Int -> IO ()
renderPaneRowRight lay grid selSi scroll' row = do
    let rw = lRightW lay
        contentRow = gContentTop grid + row
        chatH' = lChatH lay
    goto contentRow (lLeftW lay + 1)
    -- Chat message
    msg <- case selSi of
        Nothing -> pure ""
        Just si -> do
            hist <- readIORef (siHistory si)
            let msgs = reverse hist; total = length msgs
                start = max 0 (min (total - 1) (total - chatH' - scroll'))
                idx = start + row
            pure $ if idx >= 0 && idx < total then msgs !! idx else ""
    putStr (padR (rw - 1) msg)
    -- Right border
    setFg 36; putStr "\x2502"; resetSGR

-- | Render the identity panel separator and content rows, left side only.
-- Called after renderPaneRow has already drawn all content rows (the right
-- pane chat messages in those rows are preserved).
renderIdentityPanel :: Layout -> RenderGrid -> AppState -> Maybe IdentityKey -> IO ()
renderIdentityPanel lay grid st mIk = do
    focus <- readIORef (asFocus st)
    let lw = lLeftW lay
        chatH' = lChatH lay
        contactsH = chatH' - lIdentityH lay
        sepRow = gContentTop grid + contactsH
        innerW = lw - 2
        panelLines = identityPanelLines lay mIk focus
    -- Separator between contacts and identity panel (left side only)
    goto sepRow 1; setFg 36
    putStr $ "\x251C" ++ replicate innerW '\x2500' ++ "\x253C"
    resetSGR
    -- Identity panel content rows (left side only)
    forM_ (zip [0..] panelLines) $ \(i, line) -> do
        let panelRow = sepRow + 1 + i
        goto panelRow 1
        setFg 36; putStr "\x2502"; resetSGR
        putStr line; resetSGR
        goto panelRow (lw); setFg 36; putStr "\x2502"; resetSGR

-- | Mid-border separator between content and input row
renderMidBorder :: Layout -> RenderGrid -> IO ()
renderMidBorder lay grid = do
    let lw = lLeftW lay; rw = lRightW lay
        borderRow = gSepRow grid
    goto borderRow 1; setFg 36
    putStr $ "\x251C" ++ replicate (lw - 2) '\x2500' ++ "\x253C"
          ++ replicate (rw - 1) '\x2500' ++ "\x2524"
    resetSGR

-- | Input row: blank left pane, input field on right
renderInputRow :: Layout -> RenderGrid -> Pane -> String -> IO ()
renderInputRow lay grid focus buf = do
    let lw = lLeftW lay; rw = lRightW lay
        inputTop = gInputTop grid
        inputRows = Layout.inputAreaRows
        prefix = " \x25B8 "
    forM_ [0..inputRows-1] $ \i -> do
        let inputRow = inputTop + i
        -- Keep the left pane visually stable: only the first input row uses
        -- the left interior; additional rows stay as a closed pane under QR.
        goto inputRow 1; setFg 36; putStr "\x2502"; resetSGR
        if i == 0
            then do
                let actionsText = "[ Regenerate (F5) ]  [ Export Keys ]"
                putStr (centerText (lw - 2) actionsText)
            else if i == 1
                then do
                    let importText = "[ Import Keys ]"
                    putStr (centerText (lw - 2) importText)
            else putStr (replicate (lw - 2) ' ')
        setFg 36; putStr "\x2502"; resetSGR
        if i == 0
            then if focus == ChatPane then do
                    bold; setFg 32
                    let bodyW = max 0 (rw - 1 - displayWidth prefix - 1)
                    putStr (padR (rw - 1) (prefix ++ trimToWidth bodyW buf ++ "\x2588"))
                    resetSGR
                 else do
                    let bodyW = max 0 (rw - 1 - displayWidth prefix)
                    putStr (padR (rw - 1) (prefix ++ trimToWidth bodyW buf))
            else putStr (replicate (rw - 1) ' ')
        setFg 36; putStr "\x2502"; resetSGR

-- | Bottom border, edge-to-edge
renderBottomBorder :: Layout -> RenderGrid -> IO ()
renderBottomBorder lay grid = do
    let lw = lLeftW lay; rw = lRightW lay
        botRow = gBottomBorderRow grid
    goto botRow 1; setFg 36
    putStr $ "\x2570" ++ replicate (lw - 2) '\x2500' ++ "\x2534"
          ++ replicate (rw - 1) '\x2500' ++ "\x256F"
    resetSGR

statusBarConnTag :: ConnectionMode -> Maybe Bool -> Int -> String
statusBarConnTag connMode persistencePref nSessions =
    sessionCount ++ modeTag ++ " \x25C6 " ++ versionFull
  where
    sessionCount
        | nSessions > 0 = show nSessions ++ " session" ++ (if nSessions > 1 then "s" else "")
        | otherwise = "No sessions"
    modeTag
        | connMode == Chastity || persistencePref == Just False = " \x25C6 EPHEMERAL"
        | otherwise = ""

-- | Status bar: absolute last row, full width, inverted colors.
renderStatusBar :: Layout -> AppState -> String -> Int -> IO ()
renderStatusBar lay st status nSessions = do
    let totalW = lCols lay
        grid = mkRenderGrid lay
    goto (gStatusRow grid) 1; setFg 30; csi "47m"
    connMode <- readIORef (cfgConnectionMode (asConfig st))
    persistencePref <- readIORef (cfgPersistencePreference (asConfig st))
    let connTag = statusBarConnTag connMode persistencePref nSessions
        leftInfo = if null status then " Ready" else " " ++ status
        gap = max 1 (totalW - displayWidth leftInfo - displayWidth connTag - 1)
    putStr (padR totalW (leftInfo ++ replicate gap ' ' ++ connTag ++ " "))
    resetSGR

render :: AppState -> IO ()
render st = do
    (rawRows, rawCols) <- readIORef (asTermSize st)
    lastToken <- readIORef (asLastRenderToken st)
    if not (sizeValid rawRows rawCols) then do
        let warnToken = "warn|" ++ show rawRows ++ "|" ++ show rawCols
        when (lastToken /= Just warnToken) $ do
            writeIORef (asLastRenderToken st) (Just warnToken)
            renderSizeWarning rawRows rawCols
    else do
        let (rows, cols) = clampSize rawRows rawCols
            lay = calcLayout rows cols
            grid = mkRenderGrid lay
        writeIORef (asLayout st) lay
        focus <- readIORef (asFocus st); sel <- readIORef (asSelected st)
        sessions <- readIORef (cfgSessions (asConfig st))
        buf <- readIORef (asInputBuf st); scroll <- readIORef (asChatScroll st)
        status <- readIORef (asStatusMsg st)
        cScroll <- readIORef (asContactScroll st)
        mOpen <- readIORef (asMenuOpen st)
        menuIdx <- readIORef (asMenuIndex st)
        dlg <- readIORef (asDialogMode st)
        dlgBuf <- readIORef (asDialogBuf st)
        browsePage <- readIORef (asBrowsePage st)
        browseFilter <- readIORef (asBrowseFilter st)
        mIk <- readIORef (cfgIdentity (asConfig st))
        regenCb <- readIORef (asRegenCheckbox st)
        dlgScroll <- readIORef (asDialogScroll st)
        dialogToken <- case dlg of
            Just DlgHelp -> pure helpOverlayLines
            Just DlgAbout -> pure aboutOverlayLines
            Just DlgSettings -> settingsOverlayLines st
            Just DlgVerify -> verifyOverlayLines st
            Just DlgNewConn -> pure newConnOverlayLines
            Just DlgKeys -> keysOverlayLines st
            Just DlgBrowse -> browseOverlayLines st
            Just DlgRegenKey -> regenKeyOverlayLines regenCb
            Just DlgExportWarn -> pure (exportWarnOverlayLines regenCb)
            Just DlgExportKeys -> exportKeysOverlayLines st
            Just (DlgPrompt title _) -> pure (promptOverlayLines title dlgBuf)
            Nothing -> pure []
        let entries = Map.toList sessions
            chatH' = lChatH lay
            contactsH = max 1 (chatH' - lIdentityH lay)
            nSessions = length entries
            selMax = max 0 (nSessions - 1)
            sel' = clampInt 0 selMax sel
            contactMaxOff = max 0 (nSessions - contactsH)
            cScroll' = clampInt 0 contactMaxOff cScroll
            selSi = if sel' >= 0 && sel' < nSessions
                    then Map.lookup (fst (entries !! sel')) sessions else Nothing
            identityToken = maybe "nokey" (\ik -> show (ikX25519Public ik)) mIk
        chatTotal <- case selSi of
            Just si -> length <$> readIORef (siHistory si)
            Nothing -> pure 0
        let chatMaxScroll = max 0 (chatTotal - chatH')
            scroll' = clampInt 0 chatMaxScroll scroll
        when (sel' /= sel) $
            writeIORef (asSelected st) sel'
        when (cScroll' /= cScroll) $
            writeIORef (asContactScroll st) cScroll'
        when (scroll' /= scroll) $
            writeIORef (asChatScroll st) scroll'
        sessionTokens <- mapM sessionRenderToken entries
        selectedToken <- case selSi of
            Nothing -> pure "none"
            Just si -> selectedSessionRenderToken si
        let leftToken =
                show (rows, cols, focus, sel', cScroll', maybe False (const True) mIk, regenCb, sessionTokens)
            rightToken =
                show (rows, cols, sel', scroll', focus, buf, selectedToken)
            chromeToken =
                show (rows, cols, mOpen, menuIdx, dlg, browsePage, browseFilter, dlgBuf, status, dialogToken, dlgScroll)
            token =
                "ok|L:" ++ show (quickHash leftToken) ++ "|R:" ++ show (quickHash rightToken)
                ++ "|C:" ++ show (quickHash chromeToken) ++ "|I:" ++ show (quickHash identityToken)
        when (lastToken /= Just token) $ do
            writeIORef (asLastRenderToken st) (Just token)
            let (oldL, oldR, oldC, oldI) = parseRenderToken lastToken
                newL = quickHash leftToken
                newR = quickHash rightToken
                newC = quickHash chromeToken
                newI = quickHash identityToken
                leftChanged = oldL /= Just newL || oldI /= Just newI
                rightChanged = oldR /= Just newR
                chromeChanged = oldC /= Just newC
            if chromeChanged then do
                goto 1 1
                renderMenuBar lay mOpen
                forM_ [0..chatH'-1] $ \row ->
                    renderPaneRow lay grid entries selSi sel' focus scroll' cScroll' row
                when (lIdentityH lay > 0) $
                    renderIdentityPanel lay grid st mIk
                renderMidBorder lay grid
                renderInputRow lay grid focus buf
                renderBottomBorder lay grid
                renderStatusBar lay st status nSessions
            else do
                when leftChanged $ do
                    forM_ [0..chatH'-1] $ \row ->
                        renderPaneRowLeft lay grid entries sel' focus cScroll' row
                    when (lIdentityH lay > 0) $
                        renderIdentityPanel lay grid st mIk
                when rightChanged $ do
                    forM_ [0..chatH'-1] $ \row ->
                        renderPaneRowRight lay grid selSi scroll' row
                    renderInputRow lay grid focus buf
            -- Dropdown menu (rendered on top of content)
            case mOpen of
                Just tab -> renderDropdown lay tab menuIdx
                Nothing  -> pure ()
            -- Overlays
            case dlg of
                Just DlgHelp     -> renderHelpOverlay lay dlgScroll
                Just DlgAbout    -> renderAboutOverlay lay dlgScroll
                Just DlgKeys     -> renderKeysOverlay lay st dlgScroll
                Just DlgSettings -> renderSettingsOverlay lay st dlgScroll
                Just DlgNewConn  -> renderNewConnOverlay lay dlgScroll
                Just DlgVerify   -> renderVerifyOverlay lay st dlgScroll
                Just DlgBrowse   -> renderBrowseOverlay lay st dlgScroll
                Just DlgRegenKey -> renderRegenKeyOverlay lay st dlgScroll
                Just DlgExportWarn -> showOverlay lay "Export Warning" (exportWarnOverlayLines regenCb)
                Just DlgExportKeys -> do
                    lns <- exportKeysOverlayLines st
                    showOverlay lay "Export Identity Keys" lns
                Just (DlgPrompt title _) ->
                    renderPromptOverlay lay title dlgBuf dlgScroll
                Nothing -> pure ()
            hFlush stdout

sessionRenderToken :: (SessionId, SessionInfo) -> IO String
sessionRenderToken (sid, si) = do
    tag <- statusTag <$> readIORef (siStatus si)
    hist <- readIORef (siHistory si)
    let msgCount = length hist
        newest = if null hist then "" else head hist
    pure (show sid ++ "|" ++ siPeerName si ++ "|" ++ tag ++ "|" ++ show msgCount ++ "|" ++ newest)

selectedSessionRenderToken :: SessionInfo -> IO String
selectedSessionRenderToken si = do
    hist <- readIORef (siHistory si)
    let msgCount = length hist
        newest = if null hist then "" else head hist
    pure (show msgCount ++ "|" ++ newest)

quickHash :: String -> Int
quickHash = foldl (\acc ch -> (acc * 33 + fromEnum ch) `mod` 2147483647) 5381

parseRenderToken :: Maybe String -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
parseRenderToken Nothing = (Nothing, Nothing, Nothing, Nothing)
parseRenderToken (Just tok) =
    let parts = splitBy '|' tok
    in (findPref "L:" parts, findPref "R:" parts, findPref "C:" parts, findPref "I:" parts)

findPref :: String -> [String] -> Maybe Int
findPref _ [] = Nothing
findPref pfx (x:xs) = case stripPrefix pfx x of
    Just v -> readMaybeInt v
    Nothing -> findPref pfx xs

readMaybeInt :: String -> Maybe Int
readMaybeInt s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delim xs =
    let (a, b) = break (== delim) xs
    in case b of
        [] -> [a]
        (_:rest) -> a : splitBy delim rest

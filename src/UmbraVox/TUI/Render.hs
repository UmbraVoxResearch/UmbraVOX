-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Render
    ( render
    , clearScreen, goto, showCursor, hideCursor, withRawMode
    , getTermSize, clampSize, sizeValid, calcLayout
    , showOverlay, padR, isPfx
    , statusBarConnTag
    ) where

import Control.Monad (forM_, when)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.IORef (readIORef, writeIORef)
import Data.List (intercalate, stripPrefix)
import qualified Data.Map.Strict as Map
import System.IO (hFlush, stdout)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Terminal (csi, goto, clearScreen, hideCursor, showCursor,
    setFg, resetSGR, bold, padR, getTermSize, withRawMode)
import UmbraVox.TUI.Layout (clampSize, sizeValid, calcLayout)
import qualified UmbraVox.TUI.Layout as Layout
import UmbraVox.TUI.Text (displayWidth)
import UmbraVox.TUI.InputBuffer
    ( WrappedInputLine(..)
    , computeInputBufferLayout, visibleInputLines, visibleInputStart, cursorScreenOffset
    , clampInputScroll, iblShowScrollbar, iblContentW, iblMaxScroll, iblWrapped )
import UmbraVox.TUI.RichText
    ( computeRichInputLayout, richVisibleLines, richCursorScreenOffset
    , richShowScrollbar, richContentW, richMaxScroll
    , renderRichCharsPaddedSel, renderMarkdownLinePadded
    )
import UmbraVox.TUI.Dialog (showOverlay, showWarningOverlay, renderHelpOverlay, renderAboutOverlay, renderKeysOverlay,
    renderSettingsOverlay, renderNewConnOverlay, renderVerifyOverlay,
    renderBrowseOverlay, renderPromptOverlay, renderRegenKeyOverlay, renderDropdown,
    helpOverlayLines, aboutOverlayLines, newConnOverlayLines, keysOverlayLines,
    verifyOverlayLines, promptOverlayLines, settingsOverlayLines, browseOverlayLines,
    regenKeyOverlayLines, exportWarnOverlayLines, exportKeysOverlayLines,
    insertLinkOverlayLines, renderInsertLinkOverlay,
    emojiPickerOverlayLines, renderEmojiPickerOverlay,
    bridgeSelectOverlayLines, renderBridgeSelectOverlay,
    bridgeAuthOverlayLines, renderBridgeAuthOverlay,
    bridgeContactsOverlayLines, renderBridgeContactsOverlay)
import UmbraVox.Protocol.QRCode (generateSafetyNumber, renderFingerprint, generateQRCode, renderQRCode)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Version (versionFull)
import qualified Data.Set as Set
import qualified UmbraVox.Plugin.Types

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
    goto 1 1; setFg 35; putStr "\x256D"
    resetSGR; setFg 35; putStr (replicate fillW '\x2500')
    putStr "\x2502"
    mapM_ (\tab -> do
        let label = menuTabLabel tab
        if mOpen == Just tab then do
            resetSGR; bold; setFg 37; csi "45m"  -- white on cyan
            renderUnderlinedLabel label (menuTabUnderlineIndex tab)
            resetSGR; setFg 35
        else do
            resetSGR; setFg 35
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

-- | Prefix for bridge sessions in the contact list and chat header.
sessionPrefix :: SessionCrypto -> String
sessionPrefix (RatchetCrypto _) = ""
sessionPrefix (BridgeCrypto _)  = "[B] "

-- | Longer label for bridge sessions shown in the chat separator.
sessionCryptoLabel :: SessionCrypto -> String
sessionCryptoLabel (RatchetCrypto _) = ""
sessionCryptoLabel (BridgeCrypto _)  = "[External Encryption]"

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
        let pfx = sessionPrefix (siCrypto si)
            mk = if idx == sel then " \x25B8 " else "   "
            sbW = if showScrollbar then 1 else 0
            nameW = max 0 (lw - 2 - displayWidth mk - displayWidth tag - sbW)
            cell = mk ++ padR nameW (pfx ++ siPeerName si) ++ tag
        when (idx == sel) $ if focus == ContactPane then bold >> setFg 34 else bold
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
--   3. Safety number rows (up to 5 groups per row, auto-fitted to pane width)
--   4. Two-column fingerprints: " X25519:" | " Ed25519:", then rows side by side
--   5. In ephemeral mode: warning "⚠ Ephemeral — verify fingerprint out-of-band"
--   No blank lines between sections.
identityPanelLines :: Layout -> Maybe IdentityKey -> Pane -> Bool -> [String]
identityPanelLines lay mIk _focus isEphemeral =
    let innerW   = lLeftW lay - 2
        qrStdText = "Standard: X3DH safety number"
        available = lIdentityH lay - 1
        ephemeralWarn = padR innerW "\x26A0 Ephemeral \x2014 verify fingerprint out-of-band"
        noIdLines = padR innerW "No identity yet."
                  : (if isEphemeral
                        then [ephemeralWarn] ++ replicate (max 0 (available - 2)) (replicate innerW ' ')
                        else replicate (available - 1) (replicate innerW ' '))
        finalize body =
            let bodyWithWarn =
                    if isEphemeral
                        then body ++ [ephemeralWarn]
                        else body
            -- No padding — return exact content. Freed rows go to contacts.
            in take available bodyWithWarn
    in case mIk of
        Nothing -> noIdLines
        Just ik ->
            let x25519Lns = renderFingerprint (ikX25519Public ik)
                ed25519Lns = renderFingerprint (ikEd25519Public ik)
                -- One combined safety number from both keys
                safetyNum  = generateSafetyNumber (ikX25519Public ik) (ikEd25519Public ik)
                qrLns      = renderQRCode (generateQRCode safetyNum)
                body       = identityBody innerW x25519Lns ed25519Lns qrLns qrStdText safetyNum
            in finalize body

-- | Content body for the identity panel (Regenerate button not included).
-- Layout: centered QR code, standard label, safety-number rows,
-- then two-column fingerprint block (X25519 left | Ed25519 right). No blank lines.
identityBody :: Int -> [String] -> [String] -> [String] -> String -> String -> [String]
identityBody innerW x25519Lns ed25519Lns qrLns qrStdText safetyNum =
    map centerLine qrLns
    ++ [centerUnderQr innerW qrLns qrStdText]
    ++ map (centerText innerW) (formatSafetyNumberForWidth innerW safetyNum)
    ++ fpTwoColLines innerW x25519Lns ed25519Lns
  where
    centerLine s =
        centerText innerW s

formatSafetyNumberForWidth :: Int -> String -> [String]
formatSafetyNumberForWidth innerW safetyNum =
    map (intercalate " ") (groupN perRow groups5)
  where
    groups5 = groupN 5 safetyNum
    -- width of k groups is 6k-1 (5 digits + 1 space separators)
    perRow = max 1 (min 5 ((innerW + 1) `div` 6))

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

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs
    | n <= 0 = [xs]
    | otherwise =
        let (a, b) = splitAt n xs
        in a : groupN n b

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
              -> Int -> Bool -> Pane -> Int -> Int -> Int -> IO ()
renderPaneRow lay grid entries selSi sel richEnabled focus scroll' cScroll row = do
    renderPaneRowLeft lay grid entries sel focus cScroll row
    renderPaneRowRight lay grid selSi richEnabled focus scroll' row

renderPaneRowLeft :: Layout -> RenderGrid -> [(SessionId, SessionInfo)] -> Int -> Pane -> Int -> Int -> IO ()
renderPaneRowLeft lay grid entries sel focus cScroll row = do
    let contentRow = gContentTop grid + row
        chatH' = lChatH lay
        contactsH = chatH' - lIdentityH lay
        innerW = lLeftW lay - 2
        -- Contacts toolbar: separator line + 1-2 button rows
        allFour :: String
        allFour = "[ New ] [ Rename ] [ Browse ] [ Verify ]"
        btnRows = if innerW >= length allFour then 1 else 2
        toolbarRows = 1 + btnRows  -- 1 separator + button rows
        toolbarStart = max 0 (contactsH - toolbarRows)
        isContactRow = row < toolbarStart
        isToolbarRow = row >= toolbarStart && row < contactsH
    goto contentRow 1
    -- Left border
    if (isContactRow || isToolbarRow) && focus == ContactPane
        then bold >> setFg 34 >> putStr "\x2502" >> resetSGR
        else setFg 35 >> putStr "\x2502" >> resetSGR
    -- Content: contacts, then toolbar (with separator), then blank/identity rows
    if isContactRow
        then renderContactCell lay entries sel focus cScroll row
        else if isToolbarRow
        then renderContactsToolbarRow innerW (row - toolbarStart)
        else putStr (replicate innerW ' ')
    -- Right border
    if (isContactRow || isToolbarRow) && focus == ContactPane
        then bold >> setFg 34 >> putStr "\x2502" >> resetSGR
        else setFg 35 >> putStr "\x2502" >> resetSGR

renderPaneRowRight :: Layout -> RenderGrid -> Maybe SessionInfo -> Bool -> Pane -> Int -> Int -> IO ()
renderPaneRowRight lay grid selSi richEnabled focus scroll' row = do
    let rw = lRightW lay
        contentRow = gContentTop grid + row
        chatH' = lChatH lay
    goto contentRow (lLeftW lay + 1)
    -- Chat message
    (msg, total) <- case selSi of
        Nothing -> pure ("", 0)
        Just si -> do
            hist <- readIORef (siHistory si)
            let msgs = reverse hist
                totalMsgs = length msgs
                start = max 0 (min (totalMsgs - 1) (totalMsgs - chatH' - scroll'))
                idx = start + row
                line = if idx >= 0 && idx < totalMsgs then msgs !! idx else ""
            pure (line, totalMsgs)
    let showScrollbar = total > chatH'
        sbW = if showScrollbar then 1 else 0
        msgW = max 0 (rw - 1 - sbW)
        scrollbarChar =
            let trackH = max 1 chatH'
                thumbSz = max 1 (trackH * chatH' `div` max 1 total)
                maxOff = max 0 (total - chatH')
                offFromTop = max 0 (maxOff - scroll')
                thumbPos = if maxOff == 0 then 0 else offFromTop * (trackH - thumbSz) `div` max 1 maxOff
            in if row >= thumbPos && row < thumbPos + thumbSz then '\x2588' else '\x2502'
    if richEnabled
        then renderMarkdownLinePadded msgW True msg
        else putStr (padR msgW msg)
    when showScrollbar $ csi "2m" >> setFg 37 >> putChar scrollbarChar >> resetSGR
    -- Right border
    if focus == ChatPane
        then bold >> setFg 34 >> putStr "\x2502" >> resetSGR
        else setFg 35 >> putStr "\x2502" >> resetSGR

-- | Render the identity panel separator and content rows, left side only.
-- Called after renderPaneRow has already drawn all content rows (the right
-- pane chat messages in those rows are preserved).
renderIdentityPanel :: Layout -> RenderGrid -> AppState -> Maybe IdentityKey -> IO ()
renderIdentityPanel lay grid st mIk = do
    focus <- readIORef (asFocus st)
    isEphemeral <- readIORef (cfgEphemeral (asConfig st))
    let lw = lLeftW lay
        chatH' = lChatH lay
        contactsH = chatH' - lIdentityH lay
        sepRow = gContentTop grid + contactsH
        innerW = lw - 2
        panelLines = identityPanelLines lay mIk focus isEphemeral
        active = focus == IdentityPane
        withPanelFrame action =
            if active
                then bold >> setFg 34 >> action >> resetSGR
                else setFg 35 >> action >> resetSGR
    -- Separator between contacts and identity panel (left side only)
    goto sepRow 1
    withPanelFrame $
        putStr $ "\x251C" ++ replicate innerW '\x2500' ++ "\x2524"
    -- Identity panel content rows (left side only)
    forM_ (zip [0..] panelLines) $ \(i, line) -> do
        let panelRow = sepRow + 1 + i
        goto panelRow 1
        withPanelFrame (putStr "\x2502")
        when active (bold >> setFg 34)
        putStr line
        resetSGR
        goto panelRow lw
        withPanelFrame (putStr "\x2502")

-- | Mid-border separator between content and input row.
-- When the selected session uses bridge crypto, the right-pane portion
-- of this separator shows an "[External Encryption]" warning label.
renderMidBorder :: Layout -> RenderGrid -> Maybe SessionInfo -> IO ()
renderMidBorder lay grid selSi = do
    let lw = lLeftW lay; rw = lRightW lay
        borderRow = gSepRow grid
        label = maybe "" (sessionCryptoLabel . siCrypto) selSi
        labelW = displayWidth label
        dashW = max 0 (rw - 1 - labelW)
    goto borderRow 1; setFg 35
    putStr $ "\x2502" ++ replicate (lw - 2) ' ' ++ "\x2502"
    if null label
        then putStr $ replicate (rw - 1) '\x2500' ++ "\x2524"
        else do
            putStr $ replicate dashW '\x2500'
            setFg 33; bold; putStr label; resetSGR; setFg 35
            putStr "\x2524"
    resetSGR

-- | Input row: blank left pane, input field on right.
-- Row layout within inputAreaRows (0-based):
--   0            : toolbar row  │ [ Rich* ] [ Plain ] ... │
--   1            : text-entry box top border  ╭─...─╮
--   2..(rows-2)  : text content rows
--   rows-1       : text-entry box bottom border  ╰─...─╯
renderInputRow :: Layout -> RenderGrid -> Pane -> Bool -> String -> Int -> Int -> Maybe Int -> IO ()
renderInputRow lay grid focus richEnabled buf inputCursor inputScroll mSelStart = do
    let lw = lLeftW lay; rw = lRightW lay
        inputTop = gInputTop grid
        inputRows = Layout.inputAreaRows
        toolbarRow = 0
        boxTopRow = 1
        rightEntryStart = 2
        rightEntryRows = max 1 (inputRows - 3)
        bodyW = max 0 (rw - 1)
        -- R1.8.1 safety note: inputLayout and richLayout both receive the
        -- same bodyW, but may compute different contentW values because
        -- their scrollbar decisions are independent.  This is correct:
        -- contentW is only used in the plain-text branch, richW only in
        -- the rich-text branch — they are never mixed.
        inputLayout = computeInputBufferLayout bodyW rightEntryRows buf
        richLayout = computeRichInputLayout bodyW rightEntryRows buf
        inputOff = clampInputScroll inputLayout inputScroll
        richOff = min (richMaxScroll richLayout) (max 0 inputScroll)
        visibleInput = visibleInputLines inputLayout inputOff
        visibleRich = richVisibleLines richLayout richOff
        showScrollbar = iblShowScrollbar inputLayout
        showRichScrollbar = richShowScrollbar richLayout
        contentW = iblContentW inputLayout
        richW = richContentW richLayout
        totalLines = rightEntryRows + iblMaxScroll inputLayout
        richTotalLines = rightEntryRows + richMaxScroll richLayout
        mCursor = cursorScreenOffset inputLayout inputOff inputCursor
        richCursor = richCursorScreenOffset richLayout richOff inputCursor
        inputScrollbarChar lineIx =
            let trackH = max 1 rightEntryRows
                thumbSz = max 1 (trackH * rightEntryRows `div` max 1 totalLines)
                offFromTop = max 0 (iblMaxScroll inputLayout - inputOff)
                thumbPos = if iblMaxScroll inputLayout == 0 then 0 else offFromTop * (trackH - thumbSz) `div` max 1 (iblMaxScroll inputLayout)
            in if lineIx >= thumbPos && lineIx < thumbPos + thumbSz then '\x2588' else '\x2502'
        richScrollbarChar lineIx =
            let trackH = max 1 rightEntryRows
                thumbSz = max 1 (trackH * rightEntryRows `div` max 1 richTotalLines)
                offFromTop = max 0 (richMaxScroll richLayout - richOff)
                thumbPos = if richMaxScroll richLayout == 0 then 0 else offFromTop * (trackH - thumbSz) `div` max 1 (richMaxScroll richLayout)
            in if lineIx >= thumbPos && lineIx < thumbPos + thumbSz then '\x2588' else '\x2502'
        -- Selection range in raw buffer indices [selLo, selHi)
        (mSelLo, mSelHi) = case mSelStart of
            Nothing -> (Nothing, Nothing)
            Just selStart ->
                let lo = min selStart inputCursor
                    hi = max selStart inputCursor
                in if lo == hi then (Nothing, Nothing) else (Just lo, Just hi)
        -- Visible wrapped lines for plain-text selection highlighting
        visibleWrappedLines =
            let start = visibleInputStart inputLayout inputOff
            in take rightEntryRows (drop start (iblWrapped inputLayout))
    forM_ [0..inputRows-1] $ \i -> do
        let inputRow = inputTop + i
        goto inputRow 1
        -- Left pane: blank rows (toolbar is now above the identity panel)
        setFg 35; putStr "\x2502"; resetSGR
        putStr (replicate (lw - 2) ' ')
        setFg 35; putStr "\x2502"; resetSGR
        -- Right pane
        if i == toolbarRow
            then do
                if richEnabled
                    then renderEditorToolbar bodyW richEnabled
                    else putStr (replicate bodyW ' ')
                setFg 35 >> putStr "\x2502" >> resetSGR
        else if i == boxTopRow
            then do
                setFg 35
                if rw >= 2
                    then putStr $ "\x256D" ++ replicate (rw - 2) '\x2500' ++ "\x256E"
                    else putStr (replicate rw '\x2500')
                resetSGR
        else if i >= rightEntryStart && i < rightEntryStart + rightEntryRows
            then do
                let lineIx = i - rightEntryStart
                if richEnabled
                    then do
                        let chars = if lineIx < length visibleRich then visibleRich !! lineIx else []
                            cursorCol = case richCursor of
                                Just (cursorRow, cursorCol') | focus == ChatPane && cursorRow == lineIx -> Just cursorCol'
                                _ -> Nothing
                        when (focus == ChatPane) (bold >> setFg 34)
                        renderRichCharsPaddedSel richW cursorCol mSelLo mSelHi chars
                        resetSGR
                        when showRichScrollbar $ csi "2m" >> setFg 37 >> putChar (richScrollbarChar lineIx) >> resetSGR
                    else if focus == ChatPane
                        then do
                            bold; setFg 34
                            let line = if lineIx < length visibleInput then visibleInput !! lineIx else ""
                                wline = if lineIx < length visibleWrappedLines then Just (visibleWrappedLines !! lineIx) else Nothing
                                lineStart = maybe 0 wilStart wline
                            case mCursor of
                                Just (cursorRow, cursorCol)
                                    | cursorRow == lineIx ->
                                        renderPlainLineSel contentW line cursorCol lineStart mSelLo mSelHi
                                _ -> renderPlainLineSel contentW line (-1) lineStart mSelLo mSelHi
                            resetSGR
                            when showScrollbar $ csi "2m" >> setFg 37 >> putChar (inputScrollbarChar lineIx) >> resetSGR
                        else do
                            let line = if lineIx < length visibleInput then visibleInput !! lineIx else ""
                            putStr (padR contentW line)
                            when showScrollbar $ csi "2m" >> setFg 37 >> putChar (inputScrollbarChar lineIx) >> resetSGR
                setFg 35 >> putStr "\x2502" >> resetSGR
            else if i == rightEntryStart + rightEntryRows
                then do
                    -- Bottom border with [Send] [Clear] buttons on the right
                    setFg 35
                    let btns = " [Send] [Clear] "
                        btnsW = length btns
                        dashW = max 0 (rw - 2 - btnsW)
                    if rw >= 2
                        then putStr $ "\x2570" ++ replicate dashW '\x2500' ++ btns ++ "\x256F"
                        else putStr (replicate rw '\x2500')
                    resetSGR
            else putStr (replicate bodyW ' ')

-- | Render the editor toolbar content (without the trailing border character).
-- 'bodyW' is the available width for toolbar text, excluding the right border.
-- Rich/Plain toggles are in Prefs; only formatting buttons are shown here, centered.
-- | Render the contacts toolbar: [ New ] [ Rename ] [ Browse ] [ Verify ] centered in the given width.
-- If width is too narrow, show abbreviated buttons.
-- | Render one row of the contacts toolbar.  Row 0 is a thin separator line.
-- Button rows follow: when wide, all four on row 1.  When narrow, stacked
-- into rows 1 and 2.  Full button names are always preferred.
renderContactsToolbarRow :: Int -> Int -> IO ()
renderContactsToolbarRow w row
    | row == 0 = do
        -- Thin separator line (─ across the pane)
        setFg 35; putStr (replicate w '\x2500'); resetSGR
    | otherwise = do
        let btnRow = row - 1  -- 0-based button row index
            allFour = "[ New ] [ Rename ] [ Browse ] [ Verify ]"
            topTwo  = "[ New ] [ Rename ]"
            botTwo  = "[ Browse ] [ Verify ]"
        setFg 35
        if w >= length allFour
            then if btnRow == 0
                then putStr (padR w (center w allFour))
                else putStr (replicate w ' ')
            else if btnRow == 0
                then putStr (padR w (center w topTwo))
                else if btnRow == 1
                then putStr (padR w (center w botTwo))
                else putStr (replicate w ' ')
        resetSGR
  where
    center totalW s =
        let padLeft = max 0 ((totalW - length s) `div` 2)
        in replicate padLeft ' ' ++ s

renderEditorToolbar :: Int -> Bool -> IO ()
renderEditorToolbar bodyW _richEnabled = do
    let buttons = ["[ Bold ]", "[ Italic ]", "[ Color ]", "[ Link ]", "[ Emoji ]"]
        toolbar = unwords buttons
        toolbarW = length toolbar
        padLeft = max 0 ((bodyW - toolbarW) `div` 2)
        centered = replicate padLeft ' ' ++ toolbar
    setFg 35
    putStr (padR bodyW centered)
    resetSGR

-- | Render a plain-text input line with optional cursor block and optional
-- selection highlight.  cursorCol < 0 means no cursor.  lineStart is the
-- buffer offset of the first character in this visible line.  mSelLo/mSelHi
-- are raw buffer indices [lo, hi).
--
-- R1.8.2 safety note: zero-width characters (combining marks, diacritics)
-- produce charW == 0, so 'col' doesn't advance for them.  This is correct
-- because cursorCol is computed by cursorTextDisplayCol (InputBuffer.hs)
-- which also uses displayWidth and therefore also treats zero-width chars
-- as width 0.  Both sides stay in sync.
renderPlainLineSel :: Int -> String -> Int -> Int -> Maybe Int -> Maybe Int -> IO ()
renderPlainLineSel width line cursorCol lineStart mSelLo mSelHi = do
    finalCol <- go 0 0 line
    -- Draw cursor at end-of-text if cursor is there
    let endCol = finalCol
    when (cursorCol >= 0 && cursorCol == endCol && endCol < width) $ do
        csi "7m"; putChar '\x2588'; resetSGR; bold; setFg 34
    let padStart = endCol + (if cursorCol >= 0 && cursorCol == endCol && endCol < width then 1 else 0)
    putStr (replicate (max 0 (width - padStart)) ' ')
  where
    inSel bufIdx = case (mSelLo, mSelHi) of
        (Just lo, Just hi) -> bufIdx >= lo && bufIdx < hi
        _ -> False
    go col _ [] = pure col
    go col bufOff (ch:rest)
        | col >= width = pure col
        | otherwise = do
            let charW = displayWidth [ch]
                bufIdx = lineStart + bufOff
                isCursor = cursorCol >= 0 && col == cursorCol
                isSel = inSel bufIdx
            if isCursor || isSel
                then do csi "7m"; putChar ch; resetSGR; bold; setFg 34
                else putChar ch
            go (col + charW) (bufOff + 1) rest

-- | Bottom border, edge-to-edge
renderBottomBorder :: Layout -> RenderGrid -> IO ()
renderBottomBorder lay grid = do
    let lw = lLeftW lay; rw = lRightW lay
        botRow = gBottomBorderRow grid
    goto botRow 1; setFg 35
    putStr $ "\x2570" ++ replicate (lw - 2) '\x2500' ++ "\x2534"
          ++ replicate (rw - 1) '\x2500' ++ "\x256F"
    resetSGR

-- | Short 4-byte hex fingerprint for ephemeral session identity display.
shortFingerprint :: BS.ByteString -> String
shortFingerprint bs = concatMap hex2 (BS.unpack (BS.take 4 padded))
  where
    padded = bs <> BS.replicate (max 0 (4 - BS.length bs)) 0
    hex2 w = [hexC (w `shiftR` 4), hexC (w .&. 0x0f)]
    hexC n | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
           | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)

statusBarConnTag :: ConnectionMode -> Bool -> Bool -> Bool -> Int -> Int -> String
statusBarConnTag connMode isEphemeral anyPersistPlugin richEnabled nSessions discCount =
    sessionCount ++ modeTag ++ discTag ++ richTag ++ " \x25C6 " ++ versionFull
  where
    sessionCount
        | nSessions > 0 = show nSessions ++ " session" ++ (if nSessions > 1 then "s" else "")
        | otherwise = "No sessions"
    modeTag
        | connMode == Chaste || isEphemeral || not anyPersistPlugin = " \x25C6 EPHEMERAL"
        | otherwise = " \x25C6 PERSISTENT"
    discTag = " \x25C6 D:" ++ show discCount
    richTag
        | richEnabled = " \x25C6 RICH"
        | otherwise   = " \x25C6 PLAIN"

-- | Status bar: absolute last row, full width, inverted colors.
renderStatusBar :: Layout -> AppState -> String -> Bool -> Int -> IO ()
renderStatusBar lay st status richEnabled nSessions = do
    let totalW = lCols lay
        grid = mkRenderGrid lay
    goto (gStatusRow grid) 1; setFg 30; csi "47m"
    connMode <- readIORef (cfgConnectionMode (asConfig st))
    isEphemeral <- readIORef (cfgEphemeral (asConfig st))
    mIdentity <- readIORef (cfgIdentity (asConfig st))
    pluginReg <- readIORef (cfgPluginRegistry (asConfig st))
    discSources <- readIORef (cfgDiscoverySources (asConfig st))
    let anyPersistPlugin = any (\pid -> pluginEnabledReg pid pluginReg)
            ["key-persistence", "message-storage", "ratchet-persistence", "runtime-logging", "full-persistence"]
        discCount = Set.size discSources
        connTag = statusBarConnTag connMode isEphemeral anyPersistPlugin richEnabled nSessions discCount
        -- M17.7.1: show session fingerprint in ephemeral mode when idle
        fpTag = case (isEphemeral, mIdentity) of
            (True, Just ik) -> " [" ++ shortFingerprint (ikX25519Public ik) ++ "]"
            _               -> ""
        leftInfo = if null status then " Ready" ++ fpTag else " " ++ status
        gap = max 1 (totalW - displayWidth leftInfo - displayWidth connTag - 1)
    putStr (padR totalW (leftInfo ++ replicate gap ' ' ++ connTag ++ " "))
    resetSGR

pluginEnabledReg :: String -> UmbraVox.Plugin.Types.PluginRegistry -> Bool
pluginEnabledReg pid reg =
    maybe False UmbraVox.Plugin.Types.pdEnabled (Map.lookup pid reg)

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
        showIdentity <- readIORef (asShowIdentity st)
        let lay0 = calcLayout rows cols
            lay = if showIdentity then lay0 else lay0 { lIdentityH = 0 }
            grid = mkRenderGrid lay
        writeIORef (asLayout st) lay
        focus <- readIORef (asFocus st); sel <- readIORef (asSelected st)
        sessions <- readIORef (cfgSessions (asConfig st))
        buf <- readIORef (asInputBuf st); inputCursor <- readIORef (asInputCursor st); scroll <- readIORef (asChatScroll st)
        richEnabled <- readIORef (asRichText st)
        inputScroll <- readIORef (asInputScroll st)
        mSelStart <- readIORef (asSelectionStart st)
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
            Just DlgInsertLink -> do
                txt   <- readIORef (asLinkText st)
                url   <- readIORef (asLinkUrl st)
                focus <- readIORef (asLinkFocus st)
                pure (insertLinkOverlayLines txt url focus)
            Just DlgEmojiPicker -> do
                q    <- readIORef (asEmojiSearch st)
                cat  <- readIORef (asEmojiCategory st)
                page <- readIORef (asEmojiPage st)
                pure (emojiPickerOverlayLines q cat page)
            Just DlgBridgeSelect -> pure bridgeSelectOverlayLines
            Just DlgBridgeAuth -> pure bridgeAuthOverlayLines
            Just DlgBridgeContacts -> pure bridgeContactsOverlayLines
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
                show (rows, cols, focus, sel', cScroll', maybe False (const True) mIk, regenCb, sessionTokens, showIdentity)
            rightToken =
                show (rows, cols, sel', scroll', inputScroll, inputCursor, focus, richEnabled, buf, selectedToken, mSelStart)
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
                    renderPaneRow lay grid entries selSi sel' richEnabled focus scroll' cScroll' row
                when (lIdentityH lay > 0) $
                    renderIdentityPanel lay grid st mIk
                renderMidBorder lay grid selSi
                renderInputRow lay grid focus richEnabled buf inputCursor inputScroll mSelStart
                renderBottomBorder lay grid
                renderStatusBar lay st status richEnabled nSessions
            else do
                when leftChanged $ do
                    forM_ [0..chatH'-1] $ \row ->
                        renderPaneRowLeft lay grid entries sel' focus cScroll' row
                    when (lIdentityH lay > 0) $
                        renderIdentityPanel lay grid st mIk
                when rightChanged $ do
                    forM_ [0..chatH'-1] $ \row ->
                        renderPaneRowRight lay grid selSi richEnabled focus scroll' row
                    renderInputRow lay grid focus richEnabled buf inputCursor inputScroll mSelStart
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
                Just DlgExportWarn -> showWarningOverlay lay "Export Warning" (exportWarnOverlayLines regenCb)
                Just DlgExportKeys -> do
                    lns <- exportKeysOverlayLines st
                    showOverlay lay "Export Identity Keys" lns
                Just DlgInsertLink   -> renderInsertLinkOverlay lay st dlgScroll
                Just DlgEmojiPicker  -> renderEmojiPickerOverlay lay st dlgScroll
                Just DlgBridgeSelect   -> renderBridgeSelectOverlay lay dlgScroll
                Just DlgBridgeAuth     -> renderBridgeAuthOverlay lay dlgScroll
                Just DlgBridgeContacts -> renderBridgeContactsOverlay lay dlgScroll
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
        cryptoTag = sessionPrefix (siCrypto si)
    pure (show sid ++ "|" ++ cryptoTag ++ siPeerName si ++ "|" ++ tag ++ "|" ++ show msgCount ++ "|" ++ newest)

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

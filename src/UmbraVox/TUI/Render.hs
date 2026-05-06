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
import qualified Data.Map.Strict as Map
import System.IO (hFlush, stdout)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Terminal (csi, goto, clearScreen, hideCursor, showCursor,
    setFg, resetSGR, bold, padR, getTermSize, withRawMode)
import UmbraVox.TUI.Layout (clampSize, sizeValid, calcLayout)
import UmbraVox.TUI.Text (displayWidth, trimToWidth)
import UmbraVox.TUI.Dialog (showOverlay, renderHelpOverlay, renderAboutOverlay, renderKeysOverlay,
    renderSettingsOverlay, renderNewConnOverlay, renderVerifyOverlay,
    renderBrowseOverlay, renderPromptOverlay, renderDropdown,
    helpOverlayLines, aboutOverlayLines, newConnOverlayLines, keysOverlayLines,
    verifyOverlayLines, promptOverlayLines, settingsOverlayLines, browseOverlayLines)
import UmbraVox.Version (versionFull)

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
        idx = row + cScroll
    if idx >= 0 && idx < length entries then do
        let (_, si) = entries !! idx
        tag <- statusTag <$> readIORef (siStatus si)
        let mk = if idx == sel then " \x25B8 " else "   "
            nameW = max 0 (lw - 2 - displayWidth mk - displayWidth tag)
            cell = mk ++ padR nameW (siPeerName si) ++ tag
        when (idx == sel) $ if focus == ContactPane then bold >> setFg 32 else bold
        putStr cell; resetSGR
    else putStr (replicate (lw - 2) ' ')

-- | Render one content row: left contact + divider + right chat
renderPaneRow :: Layout -> [(SessionId, SessionInfo)] -> Maybe SessionInfo
              -> Int -> Pane -> Int -> Int -> Int -> IO ()
renderPaneRow lay entries selSi sel focus scroll' cScroll row = do
    let rw = lRightW lay
        contentRow = row + 2  -- row 1 = menu bar, so content starts at row 2
        chatH' = lChatH lay
    goto contentRow 1
    -- Left border
    setFg 36; putStr "\x2502"; resetSGR
    -- Contact cell
    renderContactCell lay entries sel focus cScroll row
    -- Divider
    setFg 36; putStr "\x2502"; resetSGR
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

-- | Mid-border separator between content and input row
renderMidBorder :: Layout -> Int -> IO ()
renderMidBorder lay chatH' = do
    let lw = lLeftW lay; rw = lRightW lay
        borderRow = chatH' + 2  -- after all content rows
    goto borderRow 1; setFg 36
    putStr $ "\x251C" ++ replicate (lw - 2) '\x2500' ++ "\x253C"
          ++ replicate (rw - 1) '\x2500' ++ "\x2524"
    resetSGR

-- | Input row: blank left pane, input field on right
renderInputRow :: Layout -> Pane -> String -> Int -> IO ()
renderInputRow lay focus buf chatH' = do
    let lw = lLeftW lay; rw = lRightW lay
        inputRow = chatH' + 3
        prefix = " \x25B8 "
    goto inputRow 1; setFg 36; putStr "\x2502"; resetSGR
    putStr (replicate (lw - 2) ' ')
    setFg 36; putStr "\x2502"; resetSGR
    -- Right pane: input field
    if focus == ChatPane then do
        bold; setFg 32
        let bodyW = max 0 (rw - 1 - displayWidth prefix - 1)
        putStr (padR (rw - 1) (prefix ++ trimToWidth bodyW buf ++ "\x2588"))
        resetSGR
    else do
        let bodyW = max 0 (rw - 1 - displayWidth prefix)
        putStr (padR (rw - 1) (prefix ++ trimToWidth bodyW buf))
    setFg 36; putStr "\x2502"; resetSGR

-- | Bottom border, edge-to-edge
renderBottomBorder :: Layout -> Int -> IO ()
renderBottomBorder lay chatH' = do
    let lw = lLeftW lay; rw = lRightW lay
        botRow = chatH' + 4
    goto botRow 1; setFg 36
    putStr $ "\x2570" ++ replicate (lw - 2) '\x2500' ++ "\x2534"
          ++ replicate (rw - 1) '\x2500' ++ "\x256F"
    resetSGR

statusBarConnTag :: ConnectionMode -> Int -> String
statusBarConnTag connMode nSessions =
    sessionCount ++ modeTag ++ " \x25C6 " ++ versionFull
  where
    sessionCount
        | nSessions > 0 = show nSessions ++ " session" ++ (if nSessions > 1 then "s" else "")
        | otherwise = "No sessions"
    modeTag
        | connMode == Chastity = " \x25C6 EPHEMERAL"
        | otherwise = ""

-- | Status bar: absolute last row, full width, inverted colors.
renderStatusBar :: Layout -> AppState -> String -> Int -> IO ()
renderStatusBar lay st status nSessions = do
    let totalW = lCols lay; rows = lRows lay
    goto rows 1; setFg 30; csi "47m"
    connMode <- readIORef (cfgConnectionMode (asConfig st))
    let connTag = statusBarConnTag connMode nSessions
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
        dialogToken <- case dlg of
            Just DlgHelp -> pure helpOverlayLines
            Just DlgAbout -> pure aboutOverlayLines
            Just DlgSettings -> settingsOverlayLines st
            Just DlgVerify -> verifyOverlayLines st
            Just DlgNewConn -> pure newConnOverlayLines
            Just DlgKeys -> keysOverlayLines st
            Just DlgBrowse -> browseOverlayLines st
            Just (DlgPrompt title _) -> pure (promptOverlayLines title dlgBuf)
            Nothing -> pure []
        let entries = Map.toList sessions
            chatH' = lChatH lay
            selSi = if sel >= 0 && sel < length entries
                    then Map.lookup (fst (entries !! sel)) sessions else Nothing
            nSessions = length entries
        sessionTokens <- mapM sessionRenderToken entries
        let token =
                "ok|" ++ show rows ++ "|" ++ show cols ++ "|" ++ show focus ++ "|"
                ++ show sel ++ "|" ++ show scroll ++ "|" ++ show cScroll ++ "|"
                ++ show mOpen ++ "|" ++ show menuIdx ++ "|" ++ show dlg ++ "|"
                ++ show browsePage ++ "|" ++ browseFilter ++ "|" ++ buf ++ "|" ++ dlgBuf ++ "|"
                ++ status ++ "|" ++ show dialogToken ++ "|" ++ show sessionTokens
        when (lastToken /= Just token) $ do
            writeIORef (asLastRenderToken st) (Just token)
            goto 1 1
            -- Row 1: tabbed menu bar
            renderMenuBar lay mOpen
            -- Rows 2..chatH'+1: content rows (contacts + chat)
            forM_ [0..chatH'-1] $ \row ->
                renderPaneRow lay entries selSi sel focus scroll cScroll row
            -- Separator
            renderMidBorder lay chatH'
            -- Input row
            renderInputRow lay focus buf chatH'
            -- Bottom border
            renderBottomBorder lay chatH'
            -- Status bar at absolute last row
            renderStatusBar lay st status nSessions
            -- Dropdown menu (rendered on top of content)
            case mOpen of
                Just tab -> renderDropdown lay tab menuIdx
                Nothing  -> pure ()
            -- Overlays
            case dlg of
                Just DlgHelp     -> renderHelpOverlay lay
                Just DlgAbout    -> renderAboutOverlay lay
                Just DlgKeys     -> renderKeysOverlay lay st
                Just DlgSettings -> renderSettingsOverlay lay st
                Just DlgNewConn  -> renderNewConnOverlay lay
                Just DlgVerify   -> renderVerifyOverlay lay st
                Just DlgBrowse   -> renderBrowseOverlay lay st
                Just (DlgPrompt title _) ->
                    renderPromptOverlay lay title dlgBuf
                Nothing -> pure ()
            hFlush stdout

sessionRenderToken :: (SessionId, SessionInfo) -> IO String
sessionRenderToken (sid, si) = do
    tag <- statusTag <$> readIORef (siStatus si)
    hist <- readIORef (siHistory si)
    pure (show sid ++ "|" ++ siPeerName si ++ "|" ++ tag ++ "|" ++ show hist)

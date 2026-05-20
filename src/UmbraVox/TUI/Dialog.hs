-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Dialog
    ( overlayBounds
    , overlayCloseBounds
    , overlayButtonAt
    , overlayButtonAtLine
    , overlayScrollbarBounds
    , wrapOverlayLines
    , settingsTabLabels
    , showOverlay
    , showWarningOverlay
    , showOverlayScrolled
    , helpOverlayLines, aboutOverlayLines, newConnOverlayLines
    , renderHelpOverlay, renderAboutOverlay, renderNewConnOverlay, renderVerifyOverlay
    , renderSettingsOverlay, renderKeysOverlay, renderBrowseOverlay
    , keysOverlayLines, verifyOverlayLines, promptOverlayLines
    , settingsOverlayLines, browseOverlayLines
    , regenKeyOverlayLines, renderRegenKeyOverlay
    , exportWarnOverlayLines, exportKeysOverlayLines
    , renderPromptOverlay
    , renderDropdown
    , insertLinkOverlayLines
    , renderInsertLinkOverlay
    , emojiPickerOverlayLines
    , renderEmojiPickerOverlay
    , pluginsTabLines
    ) where

import Control.Monad (forM_, when)
import Data.IORef (readIORef)
import Data.Char (toLower, toUpper, isSpace)
import Data.List (dropWhileEnd, intercalate)
import System.IO (hFlush, stdout)
import UmbraVox.BuildProfile
    ( BuildPluginId, PackagedPluginRuntime
    , bpId, bpName, buildChastityOnly, disabledPlugins
    , enabledPlugins, pluginLoadStatusLabel
    , pluginName, pmStatusTag, pprLoadStatus, pprManifest, pprPlugin
    )
import UmbraVox.Plugin.Registry (pluginEnabled, resolveEnable)
import UmbraVox.Plugin.Types (PluginRegistry)
import UmbraVox.Network.ProviderCatalog
    ( CachedTransportProvider, ProviderClass(..), ctpInherits, ctpLoadStatus
    , ctpManifest, ctpProvider, pmfEndpointTag, pmfStatusTag
    , providerEndpointSchema, providerIdLabel, providerLoadStatusLabel
    , renderProviderEndpoint, tpClass, tpName
    )
import UmbraVox.Network.ProviderRuntime (activeRuntimeProvider)
import qualified UmbraVox.Version
import UmbraVox.TUI.Types
import UmbraVox.TUI.Terminal (goto, setFg, resetSGR, bold, padR, csi)
import UmbraVox.TUI.Layout (dropdownCol)
import UmbraVox.TUI.Constants (maxOverlayW, minDropdownW)
import UmbraVox.TUI.PaginatedList (slicePage, psItems, psPage, psTotalPages)
import UmbraVox.TUI.Text (displayWidth, trimToWidth, splitAtWidth)
import UmbraVox.Protocol.QRCode (generateSafetyNumber, renderSafetyNumber,
                                    renderFingerprint, generateQRCode, renderQRCode)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Chat.Session (ChatSession(..))
import UmbraVox.Crypto.Signal.DoubleRatchet (RatchetState(..))
import UmbraVox.TUI.EmojiPicker
    ( emojiCategories, emojiByCategory, searchEmoji
    , emojiPage, emojiPageCount, selectorKeys
    )
import UmbraVox.Network.MDNS (MDNSPeer(..))
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char (toUpper)
import UmbraVox.TUI.EmojiPicker
    ( emojiCategories, emojiByCategory, searchEmoji
    , emojiPage, emojiPageCount, selectorKeys
    )

-- Overlays ----------------------------------------------------------------
overlayBounds :: Layout -> Int -> (Int, Int, Int, Int)
overlayBounds lay lineCount =
    let pageR0 = 2                 -- below menu bar
        pageH = max 1 (lRows lay - 2)  -- above status bar
        -- Center modals on the right (chat) pane, not the full screen
        rightC0 = lLeftW lay + 1   -- first column of right pane
        rightW  = max 1 (lRightW lay)
        w = max 1 (min (max 1 (rightW - 2)) maxOverlayW)
        h = max 1 (min (pageH - 2) (lineCount + 4))
        r0 = pageR0 + max 0 ((pageH - h) `div` 2)
        c0 = rightC0 + max 0 ((rightW - w) `div` 2)
    in (r0, c0, w, h)

overlayCloseBounds :: Layout -> Int -> (Int, Int, Int)
overlayCloseBounds lay lineCount =
    let (r0, c0, w, _) = overlayBounds lay lineCount
        innerW = w - 2
        closeText = "[X]"
        startCol = c0 + 1 + max 0 (innerW - displayWidth closeText)
    in (r0, startCol, startCol + displayWidth closeText - 1)

-- | Return the terminal column and full track row range for a scrollable overlay.
-- Returns (col, trackTop, trackBot) where the track spans trackTop..trackBot.
-- Returns Nothing when the content fits on screen (no scrollbar needed).
overlayScrollbarBounds :: Layout -> Int -> Int -> Maybe (Int, Int, Int)
overlayScrollbarBounds lay totalLines scrollOff =
    let (r0, c0, w, h) = overlayBounds lay totalLines
        contentH = h - 2
        sbCol    = c0 + w - 2   -- inside right content column
        trackTop = r0 + 1
        trackBot = r0 + h - 2
    in if totalLines <= contentH
        then Nothing
        else Just (sbCol, trackTop, trackBot)

overlayButtonAt :: Layout -> [String] -> Int -> Int -> Maybe String
overlayButtonAt lay lns row col = do
    let wrapped = wrapOverlayLines (wrapWidth lay) lns
    lineIx <- overlayContentLine lay (length wrapped) row col
    overlayButtonAtLine lay lns lineIx row col

overlayButtonAtLine :: Layout -> [String] -> Int -> Int -> Int -> Maybe String
overlayButtonAtLine lay lns lineIx row col
    | lineIx < 0 || lineIx >= length wrapped = Nothing
    | otherwise =
        let (r0, c0, _, _) = overlayBounds lay (length wrapped)
            line = wrapped !! lineIx
            targetRow = r0 + 1 + lineIx
            relCol = col - (c0 + 2)
        in if row /= targetRow || relCol < 0
            then Nothing
            else buttonAtColumn relCol (lineButtons line)
  where
    wrapped = wrapOverlayLines (wrapWidth lay) lns
    buttonAtColumn _ [] = Nothing
    buttonAtColumn x ((label, startCol, endCol):rest)
        | x >= startCol && x <= endCol = Just label
        | otherwise = buttonAtColumn x rest

wrapWidth :: Layout -> Int
wrapWidth lay =
    let (_, _, w, _) = overlayBounds lay 0
    in max 1 (w - 3)

overlayContentLine :: Layout -> Int -> Int -> Int -> Maybe Int
overlayContentLine lay lineCount row col =
    let (r0, c0, w, h) = overlayBounds lay lineCount
        insideRows = row > r0 && row < r0 + h - 1
        insideCols = col > c0 && col < c0 + w - 1
    in if insideRows && insideCols
        then Just (row - r0 - 1)
        else Nothing

lineButtons :: String -> [(String, Int, Int)]
lineButtons = go 0
  where
    go _ [] = []
    go col ('[':rest) =
        let (labelPart, suffix) = break (== ']') rest
            buttonText = "[" ++ labelPart ++ "]"
            buttonWidth = displayWidth buttonText
            label = trim labelPart
        in case suffix of
            ']':after ->
                (label, col, col + buttonWidth - 1) : go (col + buttonWidth) after
            _ ->
                go (col + 1) rest
    go col (ch:rest) = go (col + displayWidth [ch]) rest

    trim = dropWhile (== ' ') . dropWhileEnd (== ' ')

tabRowLine :: [String] -> Int -> String
tabRowLine labels activeIx =
    "Tabs: " ++ unwords (zipWith renderTab [0 :: Int ..] labels)
  where
    renderTab ix label
        | ix == activeIx = "*[" ++ label ++ "]*"
        | otherwise = "[" ++ label ++ "]"

settingsTabLabels :: [String]
settingsTabLabels
    | buildChastityOnly = ["Simple", "Security", "Advanced"]
    | otherwise = ["Simple", "Discovery", "Storage", "Security", "Advanced", "Plugins"]

showOverlay :: Layout -> String -> [String] -> IO ()
showOverlay lay title lns = showOverlayScrolled lay title lns 0

-- | Show a warning-styled overlay (red borders).
showWarningOverlay :: Layout -> String -> [String] -> IO ()
showWarningOverlay lay title lns = showOverlayColoredScrolled lay title lns 0 31

-- | Render a modal overlay dialog box with optional vertical scrolling.
-- 'scrollOff' is the number of content lines to skip from the top.
-- Scroll indicators (\x25b2/\x25bc) appear in the left border margin when
-- more content exists above or below the visible window.
-- A "Lines N-M of T" label is shown in the bottom border when the content
-- does not fit on screen.
-- When content exceeds the visible area a scrollbar track (│) and thumb (█)
-- are drawn on the rightmost content column inside the right border.
showOverlayScrolled :: Layout -> String -> [String] -> Int -> IO ()
showOverlayScrolled lay title lns scrollOff = showOverlayColoredScrolled lay title lns scrollOff 35

showOverlayColoredScrolled :: Layout -> String -> [String] -> Int -> Int -> IO ()
showOverlayColoredScrolled lay title lns scrollOff color = do
    let rawLineCount = length lns
        (_, _, w0, _) = overlayBounds lay rawLineCount
        wrapWidth = max 1 (w0 - 3)
        wrapped = wrapOverlayLines wrapWidth lns
        totalLines = length wrapped
        (r0, c0, w, h) = overlayBounds lay totalLines
        innerW = w - 2
        contentH = h - 2  -- rows available for content (inside top/bottom borders)
        -- Clamp scroll offset to valid range
        maxOff   = max 0 (totalLines - contentH)
        off      = min maxOff (max 0 scrollOff)
        -- Slice the visible window
        visible  = take contentH (drop off wrapped ++ repeat "")
        -- Scroll indicator state
        canScrollUp   = off > 0
        canScrollDown = off + contentH < totalLines
        needsScrollbar = totalLines > contentH
        -- Scrollbar geometry
        trackH   = contentH
        thumbSz  = max 1 (trackH * contentH `div` max 1 totalLines)
        thumbPos = if maxOff == 0 then 0
                   else off * (trackH - thumbSz) `div` max 1 maxOff
        -- Text area is narrowed by 1 when the scrollbar is shown
        textW = if needsScrollbar then innerW - 2 else innerW - 1
        -- Top border: ┌─ Title ──────────[X]┐
        closeText = "[X]"
        titleSeg  = "\x2500 " ++ title ++ " "
        fillLen   = max 0 (innerW - displayWidth titleSeg - displayWidth closeText)
        top = "\x250C" ++ titleSeg
              ++ replicate fillLen '\x2500'
              ++ closeText ++ "\x2510"
        -- Bottom border: show position label when content exceeds dialog height
        bot = if totalLines > contentH
                then let posLabel = " Lines " ++ show (off + 1) ++ "-"
                                    ++ show (min totalLines (off + contentH))
                                    ++ " of " ++ show totalLines ++ " "
                         botFill  = max 0 (innerW - displayWidth posLabel)
                     in "\x2514" ++ replicate botFill '\x2500' ++ posLabel ++ "\x2518"
                else "\x2514" ++ replicate innerW '\x2500' ++ "\x2518"
        -- Content rows: indicator + text + scrollbar placeholder + right border
        mkRow i l =
            let indicator
                    | i == 0 && canScrollUp              = "\x25b2"  -- ▲
                    | i == contentH - 1 && canScrollDown = "\x25bc"  -- ▼
                    | otherwise                          = " "
                -- scrollbar column placeholder (overdrawn after the loop)
                sbPlaceholder = if needsScrollbar then " " else ""
            in "\x2502" ++ indicator ++ padR textW (trimToWidth textW l) ++ sbPlaceholder ++ "\x2502"
        ovRows = zipWith mkRow [0 :: Int ..] visible
    forM_ (zip [0..] (top : ovRows ++ [bot])) $ \(i,line) ->
        goto (r0+i) c0 >> setFg color >> bold >> putStr line >> resetSGR
    -- Overdraw the scrollbar column with distinct colours.
    when needsScrollbar $
        forM_ [0..contentH-1] $ \i -> do
            goto (r0 + 1 + i) (c0 + w - 2)
            if i >= thumbPos && i < thumbPos + thumbSz
                then setFg 37 >> bold >> putStr "\x2588" >> resetSGR  -- bright white thumb █
                else csi "2m" >> setFg 37 >> putStr "\x2502" >> resetSGR  -- dim track │
    hFlush stdout

wrapOverlayLines :: Int -> [String] -> [String]
wrapOverlayLines width = concatMap (wrapOverlayLine width)

wrapOverlayLine :: Int -> String -> [String]
wrapOverlayLine width line
    | width <= 0 = [""]
    | null line = [""]
    | shouldKeepAsIs line = [line]
    | otherwise =
        let indent = takeWhile isSpace line
            body = dropWhile isSpace line
            bodyW = max 1 (width - displayWidth indent)
        in map (indent ++) (wrapBody bodyW body)
  where
    shouldKeepAsIs s =
        '[' `elem` s && ']' `elem` s

wrapBody :: Int -> String -> [String]
wrapBody _ [] = [""]
wrapBody width s = case breakLine width s of
    (chunk, "") -> [chunk]
    (chunk, rest) ->
        let rest' = dropWhile isSpace rest
        in chunk : wrapBody width rest'

breakLine :: Int -> String -> (String, String)
breakLine width s = go 0 [] Nothing s
  where
    go _ acc _ [] = (reverse acc, [])
    go curW acc lastSpace (ch:rest)
        | ch == ' ' =
            let nextW = curW + 1
            in if nextW > width
                then splitAtLastSpace acc lastSpace (ch:rest)
                else go nextW (ch:acc) (Just (length acc)) rest
        | otherwise =
            let chW = displayWidth [ch]
                nextW = curW + chW
            in if nextW > width
                then splitAtLastSpace acc lastSpace (ch:rest)
                else go nextW (ch:acc) lastSpace rest

    splitAtLastSpace acc Nothing rest =
        let (a, b) = splitAtWidth width (reverse acc ++ rest)
        in (a, b)
    splitAtLastSpace acc (Just spaceIx) rest =
        let full = reverse acc ++ rest
            (a, b) = splitAt spaceIx full
        in (trimRight a, dropWhile isSpace b)

    trimRight = dropWhileEnd isSpace

helpOverlayLines :: [String]
helpOverlayLines =
    [ " Navigation"
    , "   Tab              Switch focus: Contacts pane / Chat pane"
    , "   Up / Down        Move selection or scroll messages"
    , "   PgUp / PgDn      Scroll chat history one screen at a time"
    , "   Enter            Send message (Chat pane) / open session (Contacts pane)"
    , "   Backspace        Delete last character in input"
    , "   Esc              Close dialog, dismiss menu, or clear input"
    , ""
    , " Menu bar  (F-keys or underlined letter)"
    , "   F1 / H           Help overlay (this screen)"
    , "   F2 / P           Preferences  (settings, toggle rich text)"
    , "   F3 / I           Identity menu  (regen key, export, toggle info)"
    , "   Q                Quit UmbraVOX"
    , "   Left / Right     Navigate open menu items"
    , ""
    , " Keyboard shortcuts"
    , "   Ctrl+N           Start a new connection"
    , "   Ctrl+G           Open group chat"
    , "   Ctrl+Q           Quit immediately"
    , ""
    , "[ Close ]" ]

renderHelpOverlay :: Layout -> Int -> IO ()
renderHelpOverlay lay scrollOff = showOverlayScrolled lay "Help" helpOverlayLines scrollOff

aboutOverlayLines :: [String]
aboutOverlayLines =
    [ " UmbraVOX  \x2014  Post-Quantum Encrypted Messaging"
    , " " ++ UmbraVox.Version.versionFull
    , ""
    , " Copyright (c) 2026 Cyanitol and the UmbraVOX contributors"
    , ""
    , " Research-oriented encrypted messaging MVP featuring:"
    , "   \x2022  Signal Double Ratchet + ML-KEM-768 post-quantum key encapsulation"
    , "   \x2022  X3DH key agreement with Ed25519 / X25519 identity keys"
    , "   \x2022  Pluggable transport providers (direct, overlay, bridge)"
    , "   \x2022  Optional ephemeral-only mode  \x2014  no on-disk persistence"
    , ""
    ] ++ apacheLicenseLines ++
    [ ""
    , "[ Close ]" ]

-- | Full Apache 2.0 license text for display in the About overlay.
apacheLicenseLines :: [String]
apacheLicenseLines =
    [ "                          Apache License"
    , "                    Version 2.0, January 2004"
    , "                 http://www.apache.org/licenses/"
    , ""
    , "TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION"
    , ""
    , "1. Definitions."
    , ""
    , "   \"License\" shall mean the terms and conditions for use, reproduction,"
    , "   and distribution as defined by Sections 1 through 9 of this document."
    , ""
    , "   \"Licensor\" shall mean the copyright owner or entity authorized by"
    , "   the copyright owner that is granting the License."
    , ""
    , "   \"Legal Entity\" shall mean the union of the acting entity and all"
    , "   other entities that control, are controlled by, or are under common"
    , "   control with that entity. For the purposes of this definition,"
    , "   \"control\" means (i) the power, direct or indirect, to cause the"
    , "   direction or management of such entity, whether by contract or"
    , "   otherwise, or (ii) ownership of fifty percent (50%) or more of the"
    , "   outstanding shares, or (iii) beneficial ownership of such entity."
    , ""
    , "   \"You\" (or \"Your\") shall mean an individual or Legal Entity"
    , "   exercising permissions granted by this License."
    , ""
    , "   \"Source\" form shall mean the preferred form for making modifications,"
    , "   including but not limited to software source code, documentation"
    , "   source, and configuration files."
    , ""
    , "   \"Object\" form shall mean any form resulting from mechanical"
    , "   transformation or translation of a Source form, including but"
    , "   not limited to compiled object code, generated documentation,"
    , "   and conversions to other media types."
    , ""
    , "   \"Work\" shall mean the work of authorship, whether in Source or"
    , "   Object form, made available under the License, as indicated by a"
    , "   copyright notice that is included in or attached to the work."
    , ""
    , "   \"Derivative Works\" shall mean any work, whether in Source or Object"
    , "   form, that is based on (or derived from) the Work and for which the"
    , "   editorial revisions, annotations, elaborations, or other modifications"
    , "   represent, as a whole, an original work of authorship. For the purposes"
    , "   of this License, Derivative Works shall not include works that remain"
    , "   separable from, or merely link (or bind by name) to the interfaces of,"
    , "   the Work and Derivative Works thereof."
    , ""
    , "   \"Contribution\" shall mean any work of authorship, including the"
    , "   original version of the Work and any modifications or additions to that"
    , "   Work or Derivative Works thereof, that is intentionally submitted to"
    , "   Licensor for inclusion in the Work by the copyright owner or by an"
    , "   individual or Legal Entity authorized to submit on behalf of the"
    , "   copyright owner."
    , ""
    , "   \"Contributor\" shall mean Licensor and any individual or Legal Entity"
    , "   on behalf of whom a Contribution has been received by the Licensor and"
    , "   subsequently incorporated within the Work."
    , ""
    , "2. Grant of Copyright License. Subject to the terms and conditions of"
    , "   this License, each Contributor hereby grants to You a perpetual,"
    , "   worldwide, non-exclusive, no-charge, royalty-free, irrevocable"
    , "   copyright license to reproduce, prepare Derivative Works of,"
    , "   publicly display, publicly perform, sublicense, and distribute the"
    , "   Work and such Derivative Works in Source or Object form."
    , ""
    , "3. Grant of Patent License. Subject to the terms and conditions of"
    , "   this License, each Contributor hereby grants to You a perpetual,"
    , "   worldwide, non-exclusive, no-charge, royalty-free, irrevocable"
    , "   (except as stated in this section) patent license to make, have made,"
    , "   use, offer to sell, sell, import, and otherwise transfer the Work,"
    , "   where such license applies only to those patent claims licensable"
    , "   by such Contributor that are necessarily infringed by their"
    , "   Contribution(s) alone or by combination of their Contribution(s)"
    , "   with the Work to which such Contribution(s) was submitted. If You"
    , "   institute patent litigation against any entity (including a cross-claim"
    , "   or counterclaim in a lawsuit) alleging that the Work or a Contribution"
    , "   incorporated within the Work constitutes direct or contributory patent"
    , "   infringement, then any patent licenses granted to You under this"
    , "   License for that Work shall terminate as of the date such litigation"
    , "   is filed."
    , ""
    , "4. Redistribution. You may reproduce and distribute copies of the Work"
    , "   or Derivative Works thereof in any medium, with or without"
    , "   modifications, and in Source or Object form, provided that You meet"
    , "   the following conditions:"
    , ""
    , "   (a) You must give any other recipients of the Work or Derivative Works"
    , "       a copy of this License; and"
    , ""
    , "   (b) You must cause any modified files to carry prominent notices"
    , "       stating that You changed the files; and"
    , ""
    , "   (c) You must retain, in the Source form of any Derivative Works that"
    , "       You distribute, all copyright, patent, trademark, and attribution"
    , "       notices from the Source form of the Work, excluding those notices"
    , "       that do not pertain to any part of the Derivative Works; and"
    , ""
    , "   (d) If the Work includes a \"NOTICE\" text file as part of its"
    , "       distribution, then any Derivative Works that You distribute must"
    , "       include a readable copy of the attribution notices contained within"
    , "       such NOTICE file, excluding those notices that do not pertain to"
    , "       any part of the Derivative Works, in at least one of the following"
    , "       places: within a NOTICE text file distributed as part of the"
    , "       Derivative Works; within the Source form or documentation, if"
    , "       provided along with the Derivative Works; or, within a display"
    , "       generated by the Derivative Works, if and wherever such third-party"
    , "       notices normally appear. The contents of the NOTICE file are for"
    , "       informational purposes only and do not modify the License. You may"
    , "       add Your own attribution notices within Derivative Works that You"
    , "       distribute, alongside or as an addendum to the NOTICE text from"
    , "       the Work, provided that such additional attribution notices cannot"
    , "       be construed as modifying the License."
    , ""
    , "   You may add Your own copyright statement to Your modifications and may"
    , "   provide additional or different license terms and conditions for use,"
    , "   reproduction, or distribution of Your modifications, or for any such"
    , "   Derivative Works as a whole, provided Your use, reproduction, and"
    , "   distribution of the Work otherwise complies with the conditions stated"
    , "   in this License."
    , ""
    , "5. Submission of Contributions. Unless You explicitly state otherwise,"
    , "   any Contribution intentionally submitted for inclusion in the Work by"
    , "   You to the Licensor shall be under the terms and conditions of this"
    , "   License, without any additional terms or conditions. Notwithstanding"
    , "   the above, nothing herein shall supersede or modify the terms of any"
    , "   separate license agreement you may have executed with Licensor"
    , "   regarding such Contributions."
    , ""
    , "6. Trademarks. This License does not grant permission to use the trade"
    , "   names, trademarks, service marks, or product names of the Licensor,"
    , "   except as required for reasonable and customary use in describing the"
    , "   origin of the Work and reproducing the content of the NOTICE file."
    , ""
    , "7. Disclaimer of Warranty. Unless required by applicable law or agreed"
    , "   to in writing, Licensor provides the Work (and each Contributor"
    , "   provides its Contributions) on an \"AS IS\" BASIS, WITHOUT WARRANTIES"
    , "   OR CONDITIONS OF ANY KIND, either express or implied, including,"
    , "   without limitation, any warranties or conditions of TITLE,"
    , "   NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A PARTICULAR"
    , "   PURPOSE. You are solely responsible for determining the"
    , "   appropriateness of using or redistributing the Work and assume any"
    , "   risks associated with Your exercise of permissions under this License."
    , ""
    , "8. Limitation of Liability. In no event and under no legal theory,"
    , "   whether in tort (including negligence), contract, or otherwise,"
    , "   unless required by applicable law (such as deliberate and grossly"
    , "   negligent acts) or agreed to in writing, shall any Contributor be"
    , "   liable to You for damages, including any direct, indirect, special,"
    , "   incidental, or consequential damages of any character arising as a"
    , "   result of this License or out of the use or inability to use the"
    , "   Work (including but not limited to damages for loss of goodwill,"
    , "   work stoppage, computer failure or malfunction, or any and all other"
    , "   commercial damages or losses), even if such Contributor has been"
    , "   advised of the possibility of such damages."
    , ""
    , "9. Accepting Warranty or Additional Liability. While redistributing the"
    , "   Work or Derivative Works thereof, You may choose to offer, and charge"
    , "   a fee for, acceptance of support, warranty, indemnity, or other"
    , "   liability obligations and/or rights consistent with this License."
    , "   However, in accepting such obligations, You may act only on Your own"
    , "   behalf and on Your sole responsibility, not on behalf of any other"
    , "   Contributor, and only if You agree to indemnify, defend, and hold each"
    , "   Contributor harmless for any liability incurred by, or claims asserted"
    , "   against, such Contributor by reason of your accepting any such"
    , "   warranty or additional liability."
    , ""
    , "END OF TERMS AND CONDITIONS"
    ]

renderAboutOverlay :: Layout -> Int -> IO ()
renderAboutOverlay lay scrollOff = showOverlayScrolled lay "About UmbraVOX" aboutOverlayLines scrollOff

newConnOverlayLines :: [String]
newConnOverlayLines =
    [ " Choose a conversation type:"
    , ""
    , "   1. Private   \x2014  secure local notes (no peer, stored on this device only)"
    , "   2. Single    \x2014  end-to-end encrypted session with one remote peer"
    , "   3. Group     \x2014  multi-party encrypted conversation (Sender Keys)"
    , ""
    , "[ Private ]  [ Single ]  [ Group ]  [ Cancel ]" ]

renderNewConnOverlay :: Layout -> Int -> IO ()
renderNewConnOverlay lay scrollOff = showOverlayScrolled lay "New Conversation" newConnOverlayLines scrollOff

verifyOverlayLines :: AppState -> IO [String]
verifyOverlayLines st = do
    sessions <- readIORef (cfgSessions (asConfig st))
    sel <- readIORef (asSelected st)
    mIk <- readIORef (cfgIdentity (asConfig st))
    let entries = Map.toList sessions
    if sel >= 0 && sel < length entries then do
        let (_,si) = entries !! sel
        case mIk of
            Nothing -> pure
                ["No identity generated yet.", "", "Press K to generate keys first.", "", "[ Close ]" ]
            Just ik -> case siCrypto si of
                BridgeCrypto _bs -> pure
                    ["Peer: " ++ siPeerName si, ""
                    , "Safety number unavailable: bridge session."
                    , "", "[ Close ]" ]
                RatchetCrypto ref -> do
                    session <- readIORef ref
                    let ourKey  = ikX25519Public ik
                        mPeerKey = rsDHRecv (csRatchet session)
                    case mPeerKey of
                        Nothing -> pure
                            ["Peer: " ++ siPeerName si, ""
                            , "Safety number unavailable: no key exchange yet."
                            , "(Send or receive a message first.)", ""
                            , "[ Close ]" ]
                        Just peerKey -> do
                            let safetyNum = generateSafetyNumber ourKey peerKey
                                safetyRows = renderSafetyNumber safetyNum
                                qrMatrix = generateQRCode safetyNum
                                qrLines  = renderQRCode qrMatrix
                            pure $
                                ["Peer: " ++ siPeerName si, "", "Safety Number:"] ++ safetyRows ++
                                ["", "QR Code:"] ++ map ("  " ++) qrLines ++
                                ["", "Compare via a separate channel.", "", "[ Close ]" ]
    else pure ["No contact selected", "", "[ Close ]" ]

renderVerifyOverlay :: Layout -> AppState -> Int -> IO ()
renderVerifyOverlay lay st scrollOff = do
    lns <- verifyOverlayLines st
    showOverlayScrolled lay "Verify Keys" lns scrollOff

renderSettingsOverlay :: Layout -> AppState -> Int -> IO ()
renderSettingsOverlay lay st scrollOff = do
    lns <- settingsOverlayLines st
    showOverlayScrolled lay "Preferences" lns scrollOff

settingsOverlayLines :: AppState -> IO [String]
settingsOverlayLines st = do
    tabIx     <- readIORef (asDialogTab st)
    port      <- readIORef (cfgListenPort (asConfig st))
    name      <- readIORef (cfgDisplayName (asConfig st))
    mdns      <- readIORef (cfgMDNSEnabled (asConfig st))
    pex       <- readIORef (cfgPEXEnabled (asConfig st))
    debugLog  <- readIORef (cfgDebugLogging (asConfig st))
    debugPath <- readIORef (cfgDebugLogPath (asConfig st))
    dbEnabled <- readIORef (cfgDBEnabled (asConfig st))
    mDb       <- readIORef (cfgAnthonyDB (asConfig st))
    richText  <- readIORef (asRichText st)
    pluginReg <- readIORef (cfgPluginRegistry (asConfig st))
    let tf True = "ON"; tf False = "OFF"
        ephemeral = case mDb of { Nothing -> True; Just _ -> False }
    storageLines <- settingsStorageLines st tf dbEnabled ephemeral
    connMode <- readIORef (cfgConnectionMode (asConfig st))
    packagedPluginRuntimeCatalog <- readIORef (cfgPackagedPluginRuntimeCatalog (asConfig st))
    transportProviderRuntimeCatalog <- readIORef (cfgTransportProviderRuntimeCatalog (asConfig st))
    let modeLabel = settingsConnModeLabel connMode
        tabLine = tabRowLine settingsTabLabels tabIx
        ctx = SettingsCtx port name mdns pex debugLog debugPath modeLabel storageLines
                packagedPluginRuntimeCatalog transportProviderRuntimeCatalog richText
                pluginReg
    tabBody <-
        if buildChastityOnly
            then pure (settingsChastityTab ctx tabIx)
            else settingsFullTab ctx tabIx
    pure $
        [ tabLine
        , ""
        ] ++ tabBody ++
        [ ""
        , " Press Left/Right to switch tabs"
        , " Press 0-9/a/b/c to change, Esc to close"
        , "[ Close ]" ]

data SettingsCtx = SettingsCtx
    { sctxPort :: Int
    , sctxName :: String
    , sctxMdns :: Bool
    , sctxPex :: Bool
    , sctxDebugLog :: Bool
    , sctxDebugPath :: String
    , sctxModeLabel :: String
    , sctxStorageLines :: [String]
    , sctxPackagedPlugins :: [PackagedPluginRuntime]
    , sctxTransportProviders :: [CachedTransportProvider]
    , sctxRichText :: Bool
    , sctxPluginRegistry :: PluginRegistry
    }

settingsConnModeLabel :: ConnectionMode -> String
settingsConnModeLabel Swing       = "SWING"
settingsConnModeLabel Promiscuous = "PROMISCUOUS"
settingsConnModeLabel Selective   = "SELECTIVE"
settingsConnModeLabel Chaste      = "CHASTE"
settingsConnModeLabel Chastity    = "CHASTITY"

settingsStorageLines :: AppState -> (Bool -> String) -> Bool -> Bool -> IO [String]
settingsStorageLines st tf dbEnabled ephemeral =
    if ephemeral
        then pure
            [ " Storage"
            , "   5. Persistent DB: [" ++ tf dbEnabled ++ "]"
            , "   [EPHEMERAL MODE - no persistence]"
            , "   Messages exist only in memory."
            , "   9. Clear history..." ]
        else do
            dbPath'   <- readIORef (cfgDBPath (asConfig st))
            retention <- readIORef (cfgRetentionDays (asConfig st))
            autoSave  <- readIORef (cfgAutoSaveMessages (asConfig st))
            let retLabel = if retention == 0 then "forever"
                           else show retention ++ " days"
            pure [ " Storage"
                 , "   5. Persistent DB: [" ++ tf dbEnabled ++ "]"
                 , "   6. DB path:       " ++ dbPath'
                 , "   7. Retention:     " ++ retLabel
                 , "   8. Auto-save msgs: [" ++ tf autoSave ++ "]"
                 , "   9. Clear history..." ]

settingsChastityTab :: SettingsCtx -> Int -> [String]
settingsChastityTab ctx tabIx =
    let tf True = "ON"; tf False = "OFF"
    in case tabIx of
        0 ->
            [ " Simple"
            , "   1. Listen port:    " ++ show (sctxPort ctx)
            , "   2. Display name:   " ++ sctxName ctx
            , "   5. Rich text:     [" ++ tf (sctxRichText ctx) ++ "]"
            , ""
            , " Discovery, storage, export/import, and logs"
            , " are compile-time disabled in this build."
            ]
        1 ->
            [ " Security"
            , "   Build profile:     CHASTITY"
            , "   Connection mode:   [" ++ sctxModeLabel ctx ++ "]"
            , ""
            , " Persistent storage is compile-time locked OFF."
            , " Mode changes are compile-time locked OFF."
            ]
        _ ->
            [ " Advanced"
            , "   Disabled built-ins:"
            ]
            ++ map renderDisabledPlugin disabledPluginIds
            ++ [ ""
               , " This build keeps all chats and identity state"
               , " in memory only."
               ]

settingsFullTab :: SettingsCtx -> Int -> IO [String]
settingsFullTab ctx tabIx =
    let tf True = "ON"; tf False = "OFF"
    in case tabIx of
        0 ->
            pure
                [ " Simple"
                , "   1. Listen port:    " ++ show (sctxPort ctx)
                , "   2. Display name:   " ++ sctxName ctx
                , "   3. mDNS (LAN):    [" ++ tf (sctxMdns ctx) ++ "]"
                , "   5. Rich text:     [" ++ tf (sctxRichText ctx) ++ "]"
                , "   c. Connection mode: [" ++ sctxModeLabel ctx ++ "]"
                ]
        1 ->
            pure
                [ " Discovery"
                , "   3. mDNS (LAN):    [" ++ tf (sctxMdns ctx) ++ "]"
                , "   4. Peer Exchange: [" ++ tf (sctxPex ctx) ++ "]"
                , ""
                , " Peer discovery applies immediately."
                ]
        2 ->
            pure ([ " Storage" ] ++ tail (sctxStorageLines ctx))
        3 ->
            pure
                [ " Security"
                , "   c. Connection mode: [" ++ sctxModeLabel ctx ++ "]"
                , "   (Swing / Promiscuous / Selective / Chaste / Chastity)"
                , ""
                , " Switching mode renegotiates remote sessions."
                ]
        5 -> pure (pluginsTabLines (sctxPluginRegistry ctx))
        _ -> do
            let packagedPluginLines = summarizePackagedPlugins (sctxPackagedPlugins ctx)
                providerLines = summarizeTransportProviders (sctxTransportProviders ctx)
            pure $
                [ " Advanced"
                , "   a. Debug logging:  [" ++ tf (sctxDebugLog ctx) ++ "]"
                , "   b. Log path:       " ++ sctxDebugPath ctx
                , ""
                , "   Compiled built-ins:"
                ]
                ++ summarizeBuiltInPlugins enabledPluginIds
                ++ [ ""
                   , "   Packaged feature slots:"
                   ]
                ++ packagedPluginLines
                ++ [ ""
                   , "   Transport providers:"
                   ]
                ++ providerLines
                ++ [ ""
                   , " Diagnostic controls stay off by default."
                   ]

enabledPluginIds :: [BuildPluginId]
enabledPluginIds = map bpId enabledPlugins

disabledPluginIds :: [BuildPluginId]
disabledPluginIds = map bpId disabledPlugins

renderDisabledPlugin :: BuildPluginId -> String
renderDisabledPlugin pid =
    "   - " ++ pluginName pid ++ ": [OFF]"

summarizeBuiltInPlugins :: [BuildPluginId] -> [String]
summarizeBuiltInPlugins [] =
    [ "   - none" ]
summarizeBuiltInPlugins pluginIds =
    map (\row -> "   - " ++ intercalate ", " (map pluginName row)) (chunkItems 3 pluginIds)

summarizePackagedPlugins :: [PackagedPluginRuntime] -> [String]
summarizePackagedPlugins [] =
    [ "   - none discovered" ]
summarizePackagedPlugins catalog =
    map (\row -> "   - " ++ intercalate ", " (map renderPackagedPlugin row)) (chunkItems 2 catalog)

renderPackagedPlugin :: PackagedPluginRuntime -> String
renderPackagedPlugin runtimeEntry =
    let plugin = pprPlugin runtimeEntry
        manifest = pprManifest runtimeEntry
        loadStatus = pprLoadStatus runtimeEntry
    in bpName plugin
        ++ " ("
        ++ pmStatusTag manifest
        ++ "/"
        ++ pluginLoadStatusLabel loadStatus
        ++ ")"

summarizeTransportProviders :: [CachedTransportProvider] -> [String]
summarizeTransportProviders [] =
    [ "   - none discovered" ]
summarizeTransportProviders catalog =
    renderProviderGroup ProviderDirectCarrier "   - Direct carriers" catalog
    ++ renderProviderGroup ProviderOverlayCarrier "   - Overlay carriers" catalog
    ++ renderProviderGroup ProviderOpenBridge "   - Open bridges" catalog
    ++ renderProviderGroup ProviderClosedBridge "   - Closed bridges" catalog

renderProviderGroup :: ProviderClass -> String -> [CachedTransportProvider] -> [String]
renderProviderGroup providerClass label catalog =
    let entries = filter ((== providerClass) . tpClass . ctpProvider) catalog
    in if null entries
        then []
        else [label ++ ": " ++ intercalate ", " (map renderProviderEntry entries)]

renderProviderEntry :: CachedTransportProvider -> String
renderProviderEntry runtimeEntry =
    let provider = ctpProvider runtimeEntry
        manifest = ctpManifest runtimeEntry
        inheritsLabel =
            case map providerIdLabel (ctpInherits runtimeEntry) of
                [] -> ""
                names -> "<-" ++ intercalate "+" names
    in tpName provider
        ++ "("
        ++ pmfStatusTag manifest
        ++ inheritsLabel
        ++ "/"
        ++ pmfEndpointTag manifest
        ++ "/"
        ++ providerLoadStatusLabel (ctpLoadStatus runtimeEntry)
        ++ ")"

chunkItems :: Int -> [a] -> [[a]]
chunkItems _ [] = []
chunkItems n xs =
    take n xs : chunkItems n (drop n xs)

-- | Render the Plugins tab lines for the preferences dialog.
-- Shows the five user-facing persistence plugins with their ON/OFF state.
-- Press 1-5 to toggle; Full Persistence (5) auto-enables all above.
pluginsTabLines :: PluginRegistry -> [String]
pluginsTabLines reg =
    let tf b = if b then "ON" else "OFF"
        st pid = tf (pluginEnabled pid reg)
        hasDeps pid =
            case resolveEnable pid reg of
                Right ds -> length ds > 1
                Left _   -> False
        depNote pid
            | hasDeps pid = " (+ deps)"
            | otherwise   = ""
    in [ " Plugins"
       , "   1. Key Persistence    [" ++ st "key-persistence"     ++ "]" ++ depNote "key-persistence"     ++ " \x2014 Ephemeral mode"
       , "   2. Message Storage    [" ++ st "message-storage"     ++ "]" ++ depNote "message-storage"     ++ " \x2014 Ephemeral mode"
       , "   3. Ratchet Safety     [" ++ st "ratchet-persistence" ++ "]" ++ depNote "ratchet-persistence" ++ " \x2014 Ephemeral mode"
       , "   4. Runtime Logging    [" ++ st "runtime-logging"     ++ "]" ++ depNote "runtime-logging"     ++ " \x2014 Ephemeral mode"
       , "   5. Full Persistence   [" ++ st "full-persistence"    ++ "]" ++ " \x2014 Enable all above"
       , ""
       , " Press 1-5 to toggle. Enabling auto-resolves dependencies."
       ]

renderKeysOverlay :: Layout -> AppState -> Int -> IO ()
renderKeysOverlay lay st scrollOff = do
    lns <- keysOverlayLines st
    showOverlayScrolled lay "Identity & Keys" lns scrollOff

keysOverlayLines :: AppState -> IO [String]
keysOverlayLines st = do
    mIk <- readIORef (cfgIdentity (asConfig st))
    case mIk of
        Nothing -> pure ["No identity generated yet.", "", "[ Close ]" ]
        Just ik -> do
            let x25519Lines  = renderFingerprint (ikX25519Public ik)
                ed25519Lines = renderFingerprint (ikEd25519Public ik)
                safetyNum = generateSafetyNumber (ikX25519Public ik) (ikX25519Public ik)
                qrLines = renderQRCode (generateQRCode safetyNum)
            pure $
                [ "X25519 fingerprint:" ] ++ x25519Lines ++
                [ "", "Ed25519 fingerprint:" ] ++ ed25519Lines ++
                [ "", "QR Code (X25519):" ] ++ map ("  " ++) qrLines ++
                [ ""
                , "[ Regenerate (F5) ]  [ Export Keys ]  [ Import Keys ]  [ Close ]"
                ]

exportWarnOverlayLines :: Bool -> [String]
exportWarnOverlayLines checked =
    [ "WARNING: Exporting private keys can permanently compromise identity safety."
    , "Anyone with this material can impersonate this identity."
    , ""
    , if checked then "[x] I understand and accept this risk"
                 else "[ ] I understand and accept this risk"
    , ""
    , if checked then "[ Continue Export ]  [ Cancel ]"
                 else "  (check the box to enable)   [ Cancel ]"
    ]

exportKeysOverlayLines :: AppState -> IO [String]
exportKeysOverlayLines st = do
    mIk <- readIORef (cfgIdentity (asConfig st))
    case mIk of
        Nothing -> pure ["No identity generated yet.", "", "[ Close ]" ]
        Just ik -> do
            let payload = encodeIdentityHex ik
                qrPayload = encodeIdentityNumeric payload
                qrLines = renderQRCode (generateQRCode qrPayload)
            pure $
                [ "Export Identity Keys (private + public)"
                , "Manual entry payload (hex):"
                , payload
                , ""
                , "QR payload (numeric):"
                , qrPayload
                , ""
                , "QR code:"
                ] ++ map ("  " ++) qrLines ++ ["", "[ Close ]"]

encodeIdentityHex :: IdentityKey -> String
encodeIdentityHex ik = C8.unpack (hexLower bytes)
  where
    bytes = BS.concat
        [ ikEd25519Secret ik
        , ikEd25519Public ik
        , ikX25519Secret ik
        , ikX25519Public ik
        ]
    hexLower = BS.concatMap (\b -> BS.pack [hexNibble (b `div` 16), hexNibble (b `mod` 16)])
    hexNibble n
        | n < 10 = 48 + n
        | otherwise = 87 + n
numericDigits :: String -> String
numericDigits = filter (\c -> c >= '0' && c <= '9')

encodeIdentityNumeric :: String -> String
encodeIdentityNumeric = concatMap enc
  where
    enc c
        | c >= '0' && c <= '9' = ['0', c]
        | c >= 'a' && c <= 'f' = ['1', toEnum (fromEnum '0' + fromEnum c - fromEnum 'a')]
        | c >= 'A' && c <= 'F' = enc (toEnum (fromEnum c + 32))
        | otherwise = "99"

browseOverlayLines :: AppState -> IO [String]
browseOverlayLines st = do
    peers <- readIORef (cfgMDNSPeers (asConfig st))
    query <- readIORef (asBrowseFilter st)
    page <- readIORef (asBrowsePage st)
    mdnsOn <- readIORef (cfgMDNSEnabled (asConfig st))
    pexOn  <- readIORef (cfgPEXEnabled (asConfig st))
    let filtered = filter (browseMatches query) peers
        pageSlice = slicePage 10 page filtered
        page' = psPage pageSlice
        totalPages = psTotalPages pageSlice
        visible = psItems pageSlice
        runtimeProvider = activeRuntimeProvider
    let header = [ "Discovered peers on the local network:"
                 , "  Provider: " ++ providerIdLabel runtimeProvider
                    ++ "  |  Endpoint: " ++ providerEndpointSchema runtimeProvider
                 , "  mDNS: " ++ (if mdnsOn then "ON" else "OFF")
                    ++ "  |  PEX: " ++ (if pexOn then "ON" else "OFF")
                 , "  Search: " ++ if null query then "(none)" else query
                 , "  Page: " ++ show (page' + 1) ++ "/" ++ show totalPages
                 , "" ]
        peerLines = if null filtered
            then ["  (no peers discovered yet)"
                 , ""
                 , "  Make sure mDNS is enabled in Preferences,"
                 , "  or adjust the current search filter." ]
            else concatMap fmtPeer visible
        fmtPeer (i, p) =
            [ "  " ++ show i ++ ". "
                ++ peerDisplayName p
                ++ "  "
                ++ renderProviderEndpoint runtimeProvider (mdnsIP p) (mdnsPort p)
                ++ "  "
                ++ take 16 (pubkeyHex p)
            ]
    pure $
        header ++ peerLines ++
        [ ""
        , "  0-9 connect  / \8592/\8594 page"
        , "[ Prev ]  [ Next ]  [ Search ]  [ Clear ]  [ Close ]"
        ]

renderBrowseOverlay :: Layout -> AppState -> Int -> IO ()
renderBrowseOverlay lay st scrollOff = do
    lns <- browseOverlayLines st
    showOverlayScrolled lay "Browse Peers" lns scrollOff

browseMatches :: String -> MDNSPeer -> Bool
browseMatches raw peer =
    null needles
    || all (\needle -> any (contains needle) haystacks) needles
  where
    needles = queryTerms raw
    haystacks =
        [ map toLower (maybe "" id (mdnsName peer))
        , map toLower (pubkeyHex peer)
        ]
    contains sub s = any (sub `isPrefixOf`) (tails s)
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(_:rest) = xs : tails rest

queryTerms :: String -> [String]
queryTerms =
    words . map normalize
  where
    normalize c
        | c `elem` [',', ':', ';'] = ' '
        | otherwise = toLower c

peerDisplayName :: MDNSPeer -> String
peerDisplayName peer = maybe "(unnamed)" id (mdnsName peer)

pubkeyHex :: MDNSPeer -> String
pubkeyHex = concatMap byteHex . BS.unpack . mdnsPubkey
  where
    byteHex b =
        let digits = "0123456789abcdef"
            hi = digits !! fromIntegral (b `div` 16)
            lo = digits !! fromIntegral (b `mod` 16)
        in [hi, lo]

promptOverlayLines :: String -> String -> [String]
promptOverlayLines _ buf =
    ["Enter value:", "\x25B8 " ++ buf ++ "\x2588", "", "[ OK ]  [ Cancel ]"]

renderPromptOverlay :: Layout -> String -> String -> Int -> IO ()
renderPromptOverlay lay title buf scrollOff =
    showOverlayScrolled lay title (promptOverlayLines title buf) scrollOff

-- | Lines for the key-regeneration confirmation dialog.
-- The checkbox state is shown inline; the "Yes" button is only labelled
-- actionable when the checkbox is checked.
regenKeyOverlayLines :: Bool -> IO [String]
regenKeyOverlayLines checked =
    pure
        [ "WARNING: This will generate a new identity key."
        , "All existing sessions will be invalidated."
        , ""
        , checkboxLine
        , ""
        , buttonLine
        ]
  where
    checkboxLine
        | checked   = "[x] I understand this action is irreversible"
        | otherwise = "[ ] I understand this action is irreversible"
    buttonLine
        | checked   = "[ Yes, Regenerate ]  [ Cancel ]"
        | otherwise = "  (check the box to enable)   [ Cancel ]"

renderRegenKeyOverlay :: Layout -> AppState -> Int -> IO ()
renderRegenKeyOverlay lay st scrollOff = do
    checked <- readIORef (asRegenCheckbox st)
    lns <- regenKeyOverlayLines checked
    showOverlayColoredScrolled lay "Regenerate Identity Key" lns scrollOff 31  -- red warning

-- | Lines for the Insert Link modal.
-- focusIx: 0=Text field, 1=URL field, 2=Insert btn, 3=Cancel btn.
insertLinkOverlayLines :: String -> String -> Int -> [String]
insertLinkOverlayLines linkText linkUrl focusIx =
    [ ""
    , "  Text: " ++ markField 0 linkText
    , "  URL:  " ++ markField 1 linkUrl
    , ""
    , "  " ++ markBtn 2 "[ Insert ]" ++ "  " ++ markBtn 3 "[ Cancel ]"
    , ""
    ]
  where
    markField fi val
        | fi == focusIx = "\x25B8 " ++ val ++ "\x2588"
        | otherwise     = val ++ "_"
    markBtn bi label
        | bi == focusIx = "*" ++ label ++ "*"
        | otherwise     = label

renderInsertLinkOverlay :: Layout -> AppState -> Int -> IO ()
renderInsertLinkOverlay lay st scrollOff = do
    txt   <- readIORef (asLinkText st)
    url   <- readIORef (asLinkUrl st)
    focus <- readIORef (asLinkFocus st)
    let lns = insertLinkOverlayLines txt url focus
    showOverlayScrolled lay "Insert Link" lns scrollOff

-- Emoji Picker ------------------------------------------------------------

-- | Build the display lines for the emoji picker overlay.
-- Layout:
--   blank line
--   Search: [query_____]
--   category tabs
--   blank line
--   6 rows of 6 emoji cells: "  K <emoji> ..."
--   blank line
--   page indicator + navigation hint
--   blank line
--   [ Close ]
emojiPickerOverlayLines :: String -> Int -> Int -> [String]
emojiPickerOverlayLines searchQuery catIx pageIx =
    let entries =
            if null searchQuery
                then emojiByCategory catIx
                else searchEmoji searchQuery
        totalPages = emojiPageCount entries
        page = max 0 (min (totalPages - 1) pageIx)
        pageEntries = emojiPage page entries
        -- Search bar
        searchLine = "  Search: [" ++ padRight 16 searchQuery ++ "]"
        -- Category tabs (only shown when no search query active)
        catLine = "  " ++ unwords (zipWith renderCatTab [0..] emojiCategories)
        renderCatTab i name
            | i == catIx && null searchQuery = "*[" ++ name ++ "]*"
            | otherwise = "[" ++ name ++ "]"
        -- Emoji grid: 6 rows × 6 cols = 36 cells
        gridLines = buildGrid pageEntries
        -- Page indicator
        pageLabel = "  Page " ++ show (page + 1) ++ "/" ++ show totalPages
                 ++ "  \x25C0 Left  Right \x25B6"
    in [ ""
       , searchLine
       , catLine
       , ""
       ] ++ gridLines ++
       [ ""
       , pageLabel
       , ""
       , "[ Close ]"
       ]
  where
    padRight n s =
        let s' = take n s
        in s' ++ replicate (n - length s') '_'

    buildGrid entries =
        let rows = chunkOf 6 (zip selectorKeys (entries ++ repeat ("", "")))
        in map renderRow rows

    chunkOf _ [] = []
    chunkOf n xs = take n xs : chunkOf n (drop n xs)

    renderRow cells =
        concatMap renderCell cells

    renderCell (key, (emoji, _)) =
        "  " ++ [toUpper key] ++ " " ++ emoji

renderEmojiPickerOverlay :: Layout -> AppState -> Int -> IO ()
renderEmojiPickerOverlay lay st scrollOff = do
    q    <- readIORef (asEmojiSearch st)
    cat  <- readIORef (asEmojiCategory st)
    page <- readIORef (asEmojiPage st)
    let lns = emojiPickerOverlayLines q cat page
    showOverlayScrolled lay "Emoji Picker" lns scrollOff

-- | Render a dropdown menu below its tab position
renderDropdown :: Layout -> MenuTab -> Int -> IO ()
renderDropdown lay tab selIdx = do
    let items = menuTabItems tab
        boxW = max minDropdownW (maximum (map length items) + 4)
        col = dropdownCol (lCols lay) tab
        startRow = 2
        topLine = "\x2554" ++ replicate (boxW - 2) '\x2550' ++ "\x2557"
        botLine = "\x255A" ++ replicate (boxW - 2) '\x2550' ++ "\x255D"
    when (startRow + length items + 1 < lRows lay && col + boxW <= lCols lay) $ do
        goto startRow col; setFg 35; bold; putStr topLine; resetSGR
        forM_ (zip [0..] items) $ \(i, label) -> do
            goto (startRow + 1 + i) col
            setFg 35; bold; putStr "\x2551"; resetSGR
            if i == selIdx then do
                csi "7m"  -- inverse video
                putStr (padR (boxW - 2) (" " ++ label))
                resetSGR
            else
                putStr (padR (boxW - 2) (" " ++ label))
            setFg 35; bold; putStr "\x2551"; resetSGR
        goto (startRow + 1 + length items) col
        setFg 35; bold; putStr botLine; resetSGR

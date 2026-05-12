-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Dialog
    ( overlayBounds
    , overlayCloseBounds
    , overlayButtonAt
    , overlayButtonAtLine
    , settingsTabLabels
    , showOverlay
    , helpOverlayLines, aboutOverlayLines, newConnOverlayLines
    , renderHelpOverlay, renderAboutOverlay, renderNewConnOverlay, renderVerifyOverlay
    , renderSettingsOverlay, renderKeysOverlay, renderBrowseOverlay
    , keysOverlayLines, verifyOverlayLines, promptOverlayLines
    , settingsOverlayLines, browseOverlayLines
    , regenKeyOverlayLines, renderRegenKeyOverlay
    , renderPromptOverlay
    , renderDropdown
    ) where

import Control.Monad (forM_, when)
import Data.IORef (readIORef)
import Data.Char (toLower)
import Data.List (dropWhileEnd, intercalate)
import System.IO (hFlush, stdout)
import UmbraVox.BuildProfile
    ( BuildPluginId, PackagedPluginRuntime
    , bpId, bpName, buildChastityOnly, disabledPlugins
    , enabledPlugins, pluginLoadStatusLabel
    , pluginName, pmStatusTag, pprLoadStatus, pprManifest, pprPlugin
    )
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
import UmbraVox.TUI.Text (displayWidth, trimToWidth)
import UmbraVox.Protocol.QRCode (generateSafetyNumber, renderSafetyNumber,
                                    renderFingerprint, generateQRCode, renderQRCode)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Chat.Session (ChatSession(..))
import UmbraVox.Crypto.Signal.DoubleRatchet (RatchetState(..))
import UmbraVox.Network.MDNS (MDNSPeer(..))
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS

-- Overlays ----------------------------------------------------------------
overlayBounds :: Layout -> Int -> (Int, Int, Int, Int)
overlayBounds lay lineCount =
    let cols = lCols lay
        rows = lRows lay
        w = min (cols - 4) maxOverlayW
        h = min (rows - 4) (lineCount + 4)
        r0 = max 1 ((rows - h) `div` 2)
        c0 = max 1 ((cols - w) `div` 2)
    in (r0, c0, w, h)

overlayCloseBounds :: Layout -> Int -> (Int, Int, Int)
overlayCloseBounds lay lineCount =
    let (r0, c0, w, _) = overlayBounds lay lineCount
        innerW = w - 2
        closeText = "[X]"
        startCol = c0 + 1 + max 0 (innerW - displayWidth closeText)
    in (r0, startCol, startCol + displayWidth closeText - 1)

overlayButtonAt :: Layout -> [String] -> Int -> Int -> Maybe String
overlayButtonAt lay lns row col = do
    lineIx <- overlayContentLine lay (length lns) row col
    overlayButtonAtLine lay lns lineIx row col

overlayButtonAtLine :: Layout -> [String] -> Int -> Int -> Int -> Maybe String
overlayButtonAtLine lay lns lineIx row col
    | lineIx < 0 || lineIx >= length lns = Nothing
    | otherwise =
        let (r0, c0, _, _) = overlayBounds lay (length lns)
            line = lns !! lineIx
            targetRow = r0 + 1 + lineIx
            relCol = col - (c0 + 2)
        in if row /= targetRow || relCol < 0
            then Nothing
            else buttonAtColumn relCol (lineButtons line)
  where
    buttonAtColumn _ [] = Nothing
    buttonAtColumn x ((label, startCol, endCol):rest)
        | x >= startCol && x <= endCol = Just label
        | otherwise = buttonAtColumn x rest

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
    | otherwise = ["Simple", "Discovery", "Storage", "Security", "Advanced"]

showOverlay :: Layout -> String -> [String] -> IO ()
showOverlay lay title lns = do
    let (r0, c0, w, h) = overlayBounds lay (length lns)
        innerW = w - 2
        -- Title in top border: ┌─ Title ──────────┐
        closeText = "[X]"
        titleSeg = "\x2500 " ++ title ++ " "
        fillLen = max 0 (innerW - displayWidth titleSeg - displayWidth closeText)
        top = "\x250C" ++ titleSeg
              ++ replicate fillLen '\x2500'
              ++ closeText ++ "\x2510"
        bot = "\x2514" ++ replicate innerW '\x2500' ++ "\x2518"
        -- Pad content lines to fit the dialog, trimming if needed
        contentH = h - 2  -- rows available for content (inside top/bottom borders)
        padded = take contentH (lns ++ repeat "")
        ovRows = map (\l -> "\x2502 " ++ padR (innerW - 1) (trimToWidth (innerW - 1) l) ++ "\x2502") padded
    forM_ (zip [0..] (top : ovRows ++ [bot])) $ \(i,line) ->
        goto (r0+i) c0 >> setFg 36 >> bold >> putStr line >> resetSGR
    hFlush stdout

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
    , "   F2 / C           Contacts menu  (new session, browse peers, verify)"
    , "   F3 / T           Chat menu  (session info, export, clear)"
    , "   F4 / P           Preferences / settings overlay"
    , "   Q                Quit UmbraVOX"
    , "   Left / Right     Navigate open menu items"
    , ""
    , " Keyboard shortcuts"
    , "   Ctrl+N           Start a new connection"
    , "   Ctrl+G           Open group chat"
    , "   Ctrl+Q           Quit immediately"
    , ""
    , "[ Close ]" ]

renderHelpOverlay :: Layout -> IO ()
renderHelpOverlay lay = showOverlay lay "Help" helpOverlayLines

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

renderAboutOverlay :: Layout -> IO ()
renderAboutOverlay lay = showOverlay lay "About UmbraVOX" aboutOverlayLines

newConnOverlayLines :: [String]
newConnOverlayLines =
    [ " Choose a conversation type:"
    , ""
    , "   1. Private   \x2014  secure local notes (no peer, stored on this device only)"
    , "   2. Single    \x2014  end-to-end encrypted session with one remote peer"
    , "   3. Group     \x2014  multi-party encrypted conversation (Sender Keys)"
    , ""
    , "[ Private ]  [ Single ]  [ Group ]  [ Cancel ]" ]

renderNewConnOverlay :: Layout -> IO ()
renderNewConnOverlay lay = showOverlay lay "New Conversation" newConnOverlayLines

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
            Just ik -> do
                session <- readIORef (siSession si)
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

renderVerifyOverlay :: Layout -> AppState -> IO ()
renderVerifyOverlay lay st = showOverlay lay "Verify Keys" =<< verifyOverlayLines st

renderSettingsOverlay :: Layout -> AppState -> IO ()
renderSettingsOverlay lay st = showOverlay lay "Preferences" =<< settingsOverlayLines st

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
    let tf True = "ON"; tf False = "OFF"
        ephemeral = case mDb of { Nothing -> True; Just _ -> False }
    storageLines <- settingsStorageLines st tf dbEnabled ephemeral
    connMode <- readIORef (cfgConnectionMode (asConfig st))
    packagedPluginRuntimeCatalog <- readIORef (cfgPackagedPluginRuntimeCatalog (asConfig st))
    transportProviderRuntimeCatalog <- readIORef (cfgTransportProviderRuntimeCatalog (asConfig st))
    let modeLabel = settingsConnModeLabel connMode
        tabLine = tabRowLine settingsTabLabels tabIx
        ctx = SettingsCtx port name mdns pex debugLog debugPath modeLabel storageLines
                packagedPluginRuntimeCatalog transportProviderRuntimeCatalog
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
            , "   0. View/regenerate keys"
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
                , "   c. Connection mode: [" ++ sctxModeLabel ctx ++ "]"
                , "   0. View/regenerate keys"
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

renderKeysOverlay :: Layout -> AppState -> IO ()
renderKeysOverlay lay st = showOverlay lay "Identity & Keys" =<< keysOverlayLines st

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
                [ "", "[ Close ]" ]

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

renderBrowseOverlay :: Layout -> AppState -> IO ()
renderBrowseOverlay lay st = showOverlay lay "Browse Peers" =<< browseOverlayLines st

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

renderPromptOverlay :: Layout -> String -> String -> IO ()
renderPromptOverlay lay title buf = showOverlay lay title (promptOverlayLines title buf)

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

renderRegenKeyOverlay :: Layout -> AppState -> IO ()
renderRegenKeyOverlay lay st = do
    checked <- readIORef (asRegenCheckbox st)
    lns <- regenKeyOverlayLines checked
    showOverlay lay "Regenerate Identity Key" lns

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
        goto startRow col; setFg 36; bold; putStr topLine; resetSGR
        forM_ (zip [0..] items) $ \(i, label) -> do
            goto (startRow + 1 + i) col
            setFg 36; bold; putStr "\x2551"; resetSGR
            if i == selIdx then do
                csi "7m"  -- inverse video
                putStr (padR (boxW - 2) (" " ++ label))
                resetSGR
            else
                putStr (padR (boxW - 2) (" " ++ label))
            setFg 36; bold; putStr "\x2551"; resetSGR
        goto (startRow + 1 + length items) col
        setFg 36; bold; putStr botLine; resetSGR

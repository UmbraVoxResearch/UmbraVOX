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
    , renderPromptOverlay
    , renderDropdown
    ) where

import Control.Monad (forM_, when)
import Data.IORef (readIORef)
import Data.Char (toLower)
import Data.List (dropWhileEnd, intercalate)
import System.IO (hFlush, stdout)
import UmbraVox.BuildProfile
    ( bpId, bpName, buildChastityOnly, disabledPlugins
    , enabledPlugins, pluginLoadStatusLabel
    , pluginName, pmStatusTag, pprLoadStatus, pprManifest, pprPlugin
    )
import UmbraVox.Network.ProviderCatalog
    ( ProviderClass(..), ctpInherits, ctpLoadStatus
    , ctpManifest, ctpProvider, pmfEndpointTag, pmfStatusTag
    , providerEndpointSchema, providerIdLabel, providerLoadStatusLabel
    , renderProviderEndpoint, tpClass, tpName
    )
import UmbraVox.Network.ProviderRuntime (activeRuntimeProvider)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Terminal (goto, setFg, resetSGR, bold, padR, csi)
import UmbraVox.TUI.Layout (dropdownCol)
import UmbraVox.TUI.Constants (maxOverlayW, overlayMinMargin, minDropdownW)
import UmbraVox.TUI.PaginatedList (slicePage, psItems, psPage, psTotalPages)
import UmbraVox.TUI.Text (displayWidth, trimToWidth)
import UmbraVox.Protocol.QRCode (generateSafetyNumber, renderSafetyNumber,
                                    renderFingerprint, generateQRCode, renderQRCode)
import qualified Data.ByteString.Char8 as BC
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Network.MDNS (MDNSPeer(..))
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS

-- Overlays ----------------------------------------------------------------
overlayBounds :: Layout -> Int -> (Int, Int, Int, Int)
overlayBounds lay lineCount =
    let chatLeft = lLeftW lay + 1
        chatWidth = max 20 (lRightW lay - 1)
        chatTop = 2
        chatHeight = max 6 (lRows lay - 2)
        w = max 20 (min maxOverlayW (chatWidth - overlayMinMargin))
        h = lineCount + 2
        r0 = max chatTop (chatTop + ((chatHeight - h) `div` 2))
        c0 = max chatLeft (chatLeft + ((chatWidth - w) `div` 2))
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
    let (r0, c0, w, _) = overlayBounds lay (length lns)
        innerW = w - 2
        closeText = "[X]"
        titleText = " " ++ trimToWidth (max 0 (innerW - displayWidth closeText - 1)) title
        top = "\x2554" ++ titleText
              ++ replicate (max 0 (innerW - displayWidth titleText - displayWidth closeText)) '\x2550'
              ++ closeText ++ "\x2557"
        bot = "\x255A" ++ replicate innerW '\x2550' ++ "\x255D"
        ovRows = map (\l -> "\x2551 " ++ padR (w - 3) l ++ "\x2551") lns
    forM_ (zip [0..] (top : ovRows ++ [bot])) $ \(i,line) ->
        goto (r0+i) c0 >> setFg 36 >> bold >> putStr line >> resetSGR
    hFlush stdout

helpOverlayLines :: [String]
helpOverlayLines =
    [ "Navigation:"
    , "  Tab         Switch pane (Contacts/Chat)"
    , "  Up/Down     Navigate / scroll"
    , "  Enter       Send message / select"
    , "  Esc         Close dialog / menu"
    , ""
    , "Menus:"
    , "  F1 Help     F2 Contacts   F3 Chat"
    , "  F4 Prefs    Q Quit"
    , "  Arrow keys to navigate, Enter to select"
    , ""
    , "Shortcuts:"
    , "  Ctrl+N  New connection"
    , "  Ctrl+G  Group chat"
    , "  Ctrl+Q  Quit"
    , ""
    , "[ Close ]" ]

renderHelpOverlay :: Layout -> IO ()
renderHelpOverlay lay = showOverlay lay "Help" helpOverlayLines

aboutOverlayLines :: [String]
aboutOverlayLines =
    [ " UmbraVOX"
    , " Post-Quantum Encrypted Messaging"
    , ""
    , " Research-oriented encrypted messaging MVP"
    , ""
    , " License"
    , "   Apache License"
    , "   Version 2.0, January 2004"
    , "   See LICENSE for full terms"
    , ""
    , "[ Close ]" ]

renderAboutOverlay :: Layout -> IO ()
renderAboutOverlay lay = showOverlay lay "About UmbraVOX" aboutOverlayLines

newConnOverlayLines :: [String]
newConnOverlayLines =
    [ " 1. Private (secure notes, local only)"
    , " 2. Single  (connect to one peer)"
    , " 3. Group   (connect to multiple peers)"
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
                let ourKey  = ikX25519Public ik
                    peerKey = BC.pack (siPeerName si)
                    safetyNum = generateSafetyNumber ourKey peerKey
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
    storageLines <- if ephemeral
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
    connMode <- readIORef (cfgConnectionMode (asConfig st))
    packagedPluginRuntimeCatalog <- readIORef (cfgPackagedPluginRuntimeCatalog (asConfig st))
    transportProviderRuntimeCatalog <- readIORef (cfgTransportProviderRuntimeCatalog (asConfig st))
    let modeLabel = case connMode of
            Swing       -> "SWING"
            Promiscuous -> "PROMISCUOUS"
            Selective   -> "SELECTIVE"
            Chaste      -> "CHASTE"
            Chastity    -> "CHASTITY"
        tabLine = tabRowLine settingsTabLabels tabIx
    tabBody <-
        if buildChastityOnly
            then pure $
                case tabIx of
                    0 ->
                        [ " Simple"
                        , "   1. Listen port:    " ++ show port
                        , "   2. Display name:   " ++ name
                        , "   0. View/regenerate keys"
                        , ""
                        , " Discovery, storage, export/import, and logs"
                        , " are compile-time disabled in this build."
                        ]
                    1 ->
                        [ " Security"
                        , "   Build profile:     CHASTITY"
                        , "   Connection mode:   [" ++ modeLabel ++ "]"
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
            else
                case tabIx of
                    0 ->
                        pure
                            [ " Simple"
                            , "   1. Listen port:    " ++ show port
                            , "   2. Display name:   " ++ name
                            , "   3. mDNS (LAN):    [" ++ tf mdns ++ "]"
                            , "   c. Connection mode: [" ++ modeLabel ++ "]"
                            , "   0. View/regenerate keys"
                            ]
                    1 ->
                        pure
                            [ " Discovery"
                            , "   3. mDNS (LAN):    [" ++ tf mdns ++ "]"
                            , "   4. Peer Exchange: [" ++ tf pex ++ "]"
                            , ""
                            , " Peer discovery applies immediately."
                            ]
                    2 ->
                        pure ([ " Storage" ] ++ tail storageLines)
                    3 ->
                        pure
                            [ " Security"
                            , "   c. Connection mode: [" ++ modeLabel ++ "]"
                            , "   (Swing / Promiscuous / Selective / Chaste / Chastity)"
                            , ""
                            , " Switching mode renegotiates remote sessions."
                            ]
                    _ -> do
                        let packagedPluginLines = summarizePackagedPlugins packagedPluginRuntimeCatalog
                            providerLines = summarizeTransportProviders transportProviderRuntimeCatalog
                        pure $
                            [ " Advanced"
                            , "   a. Debug logging:  [" ++ tf debugLog ++ "]"
                            , "   b. Log path:       " ++ debugPath
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
    pure $
        [ tabLine
        , ""
        ] ++ tabBody ++
        [ ""
        , " Press Left/Right to switch tabs"
        , " Press 0-9/a/b/c to change, Esc to close"
        , "[ Close ]" ]
  where
    enabledPluginIds = map bpId enabledPlugins
    disabledPluginIds = map bpId disabledPlugins
    renderDisabledPlugin pid =
        "   - " ++ pluginName pid ++ ": [OFF]"
    summarizeBuiltInPlugins [] =
        [ "   - none" ]
    summarizeBuiltInPlugins pluginIds =
        map (\row -> "   - " ++ intercalate ", " (map pluginName row)) (chunkItems 3 pluginIds)
    summarizePackagedPlugins [] =
        [ "   - none discovered" ]
    summarizePackagedPlugins catalog =
        map (\row -> "   - " ++ intercalate ", " (map renderPackagedPlugin row)) (chunkItems 2 catalog)
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
    summarizeTransportProviders [] =
        [ "   - none discovered" ]
    summarizeTransportProviders catalog =
        renderProviderGroup ProviderDirectCarrier "   - Direct carriers" catalog
        ++ renderProviderGroup ProviderOverlayCarrier "   - Overlay carriers" catalog
        ++ renderProviderGroup ProviderOpenBridge "   - Open bridges" catalog
        ++ renderProviderGroup ProviderClosedBridge "   - Closed bridges" catalog
    renderProviderGroup providerClass label catalog =
        let entries = filter ((== providerClass) . tpClass . ctpProvider) catalog
        in if null entries
            then []
            else [label ++ ": " ++ intercalate ", " (map renderProviderEntry entries)]
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

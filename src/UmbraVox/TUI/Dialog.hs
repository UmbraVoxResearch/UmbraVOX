-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Dialog
    ( showOverlay
    , renderHelpOverlay, renderNewConnOverlay, renderVerifyOverlay
    , renderSettingsOverlay, renderKeysOverlay, renderBrowseOverlay
    , renderWelcomeOverlay, renderPromptOverlay
    , renderDropdown
    ) where

import Control.Monad (forM_, when)
import Data.IORef (readIORef)
import System.IO (hFlush, stdout)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Terminal (goto, setFg, resetSGR, bold, inverse, padR, csi)
import UmbraVox.TUI.Layout (dropdownCol)
import UmbraVox.TUI.Constants (maxOverlayW, overlayMinMargin, minDropdownW)
import UmbraVox.Protocol.QRCode (generateSafetyNumber, renderSafetyNumber,
                                    renderFingerprint, generateQRCode, renderQRCode)
import qualified Data.ByteString.Char8 as BC
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Network.MDNS (MDNSPeer(..))
import qualified Data.Map.Strict as Map

-- Overlays ----------------------------------------------------------------
showOverlay :: Layout -> String -> [String] -> IO ()
showOverlay lay title lns = do
    let w = min maxOverlayW (lCols lay - overlayMinMargin)
        h = length lns + 2
        r0 = max 2 ((lRows lay - h) `div` 2)
        c0 = max 1 ((lCols lay - w) `div` 2)
        top = "\x2554\x2550" ++ take (w-6) title ++ " "
              ++ replicate (max 0 (w-5-length (take (w-6) title))) '\x2550' ++ "\x2557"
        bot = "\x255A" ++ replicate (w-2) '\x2550' ++ "\x255D"
        ovRows = map (\l -> "\x2551 "++padR (w-3) (take (w-3) l)++"\x2551") lns
    forM_ (zip [0..] (top : ovRows ++ [bot])) $ \(i,line) ->
        goto (r0+i) c0 >> setFg 36 >> bold >> putStr line >> resetSGR
    hFlush stdout

renderHelpOverlay :: Layout -> IO ()
renderHelpOverlay lay = showOverlay lay "Help"
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
    , "  Ctrl+Q  Quit"
    , ""
    , "Press Esc to close" ]

renderNewConnOverlay :: Layout -> IO ()
renderNewConnOverlay lay = showOverlay lay "New Conversation"
    [" 1. \x1F512 Private (secure notes, local only)"
    ," 2. \x25CB Single  (connect to one peer)"
    ," 3. \x1F465 Group   (connect to multiple peers)"
    ,""
    ," Press 1/2/3, Esc to cancel"]

renderVerifyOverlay :: Layout -> AppState -> IO ()
renderVerifyOverlay lay st = do
    sessions <- readIORef (cfgSessions (asConfig st))
    sel <- readIORef (asSelected st)
    mIk <- readIORef (cfgIdentity (asConfig st))
    let entries = Map.toList sessions
    if sel >= 0 && sel < length entries then do
        let (_,si) = entries !! sel
        case mIk of
            Nothing -> showOverlay lay "Verify Keys"
                ["No identity generated yet.", "", "Press K to generate keys first.", "Press Esc to close"]
            Just ik -> do
                let ourKey  = ikX25519Public ik
                    peerKey = BC.pack (siPeerName si)
                    safetyNum = generateSafetyNumber ourKey peerKey
                    safetyRows = renderSafetyNumber safetyNum
                    qrMatrix = generateQRCode safetyNum
                    qrLines  = renderQRCode qrMatrix
                showOverlay lay "Verify Keys" $
                    ["Peer: " ++ siPeerName si, "", "Safety Number:"] ++ safetyRows ++
                    ["", "QR Code:"] ++ map ("  " ++) qrLines ++
                    ["", "Compare via a separate channel.", "Press Esc to close"]
    else showOverlay lay "Verify Keys"
        ["No contact selected", "", "Press Esc to close"]

renderSettingsOverlay :: Layout -> AppState -> IO ()
renderSettingsOverlay lay st = do
    port      <- readIORef (cfgListenPort (asConfig st))
    name      <- readIORef (cfgDisplayName (asConfig st))
    mdns      <- readIORef (cfgMDNSEnabled (asConfig st))
    pex       <- readIORef (cfgPEXEnabled (asConfig st))
    debugLog  <- readIORef (cfgDebugLogging (asConfig st))
    debugPath <- readIORef (cfgDebugLogPath (asConfig st))
    mDb       <- readIORef (cfgAnthonyDB (asConfig st))
    let tf True = "ON"; tf False = "OFF"
        ephemeral = case mDb of { Nothing -> True; Just _ -> False }
    storageLines <- if ephemeral
        then pure
            [ " Storage"
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
                 , "   5. DB path:       " ++ dbPath'
                 , "   6. Retention:     " ++ retLabel
                 , "   7. Auto-save msgs: [" ++ tf autoSave ++ "]"
                 , "   8. Clear history..." ]
    connMode <- readIORef (cfgConnectionMode (asConfig st))
    let modeLabel = case connMode of
            Swing       -> "SWING"
            Promiscuous -> "PROMISCUOUS"
            Selective   -> "SELECTIVE"
            Chaste      -> "CHASTE"
            Chastity    -> "CHASTITY"
    showOverlay lay "Preferences" $
        [ " General"
        , "   1. Listen port:    " ++ show port
        , "   2. Display name:   " ++ name
        , "   a. Debug logging:  [" ++ tf debugLog ++ "]"
        , "   b. Log path:       " ++ debugPath
        , ""
        , " Discovery"
        , "   3. mDNS (LAN):    [" ++ tf mdns ++ "]"
        , "   4. Peer Exchange: [" ++ tf pex ++ "]"
        , ""
        , " Security"
        , "   Connection mode:  [" ++ modeLabel ++ "]"
        , "   (Promiscuous / Selective / Chaste)"
        , "" ] ++ storageLines ++
        [ ""
        , " Identity"
        , "   0. View/regenerate keys"
        , ""
        , " Press 0-9/a/b to change, Esc to close" ]

renderKeysOverlay :: Layout -> AppState -> IO ()
renderKeysOverlay lay st = do
    mIk <- readIORef (cfgIdentity (asConfig st))
    case mIk of
        Nothing -> showOverlay lay "Identity & Keys" ["No identity generated yet.", "Press Esc to close"]
        Just ik -> do
            let x25519Lines  = renderFingerprint (ikX25519Public ik)
                ed25519Lines = renderFingerprint (ikEd25519Public ik)
                safetyNum = generateSafetyNumber (ikX25519Public ik) (ikX25519Public ik)
                qrLines = renderQRCode (generateQRCode safetyNum)
            showOverlay lay "Identity & Keys" $
                [ "X25519 fingerprint:" ] ++ x25519Lines ++
                [ "", "Ed25519 fingerprint:" ] ++ ed25519Lines ++
                [ "", "QR Code (X25519):" ] ++ map ("  " ++) qrLines ++
                [ "", "Press Esc to close" ]

renderBrowseOverlay :: Layout -> AppState -> IO ()
renderBrowseOverlay lay st = do
    peers <- readIORef (cfgMDNSPeers (asConfig st))
    mdnsOn <- readIORef (cfgMDNSEnabled (asConfig st))
    pexOn  <- readIORef (cfgPEXEnabled (asConfig st))
    let header = [ "Discovered peers on the local network:"
                 , "  mDNS: " ++ (if mdnsOn then "ON" else "OFF")
                    ++ "  |  PEX: " ++ (if pexOn then "ON" else "OFF")
                 , "" ]
        peerLines = if null peers
            then ["  (no peers discovered yet)"
                 , ""
                 , "  Make sure mDNS is enabled in Preferences"
                 , "  and other UmbraVOX instances are running"
                 , "  on the local network." ]
            else concatMap fmtPeer (zip [1::Int ..] peers)
        fmtPeer (i, p) =
            [ "  " ++ show i ++ ". " ++ mdnsIP p ++ ":" ++ show (mdnsPort p) ]
    showOverlay lay "Browse Peers" $
        header ++ peerLines ++
        [ "", " Press Esc to close" ]

renderWelcomeOverlay :: Layout -> IO ()
renderWelcomeOverlay lay = showOverlay lay "Welcome to UmbraVOX"
    [ " Post-Quantum Encrypted Messaging"
    , ""
    , " Getting started:"
    , "   Press F3 to open Chat menu"
    , "   Select 'New' to start a conversation"
    , ""
    , " Navigation:"
    , "   F1-F4      Open menus"
    , "   Tab        Switch panes"
    , "   Ctrl+N     Quick new connection"
    , "   Ctrl+Q     Quit"
    , ""
    , " Press Esc to dismiss" ]

renderPromptOverlay :: Layout -> String -> String -> IO ()
renderPromptOverlay lay title buf = showOverlay lay title
    ["Enter value:", "\x25B8 " ++ buf ++ "\x2588", "", "Press \x23CE to confirm, Esc to cancel"]

-- | Render a dropdown menu below its tab position
renderDropdown :: Layout -> MenuTab -> Int -> IO ()
renderDropdown lay tab selIdx = do
    let items = menuTabItems tab
        boxW = max minDropdownW (maximum (map length items) + 4)
        col = dropdownCol tab
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

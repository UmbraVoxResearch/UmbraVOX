module UmbraVox.TUI.Render
    ( -- * ANSI helpers
      esc, csi, goto, gotoL, clearScreen, clearFullScreen
    , hideCursor, showCursor
    , setFg, resetSGR, bold
    , padR, isPfx
    , withRawMode
      -- * Terminal size
    , getTermSize, clampSize, sizeValid, calcLayout
      -- * Rendering
    , render
    , showOverlay
    ) where

import Control.Exception (catch, SomeException, bracket_)
import Control.Monad (forM_, when, void)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import System.IO (hFlush, stdout, stdin, hSetBuffering, BufferMode(..),
                  hSetEcho)
import System.Process (readProcess)
import UmbraVox.TUI.Types
import UmbraVox.Protocol.QRCode (generateSafetyNumber, renderSafetyNumber, renderFingerprint,
                                    generateQRCode, renderQRCode)
import qualified Data.ByteString.Char8 as BC
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))

-- ANSI / helpers ----------------------------------------------------------
esc :: String -> String; esc code = "\ESC[" ++ code
csi :: String -> IO (); csi s = putStr (esc s)
goto :: Int -> Int -> IO (); goto r c = csi (show r ++ ";" ++ show c ++ "H")
gotoL :: Layout -> Int -> Int -> IO ()
gotoL lay r c = goto (lPadY lay + r) (lPadX lay + c)
clearScreen :: IO (); clearScreen = csi "2J" >> csi "H" >> hFlush stdout
clearFullScreen :: IO (); clearFullScreen = csi "2J" >> csi "H" >> hFlush stdout
hideCursor, showCursor :: IO ()
hideCursor = putStr "\ESC[?25l"; showCursor = putStr "\ESC[?25h"
setFg :: Int -> IO (); setFg c = csi (show c ++ "m")
resetSGR :: IO (); resetSGR = csi "0m"
bold :: IO (); bold = csi "1m"
padR :: Int -> String -> String; padR w s = take w (s ++ replicate w ' ')
isPfx :: String -> String -> Bool
isPfx [] _ = True; isPfx _ [] = False
isPfx (x:xs) (y:ys) = x == y && isPfx xs ys

withRawMode :: IO a -> IO a
withRawMode = bracket_
    (do hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        hideCursor
        -- Disable XON/XOFF flow control so Ctrl+Q/Ctrl+S reach the app
        void (readProcess "stty" ["-ixon", "-ixoff"] "" `catch`
              (\(_ :: SomeException) -> pure "")))
    (do hSetBuffering stdin LineBuffering
        hSetEcho stdin True
        showCursor
        -- Restore flow control
        void (readProcess "stty" ["ixon"] "" `catch`
              (\(_ :: SomeException) -> pure "")))

-- Terminal size detection -------------------------------------------------
getTermSize :: IO (Int, Int)  -- (rows, cols)
getTermSize = do
    result <- readProcess "stty" ["-F", "/dev/tty", "size"] ""
              `catch` (\(_ :: SomeException) -> pure "24 80")
    let cleaned = filter (\c -> (c >= '0' && c <= '9') || c == ' ') result
        ws = words cleaned
    case ws of
        [r, c] -> pure (read r, read c)
        _      -> pure (24, 80)

clampSize :: Int -> Int -> (Int, Int)
clampSize rows cols = (clamp 24 100 rows, clamp 80 300 cols)
  where clamp lo hi v = max lo (min hi v)

sizeValid :: Int -> Int -> Bool
sizeValid rows cols = rows >= 24 && rows <= 100 && cols >= 80 && cols <= 300

calcLayout :: Int -> Int -> Layout
calcLayout rows cols =
    let padX = max 1 (cols * 11 `div` 100)
        padY = max 1 (rows * 11 `div` 100)
        usableW = cols - 2 * padX
        usableH = rows - 2 * padY
    in Layout
    { lCols = usableW, lRows = usableH
    , lLeftW = max 25 (usableW `div` 4)
    , lRightW = usableW - max 25 (usableW `div` 4)
    , lChatH = usableH - 6
    , lPadX = padX, lPadY = padY
    }

-- Rendering ---------------------------------------------------------------
renderSizeWarning :: Int -> Int -> IO ()
renderSizeWarning rows cols = do
    csi "2J"; csi "H"
    let msg1 = "Terminal size: " ++ show cols ++ "x" ++ show rows ++ "."
        msg2 = "Required: min 80x24, max 300x100. Please resize."
        r1 = max 1 (rows `div` 2 - 1)
        c1a = max 1 ((cols - length msg1) `div` 2)
        c1b = max 1 ((cols - length msg2) `div` 2)
    goto r1 c1a; setFg 31; bold; putStr msg1; resetSGR
    goto (r1+1) c1b; setFg 31; bold; putStr msg2; resetSGR
    hFlush stdout

renderTopBorder :: Layout -> String -> IO ()
renderTopBorder lay peer = do
    let lw = lLeftW lay; rw = lRightW lay
        peerTrunc = take (rw - 11) peer
        chatLabel = "+-Chat: " ++ peerTrunc ++ " "
        rightFill = max 0 (rw - 9 - length peerTrunc)
    gotoL lay 1 1; setFg 36
    putStr $ "+-Contacts-" ++ replicate (lw - 12) '-'
          ++ chatLabel ++ replicate rightFill '-' ++ "+"
    resetSGR

renderContactRow :: Layout -> [(SessionId, SessionInfo)] -> Int -> Pane -> Int -> Int -> IO ()
renderContactRow lay entries sel focus cScroll row = do
    let lw = lLeftW lay
        idx = row + cScroll
    setFg 36; putStr "|"; resetSGR
    if idx < length entries then do
        let (_, si) = entries !! idx
        tag <- statusTag <$> readIORef (siStatus si)
        let mk = if idx == sel then " > " else "   "
            nm = take (lw - 8) (siPeerName si)
            cell = mk ++ padR (lw - 7 - length tag) nm ++ tag
        when (idx == sel) $ if focus == ContactPane then bold >> setFg 32 else bold
        putStr (take (lw - 2) cell); resetSGR
    else putStr (replicate (lw - 2) ' ')

renderPaneRow :: Layout -> [(SessionId, SessionInfo)] -> Maybe SessionInfo
              -> Int -> Pane -> Int -> Int -> Int -> Int -> IO ()
renderPaneRow lay entries selSi sel focus scroll' chatH' cScroll row = do
    let rw = lRightW lay
    gotoL lay (row + 2) 1
    renderContactRow lay entries sel focus cScroll row
    setFg 36; putStr "|"; resetSGR
    msg <- case selSi of
        Nothing -> pure ""
        Just si -> do
            hist <- readIORef (siHistory si)
            let msgs = reverse hist; total = length msgs
                start = max 0 (total - chatH' - scroll')
                idx = start + row
            pure $ if idx >= 0 && idx < total then msgs !! idx else ""
    putStr (padR (rw - 2) (take (rw - 2) msg))
    setFg 36; putStr "|"; resetSGR

renderMidBorder :: Layout -> Int -> IO ()
renderMidBorder lay chatH' = do
    let lw = lLeftW lay; rw = lRightW lay
    gotoL lay (chatH' + 2) 1; setFg 36
    putStr $ "+" ++ replicate (lw - 2) '-' ++ "+"
          ++ replicate (rw - 2) '-' ++ "+"
    resetSGR

renderInputRow :: Layout -> Pane -> String -> Int -> IO ()
renderInputRow lay focus buf chatH' = do
    let lw = lLeftW lay; rw = lRightW lay
    gotoL lay (chatH' + 3) 1; setFg 36; putStr "|"; resetSGR
    setFg 33; putStr (padR (lw - 2) " [N]ew [R]ename"); resetSGR
    setFg 36; putStr "|"; resetSGR
    if focus == ChatPane then do
        bold; setFg 32
        putStr (padR (rw - 2) (" > " ++ take (rw - 6) buf ++ "_"))
        resetSGR
    else
        putStr (padR (rw - 2) (" > " ++ take (rw - 6) buf))
    setFg 36; putStr "|"; resetSGR

renderHintRow :: Layout -> Int -> IO ()
renderHintRow lay chatH' = do
    let lw = lLeftW lay; rw = lRightW lay
    gotoL lay (chatH' + 4) 1; setFg 36; putStr "|"; resetSGR
    setFg 33; putStr (padR (lw - 2) " [K]eys [S]elf"); resetSGR
    setFg 36; putStr "|"; resetSGR
    putStr (replicate (rw - 2) ' '); setFg 36; putStr "|"; resetSGR

renderBottomBorder :: Layout -> Int -> IO ()
renderBottomBorder lay chatH' = do
    let lw = lLeftW lay; rw = lRightW lay
    gotoL lay (chatH' + 5) 1; setFg 36
    putStr $ "+" ++ replicate (lw - 2) '-' ++ "+"
          ++ replicate (rw - 2) '-' ++ "+"
    resetSGR

renderStatusBar :: Layout -> AppState -> String -> Int -> IO ()
renderStatusBar lay st status chatH' = do
    let totalW' = lCols lay
    gotoL lay (chatH' + 6) 1; setFg 30; csi "47m"
    mDb <- readIORef (cfgAnthonyDB (asConfig st))
    let mode = case mDb of { Nothing -> " [EPHEMERAL]"; Just _ -> "" }
        bar = " ^N:New ^P:Prefs ^Q:Quit | Tab:Switch" ++ mode
        full = if null status then bar
               else bar ++ " | " ++ take (totalW' - length bar - 4) status
    putStr (padR totalW' full); resetSGR

render :: AppState -> IO ()
render st = do
    (rawRows, rawCols) <- readIORef (asTermSize st)
    if not (sizeValid rawRows rawCols) then
        renderSizeWarning rawRows rawCols
    else do
        let (rows, cols) = clampSize rawRows rawCols
            lay = calcLayout rows cols
        writeIORef (asLayout st) lay
        clearFullScreen
        focus <- readIORef (asFocus st); sel <- readIORef (asSelected st)
        sessions <- readIORef (cfgSessions (asConfig st))
        buf <- readIORef (asInputBuf st); scroll <- readIORef (asChatScroll st)
        status <- readIORef (asStatusMsg st)
        cScroll <- readIORef (asContactScroll st)
        let entries = Map.toList sessions
            chatH' = lChatH lay
            selSi = if sel < length entries
                    then Map.lookup (fst (entries !! sel)) sessions else Nothing
            peer  = maybe "(no contact)" siPeerName selSi
        renderTopBorder lay peer
        forM_ [0..chatH'-1] $ \row ->
            renderPaneRow lay entries selSi sel focus scroll chatH' cScroll row
        renderMidBorder lay chatH'
        renderInputRow lay focus buf chatH'
        renderHintRow lay chatH'
        renderBottomBorder lay chatH'
        renderStatusBar lay st status chatH'
        dlg <- readIORef (asDialogMode st)
        case dlg of
            Just DlgHelp     -> renderHelpOverlay lay
            Just DlgKeys     -> renderKeysOverlay lay st
            Just DlgSettings -> renderSettingsOverlay lay st
            Just DlgNewConn  -> renderNewConnOverlay lay
            Just DlgVerify   -> renderVerifyOverlay lay st
            Just (DlgPrompt title _) -> do
                buf' <- readIORef (asDialogBuf st)
                renderPromptOverlay lay title buf'
            Nothing ->
                when (Map.null sessions) $
                    showOverlay lay "Welcome to UmbraVOX"
                        [ " Post-Quantum Encrypted Messaging"
                        , ""
                        , " Press N or Ctrl+N to start:"
                        , "   1. Private  - encrypted notes (local)"
                        , "   2. Single   - connect to a peer"
                        , "   3. Group    - multi-peer chat"
                        , ""
                        , " Press Ctrl+P for preferences"
                        , " Press ? for help" ]
        hFlush stdout

-- Overlays ----------------------------------------------------------------
showOverlay :: Layout -> String -> [String] -> IO ()
showOverlay lay title lns = do
    let w = min 60 (lCols lay - 4)
        h = length lns + 2
        r0 = max 2 ((lRows lay - h) `div` 2)
        c0 = max 1 ((lCols lay - w) `div` 2)
        top = "+-" ++ take (w-6) title ++ " "
              ++ replicate (max 0 (w-5-length (take (w-6) title))) '-' ++ "+"
        bot = "+" ++ replicate (w-2) '-' ++ "+"
        rows = map (\l -> "| "++padR (w-3) (take (w-3) l)++"|") lns
    forM_ (zip [0..] (top : rows ++ [bot])) $ \(i,line) ->
        gotoL lay (r0+i) c0 >> setFg 36 >> bold >> putStr line >> resetSGR
    hFlush stdout

renderHelpOverlay :: Layout -> IO ()
renderHelpOverlay lay = showOverlay lay "Help - UmbraVOX"
    [ "Tab         Switch pane focus"
    , "Up/Down     Navigate / scroll"
    , "Enter       Send message / select"
    , "Esc         Close dialog"
    , ""
    , "Contact pane shortcuts:"
    , "  N  New connection   G  Group details"
    , "  K  Identity & keys  S  Self notes"
    , "  R  Rename contact   ?  Show this help"
    , ""
    , "Global shortcuts:"
    , "  Ctrl+N  New connection"
    , "  Ctrl+R  Verify keys"
    , "  Ctrl+X  Export history"
    , "  Ctrl+P  Preferences"
    , "  Ctrl+Q  Quit"
    , "", "Press Esc to close" ]

renderNewConnOverlay :: Layout -> IO ()
renderNewConnOverlay lay = showOverlay lay "New Conversation"
    [" 1. Private (secure notes, local only)"
    ," 2. Single  (connect to one peer)"
    ," 3. Group   (connect to multiple peers)"
    ,""
    ," Press 1/2/3, Esc to cancel"]

renderVerifyOverlay :: Layout -> AppState -> IO ()
renderVerifyOverlay lay st = do
    sessions <- readIORef (cfgSessions (asConfig st))
    sel <- readIORef (asSelected st)
    mIk <- readIORef (cfgIdentity (asConfig st))
    let entries = Map.toList sessions
    if sel < length entries then do
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
    showOverlay lay "Preferences" $
        [ " General"
        , "   1. Listen port:    " ++ show port
        , "   2. Display name:   " ++ name
        , ""
        , " Discovery"
        , "   3. mDNS (LAN):    [" ++ tf mdns ++ "]"
        , "   4. Peer Exchange: [" ++ tf pex ++ "]"
        , "" ] ++ storageLines ++
        [ ""
        , " Identity"
        , "   0. View/regenerate keys"
        , ""
        , " Press 0-9 to change, Esc to close" ]

renderKeysOverlay :: Layout -> AppState -> IO ()
renderKeysOverlay lay st = do
    mIk <- readIORef (cfgIdentity (asConfig st))
    case mIk of
        Nothing -> showOverlay lay "Identity & Keys" ["No identity generated yet.", "Press Esc to close"]
        Just ik -> do
            let x25519Lines  = renderFingerprint (ikX25519Public ik)
                ed25519Lines = renderFingerprint (ikEd25519Public ik)
            showOverlay lay "Identity & Keys" $
                [ "X25519 fingerprint:" ] ++ x25519Lines ++
                [ "", "Ed25519 fingerprint:" ] ++ ed25519Lines ++
                [ "", "Press Esc to close" ]

renderPromptOverlay :: Layout -> String -> String -> IO ()
renderPromptOverlay lay title buf = showOverlay lay title
    ["Enter value:", "> " ++ buf ++ "_", "", "Press Enter to confirm, Esc to cancel"]

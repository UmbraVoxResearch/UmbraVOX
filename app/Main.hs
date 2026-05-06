module Main (main) where

import Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import Control.Exception (catch, SomeException, bracket_)
import Control.Monad (void, when, forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Bits (shiftL, shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout, stdin, hSetBuffering, BufferMode(..),
                   hSetEcho, hReady)
import System.Process (readProcess)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import UmbraVox.Chat.Session
    (ChatSession, initChatSession, sendChatMessage, recvChatMessage)
import UmbraVox.Crypto.MLKEM (mlkemKeyGen,
    MLKEMEncapKey(..), MLKEMDecapKey(..), MLKEMCiphertext(..))
import UmbraVox.Crypto.BIP39 (generatePassphrase)
import UmbraVox.Crypto.Export (encryptExport, decryptExport)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.Signal.PQXDH
    (PQPreKeyBundle(..), PQXDHResult(..), pqxdhInitiate, pqxdhRespond)
import UmbraVox.Crypto.Signal.X3DH
    (KeyPair(..), IdentityKey(..), generateIdentityKey, generateKeyPair,
     signPreKey)
import UmbraVox.Network.Transport
    (Transport(..), listen, connect, send, recv, close)
import UmbraVox.Protocol.CBOR (encodeMessage)
import UmbraVox.Protocol.QRCode (generateSafetyNumber, renderSafetyNumber, renderFingerprint,
                                    generateQRCode, renderQRCode)
-- Types -------------------------------------------------------------------
type SessionId = Int
data ContactStatus = Online | Offline | Local | Group deriving stock (Eq)
statusTag :: ContactStatus -> String
statusTag Online = "[ON]"; statusTag Offline = "[OFF]"
statusTag Local = "[LOCAL]"; statusTag Group = "[GRP]"

data SessionInfo = SessionInfo
    { siTransport :: Maybe Transport, siSession :: IORef ChatSession
    , siRecvTid :: Maybe ThreadId, siPeerName :: String
    , siHistory :: IORef [String], siStatus :: IORef ContactStatus }
data Pane = ContactPane | ChatPane deriving stock (Eq)
data AppState = AppState
    { asConfig :: AppConfig, asSelected :: IORef Int, asFocus :: IORef Pane
    , asInputBuf :: IORef String, asDialogBuf :: IORef String
    , asChatScroll :: IORef Int
    , asStatusMsg :: IORef String, asRunning :: IORef Bool
    , asDialogMode :: IORef (Maybe DialogMode)
    , asLayout :: IORef Layout
    , asContactScroll :: IORef Int }
data DialogMode = DlgHelp | DlgSettings | DlgVerify | DlgNewConn
    | DlgKeys | DlgPrompt String (String -> IO ())
data AppConfig = AppConfig
    { cfgListenPort :: IORef Int, cfgDisplayName :: IORef String
    , cfgIdentity :: IORef (Maybe IdentityKey)
    , cfgSessions :: IORef (Map SessionId SessionInfo)
    , cfgNextId :: IORef SessionId }

data Layout = Layout
    { lCols :: Int, lRows :: Int
    , lLeftW :: Int, lRightW :: Int, lChatH :: Int }
    deriving stock (Eq)

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
calcLayout rows cols = Layout
    { lCols = cols, lRows = rows
    , lLeftW = max 18 (cols `div` 4)
    , lRightW = cols - max 18 (cols `div` 4)
    , lChatH = rows - 6
    }
-- ANSI / helpers ----------------------------------------------------------
esc :: String -> String; esc code = "\ESC[" ++ code
csi :: String -> IO (); csi s = putStr (esc s)
goto :: Int -> Int -> IO (); goto r c = csi (show r ++ ";" ++ show c ++ "H")
clearScreen :: IO (); clearScreen = csi "2J" >> csi "H" >> hFlush stdout
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
    (hSetBuffering stdin NoBuffering >> hSetEcho stdin False >> hideCursor)
    (hSetBuffering stdin LineBuffering >> hSetEcho stdin True >> showCursor)
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
    goto 1 1; setFg 36
    putStr $ "+-Contacts-" ++ replicate (lw - 12) '-'
          ++ chatLabel ++ replicate rightFill '-' ++ "+"
    resetSGR

renderContactRow :: Layout -> [(SessionId, SessionInfo)] -> Int -> Pane -> Int -> Int -> IO ()
renderContactRow lay entries sel focus cScroll row = do
    let lw = lLeftW lay
        idx = row + cScroll  -- offset by contact scroll
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
    goto (row + 2) 1
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
    goto (chatH' + 2) 1; setFg 36
    putStr $ "+" ++ replicate (lw - 2) '-' ++ "+"
          ++ replicate (rw - 2) '-' ++ "+"
    resetSGR

renderInputRow :: Layout -> Pane -> String -> Int -> IO ()
renderInputRow lay focus buf chatH' = do
    let lw = lLeftW lay; rw = lRightW lay
    goto (chatH' + 3) 1; setFg 36; putStr "|"; resetSGR
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
    goto (chatH' + 4) 1; setFg 36; putStr "|"; resetSGR
    setFg 33; putStr (padR (lw - 2) " [K]eys [S]elf"); resetSGR
    setFg 36; putStr "|"; resetSGR
    putStr (replicate (rw - 2) ' '); setFg 36; putStr "|"; resetSGR

renderBottomBorder :: Layout -> Int -> IO ()
renderBottomBorder lay chatH' = do
    let lw = lLeftW lay; rw = lRightW lay
    goto (chatH' + 5) 1; setFg 36
    putStr $ "+" ++ replicate (lw - 2) '-' ++ "+"
          ++ replicate (rw - 2) '-' ++ "+"
    resetSGR

renderStatusBar :: Layout -> String -> Int -> IO ()
renderStatusBar lay status chatH' = do
    let totalW' = lCols lay
    goto (chatH' + 6) 1; setFg 30; csi "47m"
    let bar = " ^N:New ^R:Verify ^X:Export ^P:Prefs ^Q:Quit | Tab:Switch"
        full = if null status then bar
               else bar ++ " | " ++ take (totalW' - length bar - 4) status
    putStr (padR totalW' full); resetSGR

render :: AppState -> IO ()
render st = do
    (rawRows, rawCols) <- getTermSize
    if not (sizeValid rawRows rawCols) then
        renderSizeWarning rawRows rawCols
    else do
        let (rows, cols) = clampSize rawRows rawCols
            lay = calcLayout rows cols
        writeIORef (asLayout st) lay
        csi "H"
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
        renderStatusBar lay status chatH'
        -- Redraw active dialog overlay on top of main UI
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
            Nothing -> pure ()
        hFlush stdout
-- Input handling ----------------------------------------------------------
-- Ctrl key codes: Ctrl+A=0x01..Ctrl+Z=0x1A
-- Avoiding conflicts: Ctrl+H=0x08=backspace, Ctrl+I=0x09=tab, Ctrl+M=0x0D=enter
-- Safe choices: Ctrl+N=0x0E, Ctrl+W=0x17, Ctrl+R=0x12, Ctrl+X=0x18,
--               Ctrl+P=0x10, Ctrl+Q=0x11, Ctrl+L=0x0C, Ctrl+D=0x04
data InputEvent = KeyChar Char | KeyEnter | KeyTab | KeyBackspace | KeyEscape
    | KeyUp | KeyDown | KeyPageUp | KeyPageDown
    | KeyCtrlN | KeyCtrlW | KeyCtrlR | KeyCtrlX
    | KeyCtrlP | KeyCtrlQ | KeyCtrlL | KeyCtrlD
    | KeyUnknown

readKey :: IO InputEvent
readKey = do
    c <- getChar
    case c of
        '\n'   -> pure KeyEnter
        '\r'   -> pure KeyEnter
        '\t'   -> pure KeyTab
        '\DEL' -> pure KeyBackspace
        '\x7F' -> pure KeyBackspace  -- DEL on some terminals
        '\x08' -> pure KeyBackspace  -- BS / Ctrl+H
        '\x0E' -> pure KeyCtrlN     -- Ctrl+N = New connection
        '\x17' -> pure KeyCtrlW     -- Ctrl+W = close/Quit
        '\x12' -> pure KeyCtrlR     -- Ctrl+R = veRify keys
        '\x18' -> pure KeyCtrlX     -- Ctrl+X = eXport
        '\x10' -> pure KeyCtrlP     -- Ctrl+P = Preferences/settings
        '\x11' -> pure KeyCtrlQ     -- Ctrl+Q = Quit
        '\x0C' -> pure KeyCtrlL     -- Ctrl+L = redraw/cLear
        '\x04' -> pure KeyCtrlD     -- Ctrl+D = EOF/Done
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
        '5' -> drainTilde >> pure KeyPageUp
        '6' -> drainTilde >> pure KeyPageDown
        _ -> drainSeq >> pure KeyUnknown
  where drainTilde = hReady stdin >>= \r -> when r (void getChar)

readSS3 :: IO InputEvent
readSS3 = getChar >> pure KeyUnknown

drainSeq :: IO ()
drainSeq = hReady stdin >>= \r -> when r (void getChar >> drainSeq)
-- Main / event loop -------------------------------------------------------
main :: IO ()
main = do
    hSetBuffering stdout (BlockBuffering (Just 8192))
    cfg <- AppConfig <$> newIORef 9999 <*> newIORef "User"
                     <*> newIORef Nothing <*> newIORef Map.empty <*> newIORef 1
    (initRows, initCols) <- getTermSize
    let (r0, c0) = clampSize initRows initCols
    st <- AppState cfg <$> newIORef 0 <*> newIORef ContactPane
                       <*> newIORef "" <*> newIORef ""
                       <*> newIORef 0 <*> newIORef ""
                       <*> newIORef True <*> newIORef Nothing
                       <*> newIORef (calcLayout r0 c0)
                       <*> newIORef 0
    withRawMode $ clearScreen >> render st >> eventLoop st

eventLoop :: AppState -> IO ()
eventLoop st = do
    running <- readIORef (asRunning st)
    when running $ do
        key <- readKey
        dlg <- readIORef (asDialogMode st)
        case dlg of { Just _ -> handleDialog st key; Nothing -> handleNormal st key }
        readIORef (asRunning st) >>= \r -> when r (render st >> eventLoop st)
-- Normal key handling -----------------------------------------------------
handleNormal :: AppState -> InputEvent -> IO ()
handleNormal st key = do
    focus <- readIORef (asFocus st)
    case key of
        KeyTab   -> modifyIORef' (asFocus st) (\p -> if p==ContactPane then ChatPane else ContactPane)
        KeyCtrlN -> startNewConn st
        KeyCtrlR -> startVerify st
        KeyCtrlX -> startExport st
        KeyCtrlP -> startSettings st
        KeyCtrlQ -> quitApp st
        KeyCtrlW -> quitApp st
        KeyCtrlL -> clearScreen
        KeyCtrlD -> quitApp st
        KeyEscape -> do
            dlg <- readIORef (asDialogMode st)
            case dlg of
                Just _  -> writeIORef (asDialogMode st) Nothing
                Nothing -> pure ()
        _ -> case focus of { ContactPane -> handleContact st key; ChatPane -> handleChat st key }

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
            modifyIORef' (asSelected st) (\i -> min (n-1) (i+1))
            adjustContactScroll st visRows
        KeyEnter -> writeIORef (asFocus st) ChatPane >> writeIORef (asChatScroll st) 0
        KeyChar 'n' -> startNewConn st; KeyChar 'N' -> startNewConn st
        KeyChar 'g' -> setStatus st "Group: add peers with N, msgs go to all"
        KeyChar 'G' -> setStatus st "Group: add peers with N, msgs go to all"
        KeyChar 'r' -> renameContact st; KeyChar 'R' -> renameContact st
        KeyChar 'k' -> startKeysView st; KeyChar 'K' -> startKeysView st
        KeyChar 's' -> addSecureNotes st; KeyChar 'S' -> addSecureNotes st
        KeyChar '?' -> showHelp st
        _ -> pure ()

adjustContactScroll :: AppState -> Int -> IO ()
adjustContactScroll st visRows = do
    sel <- readIORef (asSelected st)
    cScroll <- readIORef (asContactScroll st)
    when (sel < cScroll) $ writeIORef (asContactScroll st) sel
    when (sel >= cScroll + visRows) $
        writeIORef (asContactScroll st) (sel - visRows + 1)

handleChat :: AppState -> InputEvent -> IO ()
handleChat st key = do
    lay <- readIORef (asLayout st)
    let ch = lChatH lay
    case key of
        KeyChar c    -> modifyIORef' (asInputBuf st) (++[c])
        KeyBackspace -> modifyIORef' (asInputBuf st) (\s -> if null s then s else init s)
        KeyEnter     -> sendCurrentMessage st
        KeyUp        -> modifyIORef' (asChatScroll st) (+1)
        KeyDown      -> modifyIORef' (asChatScroll st) (\s -> max 0 (s-1))
        KeyPageUp    -> modifyIORef' (asChatScroll st) (+ch)
        KeyPageDown  -> modifyIORef' (asChatScroll st) (\s -> max 0 (s-ch))
        _            -> pure ()

sendCurrentMessage :: AppState -> IO ()
sendCurrentMessage st = do
    buf <- readIORef (asInputBuf st)
    unless (null buf) $ do
        sessions <- readIORef (cfgSessions (asConfig st))
        sel <- readIORef (asSelected st)
        let entries = Map.toList sessions
        when (sel < length entries) $ do
            let (_,si) = entries !! sel
            if isPfx "/file " buf then do
                let path = drop 6 buf
                exists <- doesFileExist path
                if exists then do
                    contents <- BS.readFile path
                    sendToSession si (BC.pack ("/file:"++path++":") <> contents)
                    now <- timestamp
                    modifyIORef' (siHistory si)
                        (("["++now++"] You: [sent file "++path++"]"):)
                else setStatus st "File not found"
            else do
                sendToSession si (BC.pack buf)
                now <- timestamp
                modifyIORef' (siHistory si) (("["++now++"] You: "++buf):)
            writeIORef (asInputBuf st) ""
            writeIORef (asChatScroll st) 0
-- Dialogs -----------------------------------------------------------------
handleDialog :: AppState -> InputEvent -> IO ()
handleDialog st KeyEscape = writeIORef (asDialogMode st) Nothing
handleDialog st key = do
    dlg <- readIORef (asDialogMode st)
    case dlg of
        Just DlgHelp    -> writeIORef (asDialogMode st) Nothing
        Just DlgKeys    -> writeIORef (asDialogMode st) Nothing
        Just DlgVerify  -> writeIORef (asDialogMode st) Nothing
        Just DlgSettings -> handleSettingsDlg st key
        Just DlgNewConn  -> handleNewConnDlg st key
        Just (DlgPrompt _ cb) -> case key of
            KeyEnter -> do
                b <- readIORef (asDialogBuf st); cb b
                writeIORef (asDialogBuf st) ""
                writeIORef (asDialogMode st) Nothing
            KeyChar c -> modifyIORef' (asDialogBuf st) (++[c])
            KeyBackspace -> modifyIORef' (asDialogBuf st) (\s -> if null s then s else init s)
            _ -> pure ()
        _ -> writeIORef (asDialogMode st) Nothing

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
        goto (r0+i) c0 >> setFg 36 >> bold >> putStr line >> resetSGR
    hFlush stdout

renderHelpOverlay :: Layout -> IO ()
renderHelpOverlay lay = showOverlay lay "Help - UmbraVOX"
    [ "Tab         Switch pane focus"
    , "Up/Down     Navigate / scroll"
    , "Enter       Send message / select"
    , "Esc         Close dialog"
    , ""
    , "Contact pane shortcuts:"
    , "  N  New connection   G  Group"
    , "  K  Identity & keys  S  Self notes"
    , ""
    , "Global shortcuts:"
    , "  Ctrl+N  New connection"
    , "  Ctrl+R  Verify keys"
    , "  Ctrl+X  Export history"
    , "  Ctrl+P  Preferences"
    , "  Ctrl+Q  Quit"
    , "", "Press Esc to close" ]

renderNewConnOverlay :: Layout -> IO ()
renderNewConnOverlay lay = showOverlay lay "New Connection"
    ["1. Connect to peer (enter host:port)"
    ,"2. Listen for peer"
    ,"3. Loopback test (self)", "", "Press 1/2/3, Esc to cancel"]

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
                    rows = renderSafetyNumber safetyNum
                    qrMatrix = generateQRCode safetyNum
                    qrLines  = renderQRCode qrMatrix
                showOverlay lay "Verify Keys" $
                    ["Peer: " ++ siPeerName si, "", "Safety Number:"] ++ rows ++
                    ["", "QR Code:"] ++ map ("  " ++) qrLines ++
                    ["", "Compare via a separate channel.", "Press Esc to close"]
    else showOverlay lay "Verify Keys"
        ["No contact selected", "", "Press Esc to close"]

renderSettingsOverlay :: Layout -> AppState -> IO ()
renderSettingsOverlay lay st = do
    port <- readIORef (cfgListenPort (asConfig st))
    name <- readIORef (cfgDisplayName (asConfig st))
    showOverlay lay "Settings"
        [ "1. Listen port: " ++ show port
        , "2. Display name: " ++ name, ""
        , "Press 1/2 to change, Esc to close" ]

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

showHelp :: AppState -> IO ()
showHelp st = writeIORef (asDialogMode st) (Just DlgHelp)

startSettings :: AppState -> IO ()
startSettings st = writeIORef (asDialogMode st) (Just DlgSettings)

handleSettingsDlg :: AppState -> InputEvent -> IO ()
handleSettingsDlg st (KeyChar '1') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Set Port" $ \val ->
        case reads val of { [(p,_)] -> writeIORef (cfgListenPort (asConfig st)) (p::Int); _ -> pure () }))
handleSettingsDlg st (KeyChar '2') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Set Display Name" $ \val ->
        unless (null val) $ writeIORef (cfgDisplayName (asConfig st)) val))
handleSettingsDlg st _ = writeIORef (asDialogMode st) Nothing

setStatus :: AppState -> String -> IO ()
setStatus st msg = writeIORef (asStatusMsg st) msg
-- Actions -----------------------------------------------------------------
startNewConn :: AppState -> IO ()
startNewConn st = writeIORef (asDialogMode st) (Just DlgNewConn)

handleNewConnDlg :: AppState -> InputEvent -> IO ()
handleNewConnDlg st (KeyChar '1') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Connect (host:port)" $ \val -> do
        let (host, portStr) = break (==':') val
            port = if null portStr then 9999 else read (drop 1 portStr) :: Int
            h = if null host then "127.0.0.1" else host
        setStatus st ("Connecting to " ++ h ++ ":" ++ show port ++ "...")
        void $ forkIO $ (do
            t <- connect h port
            session <- handshakeInitiator t
            sid <- addSession (asConfig st) t session (h ++ ":" ++ show port)
            selectLast st; setStatus st ("Connected #" ++ show sid)
            ) `catch` (\(e::SomeException) -> setStatus st ("Failed: "++show e))
        ))
handleNewConnDlg st (KeyChar '2') = do
    writeIORef (asDialogMode st) Nothing
    port <- readIORef (cfgListenPort (asConfig st))
    setStatus st ("Listening on " ++ show port ++ "...")
    void $ forkIO $ (do
        t <- listen port; session <- handshakeResponder t
        sid <- addSession (asConfig st) t session ("peer:"++show port)
        selectLast st; setStatus st ("Session #" ++ show sid ++ " established")
        ) `catch` (\(e::SomeException) -> setStatus st ("Failed: "++show e))
handleNewConnDlg st (KeyChar '3') = do
    writeIORef (asDialogMode st) Nothing
    setStatus st "Starting loopback..."
    void $ forkIO $ (do
        let port = 19999
        void $ forkIO $ do
            tS <- listen port; sB <- handshakeResponder tS
            rB <- newIORef sB; hB <- newIORef []
            recvLoopTUI tS rB hB
        threadDelay 500000
        tC <- connect "127.0.0.1" port; sA <- handshakeInitiator tC
        sid <- addSession (asConfig st) tC sA ("loopback:"++show port)
        selectLast st; setStatus st ("Loopback #" ++ show sid ++ " ready")
        ) `catch` (\(e::SomeException) -> setStatus st ("Failed: "++show e))
handleNewConnDlg st _ = writeIORef (asDialogMode st) Nothing

selectLast :: AppState -> IO ()
selectLast st = do
    n <- Map.size <$> readIORef (cfgSessions (asConfig st))
    writeIORef (asSelected st) (max 0 (n-1))
    writeIORef (asFocus st) ChatPane  -- auto-focus chat for new conversations
    writeIORef (asChatScroll st) 0

renameContact :: AppState -> IO ()
renameContact st = do
    sessions <- readIORef (cfgSessions (asConfig st))
    sel <- readIORef (asSelected st)
    let entries = Map.toList sessions
    when (sel < length entries) $ do
        let (sid, si) = entries !! sel
        writeIORef (asDialogBuf st) ""
        writeIORef (asDialogMode st) (Just (DlgPrompt ("Rename: " ++ siPeerName si) $ \val ->
            unless (null val) $ do
                let si' = si { siPeerName = val }
                modifyIORef' (cfgSessions (asConfig st)) (Map.insert sid si')
                setStatus st ("Renamed to: " ++ val)
            ))

addSecureNotes :: AppState -> IO ()
addSecureNotes st = do
    sid <- addLoopbackSession (asConfig st) "\x1F512 Secure Notes"
    selectLast st; setStatus st ("Secure Notes #" ++ show sid)

startKeysView :: AppState -> IO ()
startKeysView st = do
    writeIORef (asDialogMode st) (Just DlgKeys)
    mIk <- readIORef (cfgIdentity (asConfig st))
    case mIk of
        Nothing -> do ik <- genIdentity; writeIORef (cfgIdentity (asConfig st)) (Just ik)
        Just _  -> pure ()

startVerify :: AppState -> IO ()
startVerify st = writeIORef (asDialogMode st) (Just DlgVerify)

startExport :: AppState -> IO ()
startExport st = do
    sessions <- readIORef (cfgSessions (asConfig st))
    sel <- readIORef (asSelected st)
    let entries = Map.toList sessions
    if sel < length entries then do
        writeIORef (asDialogBuf st) ""
        writeIORef (asDialogMode st) (Just (DlgPrompt "Export path (or 'import')" $ \val ->
            if val == "import" then startImport st
            else exportToPath st val))
    else setStatus st "No contact selected"

-- | Prompt for password then encrypt and write the export.
exportToPath :: AppState -> String -> IO ()
exportToPath st path = do
    let path' = if null path then "umbravox_export.enc" else path
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Password (empty=BIP39)" $ \pw -> do
        password <- if null pw then do
            phrase <- generatePassphrase 6
            setStatus st ("BIP39 passphrase: " ++ phrase)
            pure (BC.pack phrase)
          else pure (BC.pack pw)
        sessions <- readIORef (cfgSessions (asConfig st))
        sel <- readIORef (asSelected st)
        let entries = Map.toList sessions
        when (sel < length entries) $ do
            let (_,si) = entries !! sel
            hist <- readIORef (siHistory si)
            let plaintext = BC.pack (unlines (reverse hist))
            blob <- encryptExport password plaintext
            BS.writeFile path' blob
            setStatus st ("Encrypted " ++ show (length hist)
                         ++ " msgs to " ++ path')
        ))

-- | Prompt for file path and password, decrypt, and load into history.
startImport :: AppState -> IO ()
startImport st = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Import file path" $ \path -> do
        exists <- doesFileExist path
        if not exists then setStatus st ("File not found: " ++ path)
        else do
            writeIORef (asDialogBuf st) ""
            writeIORef (asDialogMode st) (Just (DlgPrompt "Import password" $ \pw -> do
                blob <- BS.readFile path
                case decryptExport (BC.pack pw) blob of
                    Nothing -> setStatus st "Decryption failed (wrong password?)"
                    Just plaintext -> do
                        sessions <- readIORef (cfgSessions (asConfig st))
                        sel <- readIORef (asSelected st)
                        let entries = Map.toList sessions
                        when (sel < length entries) $ do
                            let (_,si) = entries !! sel
                                msgs = lines (BC.unpack plaintext)
                            modifyIORef' (siHistory si) (reverse msgs ++)
                            setStatus st ("Imported " ++ show (length msgs)
                                         ++ " msgs from " ++ path)
                ))
        ))

quitApp :: AppState -> IO ()
quitApp st = do
    writeIORef (asRunning st) False
    sessions <- readIORef (cfgSessions (asConfig st))
    forM_ (Map.elems sessions) $ \si -> do
        maybe (pure ()) killThread (siRecvTid si)
        maybe (pure ()) close (siTransport si)
    clearScreen; goto 1 1; showCursor; putStrLn "Goodbye."; hFlush stdout
-- Session management ------------------------------------------------------
addSession :: AppConfig -> Transport -> ChatSession -> String -> IO SessionId
addSession cfg t session peerName = do
    sid <- readIORef (cfgNextId cfg); writeIORef (cfgNextId cfg) (sid+1)
    ref <- newIORef session; histRef <- newIORef []; stRef <- newIORef Online
    tid <- forkIO (recvLoopTUI t ref histRef)
    let si = SessionInfo (Just t) ref (Just tid) peerName histRef stRef
    modifyIORef' (cfgSessions cfg) (Map.insert sid si); pure sid

addLoopbackSession :: AppConfig -> String -> IO SessionId
addLoopbackSession cfg label = do
    sid <- readIORef (cfgNextId cfg); writeIORef (cfgNextId cfg) (sid+1)
    secret <- randomBytes 32; dhSec <- randomBytes 32; peerPub <- randomBytes 32
    session <- initChatSession secret dhSec peerPub
    ref <- newIORef session; histRef <- newIORef []; stRef <- newIORef Local
    let si = SessionInfo Nothing ref Nothing label histRef stRef
    modifyIORef' (cfgSessions cfg) (Map.insert sid si); pure sid

sendToSession :: SessionInfo -> BS.ByteString -> IO ()
sendToSession si msg = do
    session <- readIORef (siSession si)
    (session', wire) <- sendChatMessage session msg
    writeIORef (siSession si) session'
    case siTransport si of
        Just t  -> send t (encodeMessage wire)
        Nothing -> do
            session2 <- readIORef (siSession si)
            result <- recvChatMessage session2 wire
            case result of
                Just (session3, _pt) -> writeIORef (siSession si) session3
                Nothing -> pure ()
            ts <- timestamp
            modifyIORef' (siHistory si) ((ts++" [saved] "++BC.unpack msg):)
-- Receive loop (background thread) ----------------------------------------
recvLoopTUI :: Transport -> IORef ChatSession -> IORef [String] -> IO ()
recvLoopTUI t ref histRef = go `catch` handler where
    go = do
        lenBs <- recv t 4
        if BS.length lenBs < 4
            then modifyIORef' histRef ("  [Peer disconnected]":)
            else do
                let !len = fromIntegral (getW32BE lenBs)
                payload <- recv t len
                session <- readIORef ref
                result <- recvChatMessage session payload
                case result of
                    Nothing -> modifyIORef' histRef ("  [Decryption failed]":) >> go
                    Just (session', plaintext) -> do
                        writeIORef ref session'
                        now <- timestamp
                        modifyIORef' histRef (("["++now++"] Peer: "++BC.unpack plaintext):)
                        go
    handler :: SomeException -> IO ()
    handler _ = modifyIORef' histRef ("  [Connection lost]":)
-- Helpers -----------------------------------------------------------------
timestamp :: IO String
timestamp = formatTime defaultTimeLocale "%H:%M" <$> getCurrentTime

fingerprint :: BS.ByteString -> String
fingerprint bs = concatMap hex2 (BS.unpack (BS.take 8 bs)) where
    hex2 w = [hexC (w `shiftR` 4), hexC (w .&. 0x0f), ':']
    hexC n | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
           | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)
-- PQXDH key generation ----------------------------------------------------
genIdentity :: IO IdentityKey
genIdentity = do
    edSec <- randomBytes 32; xSec <- randomBytes 32
    pure $! generateIdentityKey edSec xSec

genSignedPreKey :: IdentityKey -> IO (KeyPair, BS.ByteString)
genSignedPreKey ik = do
    spkSec <- randomBytes 32
    let !spk = generateKeyPair spkSec; !sig = signPreKey ik (kpPublic spk)
    pure (spk, sig)

genPQPreKey :: IO (MLKEMEncapKey, MLKEMDecapKey)
genPQPreKey = do
    d <- randomBytes 32; z <- randomBytes 32; pure $! mlkemKeyGen d z
-- Prekey bundle wire format -----------------------------------------------
serializeBundle :: IdentityKey -> BS.ByteString -> BS.ByteString
                -> MLKEMEncapKey -> Maybe BS.ByteString -> BS.ByteString
serializeBundle ik spkPub spkSig (MLKEMEncapKey pqpk) mOpk = BS.concat
    [ ikX25519Public ik, ikEd25519Public ik, spkPub, spkSig
    , putW32BE (fromIntegral (BS.length pqpk)), pqpk
    , maybe (BS.singleton 0x00) (\k -> BS.singleton 0x01 <> k) mOpk ]

deserializeBundle :: BS.ByteString -> Maybe PQPreKeyBundle
deserializeBundle bs
    | BS.length bs < 165 = Nothing
    | otherwise =
        let !pqLen = fromIntegral (getW32BE (bsSlice 160 4 bs)) :: Int
            !rest  = BS.drop (164 + pqLen) bs
            decOpk r | BS.null r         = Nothing
                     | BS.index r 0 == 1 = Just (BS.take 32 (BS.drop 1 r))
                     | otherwise         = Nothing
        in if BS.length bs < 164 + pqLen + 1 then Nothing
           else Just PQPreKeyBundle
               { pqpkbIdentityKey     = bsSlice 0  32 bs
               , pqpkbIdentityEd25519 = bsSlice 32 32 bs
               , pqpkbSignedPreKey    = bsSlice 64 32 bs
               , pqpkbSPKSignature    = bsSlice 96 64 bs
               , pqpkbPQPreKey        = MLKEMEncapKey (bsSlice 164 pqLen bs)
               , pqpkbOneTimePreKey   = decOpk rest }
-- PQXDH Handshake ---------------------------------------------------------
handshakeInitiator :: Transport -> IO ChatSession
handshakeInitiator t = do
    aliceIK <- genIdentity; bundle <- recvBundle t
    ekRand <- randomBytes 32; mlkemRand <- randomBytes 32
    result <- case pqxdhInitiate aliceIK bundle ekRand mlkemRand of
        Nothing -> fail "PQXDH: SPK signature verification failed"
        Just r  -> pure r
    let MLKEMCiphertext ctBS = pqxdhPQCiphertext result
    send t . encodeMessage $ BS.concat
        [ ikX25519Public aliceIK, pqxdhEphemeralKey result
        , putW32BE (fromIntegral (BS.length ctBS)), ctBS ]
    initChatSession (pqxdhSharedSecret result)
                    (ikX25519Secret aliceIK) (pqpkbSignedPreKey bundle)

handshakeResponder :: Transport -> IO ChatSession
handshakeResponder t = do
    bobIK <- genIdentity
    (spk, spkSig) <- genSignedPreKey bobIK
    (pqEK, pqDK) <- genPQPreKey
    send t . encodeMessage $ serializeBundle bobIK (kpPublic spk) spkSig pqEK Nothing
    (aliceIKPub, aliceEKPub, pqCt) <- recvInitialMessage t
    let !shared = pqxdhRespond bobIK (kpSecret spk) Nothing pqDK
                               aliceIKPub aliceEKPub pqCt
    initChatSession shared (kpSecret spk) aliceEKPub

recvBundle :: Transport -> IO PQPreKeyBundle
recvBundle t = do
    lenBs <- recv t 4
    payload <- recv t (fromIntegral (getW32BE lenBs))
    case deserializeBundle payload of
        Nothing     -> fail "PQXDH: malformed prekey bundle"
        Just bundle -> pure bundle

recvInitialMessage :: Transport -> IO (BS.ByteString, BS.ByteString, MLKEMCiphertext)
recvInitialMessage t = do
    lenBs <- recv t 4
    payload <- recv t (fromIntegral (getW32BE lenBs))
    let !ctLen = fromIntegral (getW32BE (bsSlice 64 4 payload)) :: Int
    pure (bsSlice 0 32 payload, bsSlice 32 32 payload,
          MLKEMCiphertext (bsSlice 68 ctLen payload))
-- ByteString / Word32 helpers ---------------------------------------------
bsSlice :: Int -> Int -> BS.ByteString -> BS.ByteString
bsSlice off len = BS.take len . BS.drop off

putW32BE :: Word32 -> BS.ByteString
putW32BE w = BS.pack
    [ fromIntegral (w `shiftR` 24 .&. 0xff), fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR`  8 .&. 0xff), fromIntegral (w             .&. 0xff) ]

getW32BE :: BS.ByteString -> Word32
getW32BE bs = (fromIntegral (BS.index bs 0) `shiftL` 24)
    + (fromIntegral (BS.index bs 1) `shiftL` 16)
    + (fromIntegral (BS.index bs 2) `shiftL` 8)
    + fromIntegral (BS.index bs 3)

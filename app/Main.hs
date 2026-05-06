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
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import UmbraVox.Chat.Session
    (ChatSession, initChatSession, sendChatMessage, recvChatMessage)
import UmbraVox.Crypto.MLKEM (mlkemKeyGen,
    MLKEMEncapKey(..), MLKEMDecapKey(..), MLKEMCiphertext(..))
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.Signal.PQXDH
    (PQPreKeyBundle(..), PQXDHResult(..), pqxdhInitiate, pqxdhRespond)
import UmbraVox.Crypto.Signal.X3DH
    (KeyPair(..), IdentityKey(..), generateIdentityKey, generateKeyPair,
     signPreKey)
import UmbraVox.Network.Transport
    (Transport(..), listen, connect, send, recv, close)
import UmbraVox.Protocol.CBOR (encodeMessage)
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
    , asInputBuf :: IORef String, asChatScroll :: IORef Int
    , asStatusMsg :: IORef String, asRunning :: IORef Bool
    , asDialogMode :: IORef (Maybe DialogMode) }
data DialogMode = DlgHelp | DlgSettings | DlgVerify | DlgNewConn
    | DlgKeys | DlgPrompt String (String -> IO ())
data AppConfig = AppConfig
    { cfgListenPort :: IORef Int, cfgDisplayName :: IORef String
    , cfgIdentity :: IORef (Maybe IdentityKey)
    , cfgSessions :: IORef (Map SessionId SessionInfo)
    , cfgNextId :: IORef SessionId }
-- Layout constants --------------------------------------------------------
leftW, rightW, totalW, chatH :: Int
leftW = 20; rightW = 60; totalW = leftW + rightW; chatH = 17
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
render :: AppState -> IO ()
render st = do
    csi "H"
    focus <- readIORef (asFocus st); sel <- readIORef (asSelected st)
    sessions <- readIORef (cfgSessions (asConfig st))
    buf <- readIORef (asInputBuf st); scroll <- readIORef (asChatScroll st)
    status <- readIORef (asStatusMsg st)
    let entries = Map.toList sessions
        selSi = if sel < length entries
                then Map.lookup (fst (entries !! sel)) sessions else Nothing
        peer  = maybe "(no contact)" siPeerName selSi
    -- top border
    goto 1 1; setFg 36
    putStr $ "\x250C\x2500 Contacts " ++ replicate (leftW - 12) '\x2500'
        ++ "\x2510\x250C\x2500 Chat: " ++ take (rightW - 12) peer ++ " "
        ++ replicate (max 0 (rightW - 10 - length (take (rightW-12) peer))) '\x2500'
        ++ "\x2510"; resetSGR
    -- panes
    forM_ [0..chatH-1] $ \row -> do
        goto (row+2) 1
        -- contact cell
        setFg 36; putStr "\x2502"; resetSGR
        if row < length entries then do
            let (_,si) = entries !! row
            tag <- statusTag <$> readIORef (siStatus si)
            let mk = if row==sel then " > " else "   "
                nm = take (leftW-8) (siPeerName si)
                cell = mk ++ padR (leftW-7-length tag) nm ++ tag
            when (row==sel) $ if focus==ContactPane then bold>>setFg 32 else bold
            putStr (take (leftW-2) cell); resetSGR
        else putStr (replicate (leftW-2) ' ')
        setFg 36; putStr "\x2502"; resetSGR
        -- chat cell
        setFg 36; putStr "\x2502"; resetSGR
        msg <- case selSi of
            Nothing -> pure ""
            Just si -> do
                hist <- readIORef (siHistory si)
                let msgs = reverse hist; total = length msgs
                    start = max 0 (total - chatH - scroll)
                    idx = start + row
                pure $ if idx >= 0 && idx < total then msgs !! idx else ""
        putStr (padR (rightW-2) (take (rightW-2) msg))
        setFg 36; putStr "\x2502"; resetSGR
    -- middle border
    goto (chatH+2) 1; setFg 36
    putStr $ "\x251C" ++ replicate (leftW-2) '\x2500' ++ "\x2524"
        ++ "\x251C" ++ replicate (rightW-2) '\x2500' ++ "\x2524"; resetSGR
    -- bottom panes
    goto (chatH+3) 1; setFg 36; putStr "\x2502"; resetSGR
    setFg 33; putStr (padR (leftW-2) " [N]ew [G]roup"); resetSGR
    setFg 36; putStr "\x2502\x2502"; resetSGR
    let cur = if focus==ChatPane then "_" else ""
    putStr (padR (rightW-2) (" > " ++ take (rightW-6) buf ++ cur))
    setFg 36; putStr "\x2502"; resetSGR
    goto (chatH+4) 1; setFg 36; putStr "\x2502"; resetSGR
    setFg 33; putStr (padR (leftW-2) " [K]eys [S]elf"); resetSGR
    setFg 36; putStr "\x2502\x2502"; resetSGR
    putStr (replicate (rightW-2) ' '); setFg 36; putStr "\x2502"; resetSGR
    -- bottom border
    goto (chatH+5) 1; setFg 36
    putStr $ "\x2514" ++ replicate (leftW-2) '\x2500' ++ "\x2518"
        ++ "\x2514" ++ replicate (rightW-2) '\x2500' ++ "\x2518"; resetSGR
    -- status bar
    goto (chatH+6) 1; setFg 30; csi "47m"
    let bar = " F1:Help F2:File F3:Verify F5:Export F9:Settings F10:Quit"
        full = if null status then bar
               else bar ++ " | " ++ take (totalW - length bar - 4) status
    putStr (padR totalW full); resetSGR; hFlush stdout
-- Input handling ----------------------------------------------------------
data InputEvent = KeyChar Char | KeyEnter | KeyTab | KeyBackspace | KeyEscape
    | KeyUp | KeyDown | KeyPageUp | KeyPageDown
    | KeyF1 | KeyF2 | KeyF3 | KeyF5 | KeyF9 | KeyF10 | KeyUnknown

readKey :: IO InputEvent
readKey = do
    c <- getChar
    case c of
        '\n' -> pure KeyEnter; '\r' -> pure KeyEnter; '\t' -> pure KeyTab
        '\DEL' -> pure KeyBackspace; '\BS' -> pure KeyBackspace
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
        '5' -> dt >> pure KeyPageUp; '6' -> dt >> pure KeyPageDown
        '1' -> csiSub [('5',KeyF5),('7',KeyF1),('8',KeyF2)]
        '2' -> csiSub [('0',KeyF9),('1',KeyF10)]
        _ -> drainSeq >> pure KeyUnknown
  where dt = hReady stdin >>= \r -> when r (void getChar)

csiSub :: [(Char, InputEvent)] -> IO InputEvent
csiSub tbl = do
    ready <- hReady stdin
    if not ready then pure KeyUnknown else do
        c <- getChar
        case lookup c tbl of
            Just ev -> do
                r <- hReady stdin; when r (void getChar)
                pure ev
            Nothing -> if c == '~' then pure KeyUnknown
                       else drainSeq >> pure KeyUnknown

readSS3 :: IO InputEvent
readSS3 = do
    c <- getChar
    pure $ case c of 'P'->KeyF1; 'Q'->KeyF2; 'R'->KeyF3; _->KeyUnknown

drainSeq :: IO ()
drainSeq = hReady stdin >>= \r -> when r (void getChar >> drainSeq)
-- Main / event loop -------------------------------------------------------
main :: IO ()
main = do
    hSetBuffering stdout (BlockBuffering (Just 8192))
    cfg <- AppConfig <$> newIORef 9999 <*> newIORef "User"
                     <*> newIORef Nothing <*> newIORef Map.empty <*> newIORef 1
    st <- AppState cfg <$> newIORef 0 <*> newIORef ContactPane
                       <*> newIORef "" <*> newIORef 0 <*> newIORef ""
                       <*> newIORef True <*> newIORef Nothing
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
        KeyTab  -> modifyIORef' (asFocus st) (\p -> if p==ContactPane then ChatPane else ContactPane)
        KeyF1   -> showHelp st;    KeyF2  -> setStatus st "Type /file <path> in chat"
        KeyF3   -> startVerify st; KeyF5  -> startExport st
        KeyF9   -> startSettings st; KeyF10 -> quitApp st
        _ -> case focus of { ContactPane -> handleContact st key; ChatPane -> handleChat st key }

handleContact :: AppState -> InputEvent -> IO ()
handleContact st key = do
    n <- Map.size <$> readIORef (cfgSessions (asConfig st))
    case key of
        KeyUp    -> modifyIORef' (asSelected st) (\i -> max 0 (i-1))
        KeyDown  -> modifyIORef' (asSelected st) (\i -> min (n-1) (i+1))
        KeyEnter -> writeIORef (asFocus st) ChatPane >> writeIORef (asChatScroll st) 0
        KeyChar 'n' -> startNewConn st; KeyChar 'N' -> startNewConn st
        KeyChar 'g' -> setStatus st "Group: add peers with N, msgs go to all"
        KeyChar 'G' -> setStatus st "Group: add peers with N, msgs go to all"
        KeyChar 'k' -> startKeysView st; KeyChar 'K' -> startKeysView st
        KeyChar 's' -> addSecureNotes st; KeyChar 'S' -> addSecureNotes st
        _ -> pure ()

handleChat :: AppState -> InputEvent -> IO ()
handleChat st key = case key of
    KeyChar c    -> modifyIORef' (asInputBuf st) (++[c])
    KeyBackspace -> modifyIORef' (asInputBuf st) (\s -> if null s then s else init s)
    KeyEnter     -> sendCurrentMessage st
    KeyUp        -> modifyIORef' (asChatScroll st) (+1)
    KeyDown      -> modifyIORef' (asChatScroll st) (\s -> max 0 (s-1))
    KeyPageUp    -> modifyIORef' (asChatScroll st) (+chatH)
    KeyPageDown  -> modifyIORef' (asChatScroll st) (\s -> max 0 (s-chatH))
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
                b <- readIORef (asInputBuf st); cb b
                writeIORef (asInputBuf st) ""
                writeIORef (asDialogMode st) Nothing
            _ -> pure ()
        _ -> writeIORef (asDialogMode st) Nothing

showOverlay :: String -> [String] -> IO ()
showOverlay title lns = do
    let w = 50; h = length lns + 2; r0 = max 2 ((24-h) `div` 2)
        c0 = max 1 ((totalW-w) `div` 2)
        top = "\x250C\x2500 " ++ take (w-6) title ++ " "
              ++ replicate (max 0 (w-5-length (take (w-6) title))) '\x2500' ++ "\x2510"
        bot = "\x2514" ++ replicate (w-2) '\x2500' ++ "\x2518"
        rows = map (\l -> "\x2502 "++padR (w-3) (take (w-3) l)++"\x2502") lns
    forM_ (zip [0..] (top : rows ++ [bot])) $ \(i,line) ->
        goto (r0+i) c0 >> setFg 36 >> bold >> putStr line >> resetSGR
    hFlush stdout

showHelp :: AppState -> IO ()
showHelp st = do
    writeIORef (asDialogMode st) (Just DlgHelp)
    showOverlay "Help - UmbraVOX"
        [ "Tab       Switch pane focus"
        , "Up/Down   Navigate contacts / scroll chat"
        , "Enter     Send message / select contact"
        , "N  New connection   G  Group"
        , "K  Identity & keys  S  Secure notes"
        , "F1 Help  F2 File  F3 Verify  F5 Export"
        , "F9 Settings  F10 Quit"
        , "", "Press any key to close..." ]

startSettings :: AppState -> IO ()
startSettings st = do
    writeIORef (asDialogMode st) (Just DlgSettings)
    port <- readIORef (cfgListenPort (asConfig st))
    name <- readIORef (cfgDisplayName (asConfig st))
    showOverlay "Settings"
        [ "1. Listen port: " ++ show port
        , "2. Display name: " ++ name, ""
        , "Press 1/2 to change, Esc to close" ]

handleSettingsDlg :: AppState -> InputEvent -> IO ()
handleSettingsDlg st (KeyChar '1') = do
    writeIORef (asDialogMode st) (Just (DlgPrompt "port" $ \val ->
        case reads val of { [(p,_)] -> writeIORef (cfgListenPort (asConfig st)) (p::Int); _ -> pure () }))
    showOverlay "Set Port" ["Enter new port number:", ""]
handleSettingsDlg st (KeyChar '2') = do
    writeIORef (asDialogMode st) (Just (DlgPrompt "name" $ \val ->
        unless (null val) $ writeIORef (cfgDisplayName (asConfig st)) val))
    showOverlay "Set Name" ["Enter new display name:", ""]
handleSettingsDlg st _ = writeIORef (asDialogMode st) Nothing

setStatus :: AppState -> String -> IO ()
setStatus st msg = writeIORef (asStatusMsg st) msg
-- Actions -----------------------------------------------------------------
startNewConn :: AppState -> IO ()
startNewConn st = do
    writeIORef (asDialogMode st) (Just DlgNewConn)
    showOverlay "New Connection"
        ["1. Connect to peer", "2. Listen for peer"
        ,"3. Loopback test (self)", "", "Press 1/2/3, Esc to cancel"]

handleNewConnDlg :: AppState -> InputEvent -> IO ()
handleNewConnDlg st (KeyChar '1') = do
    writeIORef (asDialogMode st) Nothing
    setStatus st "Connecting to 127.0.0.1:9999..."
    void $ forkIO $ (do
        t <- connect "127.0.0.1" 9999
        session <- handshakeInitiator t
        sid <- addSession (asConfig st) t session "127.0.0.1:9999"
        selectLast st; setStatus st ("Connected #" ++ show sid)
        ) `catch` (\(e::SomeException) -> setStatus st ("Failed: "++show e))
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

addSecureNotes :: AppState -> IO ()
addSecureNotes st = do
    sid <- addLoopbackSession (asConfig st) "\x1F512 Secure Notes"
    selectLast st; setStatus st ("Secure Notes #" ++ show sid)

startKeysView :: AppState -> IO ()
startKeysView st = do
    writeIORef (asDialogMode st) (Just DlgKeys)
    mIk <- readIORef (cfgIdentity (asConfig st))
    ik <- case mIk of
        Just ik -> pure ik
        Nothing -> do ik <- genIdentity; writeIORef (cfgIdentity (asConfig st)) (Just ik); pure ik
    showOverlay "Identity & Keys"
        [ "X25519:  " ++ fingerprint (ikX25519Public ik)
        , "Ed25519: " ++ fingerprint (ikEd25519Public ik)
        , "", "Press any key to close" ]

startVerify :: AppState -> IO ()
startVerify st = do
    writeIORef (asDialogMode st) (Just DlgVerify)
    sessions <- readIORef (cfgSessions (asConfig st))
    sel <- readIORef (asSelected st)
    let entries = Map.toList sessions
    if sel < length entries then do
        let (_,si) = entries !! sel
        showOverlay "Verify Keys"
            ["Peer: " ++ siPeerName si, ""
            ,"Key verification via safety numbers"
            ,"is not yet implemented.", "", "Press any key to close"]
    else showOverlay "Verify Keys" ["No contact selected","","Press any key"]

startExport :: AppState -> IO ()
startExport st = do
    sessions <- readIORef (cfgSessions (asConfig st))
    sel <- readIORef (asSelected st)
    let entries = Map.toList sessions
    if sel < length entries then do
        let (_,si) = entries !! sel
        hist <- readIORef (siHistory si)
        let path = "umbravox_export.txt"
        writeFile path (unlines (reverse hist))
        setStatus st ("Exported "++show (length hist)++" msgs to "++path)
    else setStatus st "No contact selected"

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

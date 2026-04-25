module Main (main) where

import Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import Control.Exception (catch, SomeException)
import Control.Monad (void, when, forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Bits (shiftL, shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout, stdin, hSetBuffering, BufferMode(..), hIsEOF)
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

data SessionInfo = SessionInfo
    { siTransport :: Maybe Transport     -- Nothing for loopback (self-chat)
    , siSession   :: IORef ChatSession
    , siRecvTid   :: Maybe ThreadId      -- Nothing for loopback
    , siPeerName  :: String
    , siHistory   :: IORef [String]
    }

data AppConfig = AppConfig
    { cfgListenPort  :: IORef Int
    , cfgDisplayName :: IORef String
    , cfgIdentity    :: IORef (Maybe IdentityKey)
    , cfgSessions    :: IORef (Map SessionId SessionInfo)
    , cfgNextId      :: IORef SessionId
    }

-- ANSI helpers ------------------------------------------------------------

esc :: String -> String
esc code = "\ESC[" ++ code

clearScreen :: IO ()
clearScreen = putStr (esc "2J" ++ esc "H") >> hFlush stdout

setColor :: Int -> IO ()
setColor c = putStr (esc (show c ++ "m"))

resetColor :: IO ()
resetColor = putStr (esc "0m")

-- Box drawing -------------------------------------------------------------

drawBox :: String -> [String] -> Int -> IO ()
drawBox title rows w = do
    let pad s = s ++ replicate (w - 2 - length s) ' '
    putStrLn $ "\x2554" ++ replicate (w - 2) '\x2550' ++ "\x2557"
    putStrLn $ "\x2551" ++ pad ("  " ++ title) ++ "\x2551"
    putStrLn $ "\x2560" ++ replicate (w - 2) '\x2550' ++ "\x2563"
    forM_ rows $ \r ->
        putStrLn $ "\x2551" ++ pad ("  " ++ r) ++ "\x2551"
    putStrLn $ "\x255A" ++ replicate (w - 2) '\x2550' ++ "\x255D"

drawMenu :: String -> [String] -> IO Int
drawMenu title items = do
    clearScreen
    setColor 36
    drawBox title (zipWith (\i s -> show i ++ ". " ++ s) [(1::Int)..] items) 46
    resetColor
    putStr "\n  Select: "
    hFlush stdout
    line <- getLine
    case reads line of
        [(n, _)] | n >= 1 && n <= length items -> pure n
        _ -> drawMenu title items

-- Main / menu loop --------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering
    cfg <- AppConfig <$> newIORef 9999 <*> newIORef "User"
                     <*> newIORef Nothing <*> newIORef Map.empty <*> newIORef 1
    mainLoop cfg

mainLoop :: AppConfig -> IO ()
mainLoop cfg = do
    n <- drawMenu "U M B R A V O X  --  Post-Quantum Encrypted Messaging"
        [ "New Conversation", "Secure Notes (Self)"
        , "Active Conversations", "Group Conversations"
        , "File Transfer", "Identity & Keys", "Import/Export"
        , "Settings", "Quit" ]
    case n of
        1 -> newConversation cfg     >> mainLoop cfg
        2 -> secureNotes cfg         >> mainLoop cfg
        3 -> activeConversations cfg >> mainLoop cfg
        4 -> groupConversations cfg  >> mainLoop cfg
        5 -> fileTransfer cfg        >> mainLoop cfg
        6 -> identityKeys cfg        >> mainLoop cfg
        7 -> importExport cfg        >> mainLoop cfg
        8 -> settings cfg            >> mainLoop cfg
        9 -> quitApp cfg
        _ -> mainLoop cfg

quitApp :: AppConfig -> IO ()
quitApp cfg = do
    sessions <- readIORef (cfgSessions cfg)
    forM_ (Map.elems sessions) $ \si -> do
        maybe (pure ()) killThread (siRecvTid si)
        maybe (pure ()) close (siTransport si)
    clearScreen
    putStrLn "Goodbye."

-- Secure Notes (Self) -----------------------------------------------------

secureNotes :: AppConfig -> IO ()
secureNotes cfg = do
    clearScreen
    setColor 36; putStrLn "--- Secure Notes (Self-Chat) ---"; resetColor
    putStr "  Note label [Secure Notes]: "; hFlush stdout
    label <- getLine
    let name = if null label then "Secure Notes" else label
    sid <- addLoopbackSession cfg ("\x1F512 " ++ name)
    putStrLn "  Loopback session created. Messages encrypt locally."
    threadDelay 500000
    chatWindow cfg sid

-- 1. New Conversation -----------------------------------------------------

newConversation :: AppConfig -> IO ()
newConversation cfg = do
    clearScreen
    setColor 36; putStrLn "--- New Conversation ---"; resetColor
    putStr "  Mode (1=Connect, 2=Listen, 3=Self): "; hFlush stdout
    mode <- getLine
    case mode of
        "1" -> do
            putStr "  Host: "; hFlush stdout; host <- getLine
            putStr "  Port: "; hFlush stdout; portS <- getLine
            putStrLn $ "  Connecting to " ++ host ++ ":" ++ portS ++ "..."
            t <- connect host (read portS)
            putStrLn "  Running PQXDH handshake..."
            session <- handshakeInitiator t
            sid <- addSession cfg t session (host ++ ":" ++ portS)
            putStrLn "  Session established!"
            threadDelay 500000
            chatWindow cfg sid
        "2" -> do
            lp <- readIORef (cfgListenPort cfg)
            putStr $ "  Port [" ++ show lp ++ "]: "; hFlush stdout
            portS <- getLine
            let port = if null portS then lp else read portS
            putStrLn $ "  Listening on port " ++ show port ++ "..."
            t <- listen port
            putStrLn "  Peer connected. Running PQXDH handshake..."
            session <- handshakeResponder t
            sid <- addSession cfg t session ("peer:" ++ show port)
            putStrLn "  Session established!"
            threadDelay 500000
            chatWindow cfg sid
        "3" -> do
            putStr "  Port [19999]: "; hFlush stdout
            portS <- getLine
            let port = if null portS then 19999 else read portS
            putStrLn "  Starting loopback test (localhost TCP)..."
            -- Start a listener on a background thread, then connect to it
            void $ forkIO $ do
                tServer <- listen port
                sessionB <- handshakeResponder tServer
                refB <- newIORef sessionB
                histB <- newIORef []
                -- Server just receives and echoes (loopback)
                recvLoopTUI tServer refB histB
            threadDelay 500000
            tClient <- connect "127.0.0.1" port
            putStrLn "  Running PQXDH handshake (loopback)..."
            sessionA <- handshakeInitiator tClient
            sid <- addSession cfg tClient sessionA ("loopback:" ++ show port)
            putStrLn "  Loopback session established!"
            threadDelay 500000
            chatWindow cfg sid
        _ -> pure ()

addSession :: AppConfig -> Transport -> ChatSession -> String -> IO SessionId
addSession cfg t session peerName = do
    sid <- readIORef (cfgNextId cfg)
    writeIORef (cfgNextId cfg) (sid + 1)
    ref <- newIORef session
    histRef <- newIORef []
    tid <- forkIO (recvLoopTUI t ref histRef)
    let si = SessionInfo (Just t) ref (Just tid) peerName histRef
    modifyIORef' (cfgSessions cfg) (Map.insert sid si)
    pure sid

-- | Create a loopback (self-chat) session for secure note-taking.
-- Messages are encrypted then immediately decrypted locally — no network.
addLoopbackSession :: AppConfig -> String -> IO SessionId
addLoopbackSession cfg label = do
    sid <- readIORef (cfgNextId cfg)
    writeIORef (cfgNextId cfg) (sid + 1)
    secret <- randomBytes 32
    dhSec  <- randomBytes 32
    peerPub <- randomBytes 32
    session <- initChatSession secret dhSec peerPub
    ref <- newIORef session
    histRef <- newIORef []
    let si = SessionInfo Nothing ref Nothing label histRef
    modifyIORef' (cfgSessions cfg) (Map.insert sid si)
    pure sid

-- 2. Active Conversations -------------------------------------------------

activeConversations :: AppConfig -> IO ()
activeConversations cfg = do
    sessions <- readIORef (cfgSessions cfg)
    if Map.null sessions
        then promptWait "  No active conversations."
        else do
            let entries = Map.toList sessions
                items = map (\(sid, si) -> "#" ++ show sid ++ " - " ++ siPeerName si) entries
            choice <- drawMenu "Active Conversations" (items ++ ["Back"])
            when (choice <= length entries) $ do
                let (sid, _) = entries !! (choice - 1)
                chatWindow cfg sid

-- 3. Group Conversations --------------------------------------------------

groupConversations :: AppConfig -> IO ()
groupConversations cfg = do
    clearScreen
    setColor 36; putStrLn "--- Group Conversation ---"; resetColor
    putStrLn "  Enter peer addresses (host:port), empty line to finish."
    peers <- readPeers
    unless (null peers) $ do
        sids <- mapM (connectPeer cfg) peers
        groupChatLoop cfg sids

readPeers :: IO [(String, Int)]
readPeers = do
    putStr "  > "; hFlush stdout
    line <- getLine
    if null line then pure []
    else case break (== ':') line of
        (host, ':':portS) -> ((host, read portS) :) <$> readPeers
        _ -> putStrLn "  Invalid format. Use host:port" >> readPeers

connectPeer :: AppConfig -> (String, Int) -> IO SessionId
connectPeer cfg (host, port) = do
    putStrLn $ "  Connecting to " ++ host ++ ":" ++ show port ++ "..."
    t <- connect host port
    session <- handshakeInitiator t
    addSession cfg t session (host ++ ":" ++ show port)

groupChatLoop :: AppConfig -> [SessionId] -> IO ()
groupChatLoop cfg sids = do
    clearScreen
    setColor 36; putStrLn $ "--- Group Chat (" ++ show (length sids) ++ " peers) ---"
    resetColor; putStrLn "  Type messages. /back to return.\n"
    go
  where
    go = do
        putStr "> "; hFlush stdout; line <- getLine
        case line of
            "/back" -> pure ()
            _ -> do
                sessions <- readIORef (cfgSessions cfg)
                forM_ sids $ \sid -> case Map.lookup sid sessions of
                    Nothing -> pure ()
                    Just si -> sendToSession si (BC.pack line)
                go

-- 4. File Transfer --------------------------------------------------------

fileTransfer :: AppConfig -> IO ()
fileTransfer cfg = do
    clearScreen
    setColor 36; putStrLn "--- File Transfer ---"; resetColor
    sessions <- readIORef (cfgSessions cfg)
    if Map.null sessions
        then promptWait "  No active sessions. Start a conversation first."
        else do
            let entries = Map.toList sessions
            forM_ entries $ \(sid, si) ->
                putStrLn $ "  " ++ show sid ++ ". " ++ siPeerName si
            putStr "  Session #: "; hFlush stdout; sidS <- getLine
            case Map.lookup (read sidS) sessions of
                Nothing -> promptWait "  Invalid session."
                Just si -> do
                    putStr "  File path: "; hFlush stdout; path <- getLine
                    exists <- doesFileExist path
                    if exists then do
                        contents <- BS.readFile path
                        sendToSession si (BC.pack ("/file:" ++ path ++ ":") <> contents)
                        promptWait $ "  Sent " ++ show (BS.length contents) ++ " bytes."
                    else promptWait "  File not found."

-- 5. Identity & Keys ------------------------------------------------------

identityKeys :: AppConfig -> IO ()
identityKeys cfg = do
    clearScreen
    setColor 36; putStrLn "--- Identity & Keys ---"; resetColor
    mIk <- readIORef (cfgIdentity cfg)
    ik <- case mIk of
        Just ik -> pure ik
        Nothing -> do
            ik <- genIdentity
            writeIORef (cfgIdentity cfg) (Just ik)
            pure ik
    putStrLn $ "  X25519 public:  " ++ fingerprint (ikX25519Public ik)
    putStrLn $ "  Ed25519 public: " ++ fingerprint (ikEd25519Public ik)
    putStr "\n  Regenerate keys? (y/N): "; hFlush stdout
    ans <- getLine
    when (ans == "y" || ans == "Y") $ do
        ik' <- genIdentity
        writeIORef (cfgIdentity cfg) (Just ik')
        putStrLn "  Keys regenerated."
    promptWait ""

fingerprint :: BS.ByteString -> String
fingerprint bs = concatMap hex2 (BS.unpack (BS.take 8 bs))
  where
    hex2 w = [hexC (w `shiftR` 4), hexC (w .&. 0x0f), ':']
    hexC n | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
           | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)

-- 6. Import/Export --------------------------------------------------------

importExport :: AppConfig -> IO ()
importExport cfg = do
    n <- drawMenu "Import/Export"
        ["Export conversation history", "Import conversation history", "Back"]
    case n of { 1 -> exportHistory cfg; 2 -> importHistory; _ -> pure () }

exportHistory :: AppConfig -> IO ()
exportHistory cfg = do
    sessions <- readIORef (cfgSessions cfg)
    if Map.null sessions then promptWait "  No sessions to export."
    else do
        let entries = Map.toList sessions
        forM_ entries $ \(sid, si) ->
            putStrLn $ "  " ++ show sid ++ ". " ++ siPeerName si
        putStr "  Session #: "; hFlush stdout; sidS <- getLine
        case Map.lookup (read sidS) sessions of
            Nothing -> promptWait "  Invalid session."
            Just si -> do
                putStr "  Output file: "; hFlush stdout; path <- getLine
                hist <- readIORef (siHistory si)
                writeFile path (unlines (reverse hist))
                promptWait $ "  Exported " ++ show (length hist) ++ " messages."

importHistory :: IO ()
importHistory = do
    putStr "  Input file: "; hFlush stdout; path <- getLine
    exists <- doesFileExist path
    if exists then do
        contents <- readFile path
        let lns = lines contents
        putStrLn $ "  Read " ++ show (length lns) ++ " messages."
        forM_ (take 20 lns) $ \l -> putStrLn $ "    " ++ l
        when (length lns > 20) $ putStrLn "    ..."
    else putStrLn "  File not found."
    promptWait ""

-- 7. Settings -------------------------------------------------------------

settings :: AppConfig -> IO ()
settings cfg = do
    port <- readIORef (cfgListenPort cfg)
    name <- readIORef (cfgDisplayName cfg)
    n <- drawMenu ("Settings  [Port: " ++ show port ++ "  Name: " ++ name ++ "]")
        ["Change listen port", "Change display name", "Back"]
    case n of
        1 -> do putStr "  New port: "; hFlush stdout
                writeIORef (cfgListenPort cfg) . read =<< getLine
                settings cfg
        2 -> do putStr "  New name: "; hFlush stdout; nm <- getLine
                unless (null nm) $ writeIORef (cfgDisplayName cfg) nm
                settings cfg
        _ -> pure ()

-- Chat Window -------------------------------------------------------------

chatWindow :: AppConfig -> SessionId -> IO ()
chatWindow cfg sid = do
    sessions <- readIORef (cfgSessions cfg)
    case Map.lookup sid sessions of
        Nothing -> putStrLn "  Session not found."
        Just si -> chatInputLoop cfg sid si

drawChatHeader :: String -> IO ()
drawChatHeader peer = do
    clearScreen; setColor 36
    let w = 46; bar = replicate (w - 2) '\x2500'
    putStrLn $ "\x250C\x2500 " ++ peer ++ " " ++ drop (length peer + 2) bar ++ "\x2510"
    resetColor

drawChatFooter :: IO ()
drawChatFooter = do
    setColor 36
    let w = 46
    putStrLn $ "\x251C" ++ replicate (w - 2) '\x2500' ++ "\x2524"
    putStrLn $ "\x2502 > Type message... (/back to return)" ++ replicate 7 ' ' ++ "\x2502"
    putStrLn $ "\x2514" ++ replicate (w - 2) '\x2500' ++ "\x2518"
    resetColor

redrawChat :: SessionInfo -> IO ()
redrawChat si = do
    drawChatHeader (siPeerName si)
    hist <- readIORef (siHistory si)
    forM_ (reverse (take 15 hist)) $ \msg -> do
        setColor 37; putStrLn $ "\x2502 " ++ msg
    resetColor
    drawChatFooter

chatInputLoop :: AppConfig -> SessionId -> SessionInfo -> IO ()
chatInputLoop cfg sid si = do
    redrawChat si
    putStr "> "; hFlush stdout
    eof <- hIsEOF stdin
    if eof then pure ()
    else do
        line <- getLine
        case line of
            "/back" -> pure ()
            "/quit" -> quitApp cfg
            "/verify" -> do
                putStrLn "  [Key verification not yet available for peer]"
                threadDelay 1000000
                chatInputLoop cfg sid si
            _ | isPfx "/file " line -> do
                    let path = drop 6 line
                    exists <- doesFileExist path
                    if exists then do
                        contents <- BS.readFile path
                        sendToSession si (BC.pack ("/file:" ++ path ++ ":") <> contents)
                        now <- timestamp
                        modifyIORef' (siHistory si)
                            (("[" ++ now ++ "] You: [sent file " ++ path ++ "]") :)
                    else modifyIORef' (siHistory si) ("  [File not found]" :)
                    chatInputLoop cfg sid si
              | null line -> chatInputLoop cfg sid si
              | otherwise -> do
                    sendToSession si (BC.pack line)
                    now <- timestamp
                    modifyIORef' (siHistory si) (("[" ++ now ++ "] You: " ++ line) :)
                    chatInputLoop cfg sid si

-- Helpers -----------------------------------------------------------------

isPfx :: String -> String -> Bool
isPfx [] _         = True
isPfx _ []         = False
isPfx (x:xs) (y:ys) = x == y && isPfx xs ys

sendToSession :: SessionInfo -> BS.ByteString -> IO ()
sendToSession si msg = do
    session <- readIORef (siSession si)
    (session', wire) <- sendChatMessage session msg
    writeIORef (siSession si) session'
    case siTransport si of
        Just t  -> send t (encodeMessage wire)
        Nothing -> do
            -- Loopback: decrypt our own message and add to history
            session2 <- readIORef (siSession si)
            result <- recvChatMessage session2 wire
            case result of
                Just (session3, _plaintext) -> writeIORef (siSession si) session3
                Nothing -> pure ()
            ts <- timestamp
            modifyIORef' (siHistory si) (++ [ts ++ " [saved] " ++ BC.unpack msg])

timestamp :: IO String
timestamp = formatTime defaultTimeLocale "%H:%M" <$> getCurrentTime

promptWait :: String -> IO ()
promptWait msg = do
    unless (null msg) $ putStrLn msg
    putStr "  Press Enter..."; hFlush stdout; void getLine

-- Receive loop (background thread) ----------------------------------------

recvLoopTUI :: Transport -> IORef ChatSession -> IORef [String] -> IO ()
recvLoopTUI t ref histRef = go `catch` handler
  where
    go = do
        lenBs <- recv t 4
        if BS.length lenBs < 4
            then modifyIORef' histRef ("  [Peer disconnected]" :)
            else do
                let !len = fromIntegral (getW32BE lenBs)
                payload <- recv t len
                session <- readIORef ref
                result <- recvChatMessage session payload
                case result of
                    Nothing -> modifyIORef' histRef ("  [Decryption failed]" :) >> go
                    Just (session', plaintext) -> do
                        writeIORef ref session'
                        now <- timestamp
                        modifyIORef' histRef (("[" ++ now ++ "] Peer: " ++ BC.unpack plaintext) :)
                        go
    handler :: SomeException -> IO ()
    handler _ = modifyIORef' histRef ("  [Connection lost]" :)

-- PQXDH key material generation -------------------------------------------

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
    d <- randomBytes 32; z <- randomBytes 32
    pure $! mlkemKeyGen d z

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
            decOpk r | BS.null r          = Nothing
                     | BS.index r 0 == 1  = Just (BS.take 32 (BS.drop 1 r))
                     | otherwise          = Nothing
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
    aliceIK <- genIdentity
    bundle  <- recvBundle t
    ekRand  <- randomBytes 32; mlkemRand <- randomBytes 32
    result  <- case pqxdhInitiate aliceIK bundle ekRand mlkemRand of
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
    bobIK         <- genIdentity
    (spk, spkSig) <- genSignedPreKey bobIK
    (pqEK, pqDK)  <- genPQPreKey
    send t . encodeMessage $ serializeBundle bobIK (kpPublic spk) spkSig pqEK Nothing
    (aliceIKPub, aliceEKPub, pqCt) <- recvInitialMessage t
    let !shared = pqxdhRespond bobIK (kpSecret spk) Nothing pqDK
                               aliceIKPub aliceEKPub pqCt
    initChatSession shared (kpSecret spk) aliceEKPub

-- Bundle send/receive helpers ---------------------------------------------

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
    pure (bsSlice 0 32 payload, bsSlice 32 32 payload, MLKEMCiphertext (bsSlice 68 ctLen payload))

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

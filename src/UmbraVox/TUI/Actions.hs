module UmbraVox.TUI.Actions
    ( startNewConn, addSession, addLoopbackSession, renameContact
    , startExport, exportToPath, startImport
    , addSecureNotes, startKeysView, startVerify, startSettings
    , showHelp, selectLast, sendCurrentMessage, sendToSession
    , quitApp, setStatus, recvLoopTUI, adjustContactScroll
    ) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (catch, SomeException)
import Control.Monad (when, unless, forM_, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout, stdin, hSetBuffering, hSetEcho, BufferMode(..))
import System.Process (readProcess)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Render (clearScreen, goto, showCursor, isPfx)
import UmbraVox.Chat.Session
    (ChatSession, initChatSession, sendChatMessage, recvChatMessage)
import UmbraVox.Crypto.BIP39 (generatePassphrase)
import UmbraVox.Crypto.Export (encryptExport, decryptExport)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Network.TransportClass
    (AnyTransport, anySend, anyRecv, anyClose)
import UmbraVox.Protocol.CBOR (encodeMessage)
import UmbraVox.Storage.Anthony (saveMessage)
import UmbraVox.TUI.Handshake (getW32BE, genIdentity, timestamp)

-- Actions -----------------------------------------------------------------
startNewConn :: AppState -> IO ()
startNewConn st = writeIORef (asDialogMode st) (Just DlgNewConn)

showHelp :: AppState -> IO ()
showHelp st = writeIORef (asDialogMode st) (Just DlgHelp)

startSettings :: AppState -> IO ()
startSettings st = writeIORef (asDialogMode st) (Just DlgSettings)

startVerify :: AppState -> IO ()
startVerify st = writeIORef (asDialogMode st) (Just DlgVerify)

setStatus :: AppState -> String -> IO ()
setStatus st msg = writeIORef (asStatusMsg st) msg

selectLast :: AppState -> IO ()
selectLast st = do
    n <- Map.size <$> readIORef (cfgSessions (asConfig st))
    writeIORef (asSelected st) (max 0 (n-1))
    writeIORef (asFocus st) ChatPane
    writeIORef (asChatScroll st) 0

adjustContactScroll :: AppState -> Int -> IO ()
adjustContactScroll st visRows = do
    sel <- readIORef (asSelected st)
    cScroll <- readIORef (asContactScroll st)
    when (sel < cScroll) $ writeIORef (asContactScroll st) sel
    when (sel >= cScroll + visRows) $
        writeIORef (asContactScroll st) (sel - visRows + 1)

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
        Nothing -> do
            ik <- genIdentity
            writeIORef (cfgIdentity (asConfig st)) (Just ik)
        Just _  -> pure ()

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
            -- Auto-save to Anthony DB if enabled
            autoSave <- readIORef (cfgAutoSaveMessages (asConfig st))
            when autoSave $ do
                mDb <- readIORef (cfgAnthonyDB (asConfig st))
                case mDb of
                    Just db -> (do
                        t <- round <$> getPOSIXTime
                        saveMessage db sel "You" buf t
                        ) `catch` (\(_ :: SomeException) -> pure ())
                    Nothing -> pure ()

quitApp :: AppState -> IO ()
quitApp st = do
    writeIORef (asRunning st) False
    sessions <- readIORef (cfgSessions (asConfig st))
    forM_ (Map.elems sessions) $ \si -> do
        maybe (pure ()) killThread (siRecvTid si)
        maybe (pure ()) anyClose (siTransport si)
    -- Restore terminal to sane state before exiting
    clearScreen; goto 1 1; showCursor
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    void (readProcess "stty" ["-F", "/dev/tty", "sane"] "" `catch`
          (\(_ :: SomeException) -> pure ""))
    putStrLn "Goodbye."; hFlush stdout

-- Session management ------------------------------------------------------
addSession :: AppConfig -> AnyTransport -> ChatSession -> String -> IO SessionId
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
        Just t  -> anySend t (encodeMessage wire)
        Nothing -> do
            session2 <- readIORef (siSession si)
            result <- recvChatMessage session2 wire
            case result of
                Just (session3, _pt) -> writeIORef (siSession si) session3
                Nothing -> pure ()
            ts <- timestamp
            modifyIORef' (siHistory si) ((ts++" [saved] "++BC.unpack msg):)

-- Receive loop (background thread) ----------------------------------------
recvLoopTUI :: AnyTransport -> IORef ChatSession -> IORef [String] -> IO ()
recvLoopTUI t ref histRef = go `catch` handler where
    go = do
        lenBs <- anyRecv t 4
        if BS.length lenBs < 4
            then modifyIORef' histRef ("  [Peer disconnected]":)
            else do
                let !len = fromIntegral (getW32BE lenBs)
                payload <- anyRecv t len
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

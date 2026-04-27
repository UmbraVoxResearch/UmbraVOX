module UmbraVox.TUI.Actions.Session
    ( addSession, addLoopbackSession, recvLoopTUI
    , sendToSession, sendCurrentMessage
    , addSecureNotes
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (catch, SomeException)
import Control.Monad (when, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (doesFileExist)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Render (isPfx)
import UmbraVox.Chat.Session
    (ChatSession, initChatSession, sendChatMessage, recvChatMessage)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Network.TransportClass
    (AnyTransport, anySend, anyRecv)
import UmbraVox.Protocol.CBOR (encodeMessage)
import UmbraVox.Storage.Anthony (saveMessage, saveConversation)
import UmbraVox.TUI.Handshake (getW32BE, timestamp)

-- | Add a new remote session with a transport and chat session.
addSession :: AppConfig -> AnyTransport -> ChatSession -> String -> IO SessionId
addSession cfg t session peerName = do
    sid <- readIORef (cfgNextId cfg); writeIORef (cfgNextId cfg) (sid+1)
    ref <- newIORef session; histRef <- newIORef []; stRef <- newIORef Online
    lock <- newMVar ()
    tid <- forkIO (recvLoopTUI t ref lock histRef)
    let si = SessionInfo (Just t) ref lock (Just tid) peerName histRef stRef
    modifyIORef' (cfgSessions cfg) (Map.insert sid si)
    -- Persist conversation to DB (graceful on failure)
    mDb <- readIORef (cfgAnthonyDB cfg)
    case mDb of
        Just db -> (do
            now <- round <$> getPOSIXTime :: IO Int
            saveConversation db sid "" peerName now
            ) `catch` (\(_ :: SomeException) -> pure ())
        Nothing -> pure ()
    pure sid

-- | Add a local-only loopback session (e.g. Secure Notes).
addLoopbackSession :: AppConfig -> String -> IO SessionId
addLoopbackSession cfg label = do
    sid <- readIORef (cfgNextId cfg); writeIORef (cfgNextId cfg) (sid+1)
    secret <- randomBytes 32; dhSec <- randomBytes 32; peerPub <- randomBytes 32
    session <- initChatSession secret dhSec peerPub
    ref <- newIORef session; histRef <- newIORef []; stRef <- newIORef Local
    lock <- newMVar ()
    let si = SessionInfo Nothing ref lock Nothing label histRef stRef
    modifyIORef' (cfgSessions cfg) (Map.insert sid si)
    -- Persist conversation to DB (graceful on failure)
    mDb <- readIORef (cfgAnthonyDB cfg)
    case mDb of
        Just db -> (do
            now <- round <$> getPOSIXTime :: IO Int
            saveConversation db sid "" label now
            ) `catch` (\(_ :: SomeException) -> pure ())
        Nothing -> pure ()
    pure sid

-- | Send a bytestring to a session (remote or loopback).
sendToSession :: SessionInfo -> BS.ByteString -> IO ()
sendToSession si msg = do
    withMVar (siSessionLock si) $ \_ -> do
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

-- | Send the current input buffer as a message.
sendCurrentMessage :: AppState -> IO ()
sendCurrentMessage st = do
    buf <- readIORef (asInputBuf st)
    unless (null buf) $ do
        sessions <- readIORef (cfgSessions (asConfig st))
        sel <- readIORef (asSelected st)
        let entries = Map.toList sessions
        when (sel < length entries) $ do
            let (sid, si) = entries !! sel
            if isPfx "/file " buf then do
                let path = drop 6 buf
                exists <- doesFileExist path
                if exists then do
                    contents <- BS.readFile path
                    sendToSession si (BC.pack ("/file:"++path++":") <> contents)
                    now <- timestamp
                    modifyIORef' (siHistory si)
                        (("["++now++"] You: [sent file "++path++"]"):)
                else setStatusLocal st "File not found"
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
                        saveMessage db sid "You" buf t
                        ) `catch` (\(_ :: SomeException) -> pure ())
                    Nothing -> pure ()

-- | Add a "Secure Notes" loopback session.
addSecureNotes :: AppState -> IO ()
addSecureNotes st = do
    sid <- addLoopbackSession (asConfig st) "\x1F512 Secure Notes"
    selectLastLocal st; setStatusLocal st ("Secure Notes #" ++ show sid)

-- | Background receive loop for a remote transport.
recvLoopTUI :: AnyTransport -> IORef ChatSession -> MVar () -> IORef [String] -> IO ()
recvLoopTUI t ref lock histRef = go `catch` handler where
    go = do
        lenBs <- anyRecv t 4
        if BS.length lenBs < 4
            then modifyIORef' histRef ("  [Peer disconnected]":)
            else do
                let !len = fromIntegral (getW32BE lenBs)
                payload <- anyRecv t len
                result <- withMVar lock $ \_ -> do
                    session <- readIORef ref
                    result <- recvChatMessage session payload
                    case result of
                        Nothing -> pure Nothing
                        Just (session', plaintext) -> do
                            writeIORef ref session'
                            pure (Just plaintext)
                case result of
                    Nothing -> modifyIORef' histRef ("  [Decryption failed]":) >> go
                    Just plaintext -> do
                        now <- timestamp
                        modifyIORef' histRef (("["++now++"] Peer: "++BC.unpack plaintext):)
                        go
    handler :: SomeException -> IO ()
    handler _ = modifyIORef' histRef ("  [Connection lost]":)

-- Internal helpers (duplicated to avoid circular imports) -----------------

setStatusLocal :: AppState -> String -> IO ()
setStatusLocal st msg = writeIORef (asStatusMsg st) msg

selectLastLocal :: AppState -> IO ()
selectLastLocal st = do
    n <- Map.size <$> readIORef (cfgSessions (asConfig st))
    writeIORef (asSelected st) (max 0 (n-1))
    writeIORef (asFocus st) ChatPane
    writeIORef (asChatScroll st) 0

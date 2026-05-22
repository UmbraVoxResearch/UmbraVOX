-- SPDX-License-Identifier: Apache-2.0
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
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word8, Word64)
import System.Directory (doesFileExist, canonicalizePath, getHomeDirectory)
import Data.List (isPrefixOf)
import UmbraVox.App.RuntimeLog (logEvent)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Render (isPfx)
import UmbraVox.Chat.Session
    (ChatSession, initChatSession, sendChatMessage, recvChatMessage)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.SHA256 (sha256)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Network.TransportClass
    (AnyTransport, anySend, anyRecv)
import UmbraVox.Protocol.CBOR (encodeMessage)
import UmbraVox.Protocol.UTF8 (encodeStringUtf8, decodeUtf8String)
import UmbraVox.Storage.Anthony (saveMessage, saveConversation)
import UmbraVox.TUI.Handshake (getW32BE, timestamp)

data SendResult = SendDelivered | SendStoredLocal | SendUnavailable
    deriving stock (Eq, Show)

-- | Add a new remote session with a transport and chat session.
addSession :: AppConfig -> AnyTransport -> ChatSession -> String -> IO SessionId
addSession cfg t session peerName = do
    sid <- readIORef (cfgNextId cfg); writeIORef (cfgNextId cfg) (sid+1)
    ref <- newIORef session; histRef <- newIORef []; stRef <- newIORef Online
    lock <- newMVar ()
    tid <- forkIO (recvLoopTUI cfg sid peerName t ref lock histRef)
    let si = SessionInfo (Just t) (RatchetCrypto ref) lock (Just tid) peerName histRef stRef
    modifyIORef' (cfgSessions cfg) (Map.insert sid si)
    -- Persist conversation to DB (graceful on failure)
    mDb <- readIORef (cfgAnthonyDB cfg)
    case mDb of
        Just db -> (do
            now <- round <$> getPOSIXTime :: IO Int
            saveConversation db sid "" peerName now
            ) `catch` (\(_ :: SomeException) -> pure ())
        Nothing -> pure ()
    logEvent cfg "session.established.remote"
        [ ("session_id", show sid)
        , ("peer", peerName)
        ]
    pure sid

-- | Add a local-only loopback session (e.g. Secure Notes).
addLoopbackSession :: AppConfig -> String -> IO SessionId
addLoopbackSession cfg label = do
    sid <- readIORef (cfgNextId cfg); writeIORef (cfgNextId cfg) (sid+1)
    secret <- randomBytes 32; dhSec <- randomBytes 32; peerPub <- randomBytes 32
    -- Random keys are never all-zero, so initChatSession cannot return Nothing here.
    mSession <- initChatSession secret dhSec peerPub
    let session = case mSession of
                      Just s  -> s
                      Nothing -> error "addLoopbackSession: ratchet init with random keys returned Nothing (impossible)"
    ref <- newIORef session; histRef <- newIORef []; stRef <- newIORef Local
    lock <- newMVar ()
    let si = SessionInfo Nothing (RatchetCrypto ref) lock Nothing label histRef stRef
    modifyIORef' (cfgSessions cfg) (Map.insert sid si)
    -- Persist conversation to DB (graceful on failure)
    mDb <- readIORef (cfgAnthonyDB cfg)
    case mDb of
        Just db -> (do
            now <- round <$> getPOSIXTime :: IO Int
            saveConversation db sid "" label now
            ) `catch` (\(_ :: SomeException) -> pure ())
        Nothing -> pure ()
    logEvent cfg "session.established.local"
        [ ("session_id", show sid)
        , ("peer", label)
        ]
    pure sid

-- | Send a bytestring to a session (remote or loopback).
-- The 32-byte @sId@ (identity key hash) is prepended inside the
-- encrypted payload per M23.1.1d.
sendToSession :: BS.ByteString -> SessionInfo -> BS.ByteString -> IO SendResult
sendToSession sId si msg = case siCrypto si of
    BridgeCrypto _bs -> error "bridge not implemented"
    RatchetCrypto ref -> do
      withMVar (siSessionLock si) $ \_ -> do
        session <- readIORef ref
        wallNow <- floor <$> getPOSIXTime :: IO Word64
        sendResult <- sendChatMessage session sId msg wallNow
        case sendResult of
          Left _ratchetErr -> pure SendUnavailable
          Right (session', wire) -> do
            writeIORef ref session'
            case siTransport si of
              Just t  -> anySend t (encodeMessage wire) >> pure SendDelivered
              Nothing -> do
                status <- readIORef (siStatus si)
                case status of
                  Local -> do
                    session2 <- readIORef ref
                    recvResult <- recvChatMessage session2 Nothing wire
                    case recvResult of
                        Left _ratchetErr2                    -> pure ()
                        Right (Just (session3, _sid, _pt))   -> writeIORef ref session3
                        Right Nothing                        -> pure ()
                    ts <- timestamp
                    let savedLine = case payloadHistoryText msg of
                            Right text -> ts ++ " [saved] " ++ text
                            Left _     -> ts ++ " [saved] [invalid UTF-8 payload]"
                    modifyIORef' (siHistory si) (savedLine :)
                    pure SendStoredLocal
                  _ -> pure SendUnavailable

-- | Derive the 32-byte sender identity hash from the local identity key.
-- Returns all-zero bytes when no identity is loaded (loopback / test).
localSenderId :: AppConfig -> IO BS.ByteString
localSenderId cfg = do
    mIk <- readIORef (cfgIdentity cfg)
    pure $ case mIk of
        Nothing -> BS.replicate 32 0
        Just ik -> sha256 (ikEd25519Public ik)

-- | Send the current input buffer as a message.
sendCurrentMessage :: AppState -> IO ()
sendCurrentMessage st = do
    buf <- readIORef (asInputBuf st)
    unless (null buf) $ do
        let cfg = asConfig st
        sid' <- localSenderId cfg
        sessions <- readIORef (cfgSessions cfg)
        sel <- readIORef (asSelected st)
        let entries = Map.toList sessions
        when (sel < length entries) $ do
            let (sid, si) = entries !! sel
            if isPfx "/file " buf then do
                let path = drop 6 buf
                exists <- doesFileExist path
                if exists then do
                    canonPath <- canonicalizePath path
                    homeDir   <- getHomeDirectory
                    cwd       <- canonicalizePath "."
                    let allowed = homeDir `isPrefixOf` canonPath
                                  || cwd   `isPrefixOf` canonPath
                    if not allowed
                        then setStatusLocal st "Access denied: file must be under home or working directory"
                        else do
                            contents <- BS.readFile canonPath
                            result <- sendToSession sid' si (encodeFilePayload canonPath contents)
                            case result of
                                SendUnavailable ->
                                    setStatusLocal st ("Session offline; reconnect required for " ++ siPeerName si)
                                _ -> do
                                    now <- timestamp
                                    modifyIORef' (siHistory si)
                                        (("["++now++"] You: [sent file "++canonPath++"]"):)
                                    logEvent cfg "message.send.file"
                                        [ ("session_id", show sid)
                                        , ("peer", siPeerName si)
                                        , ("path", canonPath)
                                        , ("bytes", show (BS.length contents))
                                        ]
                                    writeIORef (asInputBuf st) ""
                                    writeIORef (asInputCursor st) 0
                                    writeIORef (asChatScroll st) 0
                                    persistMessageIfEnabled cfg sid "You" buf
                else setStatusLocal st "File not found"
            else do
                result <- sendToSession sid' si (encodeStringUtf8 buf)
                case result of
                    SendUnavailable ->
                        setStatusLocal st ("Session offline; reconnect required for " ++ siPeerName si)
                    _ -> do
                        now <- timestamp
                        modifyIORef' (siHistory si) (("["++now++"] You: "++buf):)
                        logEvent cfg "message.send"
                            [ ("session_id", show sid)
                            , ("peer", siPeerName si)
                            , ("bytes", show (length buf))
                            ]
                        writeIORef (asInputBuf st) ""
                        writeIORef (asInputCursor st) 0
                        writeIORef (asChatScroll st) 0
                        persistMessageIfEnabled cfg sid "You" buf

-- | Add a "Secure Notes" loopback session.
addSecureNotes :: AppState -> IO ()
addSecureNotes st = do
    sid <- addLoopbackSession (asConfig st) "\x1F512 Secure Notes"
    selectLastLocal st; setStatusLocal st ("Secure Notes #" ++ show sid)

-- | Background receive loop for a remote transport.
recvLoopTUI
    :: AppConfig
    -> SessionId
    -> String
    -> AnyTransport
    -> IORef ChatSession
    -> MVar ()
    -> IORef [String]
    -> IO ()
recvLoopTUI cfg sid peerName t ref lock histRef = go `catch` handler where
    go = do
        lenBs <- anyRecv t 4
        if BS.length lenBs < 4
            then markDisconnected "transport.peer_disconnected" "  [Peer disconnected]"
            else do
                let !len = fromIntegral (getW32BE lenBs)
                payload <- anyRecv t len
                result <- withMVar lock $ \_ -> do
                    session <- readIORef ref
                    result <- recvChatMessage session Nothing payload
                    case result of
                        Left _ratchetErr             -> pure Nothing
                        Right Nothing                -> pure Nothing
                        Right (Just (session', _sid, plaintext)) -> do
                            writeIORef ref session'
                            pure (Just plaintext)
                case result of
                    Nothing -> modifyIORef' histRef ("  [Decryption failed]":) >> go
                    Just plaintext -> do
                        now <- timestamp
                        let content = either (const "[invalid UTF-8 message]") id (payloadHistoryText plaintext)
                        modifyIORef' histRef (("["++now++"] Peer: "++content):)
                        logEvent cfg "message.recv"
                            [ ("session_id", show sid)
                            , ("peer", peerName)
                            , ("bytes", show (BS.length plaintext))
                            ]
                        persistMessageIfEnabled cfg sid peerName content
                        go
    handler :: SomeException -> IO ()
    handler _ = markDisconnected "transport.connection_lost" "  [Connection lost]"

    markDisconnected :: String -> String -> IO ()
    markDisconnected eventName banner = do
        markSessionOffline cfg sid
        logEvent cfg eventName
            [ ("session_id", show sid)
            , ("peer", peerName)
            ]
        modifyIORef' histRef (banner:)

-- Internal helpers (duplicated to avoid circular imports) -----------------

persistMessageIfEnabled :: AppConfig -> SessionId -> String -> String -> IO ()
persistMessageIfEnabled cfg sid sender content = do
    autoSave <- readIORef (cfgAutoSaveMessages cfg)
    when autoSave $ do
        mDb <- readIORef (cfgAnthonyDB cfg)
        case mDb of
            Just db -> (do
                t <- round <$> getPOSIXTime
                saveMessage db sid sender content t
                logEvent cfg "persistence.message_saved"
                    [ ("session_id", show sid)
                    , ("sender", sender)
                    , ("bytes", show (length content))
                    ]
                ) `catch` (\(_ :: SomeException) -> pure ())
            Nothing -> pure ()

setStatusLocal :: AppState -> String -> IO ()
setStatusLocal st msg = writeIORef (asStatusMsg st) msg

selectLastLocal :: AppState -> IO ()
selectLastLocal st = do
    n <- Map.size <$> readIORef (cfgSessions (asConfig st))
    writeIORef (asSelected st) (max 0 (n-1))
    writeIORef (asFocus st) ChatPane
    writeIORef (asChatScroll st) 0

markSessionOffline :: AppConfig -> SessionId -> IO ()
markSessionOffline cfg sid = do
    sessions <- readIORef (cfgSessions cfg)
    case Map.lookup sid sessions of
        Nothing -> pure ()
        Just si -> do
            writeIORef (siStatus si) Offline
            let si' = si { siTransport = Nothing, siRecvTid = Nothing }
            writeIORef (cfgSessions cfg) (Map.insert sid si' sessions)

encodeFilePayload :: FilePath -> BS.ByteString -> BS.ByteString
encodeFilePayload path contents = encodeStringUtf8 ("/file:" ++ path ++ ":") <> contents

payloadHistoryText :: BS.ByteString -> Either String String
payloadHistoryText payload
    | filePrefix `BS.isPrefixOf` payload =
        case BS.break (== fileSep) (BS.drop (BS.length filePrefix) payload) of
            (pathBs, rest) ->
                case BS.uncons rest of
                    Just (_, contents) -> do
                        path <- decodeUtf8String pathBs
                        pure ("[received file " ++ path ++ " (" ++ show (BS.length contents) ++ " bytes)]")
                    Nothing -> decodeUtf8String payload
    | otherwise = decodeUtf8String payload
  where
    filePrefix = encodeStringUtf8 "/file:"
    fileSep :: Word8
    fileSep = 58

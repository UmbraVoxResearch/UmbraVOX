-- SPDX-License-Identifier: Apache-2.0
-- | Signal WebSocket client module (M19.3.5)
--
-- Provides the WebSocket transport layer for the Signal bridge plugin.
-- Signal uses WebSockets with a custom binary framing (protobuf
-- request/response) for real-time message delivery.  This module owns
-- connection lifecycle and message send/receive; the crypto + protobuf
-- layer lives in "UmbraVox.Bridge.Signal.Session".
--
-- WebSocket framing follows RFC 6455: 2-byte header with opcode,
-- mask bit, and payload length, plus optional extended length fields
-- and a 4-byte masking key for client-to-server frames.
module UmbraVox.Bridge.Signal.WebSocket
    ( SignalWSConnection(..)
    , WSRequest(..)
    , WSResponse(..)
    , WSMessage(..)
    , WSError(..)
    , connectSignalWS
    , connectSignalWSPath
    , disconnectSignalWS
    , sendWSMessage
    , recvWSMessage
    , wsHealthCheck
    ) where

import Control.Exception (SomeException, try)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8, Word16, Word64)
import Network.Socket (Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import UmbraVox.Crypto.Random (randomBytes)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | WebSocket-level errors.
data WSError
    = WSConnectionFailed !String
      -- ^ TCP or TLS handshake failure.
    | WSAuthFailed !String
      -- ^ Signal credential / auth-token rejection.
    | WSParseError !String
      -- ^ Malformed WebSocket frame or protobuf payload.
    | WSClosed !String
      -- ^ Connection was closed (server-initiated or network drop).
    deriving stock (Eq, Show)

-- | Connection state tag.
data ConnState
    = ConnDisconnected
    | ConnConnecting
    | ConnConnected
    | ConnClosing
    deriving stock (Eq, Show)

-- | A live (or pending) WebSocket connection to a Signal server.
data SignalWSConnection = SignalWSConnection
    { wscServerHost   :: !String
      -- ^ Hostname of the Signal server endpoint.
    , wscServerPort   :: !Int
      -- ^ Port (typically 443 for TLS, 8080 for dev).
    , wscAuthToken    :: !(IORef ByteString)
      -- ^ Mutable auth credential; may be refreshed on 401.
    , wscState        :: !(IORef ConnState)
      -- ^ Current connection lifecycle state.
    , wscSocket       :: !(IORef (Maybe Socket))
      -- ^ Underlying TCP socket, if connected.
    , wscRequestId    :: !(IORef Word64)
      -- ^ Monotonic counter for WebSocket sub-protocol request IDs.
    }

-- | A WebSocket sub-protocol request (client -> server).
data WSRequest = WSRequest
    { wsReqId   :: !Word64
    , wsReqVerb :: !ByteString   -- ^ e.g. "PUT", "GET", "DELETE"
    , wsReqPath :: !ByteString   -- ^ e.g. "/api/v1/message"
    , wsReqBody :: !ByteString   -- ^ protobuf payload
    } deriving stock (Eq, Show)

-- | A WebSocket sub-protocol response (server -> client).
data WSResponse = WSResponse
    { wsRspId     :: !Word64
    , wsRspStatus :: !Int         -- ^ HTTP-style status code
    , wsRspBody   :: !ByteString  -- ^ protobuf payload
    } deriving stock (Eq, Show)

-- | Messages exchanged over the Signal WebSocket sub-protocol.
--
-- Signal's WebSocket framing carries protobuf-encoded request/response
-- pairs plus pushed envelopes.  We model the three cases explicitly.
data WSMessage
    = WSMsgRequest  !WSRequest
    | WSMsgResponse !WSResponse
    | WSMsgEnvelopePush !ByteString
      -- ^ Raw Signal envelope bytes (decrypt via Session module).
    deriving stock (Eq, Show)

------------------------------------------------------------------------
-- WebSocket frame constants (RFC 6455)
------------------------------------------------------------------------

-- | WebSocket opcode for binary frames.
opBinary :: Word8
opBinary = 0x02

-- | WebSocket opcode for close frames.
opClose :: Word8
opClose = 0x08

-- | WebSocket opcode for ping frames.
opPing :: Word8
opPing = 0x09

-- | WebSocket opcode for pong frames.
opPong :: Word8
opPong = 0x0A

------------------------------------------------------------------------
-- Connection lifecycle
------------------------------------------------------------------------

-- | Attempt to open a WebSocket connection to a Signal server on the
-- default message endpoint (@\/v1\/websocket\/@).
--
-- Performs TCP connection and sends the HTTP Upgrade request.
-- On success, transitions to ConnConnected and returns the connection.
connectSignalWS
    :: String       -- ^ Server hostname
    -> Int          -- ^ Server port
    -> ByteString   -- ^ Initial auth token
    -> IO (Either WSError SignalWSConnection)
connectSignalWS host port token =
    connectSignalWSPath host port token "/v1/websocket/"

-- | Like 'connectSignalWS' but with an explicit WebSocket path.
--
-- Use this for non-default endpoints, e.g. the provisioning endpoint
-- at @\/v1\/websocket\/provisioning\/@.
connectSignalWSPath
    :: String       -- ^ Server hostname
    -> Int          -- ^ Server port
    -> ByteString   -- ^ Initial auth token
    -> String       -- ^ WebSocket path (e.g. \"\/v1\/websocket\/provisioning\/\")
    -> IO (Either WSError SignalWSConnection)
connectSignalWSPath host port token wsPath = do
    tokenRef <- newIORef token
    stateRef <- newIORef ConnConnecting
    sockRef  <- newIORef Nothing
    reqIdRef <- newIORef 0
    let conn = SignalWSConnection
            { wscServerHost = host
            , wscServerPort = port
            , wscAuthToken  = tokenRef
            , wscState      = stateRef
            , wscSocket     = sockRef
            , wscRequestId  = reqIdRef
            }
    -- Resolve and connect
    let hints = NS.defaultHints
            { NS.addrSocketType = NS.Stream
            , NS.addrFamily     = NS.AF_UNSPEC
            }
    result <- tryConnect hints conn wsPath
    case result of
        Left err -> do
            writeIORef stateRef ConnDisconnected
            pure (Left err)
        Right () -> do
            writeIORef stateRef ConnConnected
            pure (Right conn)
  where
    tryConnect hints conn path = do
        addrs <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
        case addrs of
            [] -> pure (Left (WSConnectionFailed ("no address for " ++ host)))
            (addr:_) -> do
                sock <- NS.openSocket addr
                NS.connect sock (NS.addrAddress addr)
                writeIORef (wscSocket conn) (Just sock)
                -- Send WebSocket upgrade request
                authTok <- readIORef (wscAuthToken conn)
                let upgradeReq = strToBS
                        ( "GET " ++ path ++ " HTTP/1.1\r\n"
                       ++ "Host: " ++ host ++ "\r\n"
                       ++ "Upgrade: websocket\r\n"
                       ++ "Connection: Upgrade\r\n"
                       ++ "Authorization: Basic " ++ bsToStr authTok ++ "\r\n"
                       ++ "Sec-WebSocket-Version: 13\r\n"
                       ++ "Sec-WebSocket-Protocol: signal\r\n"
                       ++ "Sec-WebSocket-Key: dW1icmF2b3g=\r\n"
                       ++ "\r\n"
                        )
                NSB.sendAll sock upgradeReq
                -- Read upgrade response (at least the status line)
                respBytes <- NSB.recv sock 4096
                if BS.null respBytes
                    then do
                        NS.close sock
                        writeIORef (wscSocket conn) Nothing
                        pure (Left (WSConnectionFailed "empty upgrade response"))
                    else
                        -- Check for 101 Switching Protocols
                        let respStr = bsToStr (BS.take 32 respBytes)
                        in if hasSubstring "101" respStr
                            then pure (Right ())
                            else do
                                NS.close sock
                                writeIORef (wscSocket conn) Nothing
                                pure (Left (WSAuthFailed
                                    ("upgrade rejected: " ++ bsToStr (BS.take 128 respBytes))))

-- | Gracefully close a WebSocket connection.
--
-- Sends a WebSocket close frame (opcode 0x08) before closing the
-- TCP socket.  Safe to call on already-closed connections (no-op).
disconnectSignalWS :: SignalWSConnection -> IO ()
disconnectSignalWS conn = do
    st <- readIORef (wscState conn)
    case st of
        ConnDisconnected -> pure ()
        _ -> do
            writeIORef (wscState conn) ConnClosing
            mSock <- readIORef (wscSocket conn)
            case mSock of
                Nothing -> pure ()
                Just sock -> do
                    -- Send close frame: FIN + opcode 0x08, masked, zero-length.
                    -- Best-effort: the peer may have already closed (RFC 6455 §7.1.7).
                    maskKey <- randomBytes 4
                    let frame = buildFrame True opClose maskKey BS.empty
                    _ <- try (safeSend sock frame) :: IO (Either SomeException ())
                    NS.close sock
            writeIORef (wscSocket conn) Nothing
            writeIORef (wscState conn) ConnDisconnected

------------------------------------------------------------------------
-- Message I/O
------------------------------------------------------------------------

-- | Send a WebSocket message over the connection.
--
-- Serializes the WSMessage into the Signal sub-protocol binary format,
-- then wraps it in a WebSocket binary frame (RFC 6455) with a random
-- masking key (required for client-to-server frames).
sendWSMessage :: SignalWSConnection -> WSMessage -> IO (Either WSError ())
sendWSMessage conn msg = do
    st <- readIORef (wscState conn)
    case st of
        ConnConnected -> do
            mSock <- readIORef (wscSocket conn)
            case mSock of
                Nothing -> pure (Left (WSClosed "no socket"))
                Just sock -> do
                    let payload = encodeWSMessage conn msg
                    maskKey <- randomBytes 4
                    let frame = buildFrame True opBinary maskKey payload
                    safeSend sock frame
                    pure (Right ())
        _ -> pure (Left (WSClosed "connection not in Connected state"))

-- | Receive the next WebSocket message from the connection.
--
-- Reads a WebSocket frame from the socket, handles control frames
-- (ping/pong/close) internally, and returns the decoded WSMessage
-- for data frames.
recvWSMessage :: SignalWSConnection -> IO (Either WSError WSMessage)
recvWSMessage conn = do
    st <- readIORef (wscState conn)
    case st of
        ConnConnected -> do
            mSock <- readIORef (wscSocket conn)
            case mSock of
                Nothing -> pure (Left (WSClosed "no socket"))
                Just sock -> readFrame sock
        _ -> pure (Left (WSClosed "connection not in Connected state"))
  where
    readFrame sock = do
        -- Read the 2-byte WebSocket header
        hdrBytes <- recvExact sock 2
        if BS.length hdrBytes < 2
            then pure (Left (WSClosed "connection closed during frame header"))
            else do
                let byte0 = BS.index hdrBytes 0
                    byte1 = BS.index hdrBytes 1
                    _fin    = byte0 .&. 0x80 /= 0
                    opcode = byte0 .&. 0x0F
                    masked = byte1 .&. 0x80 /= 0
                    len7   = byte1 .&. 0x7F
                -- Read extended length if needed
                payloadLen <- if len7 < 126
                    then pure (fromIntegral len7 :: Int)
                    else if len7 == 126
                        then do
                            extBytes <- recvExact sock 2
                            pure (fromIntegral (decodeWord16BE extBytes))
                        else do
                            extBytes <- recvExact sock 8
                            pure (fromIntegral (decodeWord64BE extBytes))
                -- Read mask key if present
                maskKey <- if masked
                    then recvExact sock 4
                    else pure BS.empty
                -- Read payload
                rawPayload <- recvExact sock payloadLen
                let payload = if masked
                        then applyMask maskKey rawPayload
                        else rawPayload
                -- Handle control frames
                case opcode of
                    op | op == opPing -> do
                        -- Reply with pong
                        pongMask <- randomBytes 4
                        let pongFrame = buildFrame True opPong pongMask payload
                        _ <- safeSend sock pongFrame
                        readFrame sock  -- continue reading
                    op | op == opClose -> do
                        writeIORef (wscState conn) ConnDisconnected
                        pure (Left (WSClosed "server sent close frame"))
                    op | op == opPong ->
                        readFrame sock  -- ignore pongs, continue
                    _ ->
                        -- Data frame (binary or text) -- parse as Signal message
                        pure (decodeWSMessage payload)

-- | Quick health-check: is the connection alive?
wsHealthCheck :: SignalWSConnection -> IO Bool
wsHealthCheck conn = do
    st <- readIORef (wscState conn)
    pure (st == ConnConnected)

------------------------------------------------------------------------
-- WebSocket frame construction (RFC 6455)
------------------------------------------------------------------------

-- | Build a WebSocket frame.
--
-- Client-to-server frames MUST be masked (RFC 6455 Section 5.1).
-- The masking key should be 4 bytes of random data.
buildFrame :: Bool         -- ^ FIN bit
           -> Word8        -- ^ Opcode (4 bits)
           -> ByteString   -- ^ Masking key (4 bytes, or empty for unmasked)
           -> ByteString   -- ^ Payload
           -> ByteString
buildFrame fin opcode maskKey payload =
    let !finBit = if fin then 0x80 else 0x00 :: Word8
        !byte0  = finBit .|. (opcode .&. 0x0F)
        !masked = not (BS.null maskKey)
        !maskBit = if masked then 0x80 else 0x00 :: Word8
        !len    = BS.length payload
        !lenBytes
            | len < 126   = BS.singleton (maskBit .|. fromIntegral len)
            | len < 65536 = BS.singleton (maskBit .|. 126)
                         <> encodeWord16BE (fromIntegral len)
            | otherwise   = BS.singleton (maskBit .|. 127)
                         <> encodeWord64BE (fromIntegral len)
        !maskedPayload = if masked
            then applyMask maskKey payload
            else payload
        !maskBytes = if masked then maskKey else BS.empty
    in BS.singleton byte0 <> lenBytes <> maskBytes <> maskedPayload

-- | Apply XOR masking to a payload (RFC 6455 Section 5.3).
applyMask :: ByteString -> ByteString -> ByteString
applyMask maskKey payload = BS.pack
    [ BS.index payload i `xor` BS.index maskKey (i `mod` 4)
    | i <- [0 .. BS.length payload - 1]
    ]

------------------------------------------------------------------------
-- Signal sub-protocol encoding
------------------------------------------------------------------------

-- | Encode a WSMessage into the Signal sub-protocol binary format.
--
-- Signal's WebSocket sub-protocol uses a simple type-length-value
-- encoding:
--   Byte 0:    message type (0x01 = request, 0x02 = response, 0x03 = push)
--   Bytes 1-8: request/response ID (big-endian Word64)
--   Remaining: type-specific payload
--
-- For requests:  verb-length (2 bytes) + verb + path-length (2 bytes) + path + body
-- For responses: status (2 bytes) + body
-- For pushes:    raw envelope bytes
encodeWSMessage :: SignalWSConnection -> WSMessage -> ByteString
encodeWSMessage _conn msg = case msg of
    WSMsgRequest req ->
        BS.singleton 0x01
        <> encodeWord64BE (wsReqId req)
        <> encodeWord16BE (fromIntegral (BS.length (wsReqVerb req)))
        <> wsReqVerb req
        <> encodeWord16BE (fromIntegral (BS.length (wsReqPath req)))
        <> wsReqPath req
        <> wsReqBody req
    WSMsgResponse rsp ->
        BS.singleton 0x02
        <> encodeWord64BE (wsRspId rsp)
        <> encodeWord16BE (fromIntegral (wsRspStatus rsp))
        <> wsRspBody rsp
    WSMsgEnvelopePush envBytes ->
        BS.singleton 0x03
        <> envBytes

-- | Decode a WSMessage from Signal sub-protocol binary format.
decodeWSMessage :: ByteString -> Either WSError WSMessage
decodeWSMessage bs
    | BS.null bs = Left (WSParseError "empty WebSocket payload")
    | otherwise =
        let !msgType = BS.index bs 0
            !rest = BS.drop 1 bs
        in case msgType of
            0x01 -> decodeRequest rest
            0x02 -> decodeResponse rest
            0x03 -> Right (WSMsgEnvelopePush rest)
            _    -> Left (WSParseError ("unknown message type: " ++ show msgType))

decodeRequest :: ByteString -> Either WSError WSMessage
decodeRequest bs
    | BS.length bs < 8 = Left (WSParseError "request too short for ID")
    | otherwise =
        let !reqId = decodeWord64BE (BS.take 8 bs)
            !rest1 = BS.drop 8 bs
        in if BS.length rest1 < 2
            then Left (WSParseError "request too short for verb length")
            else
                let !verbLen = fromIntegral (decodeWord16BE (BS.take 2 rest1))
                    !rest2 = BS.drop 2 rest1
                in if BS.length rest2 < verbLen
                    then Left (WSParseError "request too short for verb")
                    else
                        let !verb = BS.take verbLen rest2
                            !rest3 = BS.drop verbLen rest2
                        in if BS.length rest3 < 2
                            then Left (WSParseError "request too short for path length")
                            else
                                let !pathLen = fromIntegral (decodeWord16BE (BS.take 2 rest3))
                                    !rest4 = BS.drop 2 rest3
                                in if BS.length rest4 < pathLen
                                    then Left (WSParseError "request too short for path")
                                    else
                                        let !path = BS.take pathLen rest4
                                            !body = BS.drop pathLen rest4
                                        in Right (WSMsgRequest WSRequest
                                            { wsReqId   = reqId
                                            , wsReqVerb = verb
                                            , wsReqPath = path
                                            , wsReqBody = body
                                            })

decodeResponse :: ByteString -> Either WSError WSMessage
decodeResponse bs
    | BS.length bs < 10 = Left (WSParseError "response too short")
    | otherwise =
        let !rspId = decodeWord64BE (BS.take 8 bs)
            !status = fromIntegral (decodeWord16BE (BS.take 2 (BS.drop 8 bs)))
            !body = BS.drop 10 bs
        in Right (WSMsgResponse WSResponse
            { wsRspId     = rspId
            , wsRspStatus = status
            , wsRspBody   = body
            })

------------------------------------------------------------------------
-- Socket helpers
------------------------------------------------------------------------

-- | Read exactly n bytes from a socket, blocking until all are received.
recvExact :: Socket -> Int -> IO ByteString
recvExact _ 0 = pure BS.empty
recvExact sock n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
        chunk <- NSB.recv sock (min remaining 4096)
        if BS.null chunk
            then pure (BS.concat (reverse acc))
            else go (remaining - BS.length chunk) (chunk : acc)

-- | Send all bytes, ignoring broken-pipe errors on close.
safeSend :: Socket -> ByteString -> IO ()
safeSend sock bs = NSB.sendAll sock bs

------------------------------------------------------------------------
-- Binary encoding helpers
------------------------------------------------------------------------

encodeWord16BE :: Word16 -> ByteString
encodeWord16BE w = BS.pack
    [ fromIntegral (w `shiftR` 8 .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

decodeWord16BE :: ByteString -> Word16
decodeWord16BE bs
    | BS.length bs < 2 = 0
    | otherwise =
        fromIntegral (BS.index bs 0) `shiftL` 8
        .|. fromIntegral (BS.index bs 1)

encodeWord64BE :: Word64 -> ByteString
encodeWord64BE w = BS.pack
    [ fromIntegral (w `shiftR` 56 .&. 0xff)
    , fromIntegral (w `shiftR` 48 .&. 0xff)
    , fromIntegral (w `shiftR` 40 .&. 0xff)
    , fromIntegral (w `shiftR` 32 .&. 0xff)
    , fromIntegral (w `shiftR` 24 .&. 0xff)
    , fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR`  8 .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

decodeWord64BE :: ByteString -> Word64
decodeWord64BE bs
    | BS.length bs < 8 = 0
    | otherwise =
        fromIntegral (BS.index bs 0) `shiftL` 56
        .|. fromIntegral (BS.index bs 1) `shiftL` 48
        .|. fromIntegral (BS.index bs 2) `shiftL` 40
        .|. fromIntegral (BS.index bs 3) `shiftL` 32
        .|. fromIntegral (BS.index bs 4) `shiftL` 24
        .|. fromIntegral (BS.index bs 5) `shiftL` 16
        .|. fromIntegral (BS.index bs 6) `shiftL`  8
        .|. fromIntegral (BS.index bs 7)

------------------------------------------------------------------------
-- String helpers
------------------------------------------------------------------------

strToBS :: String -> ByteString
strToBS = BS.pack . map (fromIntegral . fromEnum)

bsToStr :: ByteString -> String
bsToStr = map (toEnum . fromIntegral) . BS.unpack

-- | Check if a substring exists within a string.
hasSubstring :: String -> String -> Bool
hasSubstring _    [] = False
hasSubstring []   _  = True
hasSubstring needle haystack@(_:rest)
    | take (length needle) haystack == needle = True
    | otherwise = hasSubstring needle rest

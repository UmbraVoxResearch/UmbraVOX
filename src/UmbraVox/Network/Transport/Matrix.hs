-- SPDX-License-Identifier: Apache-2.0
-- | Matrix transport — sends and receives messages as Matrix room events.
--
-- Encodes UmbraVox messages as Matrix m.room.message events with a
-- custom msgtype ("org.umbravox.encrypted") and base16-encoded body.
-- Uses HTTP PUT/GET against the Matrix client-server API (/_matrix/client/).
--
-- See: doc/spec/network.md
module UmbraVox.Network.Transport.Matrix
    ( MatrixTransport(..)
    , MatrixConfig(..)
    , newMatrixTransport
    ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Word (Word32)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import UmbraVox.Network.TransportClass (TransportHandle(..))

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

-- | Configuration for connecting to a Matrix homeserver.
data MatrixConfig = MatrixConfig
    { mcHomeserver  :: !String
      -- ^ Homeserver hostname (e.g. "matrix.example.org").
    , mcPort        :: !Int
      -- ^ Port (typically 443 for HTTPS, 8008 for dev).
    , mcRoomId      :: !String
      -- ^ Room ID to send/receive in (e.g. "!abc123:example.org").
    , mcAccessToken :: !ByteString
      -- ^ Access token for authentication.
    } deriving stock (Show)

------------------------------------------------------------------------
-- Transport
------------------------------------------------------------------------

-- | A Matrix transport that encodes messages as room events.
--
-- Outgoing messages are formatted as HTTP PUT requests to the
-- /_matrix/client/v3/rooms/{roomId}/send/m.room.message/{txnId}
-- endpoint.  Incoming messages are read from the underlying TCP socket.
data MatrixTransport = MatrixTransport
    { mtConfig  :: !MatrixConfig
    , mtSocket  :: !(IORef (Maybe NS.Socket))
      -- ^ Underlying TCP socket to the homeserver.
    , mtTxnId   :: !(IORef Word32)
      -- ^ Monotonic transaction ID for idempotent event sends.
    , mtReadBuf :: !(MVar ByteString)
      -- ^ Buffered unread bytes for partial reads.
    }

instance TransportHandle MatrixTransport where
    thSend  = matrixSend
    thRecv  = matrixRecv
    thClose = matrixClose
    thInfo t = "matrix:" ++ mcHomeserver (mtConfig t)
            ++ "/" ++ mcRoomId (mtConfig t)

-- | Create a new Matrix transport and connect to the homeserver.
--
-- Opens a TCP connection to the homeserver.  TLS is not performed at
-- this layer; use a SOCKS5 or TLS wrapper transport for production.
newMatrixTransport :: MatrixConfig -> IO MatrixTransport
newMatrixTransport cfg = do
    let hints = NS.defaultHints
            { NS.addrSocketType = NS.Stream
            , NS.addrFamily     = NS.AF_UNSPEC
            }
    addrs <- NS.getAddrInfo (Just hints) (Just (mcHomeserver cfg)) (Just (show (mcPort cfg)))
    sock <- case addrs of
        []       -> ioError (userError ("matrix: no address for " ++ mcHomeserver cfg))
        (addr:_) -> do
            s <- NS.openSocket addr
            NS.connect s (NS.addrAddress addr)
            pure s
    sockRef <- newIORef (Just sock)
    txnRef  <- newIORef 0
    bufRef  <- newMVar BS.empty
    pure MatrixTransport
        { mtConfig  = cfg
        , mtSocket  = sockRef
        , mtTxnId   = txnRef
        , mtReadBuf = bufRef
        }

------------------------------------------------------------------------
-- Send
------------------------------------------------------------------------

-- | Send a message by encoding it as a Matrix room event HTTP request.
--
-- The message body is the raw ByteString payload; we wrap it in a
-- minimal JSON envelope with msgtype "org.umbravox.encrypted" and
-- base16-encode the ciphertext for JSON safety.
matrixSend :: MatrixTransport -> ByteString -> IO ()
matrixSend t bs = do
    mSock <- readIORef (mtSocket t)
    case mSock of
        Nothing -> ioError (userError "matrix: transport closed")
        Just sock -> do
            txn <- readIORef (mtTxnId t)
            modifyIORef' (mtTxnId t) (+ 1)
            let roomId = mcRoomId (mtConfig t)
                token  = mcAccessToken (mtConfig t)
                path = "/_matrix/client/v3/rooms/"
                    ++ roomId
                    ++ "/send/m.room.message/"
                    ++ show txn
                hexBody = bytesToHex bs
                jsonBody = "{\"msgtype\":\"org.umbravox.encrypted\""
                        ++ ",\"body\":\"" ++ hexBody ++ "\"}"
                bodyBS = strToBS jsonBody
                request = strToBS
                    ( "PUT " ++ path ++ " HTTP/1.1\r\n"
                   ++ "Host: " ++ mcHomeserver (mtConfig t) ++ "\r\n"
                   ++ "Authorization: Bearer " ++ bsToStr token ++ "\r\n"
                   ++ "Content-Type: application/json\r\n"
                   ++ "Content-Length: " ++ show (BS.length bodyBS) ++ "\r\n"
                   ++ "\r\n"
                    ) <> bodyBS
            NSB.sendAll sock request

------------------------------------------------------------------------
-- Receive
------------------------------------------------------------------------

-- | Receive bytes from the Matrix homeserver connection.
--
-- Reads raw bytes from the TCP socket.  The caller is responsible for
-- parsing HTTP response framing and extracting message payloads.
matrixRecv :: MatrixTransport -> Int -> IO ByteString
matrixRecv t n = modifyMVar (mtReadBuf t) $ \buf ->
    if BS.length buf >= n
        then pure (BS.drop n buf, BS.take n buf)
        else do
            mSock <- readIORef (mtSocket t)
            case mSock of
                Nothing -> pure (BS.empty, buf)
                Just sock -> go sock (BS.length buf) [buf]
  where
    go sock have acc
        | have >= n =
            let full = BS.concat (reverse acc)
            in pure (BS.drop n full, BS.take n full)
        | otherwise = do
            chunk <- NSB.recv sock 4096
            if BS.null chunk
                then let full = BS.concat (reverse acc)
                     in pure (BS.empty, full)
                else go sock (have + BS.length chunk) (chunk : acc)

------------------------------------------------------------------------
-- Close
------------------------------------------------------------------------

matrixClose :: MatrixTransport -> IO ()
matrixClose t = do
    mSock <- readIORef (mtSocket t)
    case mSock of
        Nothing -> pure ()
        Just sock -> do
            NS.close sock
            writeIORef (mtSocket t) Nothing

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

bytesToHex :: ByteString -> String
bytesToHex = concatMap toHex . BS.unpack
  where
    toHex b = [hexChar (b `div` 16), hexChar (b `mod` 16)]
    hexChar c
        | c < 10    = toEnum (fromIntegral c + fromEnum '0')
        | otherwise = toEnum (fromIntegral c - 10 + fromEnum 'a')

strToBS :: String -> ByteString
strToBS = BS.pack . map (fromIntegral . fromEnum)

bsToStr :: ByteString -> String
bsToStr = map (toEnum . fromIntegral) . BS.unpack

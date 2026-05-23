-- SPDX-License-Identifier: Apache-2.0
-- | Signal Server transport — connects to a Signal-compatible server.
--
-- Uses the WebSocket framing from "UmbraVox.Bridge.Signal.WebSocket"
-- and the session crypto from "UmbraVox.Bridge.Signal.Session" to
-- provide a 'TransportHandle' that sends and receives encrypted
-- Signal protocol messages.
--
-- See: doc/spec/network.md
module UmbraVox.Network.Transport.SignalServer
    ( SignalServerTransport(..)
    , SignalServerConfig(..)
    , newSignalServerTransport
    ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import UmbraVox.Network.TransportClass (TransportHandle(..))

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

-- | Configuration for a Signal-compatible server transport.
data SignalServerConfig = SignalServerConfig
    { sscHost      :: !String
      -- ^ Signal server hostname.
    , sscPort      :: !Int
      -- ^ Server port (typically 443 for TLS, 8080 for dev).
    , sscAuthToken :: !ByteString
      -- ^ Authentication credential (Basic or Bearer token).
    , sscDeviceId  :: !Int
      -- ^ Local device ID for envelope construction.
    } deriving stock (Show)

------------------------------------------------------------------------
-- Transport
------------------------------------------------------------------------

-- | A transport that connects to a Signal-compatible server using
-- the Signal WebSocket sub-protocol.
--
-- Outgoing messages are framed as Signal WebSocket requests (verb +
-- path + protobuf body).  Incoming data is read from the raw TCP
-- socket; the caller is responsible for WebSocket frame parsing via
-- the "UmbraVox.Bridge.Signal.WebSocket" module.
data SignalServerTransport = SignalServerTransport
    { sstConfig  :: !SignalServerConfig
    , sstSocket  :: !(IORef (Maybe NS.Socket))
      -- ^ Underlying TCP socket to the Signal server.
    , sstReadBuf :: !(MVar ByteString)
      -- ^ Buffered unread bytes for partial reads.
    }

instance TransportHandle SignalServerTransport where
    thSend  = signalSend
    thRecv  = signalRecv
    thClose = signalClose
    thInfo t = "signal:" ++ sscHost (sstConfig t)
            ++ ":" ++ show (sscPort (sstConfig t))
            ++ "/device/" ++ show (sscDeviceId (sstConfig t))

-- | Create a new Signal server transport and connect to the server.
--
-- Opens a TCP connection and sends the WebSocket upgrade request
-- for the Signal sub-protocol (/v1/websocket/).  TLS is not performed
-- at this layer; use a TLS wrapper for production.
newSignalServerTransport :: SignalServerConfig -> IO SignalServerTransport
newSignalServerTransport cfg = do
    let hints = NS.defaultHints
            { NS.addrSocketType = NS.Stream
            , NS.addrFamily     = NS.AF_UNSPEC
            }
    addrs <- NS.getAddrInfo (Just hints) (Just (sscHost cfg)) (Just (show (sscPort cfg)))
    sock <- case addrs of
        []       -> ioError (userError ("signal: no address for " ++ sscHost cfg))
        (addr:_) -> do
            s <- NS.openSocket addr
            NS.connect s (NS.addrAddress addr)
            pure s
    -- Send WebSocket upgrade request for Signal sub-protocol
    let authHeader = "Authorization: Basic " ++ bsToStr (sscAuthToken cfg)
        upgradeReq = strToBS
            ( "GET /v1/websocket/?login="
           ++ show (sscDeviceId cfg)
           ++ " HTTP/1.1\r\n"
           ++ "Host: " ++ sscHost cfg ++ "\r\n"
           ++ "Upgrade: websocket\r\n"
           ++ "Connection: Upgrade\r\n"
           ++ authHeader ++ "\r\n"
           ++ "Sec-WebSocket-Version: 13\r\n"
           ++ "Sec-WebSocket-Protocol: signal\r\n"
           ++ "\r\n"
            )
    NSB.sendAll sock upgradeReq
    sockRef <- newIORef (Just sock)
    bufRef  <- newMVar BS.empty
    pure SignalServerTransport
        { sstConfig  = cfg
        , sstSocket  = sockRef
        , sstReadBuf = bufRef
        }

------------------------------------------------------------------------
-- Send
------------------------------------------------------------------------

-- | Send a message to the Signal server.
--
-- The ByteString payload is expected to be a pre-framed WebSocket
-- message (constructed via "UmbraVox.Bridge.Signal.WebSocket").
-- This function sends it as-is over the TCP connection.
signalSend :: SignalServerTransport -> ByteString -> IO ()
signalSend t bs = do
    mSock <- readIORef (sstSocket t)
    case mSock of
        Nothing -> ioError (userError "signal: transport closed")
        Just sock -> NSB.sendAll sock bs

------------------------------------------------------------------------
-- Receive
------------------------------------------------------------------------

-- | Receive bytes from the Signal server connection.
signalRecv :: SignalServerTransport -> Int -> IO ByteString
signalRecv t n = modifyMVar (sstReadBuf t) $ \buf ->
    if BS.length buf >= n
        then pure (BS.drop n buf, BS.take n buf)
        else do
            mSock <- readIORef (sstSocket t)
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

signalClose :: SignalServerTransport -> IO ()
signalClose t = do
    mSock <- readIORef (sstSocket t)
    case mSock of
        Nothing -> pure ()
        Just sock -> do
            NS.close sock
            writeIORef (sstSocket t) Nothing

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

strToBS :: String -> ByteString
strToBS = BS.pack . map (fromIntegral . fromEnum)

bsToStr :: ByteString -> String
bsToStr = map (toEnum . fromIntegral) . BS.unpack

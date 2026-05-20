-- SPDX-License-Identifier: Apache-2.0
-- | Signal WebSocket client module (M19.3.5)
--
-- Provides the WebSocket transport layer for the Signal bridge plugin.
-- Signal uses WebSockets with a custom binary framing (protobuf
-- request/response) for real-time message delivery.  This module owns
-- connection lifecycle and message send/receive; the crypto + protobuf
-- layer lives in "UmbraVox.Bridge.Signal.Session".
--
-- Current status: stub — all I/O functions return errors or defaults.
-- Signal-Server integration is pending.
module UmbraVox.Bridge.Signal.WebSocket
    ( SignalWSConnection(..)
    , WSRequest(..)
    , WSResponse(..)
    , WSMessage(..)
    , WSError(..)
    , connectSignalWS
    , disconnectSignalWS
    , sendWSMessage
    , recvWSMessage
    , wsHealthCheck
    ) where

import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import Network.Socket (Socket)

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
-- Connection lifecycle
------------------------------------------------------------------------

-- | Attempt to open a WebSocket connection to a Signal server.
--
-- Returns @Left@ with an error until Signal-Server integration is
-- complete.
connectSignalWS
    :: String       -- ^ Server hostname
    -> Int          -- ^ Server port
    -> ByteString   -- ^ Initial auth token
    -> IO (Either WSError SignalWSConnection)
connectSignalWS host port token = do
    tokenRef <- newIORef token
    stateRef <- newIORef ConnDisconnected
    sockRef  <- newIORef Nothing
    reqIdRef <- newIORef 0
    let _conn = SignalWSConnection
            { wscServerHost = host
            , wscServerPort = port
            , wscAuthToken  = tokenRef
            , wscState      = stateRef
            , wscSocket     = sockRef
            , wscRequestId  = reqIdRef
            }
    -- Stub: Signal-Server integration pending.
    pure $ Left (WSConnectionFailed
        "not connected: Signal-Server integration pending")

-- | Gracefully close a WebSocket connection.
--
-- Safe to call on already-closed connections (no-op).
disconnectSignalWS :: SignalWSConnection -> IO ()
disconnectSignalWS conn = do
    writeIORef (wscState conn) ConnDisconnected
    mSock <- readIORef (wscSocket conn)
    case mSock of
        Nothing -> pure ()
        Just _s -> do
            -- When real: Network.Socket.close s
            writeIORef (wscSocket conn) Nothing

------------------------------------------------------------------------
-- Message I/O
------------------------------------------------------------------------

-- | Send a WebSocket message.  Stub — always returns an error.
sendWSMessage :: SignalWSConnection -> WSMessage -> IO (Either WSError ())
sendWSMessage conn _msg = do
    st <- readIORef (wscState conn)
    case st of
        ConnConnected -> pure $ Left (WSConnectionFailed
            "send not implemented: Signal-Server integration pending")
        _ -> pure $ Left (WSClosed "connection not in Connected state")

-- | Receive the next WebSocket message.  Stub — always returns an error.
recvWSMessage :: SignalWSConnection -> IO (Either WSError WSMessage)
recvWSMessage conn = do
    st <- readIORef (wscState conn)
    case st of
        ConnConnected -> pure $ Left (WSConnectionFailed
            "recv not implemented: Signal-Server integration pending")
        _ -> pure $ Left (WSClosed "connection not in Connected state")

-- | Quick health-check: is the connection alive?
--
-- Stub — always returns @False@.
wsHealthCheck :: SignalWSConnection -> IO Bool
wsHealthCheck conn = do
    st <- readIORef (wscState conn)
    pure (st == ConnConnected)

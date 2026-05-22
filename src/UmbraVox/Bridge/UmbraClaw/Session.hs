-- SPDX-License-Identifier: Apache-2.0
-- | UmbraClaw bridge session management (M25.1.3, M25.2.1, M25.2.3)
--
-- Session state for UmbraClaw connections.  Includes authentication
-- state machine and contact list sync.
module UmbraVox.Bridge.UmbraClaw.Session
    ( UmbraClawSession(..)
    , SessionState(..)
    , SessionError(..)
    , AuthState(..)
    , UmbraClawContact(..)
    , initSession
    , closeSession
    , sessionReady
    , authenticate
    , syncContacts
    , enqueueSend
    , dequeueRecv
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Session lifecycle state.
data SessionState
    = SessionDisconnected
    | SessionConnecting
    | SessionConnected
    | SessionClosing
    deriving stock (Eq, Show)

-- | Authentication state machine (M25.2.1).
data AuthState
    = AuthNone           -- ^ Not authenticated.
    | AuthPending        -- ^ Auth request sent, waiting for response.
    | AuthComplete       -- ^ Authenticated successfully.
    | AuthFailed String  -- ^ Authentication failed with reason.
    deriving stock (Show, Eq)

-- | Errors that can occur during session operations.
data SessionError
    = SessionNotEstablished
    | ConnectionFailed String
    | AuthenticationFailed String
    | TransportError String
    deriving stock (Show)

-- | An UmbraClaw contact (M25.2.3).
data UmbraClawContact = UmbraClawContact
    { uccId       :: !ByteString
    , uccName     :: !String
    , uccStatus   :: !String
    } deriving stock (Show, Eq)

-- | A live (or pending) UmbraClaw session.
data UmbraClawSession = UmbraClawSession
    { ucsHost    :: !String
      -- ^ UmbraClaw endpoint hostname.
    , ucsPort    :: !Int
      -- ^ UmbraClaw endpoint port.
    , ucsState   :: !(IORef SessionState)
      -- ^ Current session lifecycle state.
    , ucsReady   :: !(IORef Bool)
      -- ^ Whether the session has completed key agreement.
    , ucsAuth    :: !(IORef AuthState)
      -- ^ Current authentication state.
    , ucsContacts :: !(IORef [UmbraClawContact])
      -- ^ Cached contact list.
    , ucsSendQueue :: !(IORef [ByteString])
      -- ^ Outbound message queue (FIFO via reverse-cons).
    , ucsRecvQueue :: !(IORef [ByteString])
      -- ^ Inbound message queue (FIFO via reverse-cons).
    }

------------------------------------------------------------------------
-- Session lifecycle
------------------------------------------------------------------------

-- | Initialize a new UmbraClaw session.
--
-- Creates the session in disconnected state with no authentication.
initSession :: String    -- ^ Endpoint hostname
            -> Int       -- ^ Endpoint port
            -> IO UmbraClawSession
initSession host port = do
    stateRef    <- newIORef SessionDisconnected
    readyRef    <- newIORef False
    authRef     <- newIORef AuthNone
    contactsRef <- newIORef []
    sendQRef    <- newIORef []
    recvQRef    <- newIORef []
    pure UmbraClawSession
        { ucsHost      = host
        , ucsPort      = port
        , ucsState     = stateRef
        , ucsReady     = readyRef
        , ucsAuth      = authRef
        , ucsContacts  = contactsRef
        , ucsSendQueue = sendQRef
        , ucsRecvQueue = recvQRef
        }

-- | Close the session.
closeSession :: UmbraClawSession -> IO ()
closeSession session = do
    writeIORef (ucsState session) SessionClosing
    writeIORef (ucsReady session) False
    writeIORef (ucsAuth session) AuthNone

-- | Check whether the session is ready for message exchange.
sessionReady :: UmbraClawSession -> IO Bool
sessionReady session = readIORef (ucsReady session)

------------------------------------------------------------------------
-- Authentication (M25.2.1)
------------------------------------------------------------------------

-- | Initiate authentication with credentials.
--
-- Validates the username and token, transitions through AuthPending
-- to AuthComplete on success or AuthFailed on error.
--
-- Real UmbraClaw server communication is deferred to a later milestone;
-- currently validates that credentials are non-empty and marks the
-- session as authenticated and ready.
authenticate :: UmbraClawSession -> ByteString -> ByteString -> IO AuthState
authenticate session username token = do
    writeIORef (ucsAuth session) AuthPending
    writeIORef (ucsState session) SessionConnecting
    -- Validate credentials (real server handshake deferred)
    if BS.null username
        then do
            let st = AuthFailed "empty username"
            writeIORef (ucsAuth session) st
            writeIORef (ucsState session) SessionDisconnected
            pure st
        else if BS.null token
            then do
                let st = AuthFailed "empty token"
                writeIORef (ucsAuth session) st
                writeIORef (ucsState session) SessionDisconnected
                pure st
            else do
                writeIORef (ucsAuth session) AuthComplete
                writeIORef (ucsState session) SessionConnected
                writeIORef (ucsReady session) True
                pure AuthComplete

------------------------------------------------------------------------
-- Contact sync (M25.2.3)
------------------------------------------------------------------------

-- | Synchronize the contact list from the UmbraClaw server.
--
-- Real server communication is deferred; currently returns the
-- cached contact list (initially empty).
syncContacts :: UmbraClawSession -> IO [UmbraClawContact]
syncContacts session = readIORef (ucsContacts session)

------------------------------------------------------------------------
-- Message queues (M25.2.2)
------------------------------------------------------------------------

-- | Enqueue an outbound message for sending.
enqueueSend :: UmbraClawSession -> ByteString -> IO ()
enqueueSend session msg =
    modifyIORef' (ucsSendQueue session) (++ [msg])

-- | Dequeue the next inbound message, if any.
dequeueRecv :: UmbraClawSession -> IO (Maybe ByteString)
dequeueRecv session = do
    queue <- readIORef (ucsRecvQueue session)
    case queue of
        []     -> pure Nothing
        (x:xs) -> do
            writeIORef (ucsRecvQueue session) xs
            pure (Just x)

-- SPDX-License-Identifier: Apache-2.0
-- | UmbraClaw bridge session management (M25.1.3)
--
-- Session state for UmbraClaw connections.  Stub with type definitions;
-- real transport and crypto integration deferred to later milestones.
module UmbraVox.Bridge.UmbraClaw.Session
    ( UmbraClawSession(..)
    , SessionState(..)
    , SessionError(..)
    , initSession
    , closeSession
    , sessionReady
    ) where

import Data.IORef (IORef, newIORef, readIORef)

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

-- | Errors that can occur during session operations.
data SessionError
    = SessionNotEstablished
    | ConnectionFailed String
    | AuthenticationFailed String
    | TransportError String
    deriving stock (Show)

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
    }

------------------------------------------------------------------------
-- Session lifecycle
------------------------------------------------------------------------

-- | Initialize a new UmbraClaw session (stub).
--
-- Creates the session in disconnected state.  Real key agreement
-- and transport setup will be implemented in later milestones.
initSession :: String    -- ^ Endpoint hostname
            -> Int       -- ^ Endpoint port
            -> IO UmbraClawSession
initSession host port = do
    stateRef <- newIORef SessionDisconnected
    readyRef <- newIORef False
    pure UmbraClawSession
        { ucsHost  = host
        , ucsPort  = port
        , ucsState = stateRef
        , ucsReady = readyRef
        }

-- | Close the session (stub -- no-op for disconnected sessions).
closeSession :: UmbraClawSession -> IO ()
closeSession _session = pure ()

-- | Check whether the session is ready for message exchange.
sessionReady :: UmbraClawSession -> IO Bool
sessionReady session = readIORef (ucsReady session)

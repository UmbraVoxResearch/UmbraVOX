-- SPDX-License-Identifier: Apache-2.0
-- | Signal bridge session state (M19.3.2)
--
-- Wraps the Double Ratchet state with connection metadata needed by the
-- bridge plugin.  Stub implementations are provided; the real Signal
-- Server integration will be added in Phase 4.
module UmbraVox.Bridge.Signal.Session
    ( SignalBridgeSession(..)
    , BridgeSessionError(..)
    , initBridgeSession
    , sendBridgeMessage
    , recvBridgeMessage
    ) where

import Data.ByteString (ByteString)

import UmbraVox.Crypto.Signal.DoubleRatchet (RatchetState)

-- | Bridge session combining ratchet state with connection metadata.
data SignalBridgeSession = SignalBridgeSession
    { sbsRatchet  :: RatchetState
      -- ^ Current Double Ratchet state for this peer.
    , sbsEndpoint :: String
      -- ^ Remote endpoint identifier (e164 number or username).
    , sbsReady    :: Bool
      -- ^ Whether the session has completed key agreement.
    }

-- | Errors that can occur during bridge session operations.
data BridgeSessionError
    = SessionNotEstablished
    | SessionRatchetError String
    | SessionTransportError String
    deriving stock (Show)

-- | Initialize a bridge session.  Stub -- returns an error until the
-- Signal Server VM is available.
initBridgeSession :: String -> IO (Either BridgeSessionError SignalBridgeSession)
initBridgeSession _endpoint =
    pure (Left SessionNotEstablished)

-- | Encrypt and send a message through the bridge.  Stub.
sendBridgeMessage :: SignalBridgeSession -> ByteString -> IO (Either BridgeSessionError SignalBridgeSession)
sendBridgeMessage _session _plaintext =
    pure (Left SessionNotEstablished)

-- | Receive and decrypt a message from the bridge.  Stub.
recvBridgeMessage :: SignalBridgeSession -> IO (Either BridgeSessionError (ByteString, SignalBridgeSession))
recvBridgeMessage _session =
    pure (Left SessionNotEstablished)

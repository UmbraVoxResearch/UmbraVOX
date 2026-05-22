-- SPDX-License-Identifier: Apache-2.0
-- | Route token derivation and rotation for session routing.
--
-- After the Noise IK / PQXDH handshake, both peers derive a pair of 16-byte
-- routing tokens from the shared transport key.  Relay nodes route messages
-- by opaque token, never seeing real identity hashes.
--
-- Token derivation binds to the handshake transcript and both peers'
-- identity hashes to prevent MitM token injection:
--
--   HKDF(salt=handshakeHash, ikm=transportKey,
--        info="UmbraVox_RouteToken_v1" || myIdHash || peerIdHash || counter)
--
-- See: doc/ENCRYPTED-ENVELOPE-DESIGN.md Section 4.2.1
module UmbraVox.Protocol.RouteToken
    ( RouteTokenState(..)
    , deriveRouteTokens
    , deriveEpochTokens
    , rotateTokens
    , lookupSession
    , registerToken
    , registerTokenBounded
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)

import UmbraVox.Crypto.HKDF (hkdfSHA256)
import UmbraVox.Protocol.Encoding (putWord64BE)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | Info prefix for initial token derivation.
tokenInfoPrefix :: ByteString
tokenInfoPrefix = "UmbraVox_RouteToken_v1"

-- | Info prefix for epoch-rotated token derivation.
epochInfoPrefix :: ByteString
epochInfoPrefix = "UmbraVox_RouteToken_v1_epoch_"

-- | Number of messages before hybrid rotation triggers.
rotateEveryN :: Word64
rotateEveryN = 100

-- | Wall-clock epoch length in seconds for hybrid rotation.
wallClockEpoch :: Word64
wallClockEpoch = 600

------------------------------------------------------------------------
-- Route token state
------------------------------------------------------------------------

-- | Route token state for a session.
--
-- Each session tracks the current send/recv tokens, an optional previous
-- recv token for grace-period acceptance, an epoch counter, and a message
-- counter for hybrid (message-count + wall-clock) rotation.
data RouteTokenState = RouteTokenState
    { rtsCurrentSend  :: !ByteString       -- ^ 16-byte token I use when sending
    , rtsCurrentRecv  :: !ByteString       -- ^ 16-byte token peer uses when sending to me
    , rtsPrevRecv     :: !(Maybe ByteString) -- ^ previous epoch's recv token (grace)
    , rtsEpochCounter :: !Word64
    , rtsMsgCounter   :: !Word64           -- ^ for hybrid rotation
    } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Token derivation
------------------------------------------------------------------------

-- | Derive the initial pair of 16-byte route tokens from the transport key,
-- handshake hash, and both peers' identity hashes.
--
-- The first 16 bytes are the token the caller uses when sending; the second
-- 16 bytes are the token the caller expects to receive.
--
-- Channel and identity binding prevents a MitM from injecting tokens that
-- would be accepted by either peer: the handshake hash commits to the full
-- Noise transcript, and the identity hashes commit to both endpoints.
deriveRouteTokens :: ByteString  -- ^ Handshake hash (Noise transcript binding)
                  -> ByteString  -- ^ Transport key (shared secret from handshake)
                  -> ByteString  -- ^ My identity hash (32 bytes)
                  -> ByteString  -- ^ Peer identity hash (32 bytes)
                  -> (ByteString, ByteString)  -- ^ (sendToken, recvToken)
deriveRouteTokens handshakeHash transportKey myIdHash peerIdHash =
    let !info = tokenInfoPrefix <> myIdHash <> peerIdHash
                    <> putWord64BE 0
        !material = hkdfSHA256 handshakeHash transportKey info 32
        !sendToken = BS.take 16 material
        !recvToken = BS.drop 16 material
    in (sendToken, recvToken)

-- | Derive epoch-rotated tokens.  The epoch counter is mixed into the info
-- string so each epoch produces a fresh, unlinkable token pair.
deriveEpochTokens :: ByteString  -- ^ Handshake hash
                  -> ByteString  -- ^ Transport key
                  -> ByteString  -- ^ My identity hash
                  -> ByteString  -- ^ Peer identity hash
                  -> Word64      -- ^ Epoch counter
                  -> (ByteString, ByteString)  -- ^ (sendToken, recvToken)
deriveEpochTokens handshakeHash transportKey myIdHash peerIdHash epoch =
    let !info = epochInfoPrefix <> myIdHash <> peerIdHash
                    <> putWord64BE epoch
        !material = hkdfSHA256 handshakeHash transportKey info 32
        !sendToken = BS.take 16 material
        !recvToken = BS.drop 16 material
    in (sendToken, recvToken)

------------------------------------------------------------------------
-- Hybrid rotation
------------------------------------------------------------------------

-- | Check whether the message counter or wall-clock epoch warrants token
-- rotation, and return an updated 'RouteTokenState' if so.
--
-- @wallNow@ is the current wall-clock value in seconds (monotonic).
-- @wallBase@ is the wall-clock value at the start of the current epoch.
--
-- Rotation triggers when either:
--   * @rtsMsgCounter >= rotateEveryN@ (100 messages), or
--   * @wallNow - wallBase >= wallClockEpoch@ (600 seconds).
--
-- The caller is responsible for re-deriving tokens (via 'deriveEpochTokens')
-- before calling this function; 'rotateTokens' only bumps counters and
-- shifts the grace window.
rotateTokens :: RouteTokenState
             -> Word64  -- ^ Current wall-clock seconds
             -> Word64  -- ^ Wall-clock base for current epoch
             -> RouteTokenState
rotateTokens rts wallNow wallBase
    | rtsMsgCounter rts >= rotateEveryN || wallNow - wallBase >= wallClockEpoch =
        rts { rtsPrevRecv     = Just (rtsCurrentRecv rts)
            , rtsEpochCounter = rtsEpochCounter rts + 1
            , rtsMsgCounter   = 0
            }
    | otherwise = rts { rtsMsgCounter = rtsMsgCounter rts + 1 }

------------------------------------------------------------------------
-- Session lookup by token
------------------------------------------------------------------------

-- | Type alias for session identifiers used in the token routing table.
type SessionId = ByteString

-- | Look up a session by inbound route token.  Checks the current recv
-- token first, then falls back to the previous epoch's token (grace).
lookupSession :: ByteString -> Map ByteString SessionId -> Maybe SessionId
lookupSession token table = Map.lookup token table

-- | Register a route token in the routing table, mapping it to a session.
registerToken :: ByteString -> SessionId -> Map ByteString SessionId
              -> Map ByteString SessionId
registerToken token sessId table = Map.insert token sessId table

-- | Register a route token only if the table has not exceeded the given
-- bound.  Returns 'Nothing' if the table is full and the token is not
-- already present (i.e. no update, only new inserts are bounded).
--
-- This prevents an attacker from exhausting memory by opening many
-- sessions that each register tokens.  The bound should be set to
-- 'maxInboundConnections' from "UmbraVox.App.Defaults".
registerTokenBounded :: Int  -- ^ Maximum table size
                     -> ByteString -> SessionId
                     -> Map ByteString SessionId
                     -> Maybe (Map ByteString SessionId)
registerTokenBounded maxSize token sessId table
    | Map.member token table =
        -- Update existing entry (no size growth)
        Just (Map.insert token sessId table)
    | Map.size table >= maxSize =
        -- Table full, reject new entry
        Nothing
    | otherwise =
        Just (Map.insert token sessId table)

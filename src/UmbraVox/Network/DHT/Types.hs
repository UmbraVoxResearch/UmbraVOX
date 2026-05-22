-- SPDX-License-Identifier: Apache-2.0
-- | DHT core types for Kademlia-style distributed hash table (M24.3.1).
--
-- Node identifiers are 256-bit values derived from SHA-256 of the
-- node's identity public key.  The XOR distance metric is used for
-- Kademlia routing.
module UmbraVox.Network.DHT.Types
    ( -- * Node identity
      NodeId(..)
    , xorDistance
    , deriveNodeId
    , verifyNodeId
      -- * DHT nodes
    , DHTNode(..)
      -- * K-buckets
    , KBucket(..)
    , emptyKBucket
      -- * Messages
    , DHTMessage(..)
      -- * Configuration
    , DHTConfig(..)
    , defaultDHTConfig
    ) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.Word (Word64)

import qualified Data.ByteString as BS

import UmbraVox.Crypto.SHA256 (sha256)

------------------------------------------------------------------------
-- Node identity
------------------------------------------------------------------------

-- | 256-bit node identifier derived from SHA-256(identity public key).
newtype NodeId = NodeId ByteString
    deriving stock (Eq, Ord, Show)

-- | XOR distance metric for Kademlia routing.
--
-- Returns a 32-byte ByteString representing the bitwise XOR of two
-- node identifiers.  The result is used to determine routing table
-- bucket placement and closest-node queries.
xorDistance :: NodeId -> NodeId -> ByteString
xorDistance (NodeId a) (NodeId b) =
    BS.pack (BS.zipWith xor a b)

-- | Derive a NodeId from an identity public key.
--
-- The node ID is simply SHA-256 of the raw public key bytes.  During
-- Noise IK handshake, the remote's claimed node ID is verified against
-- the handshake public key to prevent Sybil attacks.
deriveNodeId :: ByteString -> NodeId
deriveNodeId pubKey = NodeId (sha256 pubKey)

-- | Verify that a claimed NodeId matches the SHA-256 hash of the given
-- identity public key.  Returns True if valid.
verifyNodeId :: NodeId -> ByteString -> Bool
verifyNodeId (NodeId claimed) pubKey = claimed == sha256 pubKey

------------------------------------------------------------------------
-- DHT nodes
------------------------------------------------------------------------

-- | A node in the DHT network.
data DHTNode = DHTNode
    { dhtNodeId   :: !NodeId
    , dhtAddress  :: !String       -- ^ host:port
    , dhtLastSeen :: !Word64       -- ^ POSIX seconds
    , dhtRTT      :: !(Maybe Int)  -- ^ round-trip time in microseconds
    } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- K-buckets
------------------------------------------------------------------------

-- | A k-bucket entry with replacement cache.
--
-- Active entries are ordered most-recently-seen last (tail is freshest).
-- When the active list is full, new nodes go into the replacement cache
-- until an active node becomes unresponsive and is evicted.
data KBucket = KBucket
    { kbEntries     :: ![DHTNode]   -- ^ active nodes, max k
    , kbReplacement :: ![DHTNode]   -- ^ replacement cache, max k
    } deriving stock (Show, Eq)

-- | An empty k-bucket with no entries and no replacements.
emptyKBucket :: KBucket
emptyKBucket = KBucket [] []

------------------------------------------------------------------------
-- Messages
------------------------------------------------------------------------

-- | DHT RPC messages.
--
-- All messages carry the sender's NodeId for routing table updates.
-- Messages are serialized as compact binary over Noise IK channels
-- (no cleartext DHT traffic).
data DHTMessage
    = Ping          !NodeId
    | Pong          !NodeId
    | FindNode      !NodeId !NodeId             -- ^ sender, target
    | FindNodeReply !NodeId ![DHTNode]          -- ^ sender, closest k nodes
    | Store         !NodeId !ByteString !ByteString  -- ^ sender, key, value
    | FindValue     !NodeId !ByteString         -- ^ sender, key
    | FindValueReply !NodeId !(Either [DHTNode] ByteString)
                                                -- ^ sender, nodes or value
    deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

-- | DHT configuration parameters.
data DHTConfig = DHTConfig
    { dhtK                 :: !Int      -- ^ bucket size (default 20)
    , dhtAlpha             :: !Int      -- ^ parallel queries (default 3)
    , dhtRefreshInterval   :: !Int      -- ^ bucket refresh seconds (3600)
    , dhtRepublishInterval :: !Int      -- ^ value republish seconds (3600)
    , dhtExpireInterval    :: !Int      -- ^ stored value TTL seconds (86400)
    , dhtBootstrapNodes    :: ![String] -- ^ initial bootstrap addresses
    } deriving stock (Show, Eq)

-- | Sensible defaults for DHT operation.
defaultDHTConfig :: DHTConfig
defaultDHTConfig = DHTConfig
    { dhtK                 = 20
    , dhtAlpha             = 3
    , dhtRefreshInterval   = 3600
    , dhtRepublishInterval = 3600
    , dhtExpireInterval    = 86400
    , dhtBootstrapNodes    = []
    }

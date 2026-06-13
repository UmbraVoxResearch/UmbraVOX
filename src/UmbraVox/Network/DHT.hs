-- SPDX-License-Identifier: Apache-2.0
-- | Top-level DHT state and message handling (M24.3.6).
--
-- Combines the routing table, value store, and configuration into a
-- single 'DHTState'.  Provides message dispatch for incoming DHT RPCs
-- and a helper for computing announcement keys.
--
-- This module does not own transport — network I/O will be wired in
-- during the integration phase (M24.4).
module UmbraVox.Network.DHT
    ( DHTState(..)
    , DHTRoutingPolicy(..)
    , AdaptiveParams(..)
    , newDHTState
    , announceKey
    , routingPolicy
    , handleMessage
    , maintain
    , bootstrap
    , estimateNetworkSize
    , suggestParams
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Word (Word64)

import qualified UmbraVox.Crypto.Generated.FFI.SHA256 as SHA256FFI
import Data.IORef (readIORef)

import UmbraVox.Network.DHT.Lookup
    ( SendRPC
    , iterativeFindNode
    )
import UmbraVox.Network.DHT.RoutingTable
    ( RoutingTable(..)
    , newRoutingTable
    , insertNode
    , removeNode
    , findClosest
    )
import UmbraVox.Network.DHT.Store
    ( ValueStore
    , newValueStore
    , localStore
    , localLookup
    , expireEntries
    )
import UmbraVox.Network.DHT.Types
    ( NodeId(..)
    , DHTNode(..)
    , KBucket(..)
    , DHTConfig(..)
    , DHTMessage(..)
    , deriveNodeId
    , deriveEphemeralNodeId
    )

------------------------------------------------------------------------
-- DHT state
------------------------------------------------------------------------

-- | Top-level DHT state combining configuration, routing, and storage.
data DHTState = DHTState
    { dhConfig       :: !DHTConfig
    , dhRoutingTable :: !RoutingTable
    , dhStore        :: !ValueStore
    , dhSelfId       :: !NodeId
    }

-- | Create a new DHT state from configuration and identity public key.
--
-- The 'NodeId' is derived as @SHA-256(identityPublicKey)@.  The routing
-- table is initialised with @dhtK@ bucket capacity and the value store
-- with a 1024-byte maximum value size.
--
-- If an optional boot salt is provided (M27.2.2), an ephemeral NodeId
-- is derived via @SHA-256(pubKey <> bootSalt)@ instead, preventing
-- linkage of DHT presence to long-term identity across sessions.
newDHTState :: DHTConfig -> ByteString -> Maybe ByteString -> IO DHTState
newDHTState cfg identityPubKey mBootSalt = do
    selfId <- case mBootSalt of
            Nothing       -> deriveNodeId identityPubKey
            Just bootSalt -> deriveEphemeralNodeId identityPubKey bootSalt
    rt <- newRoutingTable selfId (dhtK cfg)
    vs <- newValueStore 1024
    return DHTState
        { dhConfig       = cfg
        , dhRoutingTable = rt
        , dhStore        = vs
        , dhSelfId       = selfId
        }

------------------------------------------------------------------------
-- Dandelion++ routing policy (M24.4.2)
------------------------------------------------------------------------

-- | Routing policy for a DHT operation.
data DHTRoutingPolicy
    = DHTStemRoute    -- ^ Route through Dandelion++ stem (self-lookups, self-stores)
    | DHTDirectRoute  -- ^ Route directly (lookups for other nodes)
    deriving stock (Show, Eq)

-- | Determine the routing policy for a DHT message.
-- Self-referencing operations (looking up our own ID, storing our presence)
-- use stem routing for privacy.
routingPolicy :: DHTState -> DHTMessage -> IO DHTRoutingPolicy
routingPolicy st msg = do
    myAnnounceKey <- announceKey (dhSelfId st)
    pure $ case msg of
        FindNode _ target
            | target == dhSelfId st -> DHTStemRoute
            | otherwise             -> DHTDirectRoute
        Store _ key _
            | key == myAnnounceKey -> DHTStemRoute
            | otherwise            -> DHTDirectRoute
        FindValue _ key
            | NodeId key == dhSelfId st -> DHTStemRoute
            | otherwise                 -> DHTDirectRoute
        _ -> DHTDirectRoute

------------------------------------------------------------------------
-- Announce key
------------------------------------------------------------------------

-- | Compute the storage key for a node's presence announcement.
--
-- The announce key is @SHA-256(nodeId bytes)@, giving a uniformly
-- distributed key in the DHT keyspace under which the node stores its
-- reachable address.
announceKey :: NodeId -> IO ByteString
announceKey (NodeId nidBytes) = SHA256FFI.sha256 nidBytes

------------------------------------------------------------------------
-- Message handling
------------------------------------------------------------------------

-- | Handle an incoming DHT message, returning a response if applicable.
--
-- Dispatches to the appropriate handler based on message type and
-- updates the routing table with the sender's information.  The
-- 'Word64' argument is the current POSIX timestamp, used for
-- @lastSeen@ updates and store expiration checks.
--
-- Message dispatch:
--
--   * 'Ping' — respond with 'Pong'.
--   * 'FindNode' — return the @k@ closest nodes to the target.
--   * 'Store' — store the value locally with TTL; no response.
--   * 'FindValue' — return the value if found, otherwise closest nodes.
--   * 'Pong', 'FindNodeReply', 'FindValueReply' — update routing table
--     only (responses to our outgoing queries; no reply generated).
handleMessage :: DHTState -> DHTMessage -> Word64 -> IO (Maybe DHTMessage)
handleMessage st msg now = do
    -- Update routing table with sender information for all message types.
    updateSender st msg now
    -- Dispatch by message type.
    case msg of
        Ping _senderId ->
            return (Just (Pong (dhSelfId st)))

        FindNode _senderId target -> do
            closest <- findClosest (dhRoutingTable st) target (dhtK (dhConfig st))
            return (Just (FindNodeReply (dhSelfId st) closest))

        Store _senderId key value -> do
            let expiry = now + fromIntegral (dhtExpireInterval (dhConfig st))
            _ <- localStore (dhStore st) key value expiry
            return Nothing

        FindValue _senderId key -> do
            mVal <- localLookup (dhStore st) key now
            case mVal of
                Just val ->
                    return (Just (FindValueReply (dhSelfId st) (Right val)))
                Nothing -> do
                    keyNodeId <- deriveNodeId key
                    closest <- findClosest (dhRoutingTable st) keyNodeId (dhtK (dhConfig st))
                    return (Just (FindValueReply (dhSelfId st) (Left closest)))

        -- Responses to our outgoing queries: no reply needed.
        Pong _           -> return Nothing
        FindNodeReply _ _ -> return Nothing
        FindValueReply _ _ -> return Nothing

-- | Update the routing table with the sender's node information.
--
-- Extracts the sender 'NodeId' from any message type and inserts a
-- stub 'DHTNode' with the current timestamp.  The address is left
-- empty since the transport layer will associate addresses separately.
updateSender :: DHTState -> DHTMessage -> Word64 -> IO ()
updateSender st msg now = do
    let senderId = messageSender msg
    let senderNode = DHTNode
            { dhtNodeId   = senderId
            , dhtAddress  = ""
            , dhtLastSeen = now
            , dhtRTT      = Nothing
            }
    _ <- insertNode (dhRoutingTable st) senderNode
    return ()

-- | Extract the sender 'NodeId' from a DHT message.
messageSender :: DHTMessage -> NodeId
messageSender (Ping nid)           = nid
messageSender (Pong nid)           = nid
messageSender (FindNode nid _)     = nid
messageSender (FindNodeReply nid _) = nid
messageSender (Store nid _ _)      = nid
messageSender (FindValue nid _)    = nid
messageSender (FindValueReply nid _) = nid

------------------------------------------------------------------------
-- Periodic maintenance
------------------------------------------------------------------------

-- | Perform periodic maintenance on the DHT state.
--
-- 1. Expire stale stored values via 'expireEntries'.
-- 2. Remove nodes whose 'dhtLastSeen' is older than
--    'dhtRefreshInterval' seconds ago from the routing table.
--
-- Returns @(expiredValues, staleNodes)@ — the number of expired
-- values and stale nodes removed.
maintain :: DHTState -> Word64 -> IO (Int, Int)
maintain st now = do
    -- Step 1: expire stale stored values.
    expiredCount <- expireEntries (dhStore st) now
    -- Step 2: evict stale nodes from routing table buckets.
    let rt = dhRoutingTable st
        refreshSecs = fromIntegral (dhtRefreshInterval (dhConfig st))
        cutoff = if now > refreshSecs then now - refreshSecs else 0
    buckets <- readIORef (rtBuckets rt)
    let staleNodes = concatMap (filter (\n -> dhtLastSeen n < cutoff) . kbEntries) buckets
    mapM_ (removeNode rt . dhtNodeId) staleNodes
    return (expiredCount, length staleNodes)

------------------------------------------------------------------------
-- Adaptive parameters (M24.6.6)
------------------------------------------------------------------------

-- | Suggested DHT parameters adapted to estimated network size.
-- Larger networks benefit from higher k (bucket size) and alpha
-- (parallel query) values to maintain lookup efficiency.
data AdaptiveParams = AdaptiveParams
    { apK     :: !Int    -- ^ suggested bucket size
    , apAlpha :: !Int    -- ^ suggested parallel queries
    } deriving stock (Show, Eq)

-- | Estimate the number of nodes in the network from the routing table.
--
-- Uses the density of the closest non-empty bucket to extrapolate.
-- Bucket @i@ covers a region of @2^i@ addresses in the XOR keyspace;
-- if it contains @n@ entries then the estimated total population is
-- @n * 2^(256 - i) / 2^i = n * 2^(256 - 2*i)@.  In practice we use
-- the simpler approximation @n * 2^i@ where @i@ is the index of the
-- closest non-empty bucket (higher index = closer = smaller region),
-- giving the density in the smallest observed neighbourhood.
--
-- Returns 0 if the routing table is empty.
estimateNetworkSize :: RoutingTable -> IO Int
estimateNetworkSize rt = do
    buckets <- readIORef (rtBuckets rt)
    -- Find the highest-index non-empty bucket (closest to self).
    let indexed = zip [0 :: Int ..] buckets
        nonEmpty = [(i, length (kbEntries b)) | (i, b) <- indexed
                                               , not (null (kbEntries b))]
    case nonEmpty of
        [] -> return 0
        _  -> do
            -- Use the closest (highest index) non-empty bucket.
            let (closestIdx, count) = last nonEmpty
            -- The bucket covers 2^(255 - closestIdx) of the keyspace.
            -- Density = count / 2^(255 - closestIdx); total = density * 2^256.
            -- Simplifies to: count * 2^(closestIdx + 1).
            -- Cap the shift to avoid absurd estimates on sparse tables.
            let shift = min closestIdx 30  -- cap to avoid overflow
            return (count * (2 ^ shift))

-- | Suggest DHT parameters based on estimated network size.
--
-- Small networks use conservative parameters; larger networks increase
-- bucket size and parallelism to keep lookup latency in O(log n).
suggestParams :: Int -> AdaptiveParams
suggestParams networkSize
    | networkSize < 100   = AdaptiveParams 10 2
    | networkSize < 1000  = AdaptiveParams 20 3
    | networkSize < 10000 = AdaptiveParams 20 5
    | otherwise           = AdaptiveParams 30 7

------------------------------------------------------------------------
-- Bootstrap (M24.4.6)
------------------------------------------------------------------------

-- | Bootstrap into the DHT network.
--
-- 1. For each bootstrap address, create a temporary 'DHTNode' with a
--    placeholder 'NodeId' (all zeros) and insert it into the routing
--    table.
-- 2. Perform an iterative FIND_NODE for our own 'NodeId' to discover
--    and populate nearby routing table buckets.
-- 3. Return the number of nodes discovered during the lookup.
bootstrap :: DHTState -> [String] -> SendRPC -> IO Int
bootstrap st addrs sendRPC = do
    -- Step 1: Insert bootstrap nodes with placeholder IDs.
    let placeholderId = NodeId (BS.replicate 32 0)
    mapM_ (\addr -> do
        let node = DHTNode
                { dhtNodeId   = placeholderId
                , dhtAddress  = addr
                , dhtLastSeen = 0
                , dhtRTT      = Nothing
                }
        _ <- insertNode (dhRoutingTable st) node
        return ()
        ) addrs
    -- Step 2: Iterative FIND_NODE for our own ID.
    discovered <- iterativeFindNode
        (dhRoutingTable st)
        (dhConfig st)
        sendRPC
        (dhSelfId st)
    -- Step 3: Return count of discovered nodes.
    return (length discovered)

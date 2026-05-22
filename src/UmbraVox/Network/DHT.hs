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
    , newDHTState
    , announceKey
    , routingPolicy
    , handleMessage
    , maintain
    ) where

import Data.ByteString (ByteString)
import Data.Word (Word64)

import UmbraVox.Crypto.SHA256 (sha256)
import Data.IORef (readIORef)

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
newDHTState :: DHTConfig -> ByteString -> IO DHTState
newDHTState cfg identityPubKey = do
    let selfId = deriveNodeId identityPubKey
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
routingPolicy :: DHTState -> DHTMessage -> DHTRoutingPolicy
routingPolicy st msg = case msg of
    FindNode _ target
        | target == dhSelfId st -> DHTStemRoute
        | otherwise             -> DHTDirectRoute
    Store _ key _
        | key == announceKey (dhSelfId st) -> DHTStemRoute
        | otherwise                        -> DHTDirectRoute
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
announceKey :: NodeId -> ByteString
announceKey (NodeId nidBytes) = sha256 nidBytes

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
                    closest <- findClosest (dhRoutingTable st) (deriveNodeId key) (dhtK (dhConfig st))
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

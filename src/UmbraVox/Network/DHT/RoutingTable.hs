-- SPDX-License-Identifier: Apache-2.0
-- | Kademlia k-bucket routing table (M24.3.2).
--
-- The routing table maintains 256 k-buckets (one per bit of the 256-bit
-- XOR distance space).  Bucket index 0 holds the most distant nodes
-- (highest XOR distance), bucket 255 holds the closest.
--
-- Each bucket stores up to k active nodes (default k=20) plus a
-- replacement cache of up to k candidates.
module UmbraVox.Network.DHT.RoutingTable
    ( RoutingTable(..)
    , InsertResult(..)
    , newRoutingTable
    , insertNode
    , removeNode
    , findClosest
    , bucketIndex
    ) where

import Data.Bits (testBit)
import Data.ByteString (ByteString)
import Data.IORef
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word8)

import qualified Data.ByteString as BS

import UmbraVox.Network.DHT.Types
    ( NodeId(..)
    , DHTNode(..)
    , KBucket(..)
    , emptyKBucket
    , xorDistance
    )

------------------------------------------------------------------------
-- Routing table
------------------------------------------------------------------------

-- | Routing table: 256 k-buckets indexed by XOR distance bit position.
data RoutingTable = RoutingTable
    { rtSelf    :: !NodeId
    , rtBuckets :: !(IORef [KBucket])  -- ^ 256 buckets, index 0 = farthest
    , rtK       :: !Int                -- ^ bucket capacity (default 20)
    }

-- | Result of attempting to insert a node.
data InsertResult
    = Inserted          -- ^ node added to active list
    | Updated           -- ^ existing node refreshed (lastSeen updated)
    | BucketFull        -- ^ bucket at capacity, node placed in replacement cache
    | SelfIgnored       -- ^ attempted to insert our own node ID
    deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | Create a new routing table for the given self ID.
newRoutingTable :: NodeId -> Int -> IO RoutingTable
newRoutingTable selfId k = do
    bucketsRef <- newIORef (replicate 256 emptyKBucket)
    return RoutingTable
        { rtSelf    = selfId
        , rtBuckets = bucketsRef
        , rtK       = k
        }

------------------------------------------------------------------------
-- Bucket index
------------------------------------------------------------------------

-- | Determine which bucket a target node falls into relative to self.
--
-- The bucket index is the position of the highest set bit in the XOR
-- distance.  Bucket 255 = closest (distance has highest bit at position 0),
-- bucket 0 = farthest (distance has highest bit at position 255).
--
-- Returns @-1@ if the distance is zero (target equals self), which
-- should not occur in normal operation.
bucketIndex :: NodeId -> NodeId -> Int
bucketIndex self target =
    let dist = xorDistance self target
    in if BS.all (== 0) dist
       then -1  -- self == target
       else 255 - countLeadingZeroBits dist

-- | Count leading zero bits in a ByteString treated as a big-endian
-- unsigned integer.
countLeadingZeroBits :: ByteString -> Int
countLeadingZeroBits bs = go 0
  where
    len = BS.length bs
    go i
      | i >= len  = i * 8
      | otherwise =
          let byte = BS.index bs i
          in if byte == 0
             then go (i + 1)
             else i * 8 + clzByte byte

-- | Count leading zero bits in a single byte.
clzByte :: Word8 -> Int
clzByte 0 = 8
clzByte b = go 7
  where
    go n
      | testBit b n = 7 - n
      | otherwise   = go (n - 1)

------------------------------------------------------------------------
-- Insert
------------------------------------------------------------------------

-- | Insert a node into the routing table.
--
-- Kademlia insert semantics:
--   1. If the node is self, ignore.
--   2. If the node already exists in the bucket, move it to the tail
--      (most recently seen) and update its lastSeen timestamp.
--   3. If the bucket has room, append the node.
--   4. If the bucket is full, place the node in the replacement cache.
insertNode :: RoutingTable -> DHTNode -> IO InsertResult
insertNode rt node = do
    let idx = bucketIndex (rtSelf rt) (dhtNodeId node)
    if idx < 0
        then return SelfIgnored
        else do
            buckets <- readIORef (rtBuckets rt)
            let bucket = buckets !! idx
                k = rtK rt
            case insertIntoBucket k node bucket of
                (bucket', result) -> do
                    let buckets' = replaceAt idx bucket' buckets
                    writeIORef (rtBuckets rt) buckets'
                    return result

-- | Insert a node into a single k-bucket, returning the updated bucket
-- and the result.
insertIntoBucket :: Int -> DHTNode -> KBucket -> (KBucket, InsertResult)
insertIntoBucket k node bucket =
    let entries = kbEntries bucket
        nid = dhtNodeId node
        existing = filter (\n -> dhtNodeId n == nid) entries
    in case existing of
        -- Node already in bucket: move to tail with updated timestamp
        (_:_) ->
            let entries' = filter (\n -> dhtNodeId n /= nid) entries ++ [node]
            in (bucket { kbEntries = entries' }, Updated)
        -- Node not in bucket and room available: append
        [] | length entries < k ->
            (bucket { kbEntries = entries ++ [node] }, Inserted)
        -- Bucket full: add to replacement cache (bounded to k)
        [] ->
            let repl = kbReplacement bucket
                repl' = filter (\n -> dhtNodeId n /= nid) repl ++ [node]
                repl'' = if length repl' > k then drop (length repl' - k) repl' else repl'
            in (bucket { kbReplacement = repl'' }, BucketFull)

------------------------------------------------------------------------
-- Remove
------------------------------------------------------------------------

-- | Remove a node from the routing table.
--
-- If the node is in the active list it is removed and the most recent
-- node from the replacement cache (if any) is promoted to the active
-- list.  If the node is only in the replacement cache it is removed
-- from there.
removeNode :: RoutingTable -> NodeId -> IO ()
removeNode rt nid = do
    let idx = bucketIndex (rtSelf rt) nid
    if idx < 0
        then return ()
        else do
            buckets <- readIORef (rtBuckets rt)
            let bucket = buckets !! idx
                bucket' = removeFromBucket nid bucket
                buckets' = replaceAt idx bucket' buckets
            writeIORef (rtBuckets rt) buckets'

-- | Remove a node from a bucket, promoting from replacement cache if
-- the node was in the active list.
removeFromBucket :: NodeId -> KBucket -> KBucket
removeFromBucket nid bucket =
    let entries = kbEntries bucket
        wasActive = any (\n -> dhtNodeId n == nid) entries
        entries' = filter (\n -> dhtNodeId n /= nid) entries
        repl = kbReplacement bucket
    in if wasActive
       then case repl of
           []     -> bucket { kbEntries = entries' }
           _      -> let promoted = last repl
                         repl' = init repl
                     in bucket { kbEntries = entries' ++ [promoted]
                               , kbReplacement = repl' }
       else -- Remove from replacement cache if present
           bucket { kbReplacement = filter (\n -> dhtNodeId n /= nid) repl }

------------------------------------------------------------------------
-- Lookup
------------------------------------------------------------------------

-- | Find the @n@ closest nodes to a target NodeId.
--
-- Collects nodes from all 256 buckets and returns the @n@ nearest by
-- XOR distance, sorted closest-first.
findClosest :: RoutingTable -> NodeId -> Int -> IO [DHTNode]
findClosest rt target n = do
    buckets <- readIORef (rtBuckets rt)
    let allNodes = concatMap kbEntries buckets
        sorted = sortBy (comparing (\node -> xorDistance (dhtNodeId node) target)) allNodes
    return (take n sorted)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Replace the element at index @i@ in a list.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs =
    let (before, _:after) = splitAt i xs
    in before ++ (x : after)

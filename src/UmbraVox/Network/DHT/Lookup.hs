-- SPDX-License-Identifier: Apache-2.0
-- | Iterative Kademlia lookup algorithm (M24.3.4).
--
-- Implements the iterative FIND_NODE and FIND_VALUE RPCs described in
-- section 1.4 of the DHT network plan.  Each lookup proceeds by
-- maintaining a shortlist of candidate nodes sorted by XOR distance to
-- the target, querying alpha unqueried candidates in parallel per
-- round, and terminating once the k closest candidates have all been
-- queried or no closer node is discovered in a round.
module UmbraVox.Network.DHT.Lookup
    ( -- * RPC callback
      SendRPC
      -- * Iterative lookups
    , iterativeFindNode
    , iterativeFindValue
    ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Data.ByteString (ByteString)
import Data.IORef
import Data.List (nubBy, sortBy)
import Data.Ord (comparing)

import qualified Data.Set as Set

import UmbraVox.Network.DHT.Types
    ( NodeId(..)
    , DHTNode(..)
    , DHTMessage(..)
    , DHTConfig(..)
    , xorDistance
    )

import UmbraVox.Network.DHT.RoutingTable
    ( RoutingTable(..)
    , findClosest
    )

------------------------------------------------------------------------
-- RPC callback
------------------------------------------------------------------------

-- | Callback for sending DHT messages.  The implementation provides
-- the actual transport mechanism (Noise IK encrypted channel).
-- Returns 'Nothing' on timeout or error.
type SendRPC = DHTNode -> DHTMessage -> IO (Maybe DHTMessage)

------------------------------------------------------------------------
-- Iterative FIND_NODE
------------------------------------------------------------------------

-- | Iterative FIND_NODE lookup.
--
-- 1. Seed the shortlist with the alpha closest nodes from the local
--    routing table.
-- 2. Send @FindNode@ to alpha closest unqueried nodes in parallel.
-- 3. On each reply, merge new nodes into the shortlist sorted by XOR
--    distance to the target.
-- 4. Repeat until the k closest nodes have all been queried or no
--    closer node was found in the last round.
-- 5. Return the k closest nodes.
iterativeFindNode
    :: RoutingTable
    -> DHTConfig
    -> SendRPC
    -> NodeId      -- ^ target node ID
    -> IO [DHTNode]
iterativeFindNode rt cfg sendRPC target = do
    seeds <- findClosest rt target (dhtK cfg)
    let selfId = rtSelf rt
    shortlistRef <- newIORef (sortByDistance target seeds)
    queriedRef   <- newIORef Set.empty
    lookupLoop selfId cfg sendRPC target shortlistRef queriedRef
    sl <- readIORef shortlistRef
    return (take (dhtK cfg) sl)

-- | Main lookup loop shared by FIND_NODE.  Runs rounds of parallel
-- queries until the termination condition is met.
lookupLoop
    :: NodeId          -- ^ our own node ID (for message sender field)
    -> DHTConfig
    -> SendRPC
    -> NodeId          -- ^ target
    -> IORef [DHTNode] -- ^ shortlist sorted by distance to target
    -> IORef (Set.Set ByteString) -- ^ set of queried NodeId bytes
    -> IO ()
lookupLoop selfId cfg sendRPC target shortlistRef queriedRef = go
  where
    k     = dhtK cfg
    alpha = dhtAlpha cfg

    go = do
        shortlist <- readIORef shortlistRef
        queried   <- readIORef queriedRef

        -- Pick alpha closest unqueried nodes
        let candidates = filter (not . isQueried queried) shortlist
            toQuery    = take alpha candidates

        -- Termination: nothing left to query, or k closest all queried
        if null toQuery || kClosestAllQueried k queried shortlist
            then return ()
            else do
                -- Mark as queried before sending
                let newQueried = foldr
                        (\n s -> Set.insert (nodeIdBytes (dhtNodeId n)) s)
                        queried toQuery
                writeIORef queriedRef newQueried

                -- Record the closest distance before this round
                let closestBefore = case shortlist of
                        []    -> Nothing
                        (n:_) -> Just (xorDistance (dhtNodeId n) target)

                -- Send queries in parallel using forkIO + MVar
                results <- queryParallel sendRPC selfId target toQuery

                -- Merge results into shortlist
                let newNodes = concatMap extractNodes results
                modifyIORef' shortlistRef $ \sl ->
                    dedup (sortByDistance target (sl ++ newNodes))

                -- Check if we found a strictly closer node
                shortlist' <- readIORef shortlistRef
                let closestAfter = case shortlist' of
                        []    -> Nothing
                        (n:_) -> Just (xorDistance (dhtNodeId n) target)

                let improved = case (closestBefore, closestAfter) of
                        (Nothing, _)       -> True
                        (_, Nothing)       -> False
                        (Just cb, Just ca) -> ca < cb

                -- Continue if we improved or there are still unqueried
                -- nodes within the k closest
                queried' <- readIORef queriedRef
                if improved || not (kClosestAllQueried k queried' shortlist')
                    then go
                    else return ()

------------------------------------------------------------------------
-- Iterative FIND_VALUE
------------------------------------------------------------------------

-- | Iterative FIND_VALUE lookup.
--
-- Same algorithm as 'iterativeFindNode' but uses @FindValue@ messages
-- and short-circuits immediately when a value is found.  Returns
-- @Left nodes@ if no value was found (the k closest nodes to the
-- key), or @Right value@ on success.
iterativeFindValue
    :: RoutingTable
    -> DHTConfig
    -> SendRPC
    -> ByteString  -- ^ key to look up
    -> IO (Either [DHTNode] ByteString)
iterativeFindValue rt cfg sendRPC key = do
    let target = NodeId key
        selfId = rtSelf rt
    seeds <- findClosest rt target (dhtK cfg)
    shortlistRef <- newIORef (sortByDistance target seeds)
    queriedRef   <- newIORef Set.empty
    findValueLoop selfId cfg sendRPC target key shortlistRef queriedRef

-- | Loop for FIND_VALUE: like 'lookupLoop' but checks each response
-- for a value and short-circuits on success.
findValueLoop
    :: NodeId
    -> DHTConfig
    -> SendRPC
    -> NodeId          -- ^ target (NodeId wrapping the key)
    -> ByteString      -- ^ raw key
    -> IORef [DHTNode]
    -> IORef (Set.Set ByteString)
    -> IO (Either [DHTNode] ByteString)
findValueLoop selfId cfg sendRPC target key shortlistRef queriedRef = go
  where
    k     = dhtK cfg
    alpha = dhtAlpha cfg

    go = do
        shortlist <- readIORef shortlistRef
        queried   <- readIORef queriedRef

        let candidates = filter (not . isQueried queried) shortlist
            toQuery    = take alpha candidates

        if null toQuery || kClosestAllQueried k queried shortlist
            then do
                sl <- readIORef shortlistRef
                return (Left (take k sl))
            else do
                let newQueried = foldr
                        (\n s -> Set.insert (nodeIdBytes (dhtNodeId n)) s)
                        queried toQuery
                writeIORef queriedRef newQueried

                let closestBefore = case shortlist of
                        []    -> Nothing
                        (n:_) -> Just (xorDistance (dhtNodeId n) target)

                -- Send FindValue queries in parallel
                results <- queryParallelValue sendRPC selfId key toQuery

                -- Check for value in any result
                case findValue results of
                    Just val -> return (Right val)
                    Nothing  -> do
                        -- Merge node results
                        let newNodes = concatMap extractValueNodes results
                        modifyIORef' shortlistRef $ \sl ->
                            dedup (sortByDistance target (sl ++ newNodes))

                        shortlist' <- readIORef shortlistRef
                        let closestAfter = case shortlist' of
                                []    -> Nothing
                                (n:_) -> Just (xorDistance (dhtNodeId n) target)

                        let improved = case (closestBefore, closestAfter) of
                                (Nothing, _)       -> True
                                (_, Nothing)       -> False
                                (Just cb, Just ca) -> ca < cb

                        queried' <- readIORef queriedRef
                        if improved || not (kClosestAllQueried k queried' shortlist')
                            then go
                            else do
                                sl <- readIORef shortlistRef
                                return (Left (take k sl))

------------------------------------------------------------------------
-- Parallel query helpers
------------------------------------------------------------------------

-- | Send FindNode to multiple nodes in parallel, collecting responses.
queryParallel
    :: SendRPC
    -> NodeId    -- ^ sender (self)
    -> NodeId    -- ^ target
    -> [DHTNode] -- ^ nodes to query
    -> IO [Maybe DHTMessage]
queryParallel sendRPC selfId target nodes = do
    mvars <- mapM (\node -> do
        mv <- newEmptyMVar
        _ <- forkIO $ do
            result <- sendRPC node (FindNode selfId target)
            putMVar mv result
        return mv
        ) nodes
    mapM takeMVar mvars

-- | Send FindValue to multiple nodes in parallel, collecting responses.
queryParallelValue
    :: SendRPC
    -> NodeId      -- ^ sender (self)
    -> ByteString  -- ^ key
    -> [DHTNode]   -- ^ nodes to query
    -> IO [Maybe DHTMessage]
queryParallelValue sendRPC selfId key nodes = do
    mvars <- mapM (\node -> do
        mv <- newEmptyMVar
        _ <- forkIO $ do
            result <- sendRPC node (FindValue selfId key)
            putMVar mv result
        return mv
        ) nodes
    mapM takeMVar mvars

------------------------------------------------------------------------
-- Result extraction
------------------------------------------------------------------------

-- | Extract nodes from a FindNodeReply response.
extractNodes :: Maybe DHTMessage -> [DHTNode]
extractNodes (Just (FindNodeReply _ nodes)) = nodes
extractNodes _                              = []

-- | Extract nodes from a FindValueReply that returned nodes (not a
-- value).
extractValueNodes :: Maybe DHTMessage -> [DHTNode]
extractValueNodes (Just (FindValueReply _ (Left nodes))) = nodes
extractValueNodes _                                      = []

-- | Check if any response contains a value.
findValue :: [Maybe DHTMessage] -> Maybe ByteString
findValue [] = Nothing
findValue (Just (FindValueReply _ (Right val)) : _) = Just val
findValue (_ : rest) = findValue rest

------------------------------------------------------------------------
-- Shortlist helpers
------------------------------------------------------------------------

-- | Sort nodes by XOR distance to a target, closest first.
sortByDistance :: NodeId -> [DHTNode] -> [DHTNode]
sortByDistance target =
    sortBy (comparing (\n -> xorDistance (dhtNodeId n) target))

-- | Remove duplicate nodes, keeping the first occurrence.
dedup :: [DHTNode] -> [DHTNode]
dedup = nubBy (\a b -> dhtNodeId a == dhtNodeId b)

-- | Check whether a node has been queried.
isQueried :: Set.Set ByteString -> DHTNode -> Bool
isQueried queried node =
    Set.member (nodeIdBytes (dhtNodeId node)) queried

-- | Extract raw bytes from a NodeId.
nodeIdBytes :: NodeId -> ByteString
nodeIdBytes (NodeId bs) = bs

-- | Check if the k closest nodes in the shortlist have all been
-- queried.
kClosestAllQueried :: Int -> Set.Set ByteString -> [DHTNode] -> Bool
kClosestAllQueried k queried shortlist =
    let closest = take k shortlist
    in length closest >= k && all (isQueried queried) closest

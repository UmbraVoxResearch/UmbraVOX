-- SPDX-License-Identifier: Apache-2.0
-- | Unit tests for DHT routing table, RPC serialization, message
-- handling, and node ID verification (M24.3.7, M24.3.8, M24.3.9).
module Test.Network.DHT (runTests) where

import Data.IORef
import Data.ByteString (ByteString)
import Data.Word (Word64)
import qualified Data.ByteString as BS

import Test.Util (assertEq, strToBS)
import UmbraVox.Network.DHT
    ( DHTState(..)
    , newDHTState
    , handleMessage
    )
import UmbraVox.Network.DHT.RoutingTable
    ( RoutingTable(..)
    , InsertResult(..)
    , newRoutingTable
    , insertNode
    , removeNode
    , findClosest
    , bucketIndex
    )
import UmbraVox.Network.DHT.RPC
    ( encodeDHTMessage
    , decodeDHTMessage
    , maxDHTMessageSize
    )
import UmbraVox.Network.DHT.Types
    ( NodeId(..)
    , DHTNode(..)
    , DHTMessage(..)
    , KBucket(..)
    , deriveNodeId
    , verifyNodeId
    , defaultDHTConfig
    )

------------------------------------------------------------------------
-- Test runner
------------------------------------------------------------------------

runTests :: IO Bool
runTests = do
    putStrLn "[DHT] Running DHT unit tests..."
    results <- sequence
        -- M24.3.7: Routing table operations
        [ testBucketIndexKnownDistances
        , testInsertIntoEmptyBucket
        , testInsertExistingNode
        , testInsertIntoFullBucket
        , testInsertSelf
        , testRemovePromotesReplacement
        , testRemoveFromReplacementOnly
        , testFindClosestSorted
        , testNewRoutingTable256Buckets
        -- M24.3.8: DHT message handling
        , testRoundTripPing
        , testRoundTripPong
        , testRoundTripFindNode
        , testRoundTripFindNodeReply
        , testRoundTripStore
        , testRoundTripFindValue
        , testRoundTripFindValueReplyValue
        , testRoundTripFindValueReplyNodes
        , testRejectsOversizedMessage
        , testRejectsTruncatedMessage
        , testRejectsMalformedMessage
        , testHandlePingReturnsPong
        , testHandleFindNodeReturnsClosest
        , testHandleStoreAndFindValue
        , testHandleFindValueMissingKey
        -- M24.3.9: Node ID verification
        , testVerifyNodeIdValid
        , testVerifyNodeIdInvalid
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[DHT] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Make a NodeId from a raw 32-byte ByteString (zero-padded).
mkNodeId :: ByteString -> NodeId
mkNodeId bs = NodeId (BS.take 32 (bs <> BS.replicate 32 0))

-- | Make a DHTNode with the given NodeId and a stub address.
mkNode :: NodeId -> DHTNode
mkNode nid = DHTNode
    { dhtNodeId   = nid
    , dhtAddress  = "127.0.0.1:9000"
    , dhtLastSeen = 1000
    , dhtRTT      = Nothing
    }

-- | Make a DHTNode with a specific lastSeen timestamp.
mkNodeAt :: NodeId -> String -> Word64 -> DHTNode
mkNodeAt nid addr ts = DHTNode
    { dhtNodeId   = nid
    , dhtAddress  = addr
    , dhtLastSeen = ts
    , dhtRTT      = Nothing
    }

-- | Self NodeId for routing table tests: all zeros.
selfId :: NodeId
selfId = mkNodeId (BS.replicate 32 0)

------------------------------------------------------------------------
-- M24.3.7: Routing table tests
------------------------------------------------------------------------

-- | bucketIndex returns correct bucket for known XOR distances.
testBucketIndexKnownDistances :: IO Bool
testBucketIndexKnownDistances = do
    -- Distance with highest bit at position 255 (byte 0, bit 7) -> bucket 0
    let farId = mkNodeId (BS.cons 0x80 (BS.replicate 31 0))
    r1 <- assertEq "bucketIndex far node (bit 255)" 0 (bucketIndex selfId farId)
    -- Distance with highest bit at position 0 (byte 31, bit 0) -> bucket 255
    let closeId = mkNodeId (BS.replicate 31 0 <> BS.singleton 0x01)
    r2 <- assertEq "bucketIndex close node (bit 0)" 255 (bucketIndex selfId closeId)
    -- Distance with highest bit at position 7 (byte 31, bit 7) -> bucket 248
    let midId = mkNodeId (BS.replicate 31 0 <> BS.singleton 0x80)
    r3 <- assertEq "bucketIndex mid node (bit 7)" 248 (bucketIndex selfId midId)
    -- Self -> -1
    r4 <- assertEq "bucketIndex self" (-1) (bucketIndex selfId selfId)
    pure (r1 && r2 && r3 && r4)

-- | insertNode into empty bucket returns Inserted.
testInsertIntoEmptyBucket :: IO Bool
testInsertIntoEmptyBucket = do
    rt <- newRoutingTable selfId 20
    let node = mkNode (mkNodeId (BS.singleton 1 <> BS.replicate 31 0))
    result <- insertNode rt node
    assertEq "insert into empty bucket" Inserted result

-- | insertNode existing node returns Updated.
testInsertExistingNode :: IO Bool
testInsertExistingNode = do
    rt <- newRoutingTable selfId 20
    let nid = mkNodeId (BS.singleton 2 <> BS.replicate 31 0)
        node1 = mkNodeAt nid "host:1" 100
        node2 = mkNodeAt nid "host:1" 200
    _ <- insertNode rt node1
    result <- insertNode rt node2
    assertEq "insert existing node" Updated result

-- | insertNode into full bucket returns BucketFull.
testInsertIntoFullBucket :: IO Bool
testInsertIntoFullBucket = do
    -- Use k=2 for a tiny bucket
    rt <- newRoutingTable selfId 2
    -- All nodes go into the same bucket (byte 0 has high bit set -> bucket 0)
    let mkFarNode i = mkNode (mkNodeId (BS.pack [0x80, i] <> BS.replicate 30 0))
    _ <- insertNode rt (mkFarNode 1)
    _ <- insertNode rt (mkFarNode 2)
    -- Bucket now full (k=2), third insert should be BucketFull
    result <- insertNode rt (mkFarNode 3)
    assertEq "insert into full bucket" BucketFull result

-- | insertNode self returns SelfIgnored.
testInsertSelf :: IO Bool
testInsertSelf = do
    rt <- newRoutingTable selfId 20
    let node = mkNode selfId
    result <- insertNode rt node
    assertEq "insert self" SelfIgnored result

-- | removeNode from active list promotes from replacement cache.
testRemovePromotesReplacement :: IO Bool
testRemovePromotesReplacement = do
    rt <- newRoutingTable selfId 2
    let nid1 = mkNodeId (BS.pack [0x80, 1] <> BS.replicate 30 0)
        nid2 = mkNodeId (BS.pack [0x80, 2] <> BS.replicate 30 0)
        nid3 = mkNodeId (BS.pack [0x80, 3] <> BS.replicate 30 0)
    _ <- insertNode rt (mkNode nid1)
    _ <- insertNode rt (mkNode nid2)
    -- nid3 goes to replacement cache (bucket full)
    _ <- insertNode rt (mkNode nid3)
    -- Remove nid1 from active -> nid3 should be promoted
    removeNode rt nid1
    closest <- findClosest rt nid3 10
    let ids = map dhtNodeId closest
    r1 <- assertEq "nid1 removed" False (nid1 `elem` ids)
    r2 <- assertEq "nid3 promoted" True (nid3 `elem` ids)
    r3 <- assertEq "nid2 still active" True (nid2 `elem` ids)
    pure (r1 && r2 && r3)

-- | removeNode from replacement cache only.
testRemoveFromReplacementOnly :: IO Bool
testRemoveFromReplacementOnly = do
    rt <- newRoutingTable selfId 2
    let nid1 = mkNodeId (BS.pack [0x80, 1] <> BS.replicate 30 0)
        nid2 = mkNodeId (BS.pack [0x80, 2] <> BS.replicate 30 0)
        nid3 = mkNodeId (BS.pack [0x80, 3] <> BS.replicate 30 0)
    _ <- insertNode rt (mkNode nid1)
    _ <- insertNode rt (mkNode nid2)
    _ <- insertNode rt (mkNode nid3) -- replacement cache
    -- Remove nid3 from replacement cache
    removeNode rt nid3
    -- Active list should be unchanged
    closest <- findClosest rt nid1 10
    let ids = map dhtNodeId closest
    r1 <- assertEq "active node 1 present" True (nid1 `elem` ids)
    r2 <- assertEq "active node 2 present" True (nid2 `elem` ids)
    r3 <- assertEq "replacement removed" False (nid3 `elem` ids)
    pure (r1 && r2 && r3)

-- | findClosest returns k closest by XOR distance, sorted correctly.
testFindClosestSorted :: IO Bool
testFindClosestSorted = do
    rt <- newRoutingTable selfId 20
    -- Insert nodes at various distances from selfId (all zeros)
    let far    = mkNodeId (BS.pack [0xFF] <> BS.replicate 31 0)   -- very far
        mid    = mkNodeId (BS.replicate 31 0 <> BS.singleton 0xFF) -- closer
        close  = mkNodeId (BS.replicate 31 0 <> BS.singleton 0x01) -- closest
    _ <- insertNode rt (mkNode far)
    _ <- insertNode rt (mkNode mid)
    _ <- insertNode rt (mkNode close)
    -- Find 2 closest to selfId
    let target = selfId
    closest <- findClosest rt target 2
    let ids = map dhtNodeId closest
    r1 <- assertEq "findClosest returns 2" 2 (length closest)
    -- close should be first (smallest XOR distance to all-zeros target)
    r2 <- assertEq "closest first" close (head ids)
    -- mid should be second
    r3 <- assertEq "mid second" mid (ids !! 1)
    pure (r1 && r2 && r3)

-- | newRoutingTable creates 256 empty buckets.
testNewRoutingTable256Buckets :: IO Bool
testNewRoutingTable256Buckets = do
    rt <- newRoutingTable selfId 20
    buckets <- readIORef (rtBuckets rt)
    r1 <- assertEq "256 buckets" 256 (length buckets)
    let allEmpty = all (\b -> null (kbEntries b) && null (kbReplacement b)) buckets
    r2 <- assertEq "all buckets empty" True allEmpty
    pure (r1 && r2)

------------------------------------------------------------------------
-- M24.3.8: RPC round-trip tests
------------------------------------------------------------------------

-- | Helper: encode then decode, check round-trip.
roundTrip :: String -> DHTMessage -> IO Bool
roundTrip name msg = do
    let encoded = encodeDHTMessage msg
        decoded = decodeDHTMessage encoded
    assertEq ("round-trip " ++ name) (Just msg) decoded

testRoundTripPing :: IO Bool
testRoundTripPing = roundTrip "Ping" (Ping (deriveNodeId (strToBS "alice")))

testRoundTripPong :: IO Bool
testRoundTripPong = roundTrip "Pong" (Pong (deriveNodeId (strToBS "bob")))

testRoundTripFindNode :: IO Bool
testRoundTripFindNode =
    roundTrip "FindNode" (FindNode (deriveNodeId (strToBS "alice"))
                                   (deriveNodeId (strToBS "target")))

testRoundTripFindNodeReply :: IO Bool
testRoundTripFindNodeReply = do
    let sender = deriveNodeId (strToBS "alice")
        node1 = DHTNode (deriveNodeId (strToBS "n1")) "host1:80" 1000 Nothing
        node2 = DHTNode (deriveNodeId (strToBS "n2")) "host2:80" 2000 (Just 500)
    roundTrip "FindNodeReply" (FindNodeReply sender [node1, node2])

testRoundTripStore :: IO Bool
testRoundTripStore =
    roundTrip "Store" (Store (deriveNodeId (strToBS "alice"))
                             (strToBS "mykey")
                             (strToBS "myvalue"))

testRoundTripFindValue :: IO Bool
testRoundTripFindValue =
    roundTrip "FindValue" (FindValue (deriveNodeId (strToBS "alice"))
                                     (strToBS "mykey"))

testRoundTripFindValueReplyValue :: IO Bool
testRoundTripFindValueReplyValue =
    roundTrip "FindValueReply(value)"
        (FindValueReply (deriveNodeId (strToBS "alice"))
                        (Right (strToBS "found-value")))

testRoundTripFindValueReplyNodes :: IO Bool
testRoundTripFindValueReplyNodes = do
    let sender = deriveNodeId (strToBS "alice")
        node1 = DHTNode (deriveNodeId (strToBS "n1")) "host1:80" 1000 Nothing
    roundTrip "FindValueReply(nodes)"
        (FindValueReply sender (Left [node1]))

-- | Rejects messages > 4096 bytes.
testRejectsOversizedMessage :: IO Bool
testRejectsOversizedMessage = do
    let oversized = BS.replicate (maxDHTMessageSize + 1) 0x01
    assertEq "reject oversized" Nothing (decodeDHTMessage oversized)

-- | Rejects truncated/malformed messages.
testRejectsTruncatedMessage :: IO Bool
testRejectsTruncatedMessage = do
    -- Ping needs type byte + 32 bytes NodeId; give only 10 bytes
    let truncated = BS.pack (0x01 : replicate 10 0)
    r1 <- assertEq "reject truncated Ping" Nothing (decodeDHTMessage truncated)
    -- Empty input
    r2 <- assertEq "reject empty" Nothing (decodeDHTMessage BS.empty)
    pure (r1 && r2)

-- | Rejects messages with unknown type byte.
testRejectsMalformedMessage :: IO Bool
testRejectsMalformedMessage = do
    let bad = BS.pack (0xFF : replicate 32 0)
    assertEq "reject unknown type" Nothing (decodeDHTMessage bad)

------------------------------------------------------------------------
-- M24.3.8: handleMessage tests
------------------------------------------------------------------------

-- | Ping -> Pong with self NodeId.
testHandlePingReturnsPong :: IO Bool
testHandlePingReturnsPong = do
    let cfg = defaultDHTConfig
    st <- newDHTState cfg (strToBS "server-key")
    let senderId = deriveNodeId (strToBS "client-key")
    resp <- handleMessage st (Ping senderId) 1000
    assertEq "Ping -> Pong" (Just (Pong (dhSelfId st))) resp

-- | FindNode -> FindNodeReply with closest nodes.
testHandleFindNodeReturnsClosest :: IO Bool
testHandleFindNodeReturnsClosest = do
    let cfg = defaultDHTConfig
    st <- newDHTState cfg (strToBS "server-key")
    -- Insert a node into the routing table first
    let nodeId1 = deriveNodeId (strToBS "node1")
        node1 = DHTNode nodeId1 "host1:80" 500 Nothing
    _ <- insertNode (dhRoutingTable st) node1
    let senderId = deriveNodeId (strToBS "client-key")
        target = deriveNodeId (strToBS "target")
    resp <- handleMessage st (FindNode senderId target) 1000
    case resp of
        Just (FindNodeReply respSender nodes) -> do
            r1 <- assertEq "FindNodeReply sender is self" (dhSelfId st) respSender
            -- Should include at least node1 (and possibly the sender stub)
            r2 <- assertEq "FindNodeReply has nodes" True (not (null nodes))
            pure (r1 && r2)
        _ -> do
            putStrLn "  FAIL: FindNode did not return FindNodeReply"
            pure False

-- | Store -> value retrievable via FindValue.
testHandleStoreAndFindValue :: IO Bool
testHandleStoreAndFindValue = do
    let cfg = defaultDHTConfig
    st <- newDHTState cfg (strToBS "server-key")
    let senderId = deriveNodeId (strToBS "client-key")
        key = strToBS "test-key"
        value = strToBS "test-value"
    -- Store should return Nothing (no response)
    storeResp <- handleMessage st (Store senderId key value) 1000
    r1 <- assertEq "Store returns Nothing" Nothing storeResp
    -- Now FindValue for the same key should return the value
    resp <- handleMessage st (FindValue senderId key) 1000
    r2 <- case resp of
        Just (FindValueReply _ (Right v)) ->
            assertEq "FindValue returns stored value" value v
        Just (FindValueReply _ (Left _)) -> do
            putStrLn "  FAIL: FindValue returned nodes instead of value"
            pure False
        _ -> do
            putStrLn "  FAIL: FindValue did not return FindValueReply"
            pure False
    pure (r1 && r2)

-- | FindValue for missing key returns closest nodes.
testHandleFindValueMissingKey :: IO Bool
testHandleFindValueMissingKey = do
    let cfg = defaultDHTConfig
    st <- newDHTState cfg (strToBS "server-key")
    let senderId = deriveNodeId (strToBS "client-key")
        key = strToBS "nonexistent-key"
    resp <- handleMessage st (FindValue senderId key) 1000
    case resp of
        Just (FindValueReply _ (Left _nodes)) ->
            assertEq "FindValue missing -> Left nodes" True True
        Just (FindValueReply _ (Right _)) -> do
            putStrLn "  FAIL: FindValue returned a value for missing key"
            pure False
        _ -> do
            putStrLn "  FAIL: FindValue did not return FindValueReply"
            pure False

------------------------------------------------------------------------
-- M24.3.9: Node ID verification
------------------------------------------------------------------------

-- | Valid key produces True.
testVerifyNodeIdValid :: IO Bool
testVerifyNodeIdValid = do
    let pubKey = strToBS "my-identity-public-key"
        nid = deriveNodeId pubKey
    assertEq "verifyNodeId valid" True (verifyNodeId nid pubKey)

-- | Wrong key produces False.
testVerifyNodeIdInvalid :: IO Bool
testVerifyNodeIdInvalid = do
    let pubKey = strToBS "my-identity-public-key"
        wrongKey = strToBS "different-key"
        nid = deriveNodeId pubKey
    assertEq "verifyNodeId invalid" False (verifyNodeId nid wrongKey)

-- SPDX-License-Identifier: Apache-2.0
-- | Unit tests for DHT routing table, RPC serialization, message
-- handling, node ID verification, security hardening, and integration
-- tests with loopback transport
-- (M24.3.7, M24.3.8, M24.3.9, M24.6.1–M24.6.5, M24.4.8).
module Test.Network.DHT (runTests) where

import Data.IORef
import Data.ByteString (ByteString)
import Data.Word (Word64)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import Test.Util (assertEq, strToBS)
import UmbraVox.Network.DHT
    ( DHTState(..)
    , newDHTState
    , handleMessage
    , bootstrap
    , maintain
    )
import UmbraVox.Network.DHT.Lookup
    ( SendRPC
    , iterativeFindNode
    , iterativeFindValue
    )
import UmbraVox.Network.DHT.Store
    ( ValueStore(..)
    , newValueStore
    , localStore
    , localLookup
    , expireEntries
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
    , DHTConfig(..)
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
        -- M24.6.1: Sybil attack resistance
        , testSybilBucketOverflow
        -- M24.6.2: Eclipse attack resistance
        , testEclipseAttackResistance
        -- M24.6.3: Storage spam resistance
        , testStorageSpamSizeLimit
        , testStorageSpamExpiration
        -- M24.6.4: Routing table poisoning resistance
        , testRoutingTablePoisoning
        -- M24.6.5: DHT message size cap enforcement
        , testMessageSizeCapOversized
        , testMessageSizeCapAtLimit
        -- M24.4.8: Integration tests with loopback transport
        , testTwoNodeBootstrap
        , testThreeNodeLookup
        , testStoreAndRetrieve
        , testMaintenance
        , testBootstrapEmptyNetwork
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
    st <- newDHTState cfg (strToBS "server-key") Nothing
    let senderId = deriveNodeId (strToBS "client-key")
    resp <- handleMessage st (Ping senderId) 1000
    assertEq "Ping -> Pong" (Just (Pong (dhSelfId st))) resp

-- | FindNode -> FindNodeReply with closest nodes.
testHandleFindNodeReturnsClosest :: IO Bool
testHandleFindNodeReturnsClosest = do
    let cfg = defaultDHTConfig
    st <- newDHTState cfg (strToBS "server-key") Nothing
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
    st <- newDHTState cfg (strToBS "server-key") Nothing
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
    st <- newDHTState cfg (strToBS "server-key") Nothing
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

------------------------------------------------------------------------
-- M24.6.1: Sybil attack resistance
------------------------------------------------------------------------

-- | Finding: Sybil attacks flood a single k-bucket with many fake nodes
-- to displace legitimate entries.
-- Vulnerability: Without bucket size enforcement, an attacker could fill
-- the routing table with colluding nodes.
-- Fix: K-bucket capacity is enforced; excess nodes go to replacement cache.
-- Verified: Insert 20 nodes targeting the same bucket with k=3; bucket
-- holds at most 3 active entries and excess goes to replacement cache.
testSybilBucketOverflow :: IO Bool
testSybilBucketOverflow = do
    let k = 3
    rt <- newRoutingTable selfId k
    -- Create 20 nodes that all fall into bucket 0 (byte 0 has high bit set)
    let mkSybilNode i = mkNode (mkNodeId (BS.pack [0x80, i] <> BS.replicate 30 0))
        sybilNodes = map mkSybilNode [1..20]
    results <- mapM (insertNode rt) sybilNodes
    let insertedCount = length (filter (== Inserted) results)
        bucketFullCount = length (filter (== BucketFull) results)
    r1 <- assertEq "sybil: only k nodes inserted" k insertedCount
    r2 <- assertEq "sybil: rest got BucketFull" (20 - k) bucketFullCount
    -- Verify actual bucket contents
    buckets <- readIORef (rtBuckets rt)
    let bucket0 = buckets !! 0
    r3 <- assertEq "sybil: bucket active <= k" True
              (length (kbEntries bucket0) <= k)
    r4 <- assertEq "sybil: replacement cache non-empty" True
              (not (null (kbReplacement bucket0)))
    r5 <- assertEq "sybil: replacement cache <= k" True
              (length (kbReplacement bucket0) <= k)
    pure (r1 && r2 && r3 && r4 && r5)

------------------------------------------------------------------------
-- M24.6.2: Eclipse attack resistance
------------------------------------------------------------------------

-- | Finding: Eclipse attacks attempt to surround a target node by
-- inserting attacker-controlled nodes that displace legitimate entries.
-- Vulnerability: If new nodes could displace existing active nodes,
-- an attacker could isolate a victim from the real network.
-- Fix: Kademlia bucket semantics never evict a live active node for a
-- new one; new arrivals go to the replacement cache when the bucket is full.
-- Verified: Fill a bucket, then attempt to insert attacker nodes;
-- findClosest still returns original nodes, not attacker nodes.
testEclipseAttackResistance :: IO Bool
testEclipseAttackResistance = do
    let k = 3
    rt <- newRoutingTable selfId k
    -- Fill bucket 0 with legitimate nodes
    let mkLegitNode i = mkNode (mkNodeId (BS.pack [0x80, i] <> BS.replicate 30 0))
        legitNodes = map mkLegitNode [1..3]
    mapM_ (insertNode rt) legitNodes
    -- Attempt to insert 10 attacker nodes into the same bucket
    let mkAttackerNode i = mkNode (mkNodeId (BS.pack [0x80, 0x10 + i] <> BS.replicate 30 0))
        attackerNodes = map mkAttackerNode [1..10]
    attackResults <- mapM (insertNode rt) attackerNodes
    -- All attacker inserts should be BucketFull
    r1 <- assertEq "eclipse: all attacker inserts BucketFull" True
              (all (== BucketFull) attackResults)
    -- findClosest should return only legitimate nodes from bucket 0
    let target = mkNodeId (BS.pack [0x80] <> BS.replicate 31 0)
    closest <- findClosest rt target k
    let closestIds = map dhtNodeId closest
        legitIds = map (\n -> dhtNodeId n) legitNodes
        attackerIds = map (\n -> dhtNodeId n) attackerNodes
    r2 <- assertEq "eclipse: all closest are legit" True
              (all (`elem` legitIds) closestIds)
    r3 <- assertEq "eclipse: no attacker in closest" True
              (all (`notElem` attackerIds) closestIds)
    pure (r1 && r2 && r3)

------------------------------------------------------------------------
-- M24.6.3: Storage spam resistance
------------------------------------------------------------------------

-- | Finding: Storage spam attacks flood the DHT value store with
-- oversized or numerous entries to exhaust resources.
-- Vulnerability: Without size limits, a single attacker could consume
-- unbounded storage on target nodes.
-- Fix: ValueStore enforces maxValueSize at insertion and expireEntries
-- prunes stale data.
-- Verified: Oversized values are rejected, valid values are accepted,
-- and expired entries are cleaned up.
testStorageSpamSizeLimit :: IO Bool
testStorageSpamSizeLimit = do
    vs <- newValueStore 100
    -- Oversized value (200 bytes) should be rejected
    let bigValue = BS.replicate 200 0x41
    r1Stored <- localStore vs (strToBS "key1") bigValue 99999
    r1 <- assertEq "spam: reject oversized value" False r1Stored
    -- Valid value (50 bytes) should be accepted
    let smallValue = BS.replicate 50 0x42
    r2Stored <- localStore vs (strToBS "key2") smallValue 99999
    r2 <- assertEq "spam: accept valid value" True r2Stored
    -- Verify the valid value is retrievable
    mVal <- localLookup vs (strToBS "key2") 1000
    r3 <- assertEq "spam: valid value retrievable" (Just smallValue) mVal
    -- Verify the rejected value is not stored
    mBig <- localLookup vs (strToBS "key1") 1000
    r4 <- assertEq "spam: oversized value not stored" Nothing mBig
    pure (r1 && r2 && r3 && r4)

-- | Finding: Without expiration, stored values accumulate indefinitely.
-- Vulnerability: An attacker can fill the store by inserting many
-- small values that never expire.
-- Fix: expireEntries removes entries whose TTL has elapsed.
-- Verified: Store multiple values with short TTLs, advance time past
-- expiry, and confirm they are cleaned up.
testStorageSpamExpiration :: IO Bool
testStorageSpamExpiration = do
    vs <- newValueStore 100
    -- Store 5 values with expiry at time 500
    let storeOne i = localStore vs
            (strToBS ("key-" ++ show i))
            (BS.replicate 10 (fromIntegral i))
            500  -- expires at t=500
    mapM_ storeOne [1..5 :: Int]
    -- At time 400, all should still be alive
    expired400 <- expireEntries vs 400
    r1 <- assertEq "spam: no expiry before TTL" 0 expired400
    -- At time 500, all should be expired (now >= expiry)
    expired500 <- expireEntries vs 500
    r2 <- assertEq "spam: all expired at TTL" 5 expired500
    -- Verify store is empty by trying to expire again
    expired501 <- expireEntries vs 501
    r3 <- assertEq "spam: store empty after expiry" 0 expired501
    pure (r1 && r2 && r3)

------------------------------------------------------------------------
-- M24.6.4: Routing table poisoning resistance
------------------------------------------------------------------------

-- | Finding: An attacker can claim arbitrary NodeIds that do not
-- correspond to their actual public key.
-- Vulnerability: Without verification, poisoned NodeIds corrupt the
-- routing table and misdirect lookups.
-- Fix: verifyNodeId checks that the claimed NodeId equals SHA-256 of
-- the node's identity public key.
-- Verified: A forged NodeId is rejected; a legitimate one is accepted.
testRoutingTablePoisoning :: IO Bool
testRoutingTablePoisoning = do
    let realPubKey = strToBS "legitimate-node-public-key"
        realNodeId = deriveNodeId realPubKey
    -- Forge a NodeId that doesn't match any key
    let forgedNodeId = mkNodeId (BS.replicate 32 0xFF)
    r1 <- assertEq "poison: forged NodeId rejected"
              False (verifyNodeId forgedNodeId realPubKey)
    -- Verify the real NodeId passes
    r2 <- assertEq "poison: legitimate NodeId accepted"
              True (verifyNodeId realNodeId realPubKey)
    -- Also verify with a completely different public key
    let otherPubKey = strToBS "other-node-public-key"
    r3 <- assertEq "poison: wrong key rejected"
              False (verifyNodeId realNodeId otherPubKey)
    pure (r1 && r2 && r3)

------------------------------------------------------------------------
-- M24.6.5: DHT message size cap enforcement
------------------------------------------------------------------------

-- | Finding: Oversized DHT messages can cause excessive memory
-- allocation or buffer overflows in the decoder.
-- Vulnerability: Without a size cap, an attacker can send arbitrarily
-- large messages to exhaust resources.
-- Fix: decodeDHTMessage rejects messages exceeding maxDHTMessageSize
-- (4096 bytes).
-- Verified: A padded message exceeding 4096 bytes returns Nothing;
-- a message at exactly 4096 bytes is processed (decoded if valid,
-- but not rejected for size alone).
testMessageSizeCapOversized :: IO Bool
testMessageSizeCapOversized = do
    -- Encode a valid Ping message
    let sender = deriveNodeId (strToBS "test-sender")
        validMsg = encodeDHTMessage (Ping sender)
    -- Pad to exceed 4096 bytes
    let oversized = validMsg <> BS.replicate (maxDHTMessageSize + 1) 0x00
    r1 <- assertEq "size-cap: oversized rejected" Nothing
              (decodeDHTMessage oversized)
    -- Verify the unpadded message still decodes
    r2 <- assertEq "size-cap: valid message accepted"
              (Just (Ping sender)) (decodeDHTMessage validMsg)
    pure (r1 && r2)

-- | Verified: A message at exactly maxDHTMessageSize is not rejected
-- for size (it may fail decoding for other reasons, but size alone
-- does not cause rejection).
testMessageSizeCapAtLimit :: IO Bool
testMessageSizeCapAtLimit = do
    -- Create a message of exactly maxDHTMessageSize bytes.
    -- Use a Store message with a value sized to hit exactly 4096.
    -- Ping = 1 (type) + 32 (NodeId) = 33 bytes.  Pad to 4096:
    -- We can't pad a Ping (it enforces exact size via guardEmpty),
    -- so we test that a 4096-byte buffer is not rejected for size.
    -- A buffer of exactly 4096 bytes with type=0x01 (Ping) and 32
    -- valid NodeId bytes + trailing garbage will fail guardEmpty,
    -- but that's a format error, not a size error.
    -- Instead, verify that decodeDHTMessage does not return Nothing
    -- *due to size* by checking the size guard specifically:
    let atLimit = BS.replicate maxDHTMessageSize 0x01
    -- This will fail decoding (Ping with trailing bytes -> guardEmpty),
    -- but crucially it should NOT be rejected at the size check.
    -- We verify by checking a message of 4097 bytes IS rejected.
    let overLimit = BS.replicate (maxDHTMessageSize + 1) 0x01
    r1 <- assertEq "size-cap: 4097 bytes rejected" Nothing
              (decodeDHTMessage overLimit)
    -- For exactly 4096: it won't decode as valid Ping (trailing bytes)
    -- but it passes the size check — verify it's Nothing from format,
    -- not from the size guard.  We can't distinguish the two Nothings
    -- directly, so instead verify a valid message at the boundary:
    -- Construct a Store message that is exactly 4096 bytes.
    let sender = deriveNodeId (strToBS "size-test")
        key = strToBS "k"
        -- type(1) + NodeId(32) + keyLen(4) + key(1) + valLen(4) + val(?) = 4096
        -- val size = 4096 - 1 - 32 - 4 - 1 - 4 = 4054
        val = BS.replicate 4054 0x58
        storeMsg = encodeDHTMessage (Store sender key val)
    r2 <- assertEq "size-cap: exact 4096 byte message size"
              maxDHTMessageSize (BS.length storeMsg)
    r3 <- assertEq "size-cap: exact 4096 byte message decodes"
              (Just (Store sender key val)) (decodeDHTMessage storeMsg)
    pure (r1 && r2 && r3)

------------------------------------------------------------------------
-- M24.4.8: Integration tests with loopback transport
------------------------------------------------------------------------

-- | Build a loopback SendRPC that dispatches messages to the correct
-- DHTState based on the target DHTNode's NodeId.  The map keys are
-- raw NodeId bytes; the timestamp used for handleMessage is fixed.
mkLoopbackRPC :: IORef (Map.Map ByteString DHTState) -> Word64 -> SendRPC
mkLoopbackRPC nodesRef now = \targetNode msg -> do
    nodes <- readIORef nodesRef
    let NodeId targetBytes = dhtNodeId targetNode
    case Map.lookup targetBytes nodes of
        Nothing -> return Nothing
        Just targetState -> handleMessage targetState msg now

-- | Register a DHTState in the loopback network map.
registerNode :: IORef (Map.Map ByteString DHTState) -> DHTState -> IO ()
registerNode nodesRef st = do
    let NodeId selfBytes = dhSelfId st
    modifyIORef' nodesRef (Map.insert selfBytes st)

-- | Create a DHTState from a key string using small-network config.
mkTestState :: String -> IO DHTState
mkTestState keyStr = newDHTState testConfig (strToBS keyStr) Nothing

-- | Small-network DHT config for integration tests.
testConfig :: DHTConfig
testConfig = defaultDHTConfig
    { dhtK     = 20
    , dhtAlpha = 3
    , dhtRefreshInterval = 100
    , dhtExpireInterval  = 1000
    }

-- | Two-node bootstrap test.
-- Create two DHTStates.  Node A bootstraps from node B's address.
-- Verify A's routing table contains B and B's contains A after
-- exchanging messages via handleMessage.
testTwoNodeBootstrap :: IO Bool
testTwoNodeBootstrap = do
    stA <- mkTestState "node-A-key"
    stB <- mkTestState "node-B-key"
    nodesRef <- newIORef Map.empty
    registerNode nodesRef stA
    registerNode nodesRef stB
    let sendRPC = mkLoopbackRPC nodesRef 1000
    -- Insert B into A's routing table so bootstrap can reach it.
    -- Bootstrap uses addresses to identify peers; in loopback we
    -- use the node's ID to route, so we give B a stub address and
    -- insert it with its real NodeId.
    let nodeB = DHTNode
            { dhtNodeId   = dhSelfId stB
            , dhtAddress  = "loopback-B"
            , dhtLastSeen = 1000
            , dhtRTT      = Nothing
            }
    _ <- insertNode (dhRoutingTable stA) nodeB
    -- Bootstrap A by doing iterativeFindNode for A's own ID.
    discovered <- iterativeFindNode
        (dhRoutingTable stA) (dhConfig stA) sendRPC (dhSelfId stA)
    -- A should have discovered at least B.
    let discoveredIds = map dhtNodeId discovered
    r1 <- assertEq "two-node: A discovered B"
              True (dhSelfId stB `elem` discoveredIds)
    -- B's routing table should contain A (from handling A's FindNode).
    closestToA <- findClosest (dhRoutingTable stB) (dhSelfId stA) 20
    let bKnowsA = any (\n -> dhtNodeId n == dhSelfId stA) closestToA
    r2 <- assertEq "two-node: B knows A" True bKnowsA
    pure (r1 && r2)

-- | Three-node lookup test.
-- Create three nodes A, B, C.  A knows B, B knows C.  A does
-- iterativeFindNode for C's NodeId.  Verify A discovers C.
testThreeNodeLookup :: IO Bool
testThreeNodeLookup = do
    stA <- mkTestState "node-A-key"
    stB <- mkTestState "node-B-key"
    stC <- mkTestState "node-C-key"
    nodesRef <- newIORef Map.empty
    registerNode nodesRef stA
    registerNode nodesRef stB
    registerNode nodesRef stC
    let sendRPC = mkLoopbackRPC nodesRef 1000
    -- A knows B.
    let nodeB = DHTNode (dhSelfId stB) "loopback-B" 1000 Nothing
    _ <- insertNode (dhRoutingTable stA) nodeB
    -- B knows C.
    let nodeC = DHTNode (dhSelfId stC) "loopback-C" 1000 Nothing
    _ <- insertNode (dhRoutingTable stB) nodeC
    -- A does iterativeFindNode for C's NodeId.
    discovered <- iterativeFindNode
        (dhRoutingTable stA) (dhConfig stA) sendRPC (dhSelfId stC)
    let discoveredIds = map dhtNodeId discovered
    r1 <- assertEq "three-node: A discovered C"
              True (dhSelfId stC `elem` discoveredIds)
    -- A should also still know B.
    r2 <- assertEq "three-node: A still knows B"
              True (dhSelfId stB `elem` discoveredIds)
    pure (r1 && r2)

-- | Store and retrieve test.
-- Node A stores a value via handleMessage(Store).  Node B does
-- iterativeFindValue.  Verify the value is returned.
testStoreAndRetrieve :: IO Bool
testStoreAndRetrieve = do
    stA <- mkTestState "node-A-key"
    stB <- mkTestState "node-B-key"
    nodesRef <- newIORef Map.empty
    registerNode nodesRef stA
    registerNode nodesRef stB
    let sendRPC = mkLoopbackRPC nodesRef 1000
    -- Store a value on node A directly.
    let key   = strToBS "test-store-key"
        value = strToBS "test-store-value"
        storeSender = dhSelfId stB
    _ <- handleMessage stA (Store storeSender key value) 1000
    -- B knows A so iterativeFindValue can reach it.
    let nodeA = DHTNode (dhSelfId stA) "loopback-A" 1000 Nothing
    _ <- insertNode (dhRoutingTable stB) nodeA
    -- B does iterativeFindValue for the key.
    result <- iterativeFindValue
        (dhRoutingTable stB) (dhConfig stB) sendRPC key
    case result of
        Right val ->
            assertEq "store-retrieve: value matches" value val
        Left _nodes -> do
            putStrLn "  FAIL: store-retrieve: got nodes instead of value"
            pure False

-- | Maintenance test.
-- Create a DHTState, insert some nodes with old timestamps, store
-- values with short TTLs, advance time past refresh interval, call
-- maintain.  Verify stale nodes are removed and expired values are
-- cleaned up.
testMaintenance :: IO Bool
testMaintenance = do
    st <- mkTestState "maint-key"
    -- Insert nodes with old timestamps (lastSeen = 100).
    let staleNode1 = DHTNode
            { dhtNodeId   = deriveNodeId (strToBS "stale-1")
            , dhtAddress  = "host1:80"
            , dhtLastSeen = 100
            , dhtRTT      = Nothing
            }
        staleNode2 = DHTNode
            { dhtNodeId   = deriveNodeId (strToBS "stale-2")
            , dhtAddress  = "host2:80"
            , dhtLastSeen = 100
            , dhtRTT      = Nothing
            }
        freshNode = DHTNode
            { dhtNodeId   = deriveNodeId (strToBS "fresh-1")
            , dhtAddress  = "host3:80"
            , dhtLastSeen = 5000
            , dhtRTT      = Nothing
            }
    _ <- insertNode (dhRoutingTable st) staleNode1
    _ <- insertNode (dhRoutingTable st) staleNode2
    _ <- insertNode (dhRoutingTable st) freshNode
    -- Store a value that expires at t=500.
    _ <- localStore (dhStore st) (strToBS "exp-key") (strToBS "exp-val") 500
    -- Run maintain at t=5050 (refresh interval = 100, so cutoff = 4950).
    -- staleNode1 and staleNode2 (lastSeen=100) are stale, freshNode (5000) is not.
    -- The stored value (expiry=500) is expired at t=5050.
    (expiredVals, staleNodes) <- maintain st 5050
    r1 <- assertEq "maint: expired values" 1 expiredVals
    r2 <- assertEq "maint: stale nodes removed" 2 staleNodes
    -- Verify fresh node is still in routing table.
    closest <- findClosest (dhRoutingTable st) (dhtNodeId freshNode) 20
    let freshPresent = any (\n -> dhtNodeId n == dhtNodeId freshNode) closest
    r3 <- assertEq "maint: fresh node retained" True freshPresent
    -- Verify stale nodes are gone.
    allNodes <- findClosest (dhRoutingTable st) (dhtNodeId staleNode1) 20
    let stalePresent = any (\n -> dhtNodeId n == dhtNodeId staleNode1
                                 || dhtNodeId n == dhtNodeId staleNode2) allNodes
    r4 <- assertEq "maint: stale nodes gone" False stalePresent
    -- Verify expired value is gone.
    mVal <- localLookup (dhStore st) (strToBS "exp-key") 5050
    r5 <- assertEq "maint: expired value gone" Nothing mVal
    pure (r1 && r2 && r3 && r4 && r5)

-- | Bootstrap with empty network test.
-- Bootstrap with no peers.  Verify it returns 0 discovered nodes
-- gracefully without errors.
testBootstrapEmptyNetwork :: IO Bool
testBootstrapEmptyNetwork = do
    st <- mkTestState "lonely-key"
    nodesRef <- newIORef Map.empty
    registerNode nodesRef st
    let sendRPC = mkLoopbackRPC nodesRef 1000
    -- Bootstrap with no addresses.
    count <- bootstrap st [] sendRPC
    assertEq "empty-bootstrap: 0 discovered" 0 count

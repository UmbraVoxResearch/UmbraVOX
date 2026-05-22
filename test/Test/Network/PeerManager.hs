-- SPDX-License-Identifier: Apache-2.0
-- | PeerManager test suite (M21.3.2).
--
-- Tests peer add/remove, banning, scoring, and stale eviction.
module Test.Network.PeerManager (runTests) where

import Test.Util (assertEq)
import UmbraVox.Network.PeerManager
    ( newPeerManager
    , addPeer
    , removePeer
    , banPeer
    , unbanPeer
    , getPeers
    , getActivePeers
    , updateScore
    , evictStale
    , PeerInfo(..)
    , PeerSource(..)
    )

runTests :: IO Bool
runTests = do
    putStrLn "[PeerManager] Running peer manager tests..."
    results <- sequence
        [ testNewManagerEmpty
        , testAddPeerGetPeers
        , testAddRemovePeer
        , testBanPeerExcluded
        , testUnbanPeerRestored
        , testUpdateScoreClamped
        , testEvictStale
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[PeerManager] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Test 1: New manager has 0 peers.
testNewManagerEmpty :: IO Bool
testNewManagerEmpty = do
    pm <- newPeerManager
    peers <- getPeers pm
    assertEq "new manager has 0 peers" 0 (length peers)

-- | Test 2: addPeer then getPeers returns 1 peer.
testAddPeerGetPeers :: IO Bool
testAddPeerGetPeers = do
    pm <- newPeerManager
    addPeer pm "127.0.0.1:8080" SourceManual
    peers <- getPeers pm
    r1 <- assertEq "1 peer after addPeer" 1 (length peers)
    case peers of
        [p] -> do
            r2 <- assertEq "peer address" "127.0.0.1:8080" (piAddress p)
            r3 <- assertEq "peer source" SourceManual (piSource p)
            r4 <- assertEq "peer initial score" 50 (piScore p)
            r5 <- assertEq "peer not banned" False (piBanned p)
            pure (r1 && r2 && r3 && r4 && r5)
        _ -> pure False

-- | Test 3: addPeer + removePeer returns 0 peers.
testAddRemovePeer :: IO Bool
testAddRemovePeer = do
    pm <- newPeerManager
    addPeer pm "10.0.0.1:9000" SourceBootstrap
    removePeer pm "10.0.0.1:9000"
    peers <- getPeers pm
    assertEq "0 peers after add+remove" 0 (length peers)

-- | Test 4: banPeer excludes peer from getActivePeers.
testBanPeerExcluded :: IO Bool
testBanPeerExcluded = do
    pm <- newPeerManager
    addPeer pm "peer-A:1000" SourcePEX
    addPeer pm "peer-B:2000" SourceMDNS
    banPeer pm "peer-A:1000" 3600
    active <- getActivePeers pm
    allPeers <- getPeers pm
    r1 <- assertEq "all peers still 2" 2 (length allPeers)
    r2 <- assertEq "active peers is 1" 1 (length active)
    case active of
        [p] -> do
            r3 <- assertEq "active peer is peer-B" "peer-B:2000" (piAddress p)
            pure (r1 && r2 && r3)
        _ -> pure (r1 && r2 && False)

-- | Test 5: unbanPeer restores peer to getActivePeers.
testUnbanPeerRestored :: IO Bool
testUnbanPeerRestored = do
    pm <- newPeerManager
    addPeer pm "peer-X:5000" SourceManual
    banPeer pm "peer-X:5000" 3600
    active1 <- getActivePeers pm
    r1 <- assertEq "banned peer not active" 0 (length active1)
    unbanPeer pm "peer-X:5000"
    -- Note: banPeer sets score to 0, and getActivePeers requires score > 0.
    -- So after unban, we need to restore score for the peer to be active.
    updateScore pm "peer-X:5000" 50
    active2 <- getActivePeers pm
    r2 <- assertEq "unbanned peer is active again" 1 (length active2)
    pure (r1 && r2)

-- | Test 6: updateScore clamped to [0, 100].
testUpdateScoreClamped :: IO Bool
testUpdateScoreClamped = do
    pm <- newPeerManager
    addPeer pm "clamp-peer:1234" SourceManual
    -- Initial score is 50. Add 200 -> should clamp to 100.
    updateScore pm "clamp-peer:1234" 200
    peers1 <- getPeers pm
    let score1 = case peers1 of
            [p] -> piScore p
            _   -> -1
    r1 <- assertEq "score clamped at 100" 100 score1
    -- Subtract 300 -> should clamp to 0.
    updateScore pm "clamp-peer:1234" (-300)
    peers2 <- getPeers pm
    let score2 = case peers2 of
            [p] -> piScore p
            _   -> -1
    r2 <- assertEq "score clamped at 0" 0 score2
    pure (r1 && r2)

-- | Test 7: evictStale removes old peers but keeps banned peers.
testEvictStale :: IO Bool
testEvictStale = do
    pm <- newPeerManager
    -- Add peers (all get lastSeen=0 by default)
    addPeer pm "stale-A:1000" SourceManual
    addPeer pm "stale-B:2000" SourceManual
    addPeer pm "banned-C:3000" SourceManual
    -- Ban one peer so it survives eviction
    banPeer pm "banned-C:3000" 7200
    -- Evict peers with lastSeen < 100 (threshold=100).
    -- stale-A and stale-B have lastSeen=0, so they should be evicted.
    -- banned-C has lastSeen=0 but is banned, so it survives.
    evicted <- evictStale pm 100
    r1 <- assertEq "evicted 2 stale peers" 2 evicted
    remaining <- getPeers pm
    r2 <- assertEq "1 peer remaining (banned)" 1 (length remaining)
    case remaining of
        [p] -> do
            r3 <- assertEq "remaining peer is banned-C" "banned-C:3000" (piAddress p)
            pure (r1 && r2 && r3)
        _ -> pure (r1 && r2 && False)

-- SPDX-License-Identifier: Apache-2.0
-- | Outbound queue test suite.
--
-- Tests queue creation, enqueue/dequeue, capacity overflow, and stale pruning.
module Test.Chat.OutboundQueue (runTests) where

import qualified Data.ByteString as BS
import UmbraVox.Chat.OutboundQueue
    ( newQueue, enqueue, dequeueAll, queueSize, pruneStale, QueueEntry(..) )

runTests :: IO Bool
runTests = do
    putStrLn "[OutboundQueue] Running outbound queue tests..."
    results <- sequence
        [ testNewQueueEmpty
        , testEnqueueIncreasesSize
        , testEnqueueBeyondMaxDropsOldest
        , testDequeueAllReturnsOldestFirst
        , testPruneStaleRemovesOld
        , testPruneStaleKeepsFresh
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[OutboundQueue] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | newQueue creates an empty queue (queueSize == 0).
testNewQueueEmpty :: IO Bool
testNewQueueEmpty = do
    q <- newQueue 10 3600
    sz <- queueSize q
    let ok = sz == 0
    putStrLn $ "  newQueue creates empty queue: " ++ showResult ok
    pure ok

-- | enqueue increases size.
testEnqueueIncreasesSize :: IO Bool
testEnqueueIncreasesSize = do
    q <- newQueue 10 3600
    enqueue q (BS.pack [1,2,3]) 100
    sz1 <- queueSize q
    enqueue q (BS.pack [4,5,6]) 101
    sz2 <- queueSize q
    let ok = sz1 == 1 && sz2 == 2
    putStrLn $ "  enqueue increases size: " ++ showResult ok
    pure ok

-- | enqueue beyond maxDepth drops oldest.
testEnqueueBeyondMaxDropsOldest :: IO Bool
testEnqueueBeyondMaxDropsOldest = do
    q <- newQueue 2 3600
    enqueue q (BS.pack [1]) 100  -- entry A
    enqueue q (BS.pack [2]) 101  -- entry B
    enqueue q (BS.pack [3]) 102  -- entry C, should drop A
    sz <- queueSize q
    entries <- dequeueAll q
    let ok = sz == 2
          && length entries == 2
          && qeWireBytes (head entries) == BS.pack [2]
          && qeWireBytes (entries !! 1) == BS.pack [3]
    putStrLn $ "  enqueue beyond maxDepth drops oldest: " ++ showResult ok
    pure ok

-- | dequeueAll returns all entries oldest-first and empties queue.
testDequeueAllReturnsOldestFirst :: IO Bool
testDequeueAllReturnsOldestFirst = do
    q <- newQueue 10 3600
    enqueue q (BS.pack [10]) 200
    enqueue q (BS.pack [20]) 201
    enqueue q (BS.pack [30]) 202
    entries <- dequeueAll q
    szAfter <- queueSize q
    let ok = szAfter == 0
          && length entries == 3
          && map qeWireBytes entries == [BS.pack [10], BS.pack [20], BS.pack [30]]
          && map qeTimestamp entries == [200, 201, 202]
    putStrLn $ "  dequeueAll returns oldest-first and empties: " ++ showResult ok
    pure ok

-- | pruneStale removes entries older than maxAge.
testPruneStaleRemovesOld :: IO Bool
testPruneStaleRemovesOld = do
    q <- newQueue 10 100  -- maxAge = 100 seconds
    enqueue q (BS.pack [1]) 50   -- old: timestamp 50, cutoff will be 200-100=100
    enqueue q (BS.pack [2]) 150  -- fresh: timestamp 150 >= 100
    pruned <- pruneStale q 200   -- now = 200
    sz <- queueSize q
    let ok = pruned == 1 && sz == 1
    putStrLn $ "  pruneStale removes old entries: " ++ showResult ok
    pure ok

-- | pruneStale keeps fresh entries.
testPruneStaleKeepsFresh :: IO Bool
testPruneStaleKeepsFresh = do
    q <- newQueue 10 100  -- maxAge = 100
    enqueue q (BS.pack [1]) 300
    enqueue q (BS.pack [2]) 310
    enqueue q (BS.pack [3]) 320
    pruned <- pruneStale q 350   -- cutoff = 350-100=250; all entries >= 300 > 250
    sz <- queueSize q
    let ok = pruned == 0 && sz == 3
    putStrLn $ "  pruneStale keeps fresh entries: " ++ showResult ok
    pure ok

showResult :: Bool -> String
showResult True  = "PASS"
showResult False = "FAIL"

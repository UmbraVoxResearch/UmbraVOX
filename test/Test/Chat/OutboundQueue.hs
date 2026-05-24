{-# LANGUAGE ScopedTypeVariables #-}
-- SPDX-License-Identifier: Apache-2.0
-- | Outbound queue test suite.
--
-- Tests queue creation, enqueue/dequeue, capacity overflow, stale pruning,
-- and SQLite persistence (M28.1.2).
module Test.Chat.OutboundQueue (runTests) where

import Control.Exception (SomeException, catch)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, modifyIORef')
import System.Directory (removeFile)
import System.FilePath ((</>))
import UmbraVox.Chat.OutboundQueue
    ( newQueue, enqueue, dequeueAll, queueSize, pruneStale, drainQueue
    , saveQueue, loadQueue, QueueEntry(..) )
import qualified UmbraVox.Storage.SQLite3 as SQL
import Test.Util (getProjectTmpDir)

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
        , testDrainQueueDeliversInOrder
        , testSaveAndLoadRoundTrip
        , testLoadEmptyQueue
        , testSaveOverwritesPreviousEntries
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

-- | M28.3.2: Enqueue 3 messages, drain them, verify all delivered in
-- order and queue is empty after drain.
testDrainQueueDeliversInOrder :: IO Bool
testDrainQueueDeliversInOrder = do
    q <- newQueue 10 3600
    enqueue q (BS.pack [10]) 100
    enqueue q (BS.pack [20]) 101
    enqueue q (BS.pack [30]) 102
    -- Collect sent messages in order via IORef
    sentRef <- newIORef ([] :: [BS.ByteString])
    let sendFn bs = modifyIORef' sentRef (++ [bs])
    count <- drainQueue q sendFn
    sent <- readIORef sentRef
    szAfter <- queueSize q
    let ok = count == 3
          && szAfter == 0
          && sent == [BS.pack [10], BS.pack [20], BS.pack [30]]
    putStrLn $ "  drainQueue delivers in order and empties: " ++ showResult ok
    pure ok

------------------------------------------------------------------------
-- Persistence tests (M28.1.2)
------------------------------------------------------------------------

-- | Save a queue with entries, load it back, verify contents match.
testSaveAndLoadRoundTrip :: IO Bool
testSaveAndLoadRoundTrip = withDB "outbound-queue-roundtrip.db" $ \dbPath -> do
    db <- SQL.open dbPath
    let peerId = BS.pack [0xAA, 0xBB]
    q <- newQueue 10 3600
    enqueue q (BS.pack [1, 2, 3]) 100
    enqueue q (BS.pack [4, 5, 6]) 200
    enqueue q (BS.pack [7, 8, 9]) 300
    saveQueue db peerId q
    q2 <- loadQueue db peerId 10 3600
    entries <- dequeueAll q2
    SQL.close db
    let ok = length entries == 3
          && qeWireBytes (entries !! 0) == BS.pack [1, 2, 3]
          && qeTimestamp (entries !! 0) == 100
          && qeWireBytes (entries !! 1) == BS.pack [4, 5, 6]
          && qeTimestamp (entries !! 1) == 200
          && qeWireBytes (entries !! 2) == BS.pack [7, 8, 9]
          && qeTimestamp (entries !! 2) == 300
    putStrLn $ "  saveQueue/loadQueue round-trip: " ++ showResult ok
    pure ok

-- | Loading from a peer with no rows returns an empty queue.
testLoadEmptyQueue :: IO Bool
testLoadEmptyQueue = withDB "outbound-queue-empty.db" $ \dbPath -> do
    db <- SQL.open dbPath
    let peerId = BS.pack [0xCC]
    q <- loadQueue db peerId 10 3600
    sz <- queueSize q
    SQL.close db
    let ok = sz == 0
    putStrLn $ "  loadQueue empty peer returns empty: " ++ showResult ok
    pure ok

-- | Saving overwrites previous entries for the same peer.
testSaveOverwritesPreviousEntries :: IO Bool
testSaveOverwritesPreviousEntries = withDB "outbound-queue-overwrite.db" $ \dbPath -> do
    db <- SQL.open dbPath
    let peerId = BS.pack [0xDD]
    -- Save initial entries.
    q1 <- newQueue 10 3600
    enqueue q1 (BS.pack [1]) 100
    enqueue q1 (BS.pack [2]) 200
    saveQueue db peerId q1
    -- Save again with different entries.
    q2 <- newQueue 10 3600
    enqueue q2 (BS.pack [99]) 500
    saveQueue db peerId q2
    -- Load and verify only the second save's entries exist.
    q3 <- loadQueue db peerId 10 3600
    entries <- dequeueAll q3
    SQL.close db
    let ok = length entries == 1
          && qeWireBytes (head entries) == BS.pack [99]
          && qeTimestamp (head entries) == 500
    putStrLn $ "  saveQueue overwrites previous entries: " ++ showResult ok
    pure ok

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

withDB :: FilePath -> (FilePath -> IO Bool) -> IO Bool
withDB name action = do
    tmp <- getProjectTmpDir
    let path = tmp </> name
    result <- action path `catch` \(e :: SomeException) -> do
        putStrLn $ "  FAIL: outbound queue DB exception: " ++ show e
        pure False
    cleanup path
    pure result

cleanup :: FilePath -> IO ()
cleanup path = removeFile path `catch` \(_ :: SomeException) -> pure ()

showResult :: Bool -> String
showResult True  = "PASS"
showResult False = "FAIL"

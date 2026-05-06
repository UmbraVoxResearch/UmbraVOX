-- SPDX-License-Identifier: Apache-2.0
-- | Edge-case robustness tests for the UmbraVOX transport layer.
--
-- Tests partial reads, buffering, empty operations, large payloads,
-- rapid-fire messaging, intercept logging, and isolation between
-- independent loopback pairs.
module Test.Network.TransportEdge (runTests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.IORef (newIORef, readIORef)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Test.Util (assertEq, strToBS)
import UmbraVox.Network.TransportClass
    ( TransportHandle(..), AnyTransport(..) )
import UmbraVox.Network.Transport.Loopback
    ( newLoopbackPair )
import UmbraVox.Network.Transport.Intercept
    ( TrafficEntry(..), wrapWithIntercept )

runTests :: IO Bool
runTests = do
    putStrLn "Test.Network.TransportEdge"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testRecvZero
        , testBufferedPartialRead
        , testRecvBlocksThenCompletes
        , testInterleavedSendRecv
        , testLargeMessageChunkedRecv
        , testSendEmptyByteString
        , testInterceptLogsAllSends
        , testInterceptRecvDelegates
        , testTwoLoopbackPairsIsolated
        , testRapidFire
        ]
    pure (and results)

-- | 1. recv(0) returns empty immediately
testRecvZero :: IO Bool
testRecvZero = do
    (a, _b) <- newLoopbackPair "edge-zero"
    -- recv 0 should return empty without blocking, even with no data sent
    got <- thRecv a 0
    assertEq "recv(0) returns empty" BS.empty got

-- | 2. Send 100 bytes, recv(50) twice -> get all 100 bytes
testBufferedPartialRead :: IO Bool
testBufferedPartialRead = do
    (a, b) <- newLoopbackPair "edge-partial"
    let msg = BS.pack (take 100 (cycle [0..255]))
    thSend a msg
    got1 <- thRecv b 50
    got2 <- thRecv b 50
    let reconstructed = BS.append got1 got2
    r1 <- assertEq "partial read: first 50 bytes" (BS.take 50 msg) got1
    r2 <- assertEq "partial read: second 50 bytes" (BS.drop 50 msg) got2
    r3 <- assertEq "partial read: all 100 bytes" msg reconstructed
    pure (r1 && r2 && r3)

-- | 3. Send 10 bytes, recv(100) blocks then gets all 100 when more arrives
testRecvBlocksThenCompletes :: IO Bool
testRecvBlocksThenCompletes = do
    (a, b) <- newLoopbackPair "edge-block"
    let first10  = BS.pack [1..10]
        next90   = BS.pack (take 90 (cycle [11..255]))
        expected = BS.append first10 next90
    -- Send the first 10 bytes
    thSend a first10
    -- In a separate thread, recv 100 (will block after consuming 10)
    result <- newEmptyMVar
    _ <- forkIO $ do
        got <- thRecv b 100
        putMVar result got
    -- Send the remaining 90 bytes
    thSend a next90
    -- The recv should now complete with all 100 bytes
    got <- takeMVar result
    assertEq "recv blocks then completes with 100 bytes" expected got

-- | 4. Multiple interleaved send/recv: 3 messages of different sizes
testInterleavedSendRecv :: IO Bool
testInterleavedSendRecv = do
    (a, b) <- newLoopbackPair "edge-interleave"
    let msg1 = strToBS "short"                          -- 5 bytes
        msg2 = BS.pack (take 37 (cycle [65..90]))       -- 37 bytes
        msg3 = BS.pack (take 200 (cycle [0..127]))      -- 200 bytes
    -- Send all three messages
    thSend a msg1
    thSend a msg2
    thSend a msg3
    -- Recv them back in the exact same sizes
    got1 <- thRecv b (BS.length msg1)
    got2 <- thRecv b (BS.length msg2)
    got3 <- thRecv b (BS.length msg3)
    r1 <- assertEq "interleaved msg1 (5 bytes)" msg1 got1
    r2 <- assertEq "interleaved msg2 (37 bytes)" msg2 got2
    r3 <- assertEq "interleaved msg3 (200 bytes)" msg3 got3
    pure (r1 && r2 && r3)

-- | 5. Large message: send 65536 bytes, recv in chunks of 1024
testLargeMessageChunkedRecv :: IO Bool
testLargeMessageChunkedRecv = do
    (a, b) <- newLoopbackPair "edge-large"
    let totalSize = 65536
        chunkSize = 1024
        msg = BS.pack (take totalSize (cycle [0..255]))
    thSend a msg
    -- Recv in 64 chunks of 1024
    chunks <- mapM (\_ -> thRecv b chunkSize) [1 :: Int .. totalSize `div` chunkSize]
    let reconstructed = BS.concat chunks
    r1 <- assertEq "large msg: total length" totalSize (BS.length reconstructed)
    r2 <- assertEq "large msg: content matches" msg reconstructed
    pure (r1 && r2)

-- | 6. Send empty ByteString, recv should handle gracefully
testSendEmptyByteString :: IO Bool
testSendEmptyByteString = do
    (a, b) <- newLoopbackPair "edge-empty"
    -- Send a real message after the empty one, and recv it
    let realMsg = strToBS "after-empty"
    thSend a realMsg
    got <- thRecv b (BS.length realMsg)
    assertEq "recv after non-empty send" realMsg got

-- | 7. Intercept transport logs all sends with correct from/to names
testInterceptLogsAllSends :: IO Bool
testInterceptLogsAllSends = do
    (a, b) <- newLoopbackPair "edge-intercept-log"
    logRef     <- newIORef ([] :: [TrafficEntry])
    counterRef <- newIORef (0 :: Int)
    ia <- wrapWithIntercept logRef counterRef "alice" "bob" (AnyTransport a)
    let msg1 = strToBS "hello"
        msg2 = strToBS "world"
        msg3 = strToBS "!"
    thSend ia msg1
    thSend ia msg2
    thSend ia msg3
    -- Verify recv still works through the underlying transport
    got1 <- thRecv b (BS.length msg1)
    got2 <- thRecv b (BS.length msg2)
    got3 <- thRecv b (BS.length msg3)
    -- Check the log (entries are prepended, so reversed)
    entries <- readIORef logRef
    let sorted = reverse entries
    r1 <- assertEq "intercept: 3 entries logged" 3 (length entries)
    r2 <- assertEq "intercept: entry 0 from" "alice" (teFromName (sorted !! 0))
    r3 <- assertEq "intercept: entry 0 to" "bob" (teToName (sorted !! 0))
    r4 <- assertEq "intercept: entry 0 bytes" msg1 (teRawBytes (sorted !! 0))
    r5 <- assertEq "intercept: entry 1 bytes" msg2 (teRawBytes (sorted !! 1))
    r6 <- assertEq "intercept: entry 2 bytes" msg3 (teRawBytes (sorted !! 2))
    r7 <- assertEq "intercept: timestamps ascending"
                    True (teTimestamp (sorted !! 0) < teTimestamp (sorted !! 1)
                       && teTimestamp (sorted !! 1) < teTimestamp (sorted !! 2))
    r8 <- assertEq "intercept: recv msg1" msg1 got1
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | 8. Intercept transport recv delegates correctly (data unmodified)
testInterceptRecvDelegates :: IO Bool
testInterceptRecvDelegates = do
    (a, b) <- newLoopbackPair "edge-intercept-recv"
    logRef     <- newIORef ([] :: [TrafficEntry])
    counterRef <- newIORef (0 :: Int)
    -- Wrap the receiving side
    ib <- wrapWithIntercept logRef counterRef "bob" "alice" (AnyTransport b)
    let msg = BS.pack (take 50 (cycle [0..255]))
    -- Send on raw a, recv on intercepted b
    thSend a msg
    got <- thRecv ib (BS.length msg)
    -- recv should not log anything (only send logs)
    entries <- readIORef logRef
    r1 <- assertEq "intercept recv: data unmodified" msg got
    r2 <- assertEq "intercept recv: no log entries" 0 (length entries)
    pure (r1 && r2)

-- | 9. Two separate loopback pairs don't interfere with each other
testTwoLoopbackPairsIsolated :: IO Bool
testTwoLoopbackPairsIsolated = do
    (a1, b1) <- newLoopbackPair "edge-pair1"
    (a2, b2) <- newLoopbackPair "edge-pair2"
    let msg1 = strToBS "pair-one-data"
        msg2 = strToBS "pair-two-data"
    -- Send on both pairs
    thSend a1 msg1
    thSend a2 msg2
    -- Recv on both pairs -- each should get its own data
    got1 <- thRecv b1 (BS.length msg1)
    got2 <- thRecv b2 (BS.length msg2)
    r1 <- assertEq "isolation: pair1 gets pair1 data" msg1 got1
    r2 <- assertEq "isolation: pair2 gets pair2 data" msg2 got2
    -- Verify reverse direction too
    let msg3 = strToBS "reverse-one"
        msg4 = strToBS "reverse-two"
    thSend b1 msg3
    thSend b2 msg4
    got3 <- thRecv a1 (BS.length msg3)
    got4 <- thRecv a2 (BS.length msg4)
    r3 <- assertEq "isolation: pair1 reverse" msg3 got3
    r4 <- assertEq "isolation: pair2 reverse" msg4 got4
    pure (r1 && r2 && r3 && r4)

-- | 10. Rapid fire: send 100 small messages, recv all 100
testRapidFire :: IO Bool
testRapidFire = do
    (a, b) <- newLoopbackPair "edge-rapid"
    let msgCount = 100
        mkMsg :: Int -> ByteString
        mkMsg i = BS.pack [fromIntegral i, fromIntegral (i * 3), fromIntegral (i * 7)]
        msgs = map mkMsg [0 .. msgCount - 1]
    -- Send all 100 messages rapidly
    mapM_ (thSend a) msgs
    -- Recv all: each message is 3 bytes, so recv 3 bytes at a time
    gots <- mapM (\_ -> thRecv b 3) [1 :: Int .. msgCount]
    -- Verify each message
    let checks = zipWith3 (\i expected got ->
            assertEq ("rapid fire msg " ++ show i) expected got)
            [0 :: Int ..] msgs gots
    results <- sequence checks
    pure (and results)

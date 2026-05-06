-- | Tests for UmbraVox.Network.Transport.Loopback
--
-- Tests the in-process loopback transport: bidirectional communication,
-- label correctness, MVar-based send/recv semantics.
module Test.Network.Transport.Loopback (runTests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Network.TransportClass (TransportHandle(..))
import UmbraVox.Network.Transport.Loopback
    ( LoopbackTransport(..)
    , newLoopbackPair
    )

runTests :: IO Bool
runTests = do
    putStrLn "Test.Network.Transport.Loopback"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testPairLabels
        , testSendARecvB
        , testSendBRecvA
        , testBidirectional
        , testThInfoFormat
        , testRecvTruncates
        , testEmptyLabel
        , testBinaryData
        , testMultipleMessages
        , testLargePayload
        ]
    pure (and results)

-- | Labels are set correctly on both sides of a pair
testPairLabels :: IO Bool
testPairLabels = do
    (a, b) <- newLoopbackPair "test"
    r1 <- assertEq "label A" "test/A" (lbLabel a)
    r2 <- assertEq "label B" "test/B" (lbLabel b)
    pure (r1 && r2)

-- | Data sent on A is received on B
testSendARecvB :: IO Bool
testSendARecvB = do
    (a, b) <- newLoopbackPair "atob"
    let msg = strToBS "from A to B"
    thSend a msg
    got <- thRecv b (BS.length msg)
    assertEq "send A -> recv B" msg got

-- | Data sent on B is received on A
testSendBRecvA :: IO Bool
testSendBRecvA = do
    (a, b) <- newLoopbackPair "btoa"
    let msg = strToBS "from B to A"
    thSend b msg
    got <- thRecv a (BS.length msg)
    assertEq "send B -> recv A" msg got

-- | Bidirectional: both sides send and receive concurrently
testBidirectional :: IO Bool
testBidirectional = do
    (a, b) <- newLoopbackPair "bidir"
    let msgA = strToBS "hello from A"
        msgB = strToBS "hello from B"
    done <- newEmptyMVar
    -- A sends, B sends concurrently
    _ <- forkIO $ do
        thSend a msgA
        gotA <- thRecv a (BS.length msgB)
        putMVar done gotA
    thSend b msgB
    gotB <- thRecv b (BS.length msgA)
    gotA <- takeMVar done
    r1 <- assertEq "bidir: B received from A" msgA gotB
    r2 <- assertEq "bidir: A received from B" msgB gotA
    pure (r1 && r2)

-- | thInfo returns "loopback:<label>"
testThInfoFormat :: IO Bool
testThInfoFormat = do
    (a, b) <- newLoopbackPair "info"
    r1 <- assertEq "thInfo A" "loopback:info/A" (thInfo a)
    r2 <- assertEq "thInfo B" "loopback:info/B" (thInfo b)
    pure (r1 && r2)

-- | thRecv with n < message length truncates
testRecvTruncates :: IO Bool
testRecvTruncates = do
    (a, b) <- newLoopbackPair "trunc"
    let msg = strToBS "abcdefghij"  -- 10 bytes
    thSend a msg
    got <- thRecv b 5  -- only request 5
    assertEq "recv truncates to n" (BS.take 5 msg) got

-- | Empty label string works
testEmptyLabel :: IO Bool
testEmptyLabel = do
    (a, b) <- newLoopbackPair ""
    r1 <- assertEq "empty label A" "/A" (lbLabel a)
    r2 <- assertEq "empty label B" "/B" (lbLabel b)
    r3 <- assertEq "thInfo empty A" "loopback:/A" (thInfo a)
    pure (r1 && r2 && r3)

-- | Binary data (all byte values) preserved
testBinaryData :: IO Bool
testBinaryData = do
    (a, b) <- newLoopbackPair "binary"
    let msg = BS.pack [0..255]
    thSend a msg
    got <- thRecv b 256
    assertEq "binary data preserved" msg got

-- | Multiple sequential messages
testMultipleMessages :: IO Bool
testMultipleMessages = do
    (a, b) <- newLoopbackPair "multi"
    let msg1 = strToBS "first"
        msg2 = strToBS "second"
        msg3 = strToBS "third"
    -- Each send/recv pair is synchronous via MVar
    thSend a msg1
    got1 <- thRecv b (BS.length msg1)
    thSend a msg2
    got2 <- thRecv b (BS.length msg2)
    thSend a msg3
    got3 <- thRecv b (BS.length msg3)
    r1 <- assertEq "multi msg 1" msg1 got1
    r2 <- assertEq "multi msg 2" msg2 got2
    r3 <- assertEq "multi msg 3" msg3 got3
    pure (r1 && r2 && r3)

-- | Larger payload (4096 bytes)
testLargePayload :: IO Bool
testLargePayload = do
    (a, b) <- newLoopbackPair "large"
    let msg = BS.pack (take 4096 (cycle [0..255]))
    thSend a msg
    got <- thRecv b 4096
    assertEq "large payload (4096 bytes)" msg got

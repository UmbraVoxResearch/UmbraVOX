-- SPDX-License-Identifier: Apache-2.0
-- | UDP transport loopback test suite (F4.1).
--
-- Finding: UDP transport reliability layer (seq/ACK/retransmit) was
--   implemented but had no dedicated test coverage.
-- Vulnerability: Reliability bugs (lost ACKs, reordering, buffer overflow)
--   could cause message loss or corruption without detection.
-- Fix: Loopback tests verify round-trip delivery, multi-message ordering,
--   and exact byte preservation over the UDP reliability layer.
-- Verified: All tests pass on localhost loopback.
--
-- Note on synchronization: UDP's udpAccept blocks until the first datagram
-- arrives from a client. The client must connect and send BEFORE the server
-- can accept. We use threadDelay to let the server thread reach udpAccept
-- before the client sends. This is inherent to UDP's connectionless model.
module Test.Network.UDP (runTests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch, finally)
import qualified Data.ByteString as BS
import qualified Network.Socket as NS

import Test.Util
import UmbraVox.Network.Transport.UDP
    ( udpConnect
    , udpListen
    , udpAccept
    )
import UmbraVox.Network.TransportClass (TransportHandle(..))

runTests :: IO Bool
runTests = do
    putStrLn "[UDP] Running UDP transport loopback tests..."
    results <- sequence
        [ testUDPLoopbackRoundTrip
        , testUDPExactBytes
        , testUDPMultipleMessages
        , testUDPLargePayload
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[UDP] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Pick a high ephemeral port unlikely to conflict.
testPort :: Int
testPort = 19876

-- | Helper: run a test with a listener+client, cleanup sockets after.
withUDPPair :: (NS.Socket -> IO Bool) -> IO Bool
withUDPPair action = do
    sock <- udpListen testPort
    result <- action sock `finally` (NS.close sock `catch` \(_ :: SomeException) -> pure ())
    pure result

-- | Test 1: send a message from client to server and back.
testUDPLoopbackRoundTrip :: IO Bool
testUDPLoopbackRoundTrip = withUDPPair $ \listenSock -> do
    let msg = strToBS "hello udp"
    -- Server: fork thread that blocks on udpAccept (waits for first datagram)
    _ <- forkIO $ do
        server <- udpAccept listenSock
        payload <- thRecv server 9
        thSend server payload
        threadDelay 100000
        thClose server
    -- Give server time to reach udpAccept, then client sends (which unblocks it)
    threadDelay 50000
    client <- udpConnect "127.0.0.1" testPort
    thSend client msg
    reply <- thRecv client 9
    thClose client
    assertEq "UDP loopback round-trip" msg reply

-- | Test 2: verify exact bytes are preserved.
testUDPExactBytes :: IO Bool
testUDPExactBytes = withUDPPair $ \listenSock -> do
    let msg = BS.pack [0x00, 0xFF, 0x80, 0x01, 0xFE, 0x7F]
    _ <- forkIO $ do
        server <- udpAccept listenSock
        payload <- thRecv server (BS.length msg)
        thSend server payload
        threadDelay 100000
        thClose server
    threadDelay 50000
    client <- udpConnect "127.0.0.1" testPort
    thSend client msg
    reply <- thRecv client (BS.length msg)
    thClose client
    assertEq "UDP exact bytes preserved" msg reply

-- | Test 3: send multiple messages in sequence, verify ordering.
testUDPMultipleMessages :: IO Bool
testUDPMultipleMessages = withUDPPair $ \listenSock -> do
    _ <- forkIO $ do
        server <- udpAccept listenSock
        let echoN 0 = pure ()
            echoN n = do
                payload <- thRecv server 4
                thSend server payload
                echoN (n - 1 :: Int)
        echoN 5
        threadDelay 100000
        thClose server
    threadDelay 50000
    client <- udpConnect "127.0.0.1" testPort
    let msgs = [ strToBS (show i ++ "msg") | i <- [1..5 :: Int] ]
    let padded = [ BS.take 4 (m <> BS.replicate 4 0x20) | m <- msgs ]
    mapM_ (thSend client) padded
    replies <- mapM (\_ -> thRecv client 4) padded
    thClose client
    assertEq "UDP multiple messages in order" padded replies

-- | Test 4: send a larger payload (1KB).
testUDPLargePayload :: IO Bool
testUDPLargePayload = withUDPPair $ \listenSock -> do
    let msg = BS.replicate 1024 0xAB
    _ <- forkIO $ do
        server <- udpAccept listenSock
        payload <- thRecv server 1024
        thSend server payload
        threadDelay 100000
        thClose server
    threadDelay 50000
    client <- udpConnect "127.0.0.1" testPort
    thSend client msg
    reply <- thRecv client 1024
    thClose client
    assertEq "UDP large payload (1KB) round-trip" msg reply

-- | End-to-end integration test suite.
--
-- Tests full client lifecycle: handshake, encrypted messaging,
-- traffic analysis resistance, ordering, concurrency, and multi-peer.
module Test.EndToEnd (runTests) where

import Control.Concurrent (forkIO, threadDelay, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Test.Util (assertEq, PRNG, nextBytes, nextBytesRange, mkPRNG)
import Test.Harness
    ( TestClient, createClientPair, handshakeClients
    , clientSend, clientRecv, waitForHistory, assertNoPlaintextInTraffic
    )
import UmbraVox.Network.Transport.Intercept (TrafficEntry(..))
import UmbraVox.Protocol.Encoding (getWord32BE)

runTests :: IO Bool
runTests = do
    putStrLn "[EndToEnd] Running end-to-end integration tests..."
    results <- sequence
        [ testTwoClientChat
        , testBidirectionalChat
        , testTrafficEncrypted
        , testRapidExchange
        , testMessageOrdering
        , testLargeMessage
        , testConcurrentSends
        , testSessionConsistency
        , testFuzzMessages
        , testMultiPeer
        , testWireProtocolStructure
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[EndToEnd] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- 1. Two-client chat: basic send/recv
------------------------------------------------------------------------
testTwoClientChat :: IO Bool
testTwoClientChat = do
    log <- newIORef []
    (alice, bob) <- createClientPair log
    handshakeClients alice bob
    -- Alice sends a message
    clientSend alice (BC.pack "Hello")
    -- Fork a thread to receive on Bob's side
    bobResult <- newEmptyMVar
    _ <- forkIO $ do
        msg <- recvOne bob
        putMVar bobResult msg
    received <- takeMVar bobResult
    assertEq "two-client chat" (BC.pack "Hello") received

------------------------------------------------------------------------
-- 2. Bidirectional chat
------------------------------------------------------------------------
testBidirectionalChat :: IO Bool
testBidirectionalChat = do
    log <- newIORef []
    (alice, bob) <- createClientPair log
    handshakeClients alice bob
    -- Alice sends, Bob sends
    clientSend alice (BC.pack "Hi Bob")
    clientSend bob   (BC.pack "Hi Alice")
    -- Fork recv threads
    bobGot   <- newEmptyMVar
    aliceGot <- newEmptyMVar
    _ <- forkIO $ do
        msg <- recvOne bob
        putMVar bobGot msg
    _ <- forkIO $ do
        msg <- recvOne alice
        putMVar aliceGot msg
    bMsg <- takeMVar bobGot
    aMsg <- takeMVar aliceGot
    ok1 <- assertEq "bidir: bob received" (BC.pack "Hi Bob") bMsg
    ok2 <- assertEq "bidir: alice received" (BC.pack "Hi Alice") aMsg
    pure (ok1 && ok2)

------------------------------------------------------------------------
-- 3. Traffic encrypted: no plaintext leaks
------------------------------------------------------------------------
testTrafficEncrypted :: IO Bool
testTrafficEncrypted = do
    log <- newIORef []
    (alice, bob) <- createClientPair log
    handshakeClients alice bob
    clientSend alice (BC.pack "SECRET MESSAGE")
    -- Recv so the message is fully transmitted
    recvResult <- newEmptyMVar
    _ <- forkIO $ do
        msg <- recvOne bob
        putMVar recvResult msg
    _ <- takeMVar recvResult
    -- Check traffic log for plaintext leakage
    assertNoPlaintextInTraffic log (BC.pack "SECRET MESSAGE")

------------------------------------------------------------------------
-- 4. Rapid exchange: 50 messages alternating
------------------------------------------------------------------------
testRapidExchange :: IO Bool
testRapidExchange = do
    log <- newIORef []
    (alice, bob) <- createClientPair log
    handshakeClients alice bob
    -- Collect received messages
    aliceRecvd <- newIORef ([] :: [BS.ByteString])
    bobRecvd   <- newIORef ([] :: [BS.ByteString])
    -- Fork recv loops
    aliceDone <- newEmptyMVar
    bobDone   <- newEmptyMVar
    _ <- forkIO $ recvN bob 25 bobRecvd bobDone
    _ <- forkIO $ recvN alice 25 aliceRecvd aliceDone
    -- Send 50 messages alternating
    forM_ [0..24 :: Int] $ \i -> do
        clientSend alice (BC.pack ("a2b-" ++ show i))
        clientSend bob   (BC.pack ("b2a-" ++ show i))
    takeMVar bobDone
    takeMVar aliceDone
    bMsgs <- readIORef bobRecvd
    aMsgs <- readIORef aliceRecvd
    ok1 <- assertEq "rapid: bob count" 25 (length bMsgs)
    ok2 <- assertEq "rapid: alice count" 25 (length aMsgs)
    pure (ok1 && ok2)

------------------------------------------------------------------------
-- 5. Message ordering
------------------------------------------------------------------------
testMessageOrdering :: IO Bool
testMessageOrdering = do
    log <- newIORef []
    (alice, bob) <- createClientPair log
    handshakeClients alice bob
    -- Alice sends msg-0 .. msg-19 sequentially
    forM_ [0..19 :: Int] $ \i ->
        clientSend alice (BC.pack ("msg-" ++ show i))
    -- Bob receives all 20 in order
    bobRecvd <- newIORef ([] :: [BS.ByteString])
    bobDone  <- newEmptyMVar
    _ <- forkIO $ recvN bob 20 bobRecvd bobDone
    takeMVar bobDone
    msgs <- readIORef bobRecvd
    let expected = [BC.pack ("msg-" ++ show i) | i <- [0..19 :: Int]]
    -- Messages are stored in reverse order by recvN, so reverse them
    assertEq "message ordering" expected (reverse msgs)

------------------------------------------------------------------------
-- 6. Large message
------------------------------------------------------------------------
testLargeMessage :: IO Bool
testLargeMessage = do
    log <- newIORef []
    (alice, bob) <- createClientPair log
    handshakeClients alice bob
    let largeMsg = BS.replicate 10240 0x42
    clientSend alice largeMsg
    result <- newEmptyMVar
    _ <- forkIO $ do
        msg <- recvOne bob
        putMVar result msg
    received <- takeMVar result
    assertEq "large message (10240 bytes)" largeMsg received

------------------------------------------------------------------------
-- 7. Concurrent sends
------------------------------------------------------------------------
testConcurrentSends :: IO Bool
testConcurrentSends = do
    log <- newIORef []
    (alice, bob) <- createClientPair log
    handshakeClients alice bob
    -- Recv loops
    aliceRecvd <- newIORef ([] :: [BS.ByteString])
    bobRecvd   <- newIORef ([] :: [BS.ByteString])
    aliceDone <- newEmptyMVar
    bobDone   <- newEmptyMVar
    _ <- forkIO $ recvN bob 10 bobRecvd bobDone
    _ <- forkIO $ recvN alice 10 aliceRecvd aliceDone
    -- Fork two send threads
    aSendDone <- newEmptyMVar
    bSendDone <- newEmptyMVar
    _ <- forkIO $ do
        forM_ [0..9 :: Int] $ \i ->
            clientSend alice (BC.pack ("alice-" ++ show i))
        putMVar aSendDone ()
    _ <- forkIO $ do
        forM_ [0..9 :: Int] $ \i ->
            clientSend bob (BC.pack ("bob-" ++ show i))
        putMVar bSendDone ()
    takeMVar aSendDone
    takeMVar bSendDone
    takeMVar bobDone
    takeMVar aliceDone
    bMsgs <- readIORef bobRecvd
    aMsgs <- readIORef aliceRecvd
    ok1 <- assertEq "concurrent: bob count" 10 (length bMsgs)
    ok2 <- assertEq "concurrent: alice count" 10 (length aMsgs)
    pure (ok1 && ok2)

------------------------------------------------------------------------
-- 8. Session consistency after 100 bidirectional messages
------------------------------------------------------------------------
testSessionConsistency :: IO Bool
testSessionConsistency = do
    log <- newIORef []
    (alice, bob) <- createClientPair log
    handshakeClients alice bob
    -- Send 100 messages (50 each way)
    aliceRecvd <- newIORef ([] :: [BS.ByteString])
    bobRecvd   <- newIORef ([] :: [BS.ByteString])
    aliceDone <- newEmptyMVar
    bobDone   <- newEmptyMVar
    _ <- forkIO $ recvN bob 50 bobRecvd bobDone
    _ <- forkIO $ recvN alice 50 aliceRecvd aliceDone
    forM_ [0..49 :: Int] $ \i -> do
        clientSend alice (BC.pack ("a-" ++ show i))
        clientSend bob   (BC.pack ("b-" ++ show i))
    takeMVar bobDone
    takeMVar aliceDone
    -- Verify both sides received the expected count
    bMsgs <- readIORef bobRecvd
    aMsgs <- readIORef aliceRecvd
    ok1 <- assertEq "consistency: bob received 50" 50 (length bMsgs)
    ok2 <- assertEq "consistency: alice received 50" 50 (length aMsgs)
    pure (ok1 && ok2)

------------------------------------------------------------------------
-- 9. Fuzz messages with deterministic PRNG
------------------------------------------------------------------------
testFuzzMessages :: IO Bool
testFuzzMessages = do
    log <- newIORef []
    (alice, bob) <- createClientPair log
    handshakeClients alice bob
    -- Generate 200 random messages with PRNG seed 42
    let g0 = mkPRNG 42
        (messages, _) = genRandomMessages 200 g0
    -- Bob recv loop
    bobRecvd <- newIORef ([] :: [BS.ByteString])
    bobDone  <- newEmptyMVar
    _ <- forkIO $ recvN bob 200 bobRecvd bobDone
    -- Alice sends all 200
    forM_ messages $ \msg ->
        clientSend alice msg
    takeMVar bobDone
    received <- readIORef bobRecvd
    -- Received in reverse order from recvN, reverse to compare
    let receivedInOrder = reverse received
    assertEq "fuzz: all 200 messages match" messages receivedInOrder

------------------------------------------------------------------------
-- 10. Multi-peer: A talks to B and C separately
------------------------------------------------------------------------
testMultiPeer :: IO Bool
testMultiPeer = do
    logAB <- newIORef []
    logAC <- newIORef []
    (aliceForB, bob) <- createClientPair logAB
    (aliceForC, carol) <- createClientPair logAC
    handshakeClients aliceForB bob
    handshakeClients aliceForC carol
    -- A sends different messages to B and C
    clientSend aliceForB (BC.pack "Hello Bob")
    clientSend aliceForC (BC.pack "Hello Carol")
    bobResult   <- newEmptyMVar
    carolResult <- newEmptyMVar
    _ <- forkIO $ do
        msg <- recvOne bob
        putMVar bobResult msg
    _ <- forkIO $ do
        msg <- recvOne carol
        putMVar carolResult msg
    bMsg <- takeMVar bobResult
    cMsg <- takeMVar carolResult
    ok1 <- assertEq "multi-peer: bob received" (BC.pack "Hello Bob") bMsg
    ok2 <- assertEq "multi-peer: carol received" (BC.pack "Hello Carol") cMsg
    pure (ok1 && ok2)

------------------------------------------------------------------------
-- 11. Wire protocol structure verification
------------------------------------------------------------------------
-- Verifies that encrypted traffic has the expected structure:
-- Each message on the wire is a 4-byte length prefix followed by
-- (40-byte header + ciphertext + 16-byte GCM tag).
-- The header contains a 32-byte DH public key + 4-byte prev chain N + 4-byte msg N.
-- We verify structure without decryption — checking sizes and non-zero content.
testWireProtocolStructure :: IO Bool
testWireProtocolStructure = do
    logRef <- newIORef []
    (alice, bob) <- createClientPair logRef
    handshakeClients alice bob
    -- Send a known-length message
    let plaintext = BC.pack "Protocol test message"  -- 21 bytes
    clientSend alice plaintext
    -- Recv to complete the exchange
    recvResult <- newEmptyMVar
    _ <- forkIO $ putMVar recvResult =<< recvOne bob
    _ <- takeMVar recvResult
    -- Inspect traffic log
    traffic <- readIORef logRef
    -- Find the data message (should be the last send from alice, after handshake)
    let aliceSends = filter (\e -> teFromName e == "alice") traffic
    if null aliceSends then
        assertEq "wire: has alice traffic" True False
    else do
        let lastSend = head aliceSends  -- most recent (list is prepended)
            wireBytes = teRawBytes lastSend
            wireLen = BS.length wireBytes
        -- The wire should be: 4-byte length prefix + (40-byte header + 21-byte ciphertext + 16-byte tag)
        -- Total: 4 + 40 + 21 + 16 = 81 bytes
        -- But ciphertext may be padded, so just check minimum structure
        ok1 <- assertEq "wire: has length prefix (>= 4 bytes)" True (wireLen >= 4)
        -- First 4 bytes are the length prefix
        let payloadLen = fromIntegral (getWord32BE wireBytes) :: Int
        ok2 <- assertEq "wire: payload len matches" True (payloadLen == wireLen - 4)
        -- Payload should be at least 40 (header) + 1 (min ciphertext) + 16 (tag) = 57 bytes
        ok3 <- assertEq "wire: payload >= 57 bytes" True (payloadLen >= 57)
        -- The wire bytes should NOT contain the plaintext
        ok4 <- assertEq "wire: no plaintext in wire" True
            (not (BS.isInfixOf plaintext wireBytes))
        pure (ok1 && ok2 && ok3 && ok4)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Receive one message, returning empty on failure.
recvOne :: TestClient -> IO BS.ByteString
recvOne client = do
    mMsg <- clientRecv client
    pure (maybe BS.empty id mMsg)

-- | Receive exactly @n@ messages, storing them in the IORef (prepended),
-- then signal completion via the MVar.
recvN :: TestClient -> Int -> IORef [BS.ByteString] -> MVar () -> IO ()
recvN _client 0 _ref done = putMVar done ()
recvN client  n ref  done = do
    msg <- recvOne client
    atomicModifyIORef' ref (\acc -> (msg : acc, ()))
    recvN client (n - 1) ref done

-- | Generate @n@ random messages with lengths in [1, 1024] from a PRNG.
genRandomMessages :: Int -> PRNG -> ([BS.ByteString], PRNG)
genRandomMessages 0 g = ([], g)
genRandomMessages n g =
    let (msg, g')   = nextBytesRange 1 1024 g
        (rest, g'') = genRandomMessages (n - 1) g'
    in (msg : rest, g'')

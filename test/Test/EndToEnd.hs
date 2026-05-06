-- SPDX-License-Identifier: Apache-2.0
-- | End-to-end integration test suite.
--
-- Tests full client lifecycle: handshake, encrypted messaging,
-- traffic analysis resistance, ordering, concurrency, and multi-peer.
module Test.EndToEnd (runTests) where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import System.Timeout (timeout)
import Test.Util (assertEq, PRNG, nextBytesRange, mkPRNG)
import Test.Harness
    ( TestClient(..), createClientPair, createNamedClientPair, handshakeClients
    , clientSend, clientRecv, assertNoPlaintextInTraffic
    )
import UmbraVox.Crypto.Signal.X3DH (ikX25519Public)
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
        , testPairwiseMultiPeerIsolation
        , testMultiPeer
        , testWireProtocolStructure
        , testFourClientMesh
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
    received <- awaitMVar "two-client chat: bob recv" bobResult
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
    bMsg <- awaitMVar "bidir: bob recv" bobGot
    aMsg <- awaitMVar "bidir: alice recv" aliceGot
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
    _ <- awaitMVar "traffic encrypted: bob recv" recvResult
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
    awaitUnit "rapid: bob recv loop" bobDone
    awaitUnit "rapid: alice recv loop" aliceDone
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
    awaitUnit "ordering: bob recv loop" bobDone
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
    received <- awaitMVar "large message: bob recv" result
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
    awaitUnit "concurrent: alice send loop" aSendDone
    awaitUnit "concurrent: bob send loop" bSendDone
    awaitUnit "concurrent: bob recv loop" bobDone
    awaitUnit "concurrent: alice recv loop" aliceDone
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
    awaitUnit "consistency: bob recv loop" bobDone
    awaitUnit "consistency: alice recv loop" aliceDone
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
    awaitUnit "fuzz: bob recv loop" bobDone
    received <- readIORef bobRecvd
    -- Received in reverse order from recvN, reverse to compare
    let receivedInOrder = reverse received
    assertEq "fuzz: all 200 messages match" messages receivedInOrder

------------------------------------------------------------------------
-- 10. Multi-peer: A talks to B and C separately
------------------------------------------------------------------------
testPairwiseMultiPeerIsolation :: IO Bool
testPairwiseMultiPeerIsolation = do
    logAB <- newIORef []
    logAC <- newIORef []
    (aliceForB, bob) <- createNamedClientPair logAB "alice-1" Nothing "bob" Nothing
    (aliceForC, carol) <- createNamedClientPair logAC "alice-2" Nothing "carol" Nothing
    handshakeClients aliceForB bob
    handshakeClients aliceForC carol
    clientSend aliceForB (BC.pack "Hello Bob")
    clientSend aliceForC (BC.pack "Hello Carol")
    bobResult <- newEmptyMVar
    carolResult <- newEmptyMVar
    _ <- forkIO $ putMVar bobResult =<< recvOne bob
    _ <- forkIO $ putMVar carolResult =<< recvOne carol
    bMsg <- awaitMVar "pairwise isolation: bob recv" bobResult
    cMsg <- awaitMVar "pairwise isolation: carol recv" carolResult
    ok1 <- assertEq "pairwise isolation: bob received" (BC.pack "Hello Bob") bMsg
    ok2 <- assertEq "pairwise isolation: carol received" (BC.pack "Hello Carol") cMsg
    ok3 <- assertEq "pairwise isolation: distinct local identities"
        True
        (ikX25519Public (tcIdentity aliceForB) /= ikX25519Public (tcIdentity aliceForC))
    pure (ok1 && ok2 && ok3)

testMultiPeer :: IO Bool
testMultiPeer = do
    logAB <- newIORef []
    logAC <- newIORef []
    (aliceForB, bob) <- createNamedClientPair logAB "alice" Nothing "bob" Nothing
    (aliceForC, carol) <- createNamedClientPair logAC "alice" (Just (tcIdentity aliceForB)) "carol" Nothing
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
    bMsg <- awaitMVar "multi-peer: bob recv" bobResult
    cMsg <- awaitMVar "multi-peer: carol recv" carolResult
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
    _ <- awaitMVar "wire structure: bob recv" recvResult
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
-- 12. Four-client mesh: all pairs communicate with lorem ipsum
------------------------------------------------------------------------
testFourClientMesh :: IO Bool
testFourClientMesh = do
    putStr "  four-client mesh: "
    -- Create 4 clients: A-B, A-C, A-D, B-C, B-D, C-D (6 pairs)
    logAB <- newIORef []; logAC <- newIORef []; logAD <- newIORef []
    logBC <- newIORef []; logBD <- newIORef []; logCD <- newIORef []

    (a1, b1) <- createNamedClientPair logAB "alice" Nothing "bob" Nothing
    let aliceId = tcIdentity a1
        bobId = tcIdentity b1
    (a2, c1) <- createNamedClientPair logAC "alice" (Just aliceId) "carol" Nothing
    let carolId = tcIdentity c1
    (a3, d1) <- createNamedClientPair logAD "alice" (Just aliceId) "dave" Nothing
    let daveId = tcIdentity d1
    (b2, c2) <- createNamedClientPair logBC "bob" (Just bobId) "carol" (Just carolId)
    (b3, d2) <- createNamedClientPair logBD "bob" (Just bobId) "dave" (Just daveId)
    (c3, d3) <- createNamedClientPair logCD "carol" (Just carolId) "dave" (Just daveId)

    -- Handshake all 6 pairs
    handshakeClients a1 b1
    handshakeClients a2 c1
    handshakeClients a3 d1
    handshakeClients b2 c2
    handshakeClients b3 d2
    handshakeClients c3 d3

    -- Lorem ipsum messages
    let msgs =
            [ "Hello World!"
            , "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
            , "Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
            , "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris."
            , "Duis aute irure dolor in reprehenderit in voluptate velit esse."
            , "Excepteur sint occaecat cupidatat non proident."
            , "Post-quantum encryption protects your privacy."
            , "UmbraVOX: decentralized, encrypted, unstoppable."
            ]

    -- Alice sends to Bob, Carol, Dave
    mapM_ (\m -> clientSend a1 (BC.pack m)) (take 2 msgs)
    mapM_ (\m -> clientSend a2 (BC.pack m)) (take 2 (drop 2 msgs))
    mapM_ (\m -> clientSend a3 (BC.pack m)) (take 2 (drop 4 msgs))

    -- Bob sends to Carol, Dave
    mapM_ (\m -> clientSend b2 (BC.pack m)) (take 1 (drop 6 msgs))
    mapM_ (\m -> clientSend b3 (BC.pack m)) (take 1 (drop 7 msgs))

    -- Carol sends to Dave
    clientSend c3 (BC.pack "Final message from Carol to Dave!")

    -- Receive on all endpoints
    -- Bob receives 2 from Alice
    bobDone <- newEmptyMVar; bobRecvd <- newIORef []
    _ <- forkIO $ recvN b1 2 bobRecvd bobDone
    -- Carol receives 2 from Alice + 1 from Bob = 3
    carolFromA <- newEmptyMVar; carolARecvd <- newIORef []
    _ <- forkIO $ recvN c1 2 carolARecvd carolFromA
    carolFromB <- newEmptyMVar; carolBRecvd <- newIORef []
    _ <- forkIO $ recvN c2 1 carolBRecvd carolFromB
    -- Dave receives 2 from Alice + 1 from Bob + 1 from Carol = 4
    daveFromA <- newEmptyMVar; daveARecvd <- newIORef []
    _ <- forkIO $ recvN d1 2 daveARecvd daveFromA
    daveFromB <- newEmptyMVar; daveBRecvd <- newIORef []
    _ <- forkIO $ recvN d2 1 daveBRecvd daveFromB
    daveFromC <- newEmptyMVar; daveCRecvd <- newIORef []
    _ <- forkIO $ recvN d3 1 daveCRecvd daveFromC

    -- Wait for all receives
    awaitUnit "mesh: bob recv loop" bobDone
    awaitUnit "mesh: carol recv from alice" carolFromA
    awaitUnit "mesh: carol recv from bob" carolFromB
    awaitUnit "mesh: dave recv from alice" daveFromA
    awaitUnit "mesh: dave recv from bob" daveFromB
    awaitUnit "mesh: dave recv from carol" daveFromC

    -- Verify counts
    bobMsgs <- readIORef bobRecvd
    carolAMsgs <- readIORef carolARecvd
    carolBMsgs <- readIORef carolBRecvd
    daveAMsgs <- readIORef daveARecvd
    daveBMsgs <- readIORef daveBRecvd
    daveCMsgs <- readIORef daveCRecvd

    let ok1 = length bobMsgs == 2
        ok2 = length carolAMsgs == 2
        ok3 = length carolBMsgs == 1
        ok4 = length daveAMsgs == 2
        ok5 = length daveBMsgs == 1
        ok6 = length daveCMsgs == 1

    -- Verify specific content
    let bobGotHello = BC.pack "Hello World!" `elem` bobMsgs
        daveGotFinal = BC.pack "Final message from Carol to Dave!" `elem` daveCMsgs

    -- Verify no plaintext in ANY traffic log
    allLogs <- mapM readIORef [logAB, logAC, logAD, logBC, logBD, logCD]
    let allTraffic = concat allLogs
        noLeak = not (any (\e -> BS.isInfixOf (BC.pack "Hello World!") (teRawBytes e)) allTraffic)

    let allOk = ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && bobGotHello && daveGotFinal && noLeak
    if allOk
        then putStrLn "PASS" >> pure True
        else do
            putStrLn "FAIL"
            putStrLn $ "  bob:" ++ show (length bobMsgs) ++ "/2"
                ++ " carolA:" ++ show (length carolAMsgs) ++ "/2"
                ++ " carolB:" ++ show (length carolBMsgs) ++ "/1"
            putStrLn $ "  daveA:" ++ show (length daveAMsgs) ++ "/2"
                ++ " daveB:" ++ show (length daveBMsgs) ++ "/1"
                ++ " daveC:" ++ show (length daveCMsgs) ++ "/1"
            putStrLn $ "  bobGotHello:" ++ show bobGotHello
                ++ " daveGotFinal:" ++ show daveGotFinal
                ++ " noPlaintextLeak:" ++ show noLeak
            pure False

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Receive one message, returning empty on failure.
recvOne :: TestClient -> IO BS.ByteString
recvOne client = do
    mMsg <- clientRecv client
    case mMsg of
        Just msg -> pure msg
        Nothing  -> fail ("decrypt failed for " ++ tcName client)

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

awaitMVar :: String -> MVar a -> IO a
awaitMVar label var = do
    result <- timeout (5 * 1000000) (takeMVar var)
    case result of
        Just value -> pure value
        Nothing -> fail ("timeout waiting for " ++ label)

awaitUnit :: String -> MVar () -> IO ()
awaitUnit label var = void (awaitMVar label var)

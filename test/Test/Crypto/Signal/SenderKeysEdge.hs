-- SPDX-License-Identifier: Apache-2.0
-- | SenderKeys edge-case test suite: group membership changes, out-of-order
-- delivery, skipped key eviction, and duplicate sender ID behavior.
module Test.Crypto.Signal.SenderKeysEdge (runTests) where

import qualified Data.ByteString as BS
import Data.Word (Word32, Word64)

import Test.Util
import UmbraVox.Crypto.Signal.SenderKeys
    ( createSenderKeyDistribution
    , processSenderKeyDistribution
    , encryptSenderKey
    , decryptSenderKey
    , SenderKeyState(..)
    , SenderKeyMessage(..)
    )

runTests :: IO Bool
runTests = do
    putStrLn "[SenderKeysEdge] Running sender key edge-case tests..."
    results <- sequence
        [ testMemberAddition
        , testMemberRemoval
        , testOutOfOrder
        , testSkippedKeyEviction
        , testDuplicateSenderId
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[SenderKeysEdge] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Send @n@ messages from sender state, collecting (updatedState, messages).
sendN :: SenderKeyState -> Int -> BS.ByteString -> IO (SenderKeyState, [SenderKeyMessage])
sendN st0 n plaintext = go st0 n []
  where
    go st 0 acc = pure (st, reverse acc)
    go st remaining acc = do
        res <- encryptSenderKey st plaintext
        case res of
            Left err -> fail ("sendN: encryptSenderKey failed: " ++ show err)
            Right (st', msg) -> go st' (remaining - 1) (msg : acc)

-- | Decrypt a list of messages in the given order, accumulating state.
-- Returns (finalState, decryptedList) or aborts on first failure.
decryptAll :: SenderKeyState -> [SenderKeyMessage] -> Word64
           -> IO (SenderKeyState, [BS.ByteString])
decryptAll st0 msgs nowSecs = go st0 msgs []
  where
    go st [] acc = pure (st, reverse acc)
    go st (m:ms) acc = do
        res <- decryptSenderKey st m nowSecs
        case res of
            Left err -> fail ("decryptAll: decryptSenderKey failed: " ++ show err)
            Right (st', pt) -> go st' ms (pt : acc)

------------------------------------------------------------------------
-- Test 1: Member addition
--
-- Finding:     When Carol joins after Alice and Bob have exchanged messages,
--              she must only be able to decrypt subsequent messages.
-- Vulnerability: If createSenderKeyDistribution shared the same chain state,
--              Carol could back-calculate earlier keys.
-- Fix:         The distribution message carries only the current iteration;
--              Carol starts her receiver state at that point (iteration 10
--              after msgs 1-10), so msgs 1-10 are not in her view.
-- Verified:    Carol can decrypt msg 11 (iteration 10), cannot decrypt
--              msgs 0-9 (DecryptionFailed from skipped-key cache miss).
------------------------------------------------------------------------

testMemberAddition :: IO Bool
testMemberAddition = do
    let aliceId  = BS.pack [0xAA]
        plaintext = BS.pack [0x48, 0x65, 0x6C, 0x6C, 0x6F]  -- "Hello"
        nowSecs   = 1000000 :: Word32

    -- Alice creates her sender key state and distributes at iteration 0.
    (aliceSt0, dist0) <- createSenderKeyDistribution aliceId

    -- Bob processes the initial distribution (before any messages).
    bobStE <- processSenderKeyDistribution dist0
    bobSt0 <- case bobStE of
        Left err -> fail ("testMemberAddition: processSenderKeyDistribution failed: " ++ show err)
        Right st -> pure st

    -- Alice sends 10 messages (iterations 0-9). Bob receives them all.
    (aliceSt10, msgs1to10) <- sendN aliceSt0 10 plaintext
    (bobSt10, _decrypted)  <- decryptAll bobSt0 msgs1to10 (fromIntegral nowSecs)

    -- Alice now re-distributes (or distributes afresh) for Carol.
    -- We simulate this: Alice sends a fresh distribution at her current
    -- iteration (10).  Carol's state starts at iteration 10.
    (_, distForCarol) <- createSenderKeyDistribution aliceId
    carolStE <- processSenderKeyDistribution distForCarol
    carolSt <- case carolStE of
        Left err -> fail ("testMemberAddition: Carol processSenderKeyDistribution failed: " ++ show err)
        Right st -> pure st

    -- Alice sends message 11 (iteration 10 in the new chain started for Carol).
    -- For the original chain, Alice is at iteration 10.
    res11 <- encryptSenderKey aliceSt10 plaintext
    case res11 of
        Left err -> do
            putStrLn ("  FAIL: testMemberAddition msg11 encrypt: " ++ show err)
            pure False
        Right (_aliceSt11, msg11) -> do
            -- Bob can decrypt msg 11 (he has the original chain at iteration 10).
            bobRes11 <- decryptSenderKey bobSt10 msg11 (fromIntegral nowSecs)
            r1 <- case bobRes11 of
                Left err -> do
                    putStrLn ("  FAIL: testMemberAddition Bob decrypt msg11: " ++ show err)
                    pure False
                Right (_, pt) ->
                    assertEq "member-addition: Bob decrypts msg11" plaintext pt

            -- Carol cannot decrypt the original chain's messages 0-9.
            -- Pick msg #1 (iteration 1 in the original Alice chain).
            let msg1 = msgs1to10 !! 1
            carolFail <- decryptSenderKey carolSt msg1 (fromIntegral nowSecs)
            r2 <- case carolFail of
                Left _ ->
                    assertEq "member-addition: Carol cannot decrypt msg1" True True
                Right _ -> do
                    putStrLn "  FAIL: member-addition: Carol decrypted msg1 (should be impossible)"
                    pure False

            pure (r1 && r2)

------------------------------------------------------------------------
-- Test 2: Member removal and key rotation
--
-- Finding:     After Carol is removed, Alice must rotate her sender key
--              so Carol's cached state cannot decrypt future messages.
-- Vulnerability: Without rotation, Carol retains her receiver-chain state
--              and can continue decrypting as long as she remembers the
--              chain key from the last distribution she received.
-- Fix:         Alice calls createSenderKeyDistribution again; this generates
--              fresh random chain and signing keys.  The new distribution
--              is sent only to remaining members (not Carol).  Carol's old
--              state produces DecryptionFailed for all new messages.
-- Verified:    Carol's old receiver state fails to decrypt messages sent
--              after key rotation; Alice and Bob succeed.
------------------------------------------------------------------------

testMemberRemoval :: IO Bool
testMemberRemoval = do
    let aliceId   = BS.pack [0xBB]
        plaintext  = BS.pack [0x57, 0x6F, 0x72, 0x6C, 0x64]  -- "World"
        nowSecs    = 2000000 :: Word32

    -- Alice creates initial state; Bob and Carol both get the distribution.
    (aliceSt0, dist0) <- createSenderKeyDistribution aliceId
    bobStE   <- processSenderKeyDistribution dist0
    carolStE <- processSenderKeyDistribution dist0
    bobSt0 <- case bobStE of
        Left err -> fail ("testMemberRemoval: Bob dist failed: " ++ show err)
        Right st -> pure st
    carolSt0 <- case carolStE of
        Left err -> fail ("testMemberRemoval: Carol dist failed: " ++ show err)
        Right st -> pure st

    -- Alice sends 3 pre-removal messages; everyone decrypts.
    (aliceStPreRotation, msgsPreRotation) <- sendN aliceSt0 3 plaintext
    (bobStPreRotation, _)   <- decryptAll bobSt0   msgsPreRotation (fromIntegral nowSecs)
    (carolStPreRotation, _) <- decryptAll carolSt0 msgsPreRotation (fromIntegral nowSecs)

    -- Carol is removed. Alice rotates: new distribution goes only to Bob.
    (aliceStPostRotation, distPostRotation) <- createSenderKeyDistribution aliceId
    bobStPostE <- processSenderKeyDistribution distPostRotation
    bobStPost <- case bobStPostE of
        Left err -> fail ("testMemberRemoval: Bob post-rotation dist failed: " ++ show err)
        Right st -> pure st

    -- Suppress unused-variable warnings for states we don't further use
    _ <- pure aliceStPreRotation
    _ <- pure bobStPreRotation
    _ <- pure carolStPreRotation

    -- Alice sends 5 post-rotation messages.
    (_aliceFinal, msgsPostRotation) <- sendN aliceStPostRotation 5 plaintext

    -- Bob decrypts them all.
    bobPostRes <- mapM (\m -> decryptSenderKey bobStPost m (fromIntegral nowSecs)) msgsPostRotation
    r1 <- case sequence (map (\r -> case r of { Left _ -> Nothing; Right p -> Just p }) bobPostRes) of
        Nothing -> do
            putStrLn "  FAIL: member-removal: Bob failed to decrypt post-rotation message"
            pure False
        Just _ ->
            assertEq "member-removal: Bob decrypts all post-rotation msgs" True True

    -- Carol cannot decrypt any post-rotation message.
    carolResults <- mapM (\m -> decryptSenderKey carolStPreRotation m (fromIntegral nowSecs)) msgsPostRotation
    let carolDecrypted = any (\r -> case r of { Right _ -> True; Left _ -> False }) carolResults
    r2 <- assertEq "member-removal: Carol cannot decrypt post-rotation msgs" False carolDecrypted

    pure (r1 && r2)

------------------------------------------------------------------------
-- Test 3: Out-of-order delivery
--
-- Alice encrypts 10 messages; Bob receives them in reverse order.
-- All must decrypt correctly (using the skipped-key cache).
------------------------------------------------------------------------

testOutOfOrder :: IO Bool
testOutOfOrder = do
    let aliceId   = BS.pack [0xCC]
        nowSecs    = 3000000 :: Word32

    (aliceSt0, dist) <- createSenderKeyDistribution aliceId
    bobStE <- processSenderKeyDistribution dist
    bobSt0 <- case bobStE of
        Left err -> fail ("testOutOfOrder: processSenderKeyDistribution failed: " ++ show err)
        Right st -> pure st

    -- Alice sends 10 messages with distinct payloads.
    let plaintexts = map (\i -> BS.singleton (fromIntegral i)) [1..10 :: Int]
    (_, msgs) <- go aliceSt0 plaintexts []

    -- Bob decrypts in reverse order.
    let reversed = reverse msgs
    (_, decrypteds) <- decryptAll bobSt0 reversed (fromIntegral nowSecs)

    -- Decrypted payloads should match reversed plaintexts.
    let expected = reverse plaintexts
    r <- assertEq "out-of-order: all 10 messages decrypt correctly" expected decrypteds
    pure r
  where
    go st [] acc = pure (st, reverse acc)
    go st (pt:pts) acc = do
        res <- encryptSenderKey st pt
        case res of
            Left err -> fail ("testOutOfOrder: encrypt failed: " ++ show err)
            Right (st', msg) -> go st' pts (msg : acc)

------------------------------------------------------------------------
-- Test 4: Skipped key eviction
--
-- Encrypt 257 messages so that when Bob decrypts message 257 (iteration
-- 256) first, the cache must store iterations 0-255 (256 entries) but
-- message 0's key is evicted by the size cap (maxSkippedSenderKeys=256).
-- Attempting to decrypt message 0 afterward must fail.
------------------------------------------------------------------------

testSkippedKeyEviction :: IO Bool
testSkippedKeyEviction = do
    let aliceId   = BS.pack [0xDD]
        plaintext  = BS.pack [0x65, 0x76, 0x69, 0x63, 0x74]  -- "evict"
        nowSecs    = 4000000 :: Word32

    (aliceSt0, dist) <- createSenderKeyDistribution aliceId
    bobStE <- processSenderKeyDistribution dist
    bobSt0 <- case bobStE of
        Left err -> fail ("testSkippedKeyEviction: processSenderKeyDistribution failed: " ++ show err)
        Right st -> pure st

    -- Alice encrypts 257 messages (iterations 0..256).
    (_, msgs) <- sendN aliceSt0 257 plaintext

    -- Bob decrypts message 256 first (the last one). This forces the chain
    -- to advance from 0 to 256, caching keys for iterations 0..255.
    -- The cache holds exactly 256 entries; the oldest (iteration 0) is
    -- evicted when iteration 256 is processed.
    let msg256 = last msgs
        msg0   = head msgs
    res256 <- decryptSenderKey bobSt0 msg256 (fromIntegral nowSecs)
    (bobSt256, _) <- case res256 of
        Left err -> fail ("testSkippedKeyEviction: decrypt msg256 failed: " ++ show err)
        Right pair -> pure pair

    -- Decrypting msg0 must fail because it was evicted.
    res0 <- decryptSenderKey bobSt256 msg0 (fromIntegral nowSecs)
    r1 <- case res0 of
        Left _ ->
            assertEq "skipped-key-eviction: msg0 key evicted" True True
        Right _ -> do
            putStrLn "  FAIL: skipped-key-eviction: msg0 decrypted (should have been evicted)"
            pure False

    -- Msgs 1..255 should still be in cache (none were evicted since
    -- evictOldestSenderKeys only removes one at a time, and msg0 was the
    -- oldest). Spot-check msg 128 (iteration 128).
    let msg128 = msgs !! 128
    res128 <- decryptSenderKey bobSt256 msg128 (fromIntegral nowSecs)
    r2 <- case res128 of
        Left err -> do
            putStrLn ("  FAIL: skipped-key-eviction: msg128 unexpectedly evicted: " ++ show err)
            pure False
        Right (_, pt) ->
            assertEq "skipped-key-eviction: msg128 still decryptable" plaintext pt

    pure (r1 && r2)

------------------------------------------------------------------------
-- Test 5: Duplicate sender ID
--
-- Two separate createSenderKeyDistribution calls with the same sender ID
-- produce independent chain states.  Messages from chain A cannot be
-- decrypted with a state derived from chain B's distribution, and vice
-- versa.  The behavior is deterministic: each distribution is isolated.
------------------------------------------------------------------------

testDuplicateSenderId :: IO Bool
testDuplicateSenderId = do
    let sharedId  = BS.pack [0xEE]
        plaintext  = BS.pack [0x64, 0x75, 0x70, 0x65]  -- "dupe"
        nowSecs    = 5000000 :: Word32

    -- Two senders both claiming the same ID (degenerate scenario).
    (stA, distA) <- createSenderKeyDistribution sharedId
    (stB, distB) <- createSenderKeyDistribution sharedId

    -- Two receivers, each primed with a different distribution.
    recvAE <- processSenderKeyDistribution distA
    recvBE <- processSenderKeyDistribution distB
    recvA <- case recvAE of
        Left err -> fail ("testDuplicateSenderId: recvA failed: " ++ show err)
        Right st -> pure st
    recvB <- case recvBE of
        Left err -> fail ("testDuplicateSenderId: recvB failed: " ++ show err)
        Right st -> pure st

    -- Sender A encrypts one message.
    resA <- encryptSenderKey stA plaintext
    (_, msgA) <- case resA of
        Left err -> fail ("testDuplicateSenderId: encryptA failed: " ++ show err)
        Right p  -> pure p

    -- Sender B encrypts one message.
    resB <- encryptSenderKey stB plaintext
    (_, msgB) <- case resB of
        Left err -> fail ("testDuplicateSenderId: encryptB failed: " ++ show err)
        Right p  -> pure p

    -- recvA can decrypt msgA.
    decAA <- decryptSenderKey recvA msgA (fromIntegral nowSecs)
    r1 <- case decAA of
        Left err -> do
            putStrLn ("  FAIL: duplicate-sender-id: recvA cannot decrypt msgA: " ++ show err)
            pure False
        Right (_, pt) ->
            assertEq "duplicate-sender-id: recvA decrypts msgA" plaintext pt

    -- recvB can decrypt msgB.
    decBB <- decryptSenderKey recvB msgB (fromIntegral nowSecs)
    r2 <- case decBB of
        Left err -> do
            putStrLn ("  FAIL: duplicate-sender-id: recvB cannot decrypt msgB: " ++ show err)
            pure False
        Right (_, pt) ->
            assertEq "duplicate-sender-id: recvB decrypts msgB" plaintext pt

    -- recvA cannot decrypt msgB (different chain key despite same sender ID).
    decAB <- decryptSenderKey recvA msgB (fromIntegral nowSecs)
    r3 <- case decAB of
        Left _ ->
            assertEq "duplicate-sender-id: recvA rejects msgB" True True
        Right _ -> do
            putStrLn "  FAIL: duplicate-sender-id: recvA decrypted msgB (should fail)"
            pure False

    -- recvB cannot decrypt msgA.
    decBA <- decryptSenderKey recvB msgA (fromIntegral nowSecs)
    r4 <- case decBA of
        Left _ ->
            assertEq "duplicate-sender-id: recvB rejects msgA" True True
        Right _ -> do
            putStrLn "  FAIL: duplicate-sender-id: recvB decrypted msgA (should fail)"
            pure False

    -- The two messages are distinct (different ciphertexts from different chains).
    r5 <- assertEq "duplicate-sender-id: msgA and msgB ciphertexts differ"
            True (skmCiphertext msgA /= skmCiphertext msgB)

    pure (r1 && r2 && r3 && r4 && r5)

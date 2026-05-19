{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: Apache-2.0
-- | Protocol-level self-consistency trace tests.
--
-- These verify that UmbraVOX's own X3DH and Double Ratchet implementations
-- are internally consistent by running multi-step protocol exchanges and
-- checking that both sides agree on state at each step.
--
-- These are NOT differential tests against libsignal (the oracle VM is not
-- yet built).  They exercise the protocol state machine end-to-end using
-- only our own code.
module Test.Crypto.Differential.ProtocolSelfConsistency
    ( protocolSelfConsistencyTests
    ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word8)

import Test.Util
import UmbraVox.Crypto.Signal.X3DH
    ( KeyPair(..), IdentityKey(..), PreKeyBundle(..), X3DHResult(..)
    , generateKeyPair, generateIdentityKey, signPreKey
    , x3dhInitiate, x3dhRespond
    )
import UmbraVox.Crypto.Signal.DoubleRatchet
    ( RatchetState(..), RatchetHeader(..)
    , ratchetInitAlice, ratchetInitBob, ratchetEncrypt, ratchetDecrypt
    )

-- | Run all protocol self-consistency tests.
protocolSelfConsistencyTests :: IO Bool
protocolSelfConsistencyTests = do
    putStrLn "[ProtocolSelfConsistency] Running protocol trace tests..."
    results <- sequence
        [ testX3DHFullExchange
        , testX3DHFullExchangeWithOPK
        , testDoubleRatchetMultiMessage
        , testDoubleRatchetBidirectional
        , testDoubleRatchetOutOfOrder
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[ProtocolSelfConsistency] " ++ show passed ++ "/"
            ++ show total ++ " suites passed."
    return (and results)

------------------------------------------------------------------------
-- Deterministic key material helpers
------------------------------------------------------------------------

-- | Generate deterministic key material from a seed byte.
-- Each participant gets unique secrets derived from different seed offsets.
mkIdentity :: Word8 -> IdentityKey
mkIdentity seed = generateIdentityKey
    (BS.pack [seed, seed+1 .. seed+31])
    (BS.pack [seed+32, seed+33 .. seed+63])

------------------------------------------------------------------------
-- Test 1: X3DH full exchange (without OPK)
--
-- Alice initiates with Bob's bundle (no OPK), Bob responds.
-- Both must derive the same 32-byte shared secret.
------------------------------------------------------------------------

testX3DHFullExchange :: IO Bool
testX3DHFullExchange = do
    putStrLn "  [X3DH-SelfConsistency] Full exchange without OPK"
    let aliceIK = mkIdentity 0x01
        bobIK   = mkIdentity 0x41
        spkSecret = BS.pack [0x81 .. 0xA0]
        spk     = generateKeyPair spkSecret
        spkSig  = signPreKey bobIK (kpPublic spk)
        ekSecret = BS.pack [0xA1 .. 0xC0]
        bundle  = PreKeyBundle
            { pkbIdentityKey     = ikX25519Public bobIK
            , pkbSignedPreKey    = kpPublic spk
            , pkbSPKSignature    = spkSig
            , pkbIdentityEd25519 = ikEd25519Public bobIK
            , pkbOneTimePreKey   = Nothing
            }
    case x3dhInitiate aliceIK bundle ekSecret of
        Nothing -> do
            putStrLn "    FAIL: x3dhInitiate returned Nothing"
            return False
        Just result -> do
            case x3dhRespond bobIK spkSecret Nothing
                    (ikX25519Public aliceIK) (x3dhEphemeralKey result) of
                Nothing -> do
                    putStrLn "    FAIL: x3dhRespond returned Nothing"
                    return False
                Just bobSecret -> do
                    r1 <- assertEq "X3DH no-OPK: secrets match"
                            (x3dhSharedSecret result) bobSecret
                    r2 <- assertEq "X3DH no-OPK: secret is 32 bytes"
                            32 (BS.length (x3dhSharedSecret result))
                    r3 <- assertEq "X3DH no-OPK: usedOPK is Nothing"
                            Nothing (x3dhUsedOPK result)
                    return (r1 && r2 && r3)

------------------------------------------------------------------------
-- Test 1b: X3DH full exchange (with OPK)
------------------------------------------------------------------------

testX3DHFullExchangeWithOPK :: IO Bool
testX3DHFullExchangeWithOPK = do
    putStrLn "  [X3DH-SelfConsistency] Full exchange with OPK"
    let aliceIK = mkIdentity 0x01
        bobIK   = mkIdentity 0x41
        spkSecret = BS.pack [0x81 .. 0xA0]
        spk     = generateKeyPair spkSecret
        spkSig  = signPreKey bobIK (kpPublic spk)
        opkSecret = BS.pack [0xC1 .. 0xE0]
        opk     = generateKeyPair opkSecret
        ekSecret = BS.pack [0xA1 .. 0xC0]
        bundle  = PreKeyBundle
            { pkbIdentityKey     = ikX25519Public bobIK
            , pkbSignedPreKey    = kpPublic spk
            , pkbSPKSignature    = spkSig
            , pkbIdentityEd25519 = ikEd25519Public bobIK
            , pkbOneTimePreKey   = Just (kpPublic opk)
            }
    case x3dhInitiate aliceIK bundle ekSecret of
        Nothing -> do
            putStrLn "    FAIL: x3dhInitiate returned Nothing"
            return False
        Just result -> do
            case x3dhRespond bobIK spkSecret (Just opkSecret)
                    (ikX25519Public aliceIK) (x3dhEphemeralKey result) of
                Nothing -> do
                    putStrLn "    FAIL: x3dhRespond returned Nothing"
                    return False
                Just bobSecret -> do
                    r1 <- assertEq "X3DH with-OPK: secrets match"
                            (x3dhSharedSecret result) bobSecret
                    r2 <- assertEq "X3DH with-OPK: secret is 32 bytes"
                            32 (BS.length (x3dhSharedSecret result))
                    r3 <- assertEq "X3DH with-OPK: usedOPK matches"
                            (Just (kpPublic opk)) (x3dhUsedOPK result)
                    return (r1 && r2 && r3)

------------------------------------------------------------------------
-- Test 2: Double Ratchet multi-message (5 messages, same direction)
--
-- After X3DH, Alice sends 5 messages to Bob.  Bob decrypts all 5 in
-- order.  Verify each plaintext matches.
------------------------------------------------------------------------

testDoubleRatchetMultiMessage :: IO Bool
testDoubleRatchetMultiMessage = do
    putStrLn "  [Ratchet-SelfConsistency] 5 messages A->B (same direction)"
    case setupRatchetSession of
        Nothing -> do
            putStrLn "    FAIL: ratchet session setup failed"
            return False
        Just (alice0, bob0) -> do
            let messages =
                    [ strToBS "Message one"
                    , strToBS "Message two"
                    , strToBS "Message three"
                    , strToBS "Message four"
                    , strToBS "Message five"
                    ]
            go alice0 bob0 messages 1
  where
    go :: RatchetState -> RatchetState -> [ByteString] -> Int -> IO Bool
    go _ _ [] _ = return True
    go alice bob (msg:msgs) n = do
        encResult <- ratchetEncrypt alice msg
        case encResult of
            Left err -> do
                putStrLn $ "    FAIL: encrypt msg " ++ show n ++ ": " ++ show err
                return False
            Right (alice', hdr, ct, tag) -> do
                decResult <- ratchetDecrypt bob hdr ct tag
                case decResult of
                    Left err -> do
                        putStrLn $ "    FAIL: decrypt msg " ++ show n ++ ": " ++ show err
                        return False
                    Right Nothing -> do
                        putStrLn $ "    FAIL: decrypt msg " ++ show n ++ " auth failure"
                        return False
                    Right (Just (bob', pt)) -> do
                        ok <- assertEq ("multi-msg A->B msg " ++ show n) msg pt
                        if ok
                            then go alice' bob' msgs (n + 1)
                            else return False

------------------------------------------------------------------------
-- Test 3: Double Ratchet bidirectional
--
-- Alice sends 3, Bob sends 2, Alice sends 2 more.
-- Each direction change triggers a DH ratchet step.
-- All 7 messages must decrypt correctly.
------------------------------------------------------------------------

testDoubleRatchetBidirectional :: IO Bool
testDoubleRatchetBidirectional = do
    putStrLn "  [Ratchet-SelfConsistency] Bidirectional: A(3) B(2) A(2)"
    case setupRatchetSession of
        Nothing -> do
            putStrLn "    FAIL: ratchet session setup failed"
            return False
        Just (alice0, bob0) -> do
            -- Phase 1: Alice sends 3 messages to Bob
            let msgsA1 = [ strToBS "Alice-1", strToBS "Alice-2", strToBS "Alice-3" ]
            r1 <- sendBatch alice0 bob0 msgsA1 "A->B phase1"
            case r1 of
                Nothing -> return False
                Just (alice1, bob1) -> do
                    -- Phase 2: Bob sends 2 messages to Alice
                    let msgsB = [ strToBS "Bob-1", strToBS "Bob-2" ]
                    r2 <- sendBatch bob1 alice1 msgsB "B->A phase2"
                    case r2 of
                        Nothing -> return False
                        Just (bob2, alice2) -> do
                            -- Phase 3: Alice sends 2 more messages to Bob
                            let msgsA2 = [ strToBS "Alice-4", strToBS "Alice-5" ]
                            r3 <- sendBatch alice2 bob2 msgsA2 "A->B phase3"
                            case r3 of
                                Nothing -> return False
                                Just _  -> do
                                    putStrLn "    PASS: bidirectional 7-message exchange"
                                    return True

------------------------------------------------------------------------
-- Test 4: Double Ratchet out-of-order delivery
--
-- Alice sends msg1, msg2, msg3.  Bob decrypts msg2 first (skipping
-- msg1), then msg1, then msg3.  All three must succeed via the
-- skipped-key mechanism.
------------------------------------------------------------------------

testDoubleRatchetOutOfOrder :: IO Bool
testDoubleRatchetOutOfOrder = do
    putStrLn "  [Ratchet-SelfConsistency] Out-of-order: send 3, deliver 2-1-3"
    case setupRatchetSession of
        Nothing -> do
            putStrLn "    FAIL: ratchet session setup failed"
            return False
        Just (alice0, bob0) -> do
            let msg1 = strToBS "First message"
                msg2 = strToBS "Second message"
                msg3 = strToBS "Third message"
            -- Encrypt all 3 messages
            e1 <- ratchetEncrypt alice0 msg1
            case e1 of
                Left err -> do
                    putStrLn $ "    FAIL: encrypt msg1: " ++ show err
                    return False
                Right (alice1, h1, ct1, tag1) -> do
                    e2 <- ratchetEncrypt alice1 msg2
                    case e2 of
                        Left err -> do
                            putStrLn $ "    FAIL: encrypt msg2: " ++ show err
                            return False
                        Right (alice2, h2, ct2, tag2) -> do
                            e3 <- ratchetEncrypt alice2 msg3
                            case e3 of
                                Left err -> do
                                    putStrLn $ "    FAIL: encrypt msg3: " ++ show err
                                    return False
                                Right (_alice3, h3, ct3, tag3) -> do
                                    -- Decrypt msg2 first (skip msg1)
                                    d2 <- ratchetDecrypt bob0 h2 ct2 tag2
                                    case d2 of
                                        Left err -> do
                                            putStrLn $ "    FAIL: OOO decrypt msg2: " ++ show err
                                            return False
                                        Right Nothing -> do
                                            putStrLn "    FAIL: OOO decrypt msg2 auth failure"
                                            return False
                                        Right (Just (bob1, pt2)) -> do
                                            ok2 <- assertEq "OOO msg2 (delivered first)" msg2 pt2
                                            -- Decrypt msg1 (from skipped keys)
                                            d1 <- ratchetDecrypt bob1 h1 ct1 tag1
                                            case d1 of
                                                Left err -> do
                                                    putStrLn $ "    FAIL: OOO decrypt msg1: " ++ show err
                                                    return False
                                                Right Nothing -> do
                                                    putStrLn "    FAIL: OOO decrypt msg1 auth failure"
                                                    return False
                                                Right (Just (bob2, pt1)) -> do
                                                    ok1 <- assertEq "OOO msg1 (delivered second)" msg1 pt1
                                                    -- Decrypt msg3 (in order now)
                                                    d3 <- ratchetDecrypt bob2 h3 ct3 tag3
                                                    case d3 of
                                                        Left err -> do
                                                            putStrLn $ "    FAIL: OOO decrypt msg3: " ++ show err
                                                            return False
                                                        Right Nothing -> do
                                                            putStrLn "    FAIL: OOO decrypt msg3 auth failure"
                                                            return False
                                                        Right (Just (_, pt3)) -> do
                                                            ok3 <- assertEq "OOO msg3 (delivered third)" msg3 pt3
                                                            return (ok2 && ok1 && ok3)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Set up a ratchet session via X3DH with deterministic keys.
-- Returns (aliceState, bobState) or Nothing on failure.
setupRatchetSession :: Maybe (RatchetState, RatchetState)
setupRatchetSession =
    let aliceIK   = mkIdentity 0x01
        bobIK     = mkIdentity 0x41
        spkSecret = BS.pack [0x81 .. 0xA0]
        spk       = generateKeyPair spkSecret
        spkSig    = signPreKey bobIK (kpPublic spk)
        ekSecret  = BS.pack [0xA1 .. 0xC0]
        bundle    = PreKeyBundle
            { pkbIdentityKey     = ikX25519Public bobIK
            , pkbSignedPreKey    = kpPublic spk
            , pkbSPKSignature    = spkSig
            , pkbIdentityEd25519 = ikEd25519Public bobIK
            , pkbOneTimePreKey   = Nothing
            }
        aliceDHSecret = BS.pack [0xD1 .. 0xF0]
    in do
        x3dhResult <- x3dhInitiate aliceIK bundle ekSecret
        let sharedSecret = x3dhSharedSecret x3dhResult
        aliceState <- ratchetInitAlice sharedSecret (kpPublic spk) aliceDHSecret
        let bobState = ratchetInitBob sharedSecret spkSecret
        Just (aliceState, bobState)

-- | Send a batch of messages from sender to receiver, verifying each.
-- Returns updated states or Nothing on failure.
sendBatch :: RatchetState -> RatchetState -> [ByteString] -> String
          -> IO (Maybe (RatchetState, RatchetState))
sendBatch sender receiver [] _ = return (Just (sender, receiver))
sendBatch sender receiver (msg:msgs) label = do
    encResult <- ratchetEncrypt sender msg
    case encResult of
        Left err -> do
            putStrLn $ "    FAIL: " ++ label ++ " encrypt: " ++ show err
            return Nothing
        Right (sender', hdr, ct, tag) -> do
            decResult <- ratchetDecrypt receiver hdr ct tag
            case decResult of
                Left err -> do
                    putStrLn $ "    FAIL: " ++ label ++ " decrypt: " ++ show err
                    return Nothing
                Right Nothing -> do
                    putStrLn $ "    FAIL: " ++ label ++ " auth failure"
                    return Nothing
                Right (Just (receiver', pt))
                    | pt == msg -> sendBatch sender' receiver' msgs label
                    | otherwise -> do
                        putStrLn $ "    FAIL: " ++ label ++ " plaintext mismatch"
                        return Nothing

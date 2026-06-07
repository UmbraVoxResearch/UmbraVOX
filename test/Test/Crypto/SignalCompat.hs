-- SPDX-License-Identifier: Apache-2.0
-- | Signal-compatible crypto layer round-trip tests (M19.1.3, M19.1.4).
--
-- Tests:
--   1. Protobuf round-trip: SignalMessage, PreKeySignalMessage, SignalEnvelope
--   2. Varint round-trip: edge values (0, 1, 127, 128, 255, 256, 65535, 0xFFFFFFFF, maxBound)
--   3. Signal ratchet round-trip: Alice encrypts, Bob decrypts
--   4. Signal X3DH derivation: deterministic output from known inputs
--   5. Signal KDF: signalKdfRK and signalKdfCK determinism
--   6. Multi-message ratchet: Alice sends 3, Bob decrypts all 3 in order
--   7. Cross-ratchet: Alice sends, Bob replies, Alice decrypts (DH ratchet step)
--   8. Known-answer test vectors: KDF, X3DH, ratchet init vs hardcoded expected outputs
module Test.Crypto.SignalCompat
    ( signalCompatTests
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word64)

import Test.Util

import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.Signal.Compat
    ( SignalRatchetState(..)
    , SignalRatchetHeader(..)
    , signalKdfRK
    , signalKdfCK
    , signalDeriveSecret
    , signalRatchetInitAlice
    , signalRatchetInitBob
    , signalRatchetEncrypt
    , signalRatchetDecrypt
    )
import UmbraVox.Protocol.SignalWire
    ( SignalMessage(..)
    , PreKeySignalMessage(..)
    , SignalEnvelope(..)
    , EnvelopeType(..)
    , encodeSignalMessage
    , decodeSignalMessage
    , encodePreKeySignalMessage
    , decodePreKeySignalMessage
    , encodeSignalEnvelope
    , decodeSignalEnvelope
    , encodeVarint
    , decodeVarint
    )

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

-- | Run all Signal-compatible crypto layer tests.
-- Returns True if all pass, False if any fail.
signalCompatTests :: IO Bool
signalCompatTests = do
    putStrLn "[SignalCompat] Running protobuf round-trip tests..."
    protoResults <- sequence
        [ testSignalMessageRoundTrip
        , testPreKeySignalMessageRoundTrip
        , testSignalEnvelopeRoundTrip
        ]
    putStrLn "[SignalCompat] Running varint round-trip tests..."
    varintResults <- sequence
        [ testVarintRoundTrip
        ]
    putStrLn "[SignalCompat] Running Signal KDF tests..."
    kdfResults <- sequence
        [ testSignalKdfRKDeterminism
        , testSignalKdfCKDeterminism
        ]
    putStrLn "[SignalCompat] Running Signal X3DH tests..."
    x3dhResults <- sequence
        [ testSignalDeriveSecretDeterminism
        , testSignalDeriveSecretWithOPK
        ]
    putStrLn "[SignalCompat] Running Signal ratchet round-trip tests..."
    ratchetResults <- sequence
        [ testRatchetSingleMessage
        , testRatchetMultiMessage
        , testRatchetCrossRatchet
        ]
    putStrLn "[SignalCompat] Running known-answer test vector tests..."
    katResults <- sequence
        [ testKdfRKKnownVector1
        , testKdfRKKnownVector2
        , testKdfCKKnownVector1
        , testKdfCKKnownVector2
        , testX3DHKnownVectorNoOPK
        , testX3DHKnownVectorWithOPK
        , testRatchetInitAliceKnownVector
        ]
    let results = protoResults ++ varintResults ++ kdfResults
                  ++ x3dhResults ++ ratchetResults ++ katResults
        passed  = length (filter id results)
        total   = length results
    putStrLn $ "[SignalCompat] " ++ show passed ++ "/" ++ show total
               ++ " suites passed."
    pure (and results)

------------------------------------------------------------------------
-- Test key material (deterministic, same style as DoubleRatchet tests)
------------------------------------------------------------------------

-- | 32-byte deterministic "X3DH shared secret" for ratchet init.
sharedSecret :: ByteString
sharedSecret = BS.pack [0x01 .. 0x20]

-- | Bob's signed prekey secret (32 bytes).
bobSPKSecret :: ByteString
bobSPKSecret = BS.pack [0x21 .. 0x40]

-- | Alice's first ephemeral DH secret (32 bytes).
aliceDHSecret :: ByteString
aliceDHSecret = BS.pack [0x41 .. 0x60]

-- | Bob's signed prekey public (derived).
bobSPKPublic :: ByteString
bobSPKPublic = case x25519 bobSPKSecret x25519Basepoint of
    Just pub -> pub
    Nothing  -> error "bobSPKPublic: impossible all-zero from known test secret"

------------------------------------------------------------------------
-- 1. Protobuf round-trip: SignalMessage
------------------------------------------------------------------------

testSignalMessageRoundTrip :: IO Bool
testSignalMessageRoundTrip = do
    let msg = SignalMessage
            { smRatchetKey      = BS.pack (0x05 : [0xAA .. 0xCA])  -- 33 bytes
            , smCounter         = 42
            , smPreviousCounter = 7
            , smCiphertext      = strToBS "encrypted-payload-here"
            }
        encoded = encodeSignalMessage msg
        decoded = decodeSignalMessage encoded
    assertEq "SignalMessage protobuf round-trip" (Just msg) decoded

------------------------------------------------------------------------
-- 1b. Protobuf round-trip: PreKeySignalMessage
------------------------------------------------------------------------

testPreKeySignalMessageRoundTrip :: IO Bool
testPreKeySignalMessageRoundTrip = do
    let msg = PreKeySignalMessage
            { pksmRegistrationId = 12345
            , pksmPreKeyId       = 67
            , pksmSignedPreKeyId = 89
            , pksmBaseKey        = BS.pack (0x05 : [0xBB .. 0xDB])  -- 33 bytes
            , pksmIdentityKey    = BS.pack (0x05 : [0xCC .. 0xEC])  -- 33 bytes
            , pksmMessage        = strToBS "inner-signal-message-bytes"
            }
        encoded = encodePreKeySignalMessage msg
        decoded = decodePreKeySignalMessage encoded
    assertEq "PreKeySignalMessage protobuf round-trip" (Just msg) decoded

------------------------------------------------------------------------
-- 1c. Protobuf round-trip: SignalEnvelope
------------------------------------------------------------------------

testSignalEnvelopeRoundTrip :: IO Bool
testSignalEnvelopeRoundTrip = do
    let allTypes = [ EnvelopeUnknown
                   , EnvelopeCiphertext
                   , EnvelopeKeyExchange
                   , EnvelopePreKeyBundle
                   , EnvelopeReceipt
                   , EnvelopeUnidentifiedSender
                   ]
    results <- mapM testOneEnvelope allTypes
    pure (and results)
  where
    testOneEnvelope envType = do
        let env = SignalEnvelope
                { seType         = envType
                , seSourceDevice = 1
                , seTimestamp    = 1700000000000
                , seContent      = strToBS "sealed-content"
                }
            encoded = encodeSignalEnvelope env
            decoded = decodeSignalEnvelope encoded
        assertEq ("SignalEnvelope round-trip (" ++ show envType ++ ")")
                 (Just env) decoded

------------------------------------------------------------------------
-- 2. Varint round-trip for edge values
------------------------------------------------------------------------

testVarintRoundTrip :: IO Bool
testVarintRoundTrip = do
    let values :: [Word64]
        values = [ 0, 1, 127, 128, 255, 256, 65535
                 , 0xFFFFFFFF, maxBound
                 ]
    results <- mapM testOneVarint values
    pure (and results)
  where
    testOneVarint v = do
        let encoded = encodeVarint v
            decoded = decodeVarint encoded
        case decoded of
            Nothing -> do
                putStrLn $ "  FAIL: varint round-trip " ++ show v
                           ++ " (decode returned Nothing)"
                pure False
            Just (v', remainder) -> do
                ok1 <- assertEq ("varint value " ++ show v) v v'
                ok2 <- assertEq ("varint remainder " ++ show v)
                                BS.empty remainder
                pure (ok1 && ok2)

------------------------------------------------------------------------
-- 3. Signal ratchet round-trip: Alice encrypts, Bob decrypts
------------------------------------------------------------------------

testRatchetSingleMessage :: IO Bool
testRatchetSingleMessage = do
    mAlice <- signalRatchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
    case mAlice of
        Nothing -> do
            putStrLn "  FAIL: signalRatchetInitAlice returned Nothing"
            pure False
        Just alice -> do
            bob <- signalRatchetInitBob sharedSecret bobSPKSecret
            let msg = strToBS "Hello from Signal-compat Alice!"
            encResult <- signalRatchetEncrypt alice msg
            case encResult of
                Left err -> do
                    putStrLn $ "  FAIL: signalRatchetEncrypt: " ++ err
                    pure False
                Right (_alice', hdr, ct, tag) -> do
                    decResult <- signalRatchetDecrypt bob hdr ct tag
                    case decResult of
                        Left err -> do
                            putStrLn $ "  FAIL: signalRatchetDecrypt: " ++ err
                            pure False
                        Right Nothing -> do
                            putStrLn "  FAIL: Signal ratchet single message (decryption returned Nothing)"
                            pure False
                        Right (Just (_, pt)) ->
                            assertEq "Signal ratchet single message A->B" msg pt

------------------------------------------------------------------------
-- 4. Signal X3DH derivation: deterministic output
------------------------------------------------------------------------

testSignalDeriveSecretDeterminism :: IO Bool
testSignalDeriveSecretDeterminism = do
    let dh1 = BS.pack [0x01 .. 0x20]
        dh2 = BS.pack [0x21 .. 0x40]
        dh3 = BS.pack [0x41 .. 0x60]
    secret1 <- signalDeriveSecret dh1 dh2 dh3 Nothing
    secret2 <- signalDeriveSecret dh1 dh2 dh3 Nothing
    ok1 <- assertEq "X3DH derive deterministic (no OPK)" secret1 secret2
    ok2 <- assertEq "X3DH derive output length" 32 (BS.length secret1)
    pure (ok1 && ok2)

testSignalDeriveSecretWithOPK :: IO Bool
testSignalDeriveSecretWithOPK = do
    let dh1 = BS.pack [0x01 .. 0x20]
        dh2 = BS.pack [0x21 .. 0x40]
        dh3 = BS.pack [0x41 .. 0x60]
        dh4 = BS.pack [0x61 .. 0x80]
    secretNoOPK   <- signalDeriveSecret dh1 dh2 dh3 Nothing
    secretWithOPK <- signalDeriveSecret dh1 dh2 dh3 (Just dh4)
    -- With OPK must differ from without OPK
    ok1 <- if secretNoOPK /= secretWithOPK
           then putStrLn "  PASS: X3DH with OPK differs from without" >> pure True
           else putStrLn "  FAIL: X3DH with OPK same as without" >> pure False
    -- Deterministic with OPK
    secretWithOPK2 <- signalDeriveSecret dh1 dh2 dh3 (Just dh4)
    ok2 <- assertEq "X3DH derive deterministic (with OPK)" secretWithOPK secretWithOPK2
    ok3 <- assertEq "X3DH derive with OPK output length" 32 (BS.length secretWithOPK)
    pure (ok1 && ok2 && ok3)

------------------------------------------------------------------------
-- 5. Signal KDF: signalKdfRK and signalKdfCK determinism
------------------------------------------------------------------------

testSignalKdfRKDeterminism :: IO Bool
testSignalKdfRKDeterminism = do
    let rootKey = BS.pack [0xA0 .. 0xBF]  -- 32 bytes
        dhOut   = BS.pack [0xC0 .. 0xDF]  -- 32 bytes
    (rk1, ck1) <- signalKdfRK rootKey dhOut
    (rk2, ck2) <- signalKdfRK rootKey dhOut
    ok1 <- assertEq "signalKdfRK root key deterministic" rk1 rk2
    ok2 <- assertEq "signalKdfRK chain key deterministic" ck1 ck2
    ok3 <- assertEq "signalKdfRK root key length" 32 (BS.length rk1)
    ok4 <- assertEq "signalKdfRK chain key length" 32 (BS.length ck1)
    -- Output must differ from input
    ok5 <- if rk1 /= rootKey
           then putStrLn "  PASS: signalKdfRK root key differs from input" >> pure True
           else putStrLn "  FAIL: signalKdfRK root key same as input" >> pure False
    pure (and [ok1, ok2, ok3, ok4, ok5])

testSignalKdfCKDeterminism :: IO Bool
testSignalKdfCKDeterminism = do
    let chainKey = BS.pack [0xD0 .. 0xEF]  -- 32 bytes
    (newCK1, msgKey1) <- signalKdfCK chainKey
    (newCK2, msgKey2) <- signalKdfCK chainKey
    ok1 <- assertEq "signalKdfCK new chain key deterministic" newCK1 newCK2
    ok2 <- assertEq "signalKdfCK message key deterministic" msgKey1 msgKey2
    ok3 <- assertEq "signalKdfCK new chain key length" 32 (BS.length newCK1)
    ok4 <- assertEq "signalKdfCK message key length" 32 (BS.length msgKey1)
    -- Chain key and message key must differ from each other
    ok5 <- if newCK1 /= msgKey1
           then putStrLn "  PASS: signalKdfCK chain key != message key" >> pure True
           else putStrLn "  FAIL: signalKdfCK chain key == message key" >> pure False
    -- New chain key must differ from input chain key
    ok6 <- if newCK1 /= chainKey
           then putStrLn "  PASS: signalKdfCK new chain key differs from input" >> pure True
           else putStrLn "  FAIL: signalKdfCK new chain key same as input" >> pure False
    pure (and [ok1, ok2, ok3, ok4, ok5, ok6])

------------------------------------------------------------------------
-- 6. Multi-message ratchet: Alice sends 3, Bob decrypts in order
------------------------------------------------------------------------

testRatchetMultiMessage :: IO Bool
testRatchetMultiMessage = do
    mAlice <- signalRatchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
    case mAlice of
        Nothing -> do
            putStrLn "  FAIL: signalRatchetInitAlice returned Nothing"
            pure False
        Just alice0 -> do
            bob0 <- signalRatchetInitBob sharedSecret bobSPKSecret
            let msg1 = strToBS "Signal compat message 1"
                msg2 = strToBS "Signal compat message 2"
                msg3 = strToBS "Signal compat message 3"
            -- Encrypt 3 messages
            e1 <- signalRatchetEncrypt alice0 msg1
            case e1 of
                Left err -> putStrLn ("  FAIL: multi enc1: " ++ err) >> pure False
                Right (alice1, h1, ct1, tag1) -> do
                    e2 <- signalRatchetEncrypt alice1 msg2
                    case e2 of
                        Left err -> putStrLn ("  FAIL: multi enc2: " ++ err) >> pure False
                        Right (alice2, h2, ct2, tag2) -> do
                            e3 <- signalRatchetEncrypt alice2 msg3
                            case e3 of
                                Left err -> putStrLn ("  FAIL: multi enc3: " ++ err) >> pure False
                                Right (_alice3, h3, ct3, tag3) -> do
                                    -- Decrypt all 3 in order
                                    d1 <- signalRatchetDecrypt bob0 h1 ct1 tag1
                                    case d1 of
                                        Left err -> putStrLn ("  FAIL: multi dec1: " ++ err) >> pure False
                                        Right Nothing -> putStrLn "  FAIL: multi msg1 decrypt Nothing" >> pure False
                                        Right (Just (bob1, pt1)) -> do
                                            ok1 <- assertEq "Multi-msg A->B msg1" msg1 pt1
                                            d2 <- signalRatchetDecrypt bob1 h2 ct2 tag2
                                            case d2 of
                                                Left err -> putStrLn ("  FAIL: multi dec2: " ++ err) >> pure False
                                                Right Nothing -> putStrLn "  FAIL: multi msg2 decrypt Nothing" >> pure False
                                                Right (Just (bob2, pt2)) -> do
                                                    ok2 <- assertEq "Multi-msg A->B msg2" msg2 pt2
                                                    d3 <- signalRatchetDecrypt bob2 h3 ct3 tag3
                                                    case d3 of
                                                        Left err -> putStrLn ("  FAIL: multi dec3: " ++ err) >> pure False
                                                        Right Nothing -> putStrLn "  FAIL: multi msg3 decrypt Nothing" >> pure False
                                                        Right (Just (_, pt3)) -> do
                                                            ok3 <- assertEq "Multi-msg A->B msg3" msg3 pt3
                                                            pure (ok1 && ok2 && ok3)

------------------------------------------------------------------------
-- 8. Known-answer test vectors (M19.1.4)
--
-- These tests compare against HARDCODED expected outputs, not just
-- round-trip consistency.  The expected hex values were computed by
-- tracing through HKDF-SHA-256 / HMAC-SHA-256 with the Signal domain
-- separation strings ("WhisperRatchet", "WhisperText", 0x01, 0x02).
------------------------------------------------------------------------

-- | signalKdfRK: rootKey = 0x00*32, dhOut = 0x01*32
testKdfRKKnownVector1 :: IO Bool
testKdfRKKnownVector1 = do
    let rootKey    = BS.replicate 32 0x00
        dhOut      = BS.replicate 32 0x01
        expectedRK = hexDecode
            "c03ebf01d4836de65b6a54b7db2db871e40d0ae52ef937f5667f635adbb3c935"
        expectedCK = hexDecode
            "c4d1f38c44c173d6883c0715f951b50d7f9b3c91ca039d53a4c46a7eafa9479b"
    (rk, ck) <- signalKdfRK rootKey dhOut
    ok1 <- assertEq "KAT: signalKdfRK(00x32,01x32) rootKey" expectedRK rk
    ok2 <- assertEq "KAT: signalKdfRK(00x32,01x32) chainKey" expectedCK ck
    pure (ok1 && ok2)

-- | signalKdfRK: rootKey = 0x00..0x1f, dhOut = 0x20..0x3f
testKdfRKKnownVector2 :: IO Bool
testKdfRKKnownVector2 = do
    let rootKey    = BS.pack [0x00..0x1f]
        dhOut      = BS.pack [0x20..0x3f]
        expectedRK = hexDecode
            "62ffc77945c7aae74572869ac8a9522d96bc75a79cf3863ae7335004186255b3"
        expectedCK = hexDecode
            "2de7be8dc5a58c68bcb5db2e71cb88157ed10ab4f7ea97ba5606e49733da2b94"
    (rk, ck) <- signalKdfRK rootKey dhOut
    ok1 <- assertEq "KAT: signalKdfRK(00..1f,20..3f) rootKey" expectedRK rk
    ok2 <- assertEq "KAT: signalKdfRK(00..1f,20..3f) chainKey" expectedCK ck
    pure (ok1 && ok2)

-- | signalKdfCK: chainKey = 0x00*32
testKdfCKKnownVector1 :: IO Bool
testKdfCKKnownVector1 = do
    let expectedCK = hexDecode
            "4ee7be0c7872360ca67414608081e9bd60fd580a7bbd209701d2a5a0b4316d0d"
        expectedMK = hexDecode
            "3d7afb663124ecbf2c953f863d4fc8796eeb2d372b64aad58697ec5264649cdb"
    (newCK, msgKey) <- signalKdfCK (BS.replicate 32 0x00)
    ok1 <- assertEq "KAT: signalKdfCK(00x32) newChainKey" expectedCK newCK
    ok2 <- assertEq "KAT: signalKdfCK(00x32) msgKey" expectedMK msgKey
    pure (ok1 && ok2)

-- | signalKdfCK: chainKey = 0x01*32
testKdfCKKnownVector2 :: IO Bool
testKdfCKKnownVector2 = do
    let expectedCK = hexDecode
            "c31d79abaf8f2150ee1cfe3dc732eed02a56f79647909bad055a831cb762e9a2"
        expectedMK = hexDecode
            "cc6efb872c237f565ee82df42e4cab00098b13710395e3c6d29f2907d69e4f04"
    (newCK, msgKey) <- signalKdfCK (BS.replicate 32 0x01)
    ok1 <- assertEq "KAT: signalKdfCK(01x32) newChainKey" expectedCK newCK
    ok2 <- assertEq "KAT: signalKdfCK(01x32) msgKey" expectedMK msgKey
    pure (ok1 && ok2)

-- | signalDeriveSecret: dh1=0xAA*32, dh2=0xBB*32, dh3=0xCC*32, no OPK
testX3DHKnownVectorNoOPK :: IO Bool
testX3DHKnownVectorNoOPK = do
    let dh1      = BS.replicate 32 0xAA
        dh2      = BS.replicate 32 0xBB
        dh3      = BS.replicate 32 0xCC
        expected = hexDecode
            "80b9add1b2f3738e0aac08affd08d66922496ede22a042f59a9ee8dc40952b4a"
    secret <- signalDeriveSecret dh1 dh2 dh3 Nothing
    assertEq "KAT: signalDeriveSecret(AA,BB,CC,noOPK)" expected secret

-- | signalDeriveSecret: dh1=0xAA*32, dh2=0xBB*32, dh3=0xCC*32, dh4=0xDD*32
testX3DHKnownVectorWithOPK :: IO Bool
testX3DHKnownVectorWithOPK = do
    let dh1      = BS.replicate 32 0xAA
        dh2      = BS.replicate 32 0xBB
        dh3      = BS.replicate 32 0xCC
        dh4      = BS.replicate 32 0xDD
        expected = hexDecode
            "19d0183901f8db6455800867845f3e1badd5939553416b7d42580ef862dd4da9"
    secret <- signalDeriveSecret dh1 dh2 dh3 (Just dh4)
    assertEq "KAT: signalDeriveSecret(AA,BB,CC,DD)" expected secret

-- | signalRatchetInitAlice: ss=0x42*32, spkSecret=0x07*32, aliceDH=0x09*32
-- Verifies rootKey, sendChain, and DH send public key of the initial state.
testRatchetInitAliceKnownVector :: IO Bool
testRatchetInitAliceKnownVector = do
    let ss        = BS.replicate 32 0x42
        spkSecret = BS.replicate 32 0x07
        aliceDH   = BS.replicate 32 0x09
        spkPub    = case x25519 spkSecret x25519Basepoint of
                        Just p  -> p
                        Nothing -> error "testRatchetInitAliceKnownVector: impossible"
        expectedSPKPub = hexDecode
            "13be4feaeaf204c7fd3358fc9c00721881d174278128227ec674f37f7fe97b6d"
        expectedRK = hexDecode
            "682af4741466be51ee9bb46131f4da10800f97c335c1b01b8c1296130325de86"
        expectedSC = hexDecode
            "3fc1e6dc308fc36d343d4ea9012a4eae49b70bea11a8a552a469813b4e2ad768"
        expectedDHPub = hexDecode
            "57db4b359f23ae5e146e4e2512056704722506348c150c14753d0c933d04d421"
    -- First verify the SPK public key derivation
    ok0 <- assertEq "KAT: x25519(07x32) public key" expectedSPKPub spkPub
    mSt <- signalRatchetInitAlice ss spkPub aliceDH
    case mSt of
        Nothing -> do
            putStrLn "  FAIL: KAT signalRatchetInitAlice returned Nothing"
            pure False
        Just st -> do
            ok1 <- assertEq "KAT: ratchetInitAlice rootKey" expectedRK (srsRootKey st)
            ok2 <- assertEq "KAT: ratchetInitAlice sendChain" expectedSC (srsSendChain st)
            ok3 <- assertEq "KAT: ratchetInitAlice dhSendPub"
                       expectedDHPub (snd (srsDHSend st))
            pure (ok0 && ok1 && ok2 && ok3)

------------------------------------------------------------------------
-- 7. Cross-ratchet: A->B then B->A (triggers DH ratchet step)
------------------------------------------------------------------------

testRatchetCrossRatchet :: IO Bool
testRatchetCrossRatchet = do
    mAlice <- signalRatchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
    case mAlice of
        Nothing -> do
            putStrLn "  FAIL: signalRatchetInitAlice returned Nothing"
            pure False
        Just alice0 -> do
            bob0 <- signalRatchetInitBob sharedSecret bobSPKSecret
            let msgAB  = strToBS "Alice to Bob (cross-ratchet)"
            -- Alice -> Bob
            encAB <- signalRatchetEncrypt alice0 msgAB
            case encAB of
                Left err -> putStrLn ("  FAIL: cross enc A->B: " ++ err) >> pure False
                Right (alice1, hAB, ctAB, tagAB) -> do
                    decAB <- signalRatchetDecrypt bob0 hAB ctAB tagAB
                    case decAB of
                        Left err -> putStrLn ("  FAIL: cross dec A->B: " ++ err) >> pure False
                        Right Nothing -> putStrLn "  FAIL: cross A->B decrypt Nothing" >> pure False
                        Right (Just (bob1, ptAB)) -> do
                            ok1 <- assertEq "Cross-ratchet A->B" msgAB ptAB
                            -- Bob -> Alice (triggers DH ratchet step on both sides)
                            let msgBA = strToBS "Bob to Alice (cross-ratchet)"
                            encBA <- signalRatchetEncrypt bob1 msgBA
                            case encBA of
                                Left err -> putStrLn ("  FAIL: cross enc B->A: " ++ err) >> pure False
                                Right (_bob2, hBA, ctBA, tagBA) -> do
                                    decBA <- signalRatchetDecrypt alice1 hBA ctBA tagBA
                                    case decBA of
                                        Left err -> putStrLn ("  FAIL: cross dec B->A: " ++ err) >> pure False
                                        Right Nothing -> putStrLn "  FAIL: cross B->A decrypt Nothing" >> pure False
                                        Right (Just (_, ptBA)) -> do
                                            ok2 <- assertEq "Cross-ratchet B->A" msgBA ptBA
                                            pure (ok1 && ok2)

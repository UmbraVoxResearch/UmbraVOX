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
    let results = protoResults ++ varintResults ++ kdfResults
                  ++ x3dhResults ++ ratchetResults
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
    case signalRatchetInitAlice sharedSecret bobSPKPublic aliceDHSecret of
        Nothing -> do
            putStrLn "  FAIL: signalRatchetInitAlice returned Nothing"
            pure False
        Just alice -> do
            let bob = signalRatchetInitBob sharedSecret bobSPKSecret
                msg = strToBS "Hello from Signal-compat Alice!"
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
        secret1 = signalDeriveSecret dh1 dh2 dh3 Nothing
        secret2 = signalDeriveSecret dh1 dh2 dh3 Nothing
    ok1 <- assertEq "X3DH derive deterministic (no OPK)" secret1 secret2
    ok2 <- assertEq "X3DH derive output length" 32 (BS.length secret1)
    pure (ok1 && ok2)

testSignalDeriveSecretWithOPK :: IO Bool
testSignalDeriveSecretWithOPK = do
    let dh1 = BS.pack [0x01 .. 0x20]
        dh2 = BS.pack [0x21 .. 0x40]
        dh3 = BS.pack [0x41 .. 0x60]
        dh4 = BS.pack [0x61 .. 0x80]
        secretNoOPK  = signalDeriveSecret dh1 dh2 dh3 Nothing
        secretWithOPK = signalDeriveSecret dh1 dh2 dh3 (Just dh4)
    -- With OPK must differ from without OPK
    ok1 <- if secretNoOPK /= secretWithOPK
           then putStrLn "  PASS: X3DH with OPK differs from without" >> pure True
           else putStrLn "  FAIL: X3DH with OPK same as without" >> pure False
    -- Deterministic with OPK
    let secretWithOPK2 = signalDeriveSecret dh1 dh2 dh3 (Just dh4)
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
        (rk1, ck1) = signalKdfRK rootKey dhOut
        (rk2, ck2) = signalKdfRK rootKey dhOut
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
        (newCK1, msgKey1) = signalKdfCK chainKey
        (newCK2, msgKey2) = signalKdfCK chainKey
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
    case signalRatchetInitAlice sharedSecret bobSPKPublic aliceDHSecret of
        Nothing -> do
            putStrLn "  FAIL: signalRatchetInitAlice returned Nothing"
            pure False
        Just alice0 -> do
            let bob0 = signalRatchetInitBob sharedSecret bobSPKSecret
                msg1 = strToBS "Signal compat message 1"
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
-- 7. Cross-ratchet: A->B then B->A (triggers DH ratchet step)
------------------------------------------------------------------------

testRatchetCrossRatchet :: IO Bool
testRatchetCrossRatchet = do
    case signalRatchetInitAlice sharedSecret bobSPKPublic aliceDHSecret of
        Nothing -> do
            putStrLn "  FAIL: signalRatchetInitAlice returned Nothing"
            pure False
        Just alice0 -> do
            let bob0   = signalRatchetInitBob sharedSecret bobSPKSecret
                msgAB  = strToBS "Alice to Bob (cross-ratchet)"
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

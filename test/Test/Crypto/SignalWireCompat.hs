-- SPDX-License-Identifier: Apache-2.0
-- | Signal wire-compatibility test cases (M19.4.4).
--
-- Verifies that the Signal bridge can produce wire-compatible messages
-- by exercising the full encode -> decode -> encrypt -> decrypt pipeline
-- through the bridge session layer.
--
-- Tests:
--   1. SignalMessage protobuf round-trip (encode/decode with known fields)
--   2. PreKeySignalMessage protobuf round-trip
--   3. SignalEnvelope protobuf round-trip
--   4. Bridge session encrypt/decrypt (Alice + Bob, full pipeline)
--   5. Bridge envelope wrap/unwrap + encrypt/decrypt
--   6. Multi-message ratchet (Alice sends 3, Bob receives all 3 in order)
module Test.Crypto.SignalWireCompat
    ( signalWireCompatTests
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util

import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.Signal.Compat
    ( signalRatchetInitAlice
    , signalRatchetInitBob
    )
import UmbraVox.Bridge.Signal.Session
    ( initBridgeSession
    , sendBridgeMessage
    , recvBridgeMessage
    , encodeBridgeEnvelope
    , decodeBridgeEnvelope
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
    )

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

-- | Run all Signal wire-compatibility tests.
-- Returns True if all pass, False if any fail.
signalWireCompatTests :: IO Bool
signalWireCompatTests = do
    putStrLn "[SignalWireCompat] Running protobuf wire round-trip tests..."
    protoResults <- sequence
        [ testSignalMessageRoundTrip
        , testPreKeySignalMessageRoundTrip
        , testSignalEnvelopeRoundTrip
        ]
    putStrLn "[SignalWireCompat] Running bridge session tests..."
    bridgeResults <- sequence
        [ testBridgeSessionEncryptDecrypt
        , testBridgeEnvelopeWrap
        , testMultiMessageRatchet
        ]
    let results = protoResults ++ bridgeResults
        passed  = length (filter id results)
        total   = length results
    putStrLn $ "[SignalWireCompat] " ++ show passed ++ "/" ++ show total
               ++ " suites passed."
    pure (and results)

------------------------------------------------------------------------
-- Test key material (deterministic, same as SignalCompat tests)
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
-- 1. SignalMessage protobuf round-trip
------------------------------------------------------------------------

testSignalMessageRoundTrip :: IO Bool
testSignalMessageRoundTrip = do
    let msg = SignalMessage
            { smRatchetKey      = BS.pack (0x05 : [0xAA .. 0xCA])  -- 33 bytes
            , smCounter         = 42
            , smPreviousCounter = 7
            , smCiphertext      = strToBS "wire-compat-encrypted-payload"
            }
        encoded = encodeSignalMessage msg
        decoded = decodeSignalMessage encoded
    ok1 <- assertEq "SignalMessage wire round-trip: decode matches" (Just msg) decoded
    -- Verify encoding is non-empty and deterministic
    let encoded2 = encodeSignalMessage msg
    ok2 <- assertEq "SignalMessage wire round-trip: deterministic encoding" encoded encoded2
    ok3 <- if BS.length encoded > 0
           then putStrLn "  PASS: SignalMessage encoding is non-empty" >> pure True
           else putStrLn "  FAIL: SignalMessage encoding is empty" >> pure False
    pure (ok1 && ok2 && ok3)

------------------------------------------------------------------------
-- 2. PreKeySignalMessage protobuf round-trip
------------------------------------------------------------------------

testPreKeySignalMessageRoundTrip :: IO Bool
testPreKeySignalMessageRoundTrip = do
    let msg = PreKeySignalMessage
            { pksmRegistrationId = 54321
            , pksmPreKeyId       = 100
            , pksmSignedPreKeyId = 200
            , pksmBaseKey        = BS.pack (0x05 : [0xBB .. 0xDB])  -- 33 bytes
            , pksmIdentityKey    = BS.pack (0x05 : [0xCC .. 0xEC])  -- 33 bytes
            , pksmMessage        = strToBS "inner-signal-wire-msg"
            }
        encoded = encodePreKeySignalMessage msg
        decoded = decodePreKeySignalMessage encoded
    ok1 <- assertEq "PreKeySignalMessage wire round-trip: decode matches" (Just msg) decoded
    -- Verify all field values survive encoding
    case decoded of
        Nothing -> pure False
        Just d -> do
            ok2 <- assertEq "PreKeySignalMessage regId preserved" 54321 (pksmRegistrationId d)
            ok3 <- assertEq "PreKeySignalMessage preKeyId preserved" 100 (pksmPreKeyId d)
            ok4 <- assertEq "PreKeySignalMessage signedPreKeyId preserved" 200 (pksmSignedPreKeyId d)
            pure (ok1 && ok2 && ok3 && ok4)

------------------------------------------------------------------------
-- 3. SignalEnvelope protobuf round-trip
------------------------------------------------------------------------

testSignalEnvelopeRoundTrip :: IO Bool
testSignalEnvelopeRoundTrip = do
    let env = SignalEnvelope
            { seType         = EnvelopeCiphertext
            , seSourceDevice = 1
            , seTimestamp    = 1700000000000
            , seContent      = strToBS "wire-compat-sealed-content"
            }
        encoded = encodeSignalEnvelope env
        decoded = decodeSignalEnvelope encoded
    ok1 <- assertEq "SignalEnvelope wire round-trip: decode matches" (Just env) decoded
    -- Also test PreKeyBundle envelope type
    let env2 = env { seType = EnvelopePreKeyBundle, seSourceDevice = 2 }
        encoded2 = encodeSignalEnvelope env2
        decoded2 = decodeSignalEnvelope encoded2
    ok2 <- assertEq "SignalEnvelope wire round-trip (PreKeyBundle)" (Just env2) decoded2
    -- Verify different envelope types encode differently
    ok3 <- if encoded /= encoded2
           then putStrLn "  PASS: different envelope types produce different wire bytes" >> pure True
           else putStrLn "  FAIL: different envelope types produce same wire bytes" >> pure False
    pure (ok1 && ok2 && ok3)

------------------------------------------------------------------------
-- 4. Bridge session encrypt/decrypt (Alice + Bob)
------------------------------------------------------------------------

testBridgeSessionEncryptDecrypt :: IO Bool
testBridgeSessionEncryptDecrypt = do
    case signalRatchetInitAlice sharedSecret bobSPKPublic aliceDHSecret of
        Nothing -> do
            putStrLn "  FAIL: signalRatchetInitAlice returned Nothing"
            pure False
        Just aliceRatchet -> do
            let bobRatchet = signalRatchetInitBob sharedSecret bobSPKSecret
            -- Create bridge sessions for both parties
            aliceSession <- initBridgeSession "wss://test.signal.org" aliceRatchet 1 1000
            bobSession   <- initBridgeSession "wss://test.signal.org" bobRatchet   2 2000
            let plaintext = strToBS "Hello Bob, this is wire-compatible!"
            -- Alice encrypts through the bridge
            encResult <- sendBridgeMessage aliceSession plaintext
            case encResult of
                Left err -> do
                    putStrLn $ "  FAIL: sendBridgeMessage: " ++ show err
                    pure False
                Right wireBytes -> do
                    -- Verify wire bytes are non-empty
                    ok1 <- if BS.length wireBytes > 0
                           then putStrLn "  PASS: bridge wire bytes non-empty" >> pure True
                           else putStrLn "  FAIL: bridge wire bytes empty" >> pure False
                    -- Verify wire bytes are valid protobuf (decodable as SignalMessage)
                    ok2 <- case decodeSignalMessage wireBytes of
                        Nothing -> putStrLn "  FAIL: bridge output not valid SignalMessage protobuf" >> pure False
                        Just _  -> putStrLn "  PASS: bridge output is valid SignalMessage protobuf" >> pure True
                    -- Bob decrypts through the bridge
                    decResult <- recvBridgeMessage bobSession wireBytes
                    case decResult of
                        Left err -> do
                            putStrLn $ "  FAIL: recvBridgeMessage: " ++ show err
                            pure False
                        Right recovered -> do
                            ok3 <- assertEq "Bridge encrypt/decrypt plaintext match" plaintext recovered
                            pure (ok1 && ok2 && ok3)

------------------------------------------------------------------------
-- 5. Bridge envelope wrap/unwrap + encrypt/decrypt
------------------------------------------------------------------------

testBridgeEnvelopeWrap :: IO Bool
testBridgeEnvelopeWrap = do
    case signalRatchetInitAlice sharedSecret bobSPKPublic aliceDHSecret of
        Nothing -> do
            putStrLn "  FAIL: signalRatchetInitAlice returned Nothing"
            pure False
        Just aliceRatchet -> do
            let bobRatchet = signalRatchetInitBob sharedSecret bobSPKSecret
            aliceSession <- initBridgeSession "wss://test.signal.org" aliceRatchet 1 1000
            bobSession   <- initBridgeSession "wss://test.signal.org" bobRatchet   2 2000
            let plaintext = strToBS "Envelope-wrapped wire-compatible message"
            -- Alice encrypts
            encResult <- sendBridgeMessage aliceSession plaintext
            case encResult of
                Left err -> do
                    putStrLn $ "  FAIL: sendBridgeMessage: " ++ show err
                    pure False
                Right wireBytes -> do
                    -- Wrap in envelope
                    envResult <- encodeBridgeEnvelope aliceSession wireBytes
                    case envResult of
                        Left err -> do
                            putStrLn $ "  FAIL: encodeBridgeEnvelope: " ++ show err
                            pure False
                        Right envelopeBytes -> do
                            -- Verify envelope bytes are non-empty
                            ok1 <- if BS.length envelopeBytes > 0
                                   then putStrLn "  PASS: envelope bytes non-empty" >> pure True
                                   else putStrLn "  FAIL: envelope bytes empty" >> pure False
                            -- Unwrap envelope
                            case decodeBridgeEnvelope envelopeBytes of
                                Left err -> do
                                    putStrLn $ "  FAIL: decodeBridgeEnvelope: " ++ show err
                                    pure False
                                Right (envType, innerContent) -> do
                                    ok2 <- assertEq "Envelope type is Ciphertext" EnvelopeCiphertext envType
                                    ok3 <- assertEq "Envelope inner content matches wire bytes" wireBytes innerContent
                                    -- Decrypt the inner content
                                    decResult <- recvBridgeMessage bobSession innerContent
                                    case decResult of
                                        Left err -> do
                                            putStrLn $ "  FAIL: recvBridgeMessage after unwrap: " ++ show err
                                            pure False
                                        Right recovered -> do
                                            ok4 <- assertEq "Envelope wrap/unwrap plaintext match" plaintext recovered
                                            pure (ok1 && ok2 && ok3 && ok4)

------------------------------------------------------------------------
-- 6. Multi-message ratchet (Alice sends 3, Bob receives in order)
------------------------------------------------------------------------

testMultiMessageRatchet :: IO Bool
testMultiMessageRatchet = do
    case signalRatchetInitAlice sharedSecret bobSPKPublic aliceDHSecret of
        Nothing -> do
            putStrLn "  FAIL: signalRatchetInitAlice returned Nothing"
            pure False
        Just aliceRatchet -> do
            let bobRatchet = signalRatchetInitBob sharedSecret bobSPKSecret
            aliceSession <- initBridgeSession "wss://test.signal.org" aliceRatchet 1 1000
            bobSession   <- initBridgeSession "wss://test.signal.org" bobRatchet   2 2000
            let msg1 = strToBS "Wire-compat message 1"
                msg2 = strToBS "Wire-compat message 2"
                msg3 = strToBS "Wire-compat message 3"
            -- Alice encrypts 3 messages
            e1 <- sendBridgeMessage aliceSession msg1
            case e1 of
                Left err -> putStrLn ("  FAIL: multi enc1: " ++ show err) >> pure False
                Right w1 -> do
                    e2 <- sendBridgeMessage aliceSession msg2
                    case e2 of
                        Left err -> putStrLn ("  FAIL: multi enc2: " ++ show err) >> pure False
                        Right w2 -> do
                            e3 <- sendBridgeMessage aliceSession msg3
                            case e3 of
                                Left err -> putStrLn ("  FAIL: multi enc3: " ++ show err) >> pure False
                                Right w3 -> do
                                    -- Verify all wire messages are different
                                    ok0 <- if w1 /= w2 && w2 /= w3 && w1 /= w3
                                           then putStrLn "  PASS: all 3 wire messages are distinct" >> pure True
                                           else putStrLn "  FAIL: wire messages not all distinct" >> pure False
                                    -- Bob decrypts all 3 in order
                                    d1 <- recvBridgeMessage bobSession w1
                                    case d1 of
                                        Left err -> putStrLn ("  FAIL: multi dec1: " ++ show err) >> pure False
                                        Right pt1 -> do
                                            ok1 <- assertEq "Multi-msg wire-compat msg1" msg1 pt1
                                            d2 <- recvBridgeMessage bobSession w2
                                            case d2 of
                                                Left err -> putStrLn ("  FAIL: multi dec2: " ++ show err) >> pure False
                                                Right pt2 -> do
                                                    ok2 <- assertEq "Multi-msg wire-compat msg2" msg2 pt2
                                                    d3 <- recvBridgeMessage bobSession w3
                                                    case d3 of
                                                        Left err -> putStrLn ("  FAIL: multi dec3: " ++ show err) >> pure False
                                                        Right pt3 -> do
                                                            ok3 <- assertEq "Multi-msg wire-compat msg3" msg3 pt3
                                                            pure (ok0 && ok1 && ok2 && ok3)

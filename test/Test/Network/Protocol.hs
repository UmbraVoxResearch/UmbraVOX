-- SPDX-License-Identifier: Apache-2.0
-- | Wire protocol encoding/decoding tests.
--
-- Round-trip property: for every valid P2PMessage m,
-- @decode (encode m) == Right m@.
module Test.Network.Protocol (runTests) where

import qualified Data.ByteString as BS

import UmbraVox.Network.Protocol
    ( P2PMessage(..)
    , HandshakePayload(..)
    , DataPayload(..)
    , AckPayload(..)
    , PeerPayload(..)
    , encode
    , decode
    )

runTests :: IO Bool
runTests = do
    putStrLn "Test.Network.Protocol"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testRoundTripHandshake
        , testRoundTripData
        , testRoundTripDataEmpty
        , testRoundTripAck
        , testRoundTripPeer
        , testRoundTripPeerEmpty
        , testRoundTripPing
        , testRoundTripPong
        , testDecodeTooShort
        , testDecodeBadType
        , testDecodeLengthMismatch
        , testDecodeHandshakeBadLen
        , testDecodeAckBadLen
        , testDecodePingNonEmpty
        ]
    pure (and results)

------------------------------------------------------------------------
-- Round-trip tests
------------------------------------------------------------------------

testRoundTripHandshake :: IO Bool
testRoundTripHandshake = do
    let msg = MsgHandshake HandshakePayload
                { hpVersion      = 1
                , hpPublicKey    = BS.replicate 32 0xAB
                , hpCapabilities = 0x0003
                }
    check "roundtrip Handshake" msg

testRoundTripData :: IO Bool
testRoundTripData = do
    let msg = MsgData DataPayload
                { dpSequence = 42
                , dpPayload  = BS.pack [1,2,3,4,5]
                }
    check "roundtrip Data" msg

testRoundTripDataEmpty :: IO Bool
testRoundTripDataEmpty = do
    let msg = MsgData DataPayload
                { dpSequence = 0
                , dpPayload  = BS.empty
                }
    check "roundtrip Data (empty payload)" msg

testRoundTripAck :: IO Bool
testRoundTripAck = do
    let msg = MsgAck AckPayload { apSequence = 99 }
    check "roundtrip Ack" msg

testRoundTripPeer :: IO Bool
testRoundTripPeer = do
    let msg = MsgPeer PeerPayload
                { ppPeers = [("192.168.1.1:8080", 0x0001)
                            ,("10.0.0.1:9090", 0x0003)]
                }
    check "roundtrip Peer" msg

testRoundTripPeerEmpty :: IO Bool
testRoundTripPeerEmpty = do
    let msg = MsgPeer PeerPayload { ppPeers = [] }
    check "roundtrip Peer (empty)" msg

testRoundTripPing :: IO Bool
testRoundTripPing = check "roundtrip Ping" MsgPing

testRoundTripPong :: IO Bool
testRoundTripPong = check "roundtrip Pong" MsgPong

------------------------------------------------------------------------
-- Error tests
------------------------------------------------------------------------

testDecodeTooShort :: IO Bool
testDecodeTooShort = do
    case decode (BS.pack [0x01, 0x00]) of
        Left _  -> pass "decode rejects short input"
        Right _ -> fail' "decode should reject short input"

testDecodeBadType :: IO Bool
testDecodeBadType = do
    -- type 0xFF, length 0, no payload
    case decode (BS.pack [0xFF, 0x00, 0x00, 0x00, 0x00]) of
        Left _  -> pass "decode rejects unknown type"
        Right _ -> fail' "decode should reject unknown type"

testDecodeLengthMismatch :: IO Bool
testDecodeLengthMismatch = do
    -- type 0x05 (ping), length says 1 but no payload
    case decode (BS.pack [0x05, 0x00, 0x00, 0x00, 0x01]) of
        Left _  -> pass "decode rejects length mismatch"
        Right _ -> fail' "decode should reject length mismatch"

testDecodeHandshakeBadLen :: IO Bool
testDecodeHandshakeBadLen = do
    -- handshake with 10-byte payload (should be 35)
    let hdr = BS.pack [0x01, 0x00, 0x00, 0x00, 0x0A]
    case decode (BS.append hdr (BS.replicate 10 0x00)) of
        Left _  -> pass "decode rejects bad handshake length"
        Right _ -> fail' "decode should reject bad handshake length"

testDecodeAckBadLen :: IO Bool
testDecodeAckBadLen = do
    -- ack with 2-byte payload (should be 4)
    let hdr = BS.pack [0x03, 0x00, 0x00, 0x00, 0x02]
    case decode (BS.append hdr (BS.replicate 2 0x00)) of
        Left _  -> pass "decode rejects bad ack length"
        Right _ -> fail' "decode should reject bad ack length"

testDecodePingNonEmpty :: IO Bool
testDecodePingNonEmpty = do
    -- ping with 1-byte payload (should be 0)
    let hdr = BS.pack [0x05, 0x00, 0x00, 0x00, 0x01, 0x00]
    case decode hdr of
        Left _  -> pass "decode rejects non-empty ping"
        Right _ -> fail' "decode should reject non-empty ping"

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

check :: String -> P2PMessage -> IO Bool
check label msg =
    case decode (encode msg) of
        Right decoded | decoded == msg -> pass label
        Right decoded -> do
            putStrLn $ "  FAIL: " ++ label ++ " (mismatch)"
            putStrLn $ "    expected: " ++ show msg
            putStrLn $ "    got:      " ++ show decoded
            pure False
        Left err -> do
            putStrLn $ "  FAIL: " ++ label ++ " (decode error: " ++ err ++ ")"
            pure False

pass :: String -> IO Bool
pass label = do
    putStrLn $ "  PASS: " ++ label
    pure True

fail' :: String -> IO Bool
fail' label = do
    putStrLn $ "  FAIL: " ++ label
    pure False

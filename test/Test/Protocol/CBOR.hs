-- SPDX-License-Identifier: Apache-2.0
-- | CBOR (length-prefixed serialization) test suite.
--
-- Tests encode/decode round-trip, empty message handling,
-- and large message support.
module Test.Protocol.CBOR (runTests) where

import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Protocol.CBOR (encodeMessage, decodeMessage)

runTests :: IO Bool
runTests = do
    putStrLn "[CBOR] Running length-prefixed encoding tests..."
    results <- sequence
        [ testRoundTrip
        , testEmptyMessage
        , testLargeMessage
        , testDecodeShortInput
        , testRoundTripProperty
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[CBOR] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Encode then decode should return the original payload.
testRoundTrip :: IO Bool
testRoundTrip = do
    let payload = strToBS "Hello, World!"
        encoded = encodeMessage payload
    case decodeMessage encoded of
        Just (decoded, remaining) ->
            do r1 <- assertEq "round-trip payload" payload decoded
               r2 <- assertEq "round-trip no remaining" BS.empty remaining
               pure (r1 && r2)
        Nothing ->
            assertEq "round-trip decode succeeded" True False

-- | Empty message should encode and decode correctly.
testEmptyMessage :: IO Bool
testEmptyMessage = do
    let payload = BS.empty
        encoded = encodeMessage payload
    case decodeMessage encoded of
        Just (decoded, remaining) ->
            do r1 <- assertEq "empty message payload" payload decoded
               r2 <- assertEq "empty message no remaining" BS.empty remaining
               pure (r1 && r2)
        Nothing ->
            assertEq "empty message decode succeeded" True False

-- | Large message (64KB) should encode and decode correctly.
testLargeMessage :: IO Bool
testLargeMessage = do
    let payload = BS.replicate 65536 0xAB
        encoded = encodeMessage payload
    case decodeMessage encoded of
        Just (decoded, remaining) ->
            do r1 <- assertEq "large message length" (BS.length payload) (BS.length decoded)
               r2 <- assertEq "large message content" payload decoded
               r3 <- assertEq "large message no remaining" BS.empty remaining
               pure (r1 && r2 && r3)
        Nothing ->
            assertEq "large message decode succeeded" True False

-- | Decoding fewer than 4 bytes should return Nothing.
testDecodeShortInput :: IO Bool
testDecodeShortInput = do
    let r1 = decodeMessage BS.empty
        r2 = decodeMessage (BS.pack [0x00])
        r3 = decodeMessage (BS.pack [0x00, 0x00, 0x00])
    a <- assertEq "decode empty input" Nothing r1
    b <- assertEq "decode 1 byte" Nothing r2
    c <- assertEq "decode 3 bytes" Nothing r3
    pure (a && b && c)

-- | Property: encode/decode round-trip for random payloads.
testRoundTripProperty :: IO Bool
testRoundTripProperty =
    checkProperty "encode/decode round-trip (property)" 100 $ \g ->
        let (payload, _) = nextBytesRange 0 1024 g
            encoded = encodeMessage payload
        in case decodeMessage encoded of
            Just (decoded, remaining) ->
                decoded == payload && BS.null remaining
            Nothing -> False

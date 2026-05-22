-- SPDX-License-Identifier: Apache-2.0
-- | Session test suite: verify session state init and round-trip serialization.
module Test.Crypto.Signal.Session (runTests) where

import qualified Data.ByteString as BS

import UmbraVox.Crypto.Signal.Session
    ( SessionState(..)
    , initSession
    , serializeSession
    , deserializeSession
    )

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.Signal.Session"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testInitSession
        , testSerializeRoundTrip
        , testDeserializeTruncated
        , testDeserializeEmpty
        ]
    pure (and results)

-- | initSession should produce a valid SessionState.
testInitSession :: IO Bool
testInitSession = do
    let secret = BS.replicate 32 0xAB
        ss = initSession secret
    if ssMessageCount ss == 0 && ssCreatedAt ss == 0
        then putStrLn "  PASS: initSession produces valid state" >> pure True
        else putStrLn "  FAIL: initSession produced unexpected state" >> pure False

-- | Serialization round-trip: serialize then deserialize should yield the
-- original session state.
testSerializeRoundTrip :: IO Bool
testSerializeRoundTrip = do
    let secret = BS.replicate 32 0xCD
        ss = (initSession secret) { ssCreatedAt = 1700000000, ssMessageCount = 42 }
        encoded = serializeSession ss
    case deserializeSession encoded of
        Just ss' | ss' == ss ->
            putStrLn "  PASS: serialize/deserialize round-trip" >> pure True
        Just _  ->
            putStrLn "  FAIL: round-trip mismatch" >> pure False
        Nothing ->
            putStrLn "  FAIL: deserializeSession returned Nothing" >> pure False

-- | Truncated input should return Nothing.
testDeserializeTruncated :: IO Bool
testDeserializeTruncated = do
    let secret = BS.replicate 32 0xEF
        ss = initSession secret
        encoded = serializeSession ss
        truncated = BS.take 10 encoded
    case deserializeSession truncated of
        Nothing -> putStrLn "  PASS: truncated input returns Nothing" >> pure True
        Just _  -> putStrLn "  FAIL: truncated input should fail" >> pure False

-- | Empty input should return Nothing.
testDeserializeEmpty :: IO Bool
testDeserializeEmpty =
    case deserializeSession BS.empty of
        Nothing -> putStrLn "  PASS: empty input returns Nothing" >> pure True
        Just _  -> putStrLn "  FAIL: empty input should fail" >> pure False

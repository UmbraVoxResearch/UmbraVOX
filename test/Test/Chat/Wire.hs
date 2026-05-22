-- SPDX-License-Identifier: Apache-2.0
-- | Inner payload (M23.1.1d) test suite for Chat.Wire.
--
-- Covers: InnerPayload round-trip (senderId + appData), short payload
-- rejection (<32 bytes), and empty appData round-trip.
module Test.Chat.Wire (runTests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Test.Util (assertEq, checkProperty, PRNG, nextBytes, nextWord32)
import UmbraVox.Chat.Wire
    ( encodeInnerPayload, decodeInnerPayload, senderIdSize )

runTests :: IO Bool
runTests = do
    putStrLn "Test.Chat.Wire"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testInnerPayloadRoundTrip
        , testShortPayloadRejection
        , testEmptyAppDataRoundTrip
        , testExactly32BytesEmptyApp
        , testPropertyInnerPayloadRoundTrip
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

-- | Encode then decode an inner payload with both senderId and appData.
testInnerPayloadRoundTrip :: IO Bool
testInnerPayloadRoundTrip = do
    let senderId = BS.replicate 32 0xAA
        appData  = BS.pack [1, 2, 3, 4, 5]
        encoded  = encodeInnerPayload senderId appData
    case decodeInnerPayload encoded of
        Nothing -> assertEq "inner payload round-trip" True False
        Just (sid, ad) -> do
            r1 <- assertEq "inner payload: senderId" senderId sid
            r2 <- assertEq "inner payload: appData" appData ad
            pure (r1 && r2)

-- | Payloads shorter than 32 bytes are rejected.
testShortPayloadRejection :: IO Bool
testShortPayloadRejection = do
    r1 <- assertEq "short payload (0 bytes)" Nothing (decodeInnerPayload BS.empty)
    r2 <- assertEq "short payload (1 byte)" Nothing (decodeInnerPayload (BS.singleton 0x42))
    r3 <- assertEq "short payload (31 bytes)" Nothing
               (decodeInnerPayload (BS.replicate 31 0xFF))
    pure (r1 && r2 && r3)

-- | Encode then decode with empty appData.
testEmptyAppDataRoundTrip :: IO Bool
testEmptyAppDataRoundTrip = do
    let senderId = BS.replicate 32 0xBB
        appData  = BS.empty
        encoded  = encodeInnerPayload senderId appData
    case decodeInnerPayload encoded of
        Nothing -> assertEq "empty appData round-trip" True False
        Just (sid, ad) -> do
            r1 <- assertEq "empty appData: senderId" senderId sid
            r2 <- assertEq "empty appData: appData is empty" BS.empty ad
            pure (r1 && r2)

-- | Exactly 32 bytes decodes with empty appData.
testExactly32BytesEmptyApp :: IO Bool
testExactly32BytesEmptyApp = do
    let bs = BS.replicate 32 0xCC
    case decodeInnerPayload bs of
        Nothing -> assertEq "exactly 32 bytes" True False
        Just (sid, ad) -> do
            r1 <- assertEq "exactly 32 bytes: senderId" bs sid
            r2 <- assertEq "exactly 32 bytes: appData empty" BS.empty ad
            r3 <- assertEq "senderIdSize == 32" 32 senderIdSize
            pure (r1 && r2 && r3)

-- | Property: encode/decode inner payload round-trip (500 iterations).
testPropertyInnerPayloadRoundTrip :: IO Bool
testPropertyInnerPayloadRoundTrip =
    checkProperty "inner payload round-trip property (500 iterations)" 500 $ \g0 ->
        let (senderId, g1) = nextBytes 32 g0
            (lenW, g2)     = nextWord32 g1
            adLen          = fromIntegral (lenW `mod` 4096) :: Int
            (appData, _)   = nextBytes adLen g2
            encoded        = encodeInnerPayload senderId appData
        in case decodeInnerPayload encoded of
            Nothing         -> False
            Just (sid, ad)  -> sid == senderId && ad == appData

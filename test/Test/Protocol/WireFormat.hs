-- SPDX-License-Identifier: Apache-2.0
-- | WireFormat test suite.
module Test.Protocol.WireFormat (runTests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word8, Word32)

import UmbraVox.Protocol.WireFormat
    ( Envelope(..), wrapEnvelope, unwrapEnvelope
    , encodeEnvelope, decodeEnvelope )

runTests :: IO Bool
runTests = do
    putStrLn "Test.Protocol.WireFormat"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testRoundTrip
        , testRoundTripEmpty
        , testUnwrapExtractsPayload
        , testDecodeShortInput
        , testDecodeTruncatedPayload
        , testEnvelopeFields
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Helpers ------------------------------------------------------------------

fakeId :: Word8 -> ByteString
fakeId w = BS.replicate 32 w

check :: String -> Bool -> IO Bool
check label True  = putStrLn ("  PASS: " ++ label) >> pure True
check label False = putStrLn ("  FAIL: " ++ label) >> pure False

-- Tests --------------------------------------------------------------------

-- | encode then decode should recover the original envelope.
testRoundTrip :: IO Bool
testRoundTrip = do
    let payload = BS.pack [0x41, 0x42, 0x43]  -- "ABC"
        env     = wrapEnvelope 0 42 (fakeId 0xAA) (fakeId 0xBB) payload
        wire    = encodeEnvelope env
    case decodeEnvelope wire of
        Nothing   -> check "round-trip" False
        Just env' -> check "round-trip" (env == env')

-- | Round-trip with empty payload.
testRoundTripEmpty :: IO Bool
testRoundTripEmpty = do
    let env  = wrapEnvelope 1 0 (fakeId 0x01) (fakeId 0x02) BS.empty
        wire = encodeEnvelope env
    case decodeEnvelope wire of
        Nothing   -> check "round-trip empty payload" False
        Just env' -> check "round-trip empty payload" (env == env')

-- | unwrapEnvelope extracts the payload.
testUnwrapExtractsPayload :: IO Bool
testUnwrapExtractsPayload = do
    let payload = BS.pack [1,2,3,4,5]
        env     = wrapEnvelope 2 99 (fakeId 0xCC) (fakeId 0xDD) payload
    check "unwrap extracts payload" (unwrapEnvelope env == payload)

-- | decodeEnvelope returns Nothing for too-short input.
testDecodeShortInput :: IO Bool
testDecodeShortInput =
    check "decode short input" (decodeEnvelope (BS.replicate 10 0) == Nothing)

-- | decodeEnvelope returns Nothing when payload length exceeds available bytes.
testDecodeTruncatedPayload :: IO Bool
testDecodeTruncatedPayload = do
    let env  = wrapEnvelope 0 1 (fakeId 0x11) (fakeId 0x22) (BS.replicate 100 0xFF)
        wire = encodeEnvelope env
        -- Chop off the last 50 bytes so payloadLen says 100 but only 50 are present
        truncated = BS.take (BS.length wire - 50) wire
    check "decode truncated payload" (decodeEnvelope truncated == Nothing)

-- | Verify individual envelope fields survive encode/decode.
testEnvelopeFields :: IO Bool
testEnvelopeFields = do
    let src = fakeId 0x10
        dst = fakeId 0x20
        env = wrapEnvelope 3 0xDEADBEEF src dst (BS.pack [0x99])
    case decodeEnvelope (encodeEnvelope env) of
        Nothing   -> check "envelope fields" False
        Just env' -> check "envelope fields" $
            envVersion env'  == 1 &&
            envType env'     == 3 &&
            envSequence env' == 0xDEADBEEF &&
            envSourceId env' == src &&
            envDestId env'   == dst &&
            envPayload env'  == BS.pack [0x99]

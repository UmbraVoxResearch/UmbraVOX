-- SPDX-License-Identifier: Apache-2.0
-- | WireFormat test suite.
--
-- Covers: round-trip encode/decode for each message type (0-3), empty
-- payload, short input rejection, truncated payload rejection, field-level
-- verification (version, type, sequence, source, dest), property-based
-- round-trips, and garbage input resilience.
module Test.Protocol.WireFormat (runTests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word8, Word32)

import UmbraVox.Protocol.WireFormat
    ( Envelope(..), wrapEnvelope, unwrapEnvelope
    , encodeEnvelope, decodeEnvelope )
import Test.Util (checkProperty, PRNG, nextBytes, nextWord32, nextWord8)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Protocol.WireFormat"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testRoundTrip
        , testRoundTripEmpty
        , testRoundTripTypeData
        , testRoundTripTypeAck
        , testRoundTripTypeHandshake
        , testRoundTripTypePeer
        , testUnwrapExtractsPayload
        , testDecodeShortInput
        , testDecodeEmpty
        , testDecodeExactHeader
        , testDecodeTruncatedPayload
        , testDecodeTruncatedByOne
        , testEnvelopeFields
        , testFieldVersion
        , testFieldType
        , testFieldSequence
        , testFieldSourceId
        , testFieldDestId
        , testWrapEnvelopeTruncatesIds
        , testPropertyRoundTrip
        , testPropertyNoCrash
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

-- | Build an Envelope with a specific message type for round-trip tests.
mkEnvelope :: Word8 -> Word32 -> ByteString -> Envelope
mkEnvelope typ seqN payload =
    wrapEnvelope typ seqN (fakeId 0xAA) (fakeId 0xBB) payload

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

-- | Round-trip for message type 0 (data).
testRoundTripTypeData :: IO Bool
testRoundTripTypeData = do
    let env  = mkEnvelope 0 100 (BS.pack [1,2,3,4,5])
        wire = encodeEnvelope env
    case decodeEnvelope wire of
        Nothing   -> check "round-trip type=data(0)" False
        Just env' -> check "round-trip type=data(0)" (env == env')

-- | Round-trip for message type 1 (ack).
testRoundTripTypeAck :: IO Bool
testRoundTripTypeAck = do
    let env  = mkEnvelope 1 200 (BS.pack [0xFF])
        wire = encodeEnvelope env
    case decodeEnvelope wire of
        Nothing   -> check "round-trip type=ack(1)" False
        Just env' -> check "round-trip type=ack(1)" (env == env')

-- | Round-trip for message type 2 (handshake).
testRoundTripTypeHandshake :: IO Bool
testRoundTripTypeHandshake = do
    let env  = mkEnvelope 2 300 (BS.replicate 256 0xDE)
        wire = encodeEnvelope env
    case decodeEnvelope wire of
        Nothing   -> check "round-trip type=handshake(2)" False
        Just env' -> check "round-trip type=handshake(2)" (env == env')

-- | Round-trip for message type 3 (peer).
testRoundTripTypePeer :: IO Bool
testRoundTripTypePeer = do
    let env  = mkEnvelope 3 400 (BS.replicate 64 0xCA)
        wire = encodeEnvelope env
    case decodeEnvelope wire of
        Nothing   -> check "round-trip type=peer(3)" False
        Just env' -> check "round-trip type=peer(3)" (env == env')

-- | unwrapEnvelope extracts the payload.
testUnwrapExtractsPayload :: IO Bool
testUnwrapExtractsPayload = do
    let payload = BS.pack [1,2,3,4,5]
        env     = wrapEnvelope 2 99 (fakeId 0xCC) (fakeId 0xDD) payload
    check "unwrap extracts payload" (unwrapEnvelope env == payload)

-- | decodeEnvelope returns Nothing for too-short input.
testDecodeShortInput :: IO Bool
testDecodeShortInput =
    check "decode short input (10 bytes)" (decodeEnvelope (BS.replicate 10 0) == Nothing)

-- | decodeEnvelope returns Nothing for empty input.
testDecodeEmpty :: IO Bool
testDecodeEmpty =
    check "decode empty input" (decodeEnvelope BS.empty == Nothing)

-- | decodeEnvelope with exactly 74 bytes (header only, payloadLen=0) should
-- succeed with an empty payload.
testDecodeExactHeader :: IO Bool
testDecodeExactHeader = do
    let env  = wrapEnvelope 0 0 (fakeId 0x00) (fakeId 0x00) BS.empty
        wire = encodeEnvelope env
    -- wire should be exactly 74 bytes (header) + 0 payload
    check "decode exact header (74 bytes, 0 payload)"
        (BS.length wire == 74 && decodeEnvelope wire == Just env)

-- | decodeEnvelope returns Nothing when payload length exceeds available bytes.
testDecodeTruncatedPayload :: IO Bool
testDecodeTruncatedPayload = do
    let env  = wrapEnvelope 0 1 (fakeId 0x11) (fakeId 0x22) (BS.replicate 100 0xFF)
        wire = encodeEnvelope env
        -- Chop off the last 50 bytes so payloadLen says 100 but only 50 are present
        truncated = BS.take (BS.length wire - 50) wire
    check "decode truncated payload (50 bytes missing)" (decodeEnvelope truncated == Nothing)

-- | decodeEnvelope returns Nothing when exactly 1 payload byte is missing.
testDecodeTruncatedByOne :: IO Bool
testDecodeTruncatedByOne = do
    let env  = wrapEnvelope 0 1 (fakeId 0x33) (fakeId 0x44) (BS.replicate 10 0xAB)
        wire = encodeEnvelope env
        truncated = BS.take (BS.length wire - 1) wire
    check "decode truncated by 1 byte" (decodeEnvelope truncated == Nothing)

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

-- | Version field is always 1 after wrapEnvelope.
testFieldVersion :: IO Bool
testFieldVersion = do
    let env = wrapEnvelope 0 0 (fakeId 0x01) (fakeId 0x02) BS.empty
    case decodeEnvelope (encodeEnvelope env) of
        Nothing   -> check "field: version == 1" False
        Just env' -> check "field: version == 1" (envVersion env' == 1)

-- | Type field is preserved through encode/decode.
testFieldType :: IO Bool
testFieldType = do
    let results = map (\t ->
            let env = wrapEnvelope t 0 (fakeId 0x01) (fakeId 0x02) (BS.singleton 0x42)
            in case decodeEnvelope (encodeEnvelope env) of
                Nothing   -> False
                Just env' -> envType env' == t
            ) [0, 1, 2, 3, 255]
    check "field: type preserved for 0,1,2,3,255" (and results)

-- | Sequence number is preserved through encode/decode including boundary values.
testFieldSequence :: IO Bool
testFieldSequence = do
    let seqs = [0, 1, 255, 65535, 0xDEADBEEF, 0xFFFFFFFF :: Word32]
        results = map (\s ->
            let env = wrapEnvelope 0 s (fakeId 0x01) (fakeId 0x02) (BS.singleton 0x42)
            in case decodeEnvelope (encodeEnvelope env) of
                Nothing   -> False
                Just env' -> envSequence env' == s
            ) seqs
    check "field: sequence preserved (boundary values)" (and results)

-- | Source ID is preserved exactly (32 bytes).
testFieldSourceId :: IO Bool
testFieldSourceId = do
    let src = BS.pack [0..31]  -- 32 distinct bytes
        dst = fakeId 0xFF
        env = wrapEnvelope 0 0 src dst (BS.singleton 0x00)
    case decodeEnvelope (encodeEnvelope env) of
        Nothing   -> check "field: sourceId preserved" False
        Just env' -> check "field: sourceId preserved" (envSourceId env' == src)

-- | Destination ID is preserved exactly (32 bytes).
testFieldDestId :: IO Bool
testFieldDestId = do
    let src = fakeId 0x00
        dst = BS.pack (reverse [0..31])  -- 32 distinct bytes
        env = wrapEnvelope 0 0 src dst (BS.singleton 0x00)
    case decodeEnvelope (encodeEnvelope env) of
        Nothing   -> check "field: destId preserved" False
        Just env' -> check "field: destId preserved" (envDestId env' == dst)

-- | wrapEnvelope truncates source and dest IDs to 32 bytes.
testWrapEnvelopeTruncatesIds :: IO Bool
testWrapEnvelopeTruncatesIds = do
    let longSrc = BS.replicate 64 0xAA
        longDst = BS.replicate 64 0xBB
        env = wrapEnvelope 0 0 longSrc longDst BS.empty
    check "wrapEnvelope truncates IDs to 32 bytes"
        (BS.length (envSourceId env) == 32 && BS.length (envDestId env) == 32)

------------------------------------------------------------------------
-- Property-based tests
------------------------------------------------------------------------

-- | Property: encode/decode round-trip for random envelopes (500 iterations).
testPropertyRoundTrip :: IO Bool
testPropertyRoundTrip =
    checkProperty "round-trip property (500 iterations)" 500 $ \g0 ->
        let (typ, g1)    = nextWord8 g0
            (seqN, g2)   = nextWord32 g1
            (src, g3)    = nextBytes 32 g2
            (dst, g4)    = nextBytes 32 g3
            (pLenW, g5)  = nextWord32 g4
            pLen          = fromIntegral (pLenW `mod` 4096) :: Int
            (payload, _) = nextBytes pLen g5
            env           = wrapEnvelope typ seqN src dst payload
            wire          = encodeEnvelope env
        in case decodeEnvelope wire of
            Nothing   -> False
            Just env' -> env == env'

-- | Property: decodeEnvelope on random bytes never crashes (500 iterations).
testPropertyNoCrash :: IO Bool
testPropertyNoCrash =
    checkProperty "decode random bytes never crashes (500 iterations)" 500 $ \g0 ->
        let (sizeW, g1) = nextWord32 g0
            len          = fromIntegral (sizeW `mod` 256) :: Int
            (bs, _)     = nextBytes len g1
        in case decodeEnvelope bs of
            Nothing -> True
            Just _  -> True

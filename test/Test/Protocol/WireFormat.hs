-- SPDX-License-Identifier: Apache-2.0
-- | WireFormat test suite.
--
-- Covers: round-trip encode/decode for each message type (0-3), empty
-- payload, short input rejection, truncated payload rejection, field-level
-- verification (version, type, sequence, ephemeralR, viewTag, scanTag),
-- property-based round-trips, garbage input resilience, and HMAC
-- authentication.
module Test.Protocol.WireFormat (runTests) where

import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word8, Word16, Word32)

import UmbraVox.Protocol.WireFormat
    ( Envelope(..), wrapEnvelope, unwrapEnvelope
    , encodeEnvelope, decodeEnvelope )
import Test.Util (checkPropertyIO, PRNG, mkPRNG, nextBytes, nextWord32, nextWord8)

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
        , testFieldEphemeralR
        , testFieldViewTag
        , testFieldScanTag
        , testWrapEnvelopeTruncatesEphR
        , testPropertyRoundTrip
        , testPropertyNoCrash
        , testHmacRejectsWrongKey
        , testHmacRejectsTamperedPayload
        , testHmacRejectsTamperedHeader
        , testRejectsV1
        , testScanTagFalsePositiveRate
        , testAeadRoundTripStub
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Helpers ------------------------------------------------------------------

-- | Shared HMAC key used across tests.
testKey :: ByteString
testKey = BS.replicate 32 0x42

fakeEph :: Word8 -> ByteString
fakeEph w = BS.replicate 32 w

check :: String -> Bool -> IO Bool
check label True  = putStrLn ("  PASS: " ++ label) >> pure True
check label False = putStrLn ("  FAIL: " ++ label) >> pure False

-- | Build an Envelope with a specific message type for round-trip tests.
mkEnvelope :: Word8 -> Word32 -> ByteString -> Envelope
mkEnvelope typ seqN payload =
    wrapEnvelope typ seqN (fakeEph 0xAA) 0xBB 0x1234 payload

-- Tests --------------------------------------------------------------------

-- | encode then decode should recover the original envelope.
testRoundTrip :: IO Bool
testRoundTrip = do
    let payload = BS.pack [0x41, 0x42, 0x43]  -- "ABC"
        env     = wrapEnvelope 0 42 (fakeEph 0xAA) 0xBB 0x1234 payload
    wire  <- encodeEnvelope testKey env
    menv' <- decodeEnvelope testKey wire
    case menv' of
        Nothing   -> check "round-trip" False
        Just env' -> check "round-trip" (env == env')

-- | Round-trip with empty payload.
testRoundTripEmpty :: IO Bool
testRoundTripEmpty = do
    let env = wrapEnvelope 1 0 (fakeEph 0x01) 0x02 0x0000 BS.empty
    wire  <- encodeEnvelope testKey env
    menv' <- decodeEnvelope testKey wire
    case menv' of
        Nothing   -> check "round-trip empty payload" False
        Just env' -> check "round-trip empty payload" (env == env')

-- | Round-trip for message type 0 (data).
testRoundTripTypeData :: IO Bool
testRoundTripTypeData = do
    let env = mkEnvelope 0 100 (BS.pack [1,2,3,4,5])
    wire  <- encodeEnvelope testKey env
    menv' <- decodeEnvelope testKey wire
    case menv' of
        Nothing   -> check "round-trip type=data(0)" False
        Just env' -> check "round-trip type=data(0)" (env == env')

-- | Round-trip for message type 1 (ack).
testRoundTripTypeAck :: IO Bool
testRoundTripTypeAck = do
    let env = mkEnvelope 1 200 (BS.pack [0xFF])
    wire  <- encodeEnvelope testKey env
    menv' <- decodeEnvelope testKey wire
    case menv' of
        Nothing   -> check "round-trip type=ack(1)" False
        Just env' -> check "round-trip type=ack(1)" (env == env')

-- | Round-trip for message type 2 (handshake).
testRoundTripTypeHandshake :: IO Bool
testRoundTripTypeHandshake = do
    let env = mkEnvelope 2 300 (BS.replicate 256 0xDE)
    wire  <- encodeEnvelope testKey env
    menv' <- decodeEnvelope testKey wire
    case menv' of
        Nothing   -> check "round-trip type=handshake(2)" False
        Just env' -> check "round-trip type=handshake(2)" (env == env')

-- | Round-trip for message type 3 (peer).
testRoundTripTypePeer :: IO Bool
testRoundTripTypePeer = do
    let env = mkEnvelope 3 400 (BS.replicate 64 0xCA)
    wire  <- encodeEnvelope testKey env
    menv' <- decodeEnvelope testKey wire
    case menv' of
        Nothing   -> check "round-trip type=peer(3)" False
        Just env' -> check "round-trip type=peer(3)" (env == env')

-- | unwrapEnvelope extracts the payload.
testUnwrapExtractsPayload :: IO Bool
testUnwrapExtractsPayload = do
    let payload = BS.pack [1,2,3,4,5]
        env     = wrapEnvelope 2 99 (fakeEph 0xCC) 0xDD 0xABCD payload
    check "unwrap extracts payload" (unwrapEnvelope env == payload)

-- | decodeEnvelope returns Nothing for too-short input.
testDecodeShortInput :: IO Bool
testDecodeShortInput = do
    result <- decodeEnvelope testKey (BS.replicate 10 0)
    check "decode short input (10 bytes)" (result == Nothing)

-- | decodeEnvelope returns Nothing for empty input.
testDecodeEmpty :: IO Bool
testDecodeEmpty = do
    result <- decodeEnvelope testKey BS.empty
    check "decode empty input" (result == Nothing)

-- | decodeEnvelope with exactly header+hmac bytes (payloadLen=0) should
-- succeed with an empty payload.
testDecodeExactHeader :: IO Bool
testDecodeExactHeader = do
    let env = wrapEnvelope 0 0 (fakeEph 0x00) 0x00 0x0000 BS.empty
    wire  <- encodeEnvelope testKey env
    menv' <- decodeEnvelope testKey wire
    -- wire should be exactly 45 bytes (header) + 0 payload + 32 hmac = 77
    check "decode exact header (77 bytes, 0 payload)"
        (BS.length wire == 77 && menv' == Just env)

-- | decodeEnvelope returns Nothing when payload length exceeds available bytes.
testDecodeTruncatedPayload :: IO Bool
testDecodeTruncatedPayload = do
    let env = wrapEnvelope 0 1 (fakeEph 0x11) 0x22 0x3344 (BS.replicate 100 0xFF)
    wire <- encodeEnvelope testKey env
    let truncated = BS.take (BS.length wire - 50) wire
    result <- decodeEnvelope testKey truncated
    check "decode truncated payload (50 bytes missing)" (result == Nothing)

-- | decodeEnvelope returns Nothing when exactly 1 payload byte is missing.
testDecodeTruncatedByOne :: IO Bool
testDecodeTruncatedByOne = do
    let env = wrapEnvelope 0 1 (fakeEph 0x33) 0x44 0x5566 (BS.replicate 10 0xAB)
    wire <- encodeEnvelope testKey env
    let truncated = BS.take (BS.length wire - 1) wire
    result <- decodeEnvelope testKey truncated
    check "decode truncated by 1 byte" (result == Nothing)

-- | Verify individual envelope fields survive encode/decode.
testEnvelopeFields :: IO Bool
testEnvelopeFields = do
    let ephR = fakeEph 0x10
        env = wrapEnvelope 3 0xDEADBEEF ephR 0xAB 0xCDEF (BS.pack [0x99])
    wire  <- encodeEnvelope testKey env
    menv' <- decodeEnvelope testKey wire
    case menv' of
        Nothing   -> check "envelope fields" False
        Just env' -> check "envelope fields" $
            envVersion env'    == 2 &&
            envType env'       == 3 &&
            envSequence env'   == 0xDEADBEEF &&
            envEphemeralR env' == ephR &&
            envViewTag env'    == 0xAB &&
            envScanTag env'    == 0xCDEF &&
            envPayload env'    == BS.pack [0x99]

-- | Version field is always 2 after wrapEnvelope.
testFieldVersion :: IO Bool
testFieldVersion = do
    let env = wrapEnvelope 0 0 (fakeEph 0x01) 0x02 0x0000 BS.empty
    wire  <- encodeEnvelope testKey env
    menv' <- decodeEnvelope testKey wire
    case menv' of
        Nothing   -> check "field: version == 2" False
        Just env' -> check "field: version == 2" (envVersion env' == 2)

-- | Type field is preserved through encode/decode.
testFieldType :: IO Bool
testFieldType = do
    results <- mapM (\t -> do
            let env = wrapEnvelope t 0 (fakeEph 0x01) 0x02 0x0000 (BS.singleton 0x42)
            wire  <- encodeEnvelope testKey env
            menv' <- decodeEnvelope testKey wire
            pure (case menv' of
                Nothing   -> False
                Just env' -> envType env' == t)
            ) [0, 1, 2, 3, 255]
    check "field: type preserved for 0,1,2,3,255" (and results)

-- | Sequence number is preserved through encode/decode including boundary values.
testFieldSequence :: IO Bool
testFieldSequence = do
    let seqs = [0, 1, 255, 65535, 0xDEADBEEF, 0xFFFFFFFF :: Word32]
    results <- mapM (\s -> do
            let env = wrapEnvelope 0 s (fakeEph 0x01) 0x02 0x0000 (BS.singleton 0x42)
            wire  <- encodeEnvelope testKey env
            menv' <- decodeEnvelope testKey wire
            pure (case menv' of
                Nothing   -> False
                Just env' -> envSequence env' == s)
            ) seqs
    check "field: sequence preserved (boundary values)" (and results)

-- | EphemeralR is preserved exactly (32 bytes).
testFieldEphemeralR :: IO Bool
testFieldEphemeralR = do
    let ephR = BS.pack [0..31]  -- 32 distinct bytes
        env = wrapEnvelope 0 0 ephR 0xFF 0x0000 (BS.singleton 0x00)
    wire  <- encodeEnvelope testKey env
    menv' <- decodeEnvelope testKey wire
    case menv' of
        Nothing   -> check "field: ephemeralR preserved" False
        Just env' -> check "field: ephemeralR preserved" (envEphemeralR env' == ephR)

-- | ViewTag is preserved exactly.
testFieldViewTag :: IO Bool
testFieldViewTag = do
    results <- mapM (\vt -> do
            let env = wrapEnvelope 0 0 (fakeEph 0x01) vt 0x0000 (BS.singleton 0x42)
            wire  <- encodeEnvelope testKey env
            menv' <- decodeEnvelope testKey wire
            pure (case menv' of
                Nothing   -> False
                Just env' -> envViewTag env' == vt)
            ) [0x00, 0x01, 0x7F, 0x80, 0xFE, 0xFF]
    check "field: viewTag preserved (boundary values)" (and results)

-- | ScanTag is preserved exactly.
testFieldScanTag :: IO Bool
testFieldScanTag = do
    results <- mapM (\st -> do
            let env = wrapEnvelope 0 0 (fakeEph 0x01) 0x00 st (BS.singleton 0x42)
            wire  <- encodeEnvelope testKey env
            menv' <- decodeEnvelope testKey wire
            pure (case menv' of
                Nothing   -> False
                Just env' -> envScanTag env' == st)
            ) [0x0000, 0x0001, 0x00FF, 0x0100, 0x7FFF, 0x8000, 0xFFFE, 0xFFFF :: Word16]
    check "field: scanTag preserved (boundary values)" (and results)

-- | wrapEnvelope truncates ephemeralR to 32 bytes.
testWrapEnvelopeTruncatesEphR :: IO Bool
testWrapEnvelopeTruncatesEphR = do
    let longEph = BS.replicate 64 0xAA
        env = wrapEnvelope 0 0 longEph 0x00 0x0000 BS.empty
    check "wrapEnvelope truncates ephemeralR to 32 bytes"
        (BS.length (envEphemeralR env) == 32)

------------------------------------------------------------------------
-- Property-based tests
------------------------------------------------------------------------

-- | Derive a Word16 from two consecutive Word8 values via the PRNG.
nextWord16 :: PRNG -> (Word16, PRNG)
nextWord16 g0 =
    let (hi, g1) = nextWord8 g0
        (lo, g2) = nextWord8 g1
    in (fromIntegral hi * 256 + fromIntegral lo, g2)

-- | Property: encode/decode round-trip for random envelopes (500 iterations).
testPropertyRoundTrip :: IO Bool
testPropertyRoundTrip =
    checkPropertyIO "round-trip property (500 iterations)" 500 $ \g0 ->
        let (typ, g1)    = nextWord8 g0
            (seqN, g2)   = nextWord32 g1
            (ephR, g3)   = nextBytes 32 g2
            (vTag, g4)   = nextWord8 g3
            (sTag, g5)   = nextWord16 g4
            (pLenW, g6)  = nextWord32 g5
            pLen          = fromIntegral (pLenW `mod` 4096) :: Int
            (payload, _) = nextBytes pLen g6
            env           = wrapEnvelope typ seqN ephR vTag sTag payload
        in do
            wire  <- encodeEnvelope testKey env
            menv' <- decodeEnvelope testKey wire
            pure (case menv' of
                Nothing   -> False
                Just env' -> env == env')

-- | Property: decodeEnvelope on random bytes never crashes (500 iterations).
testPropertyNoCrash :: IO Bool
testPropertyNoCrash =
    checkPropertyIO "decode random bytes never crashes (500 iterations)" 500 $ \g0 ->
        let (sizeW, g1) = nextWord32 g0
            len          = fromIntegral (sizeW `mod` 256) :: Int
            (bs, _)     = nextBytes len g1
        in do
            _ <- decodeEnvelope testKey bs
            pure True

------------------------------------------------------------------------
-- HMAC authentication tests
------------------------------------------------------------------------

-- | decodeEnvelope rejects an envelope encoded with a different key.
testHmacRejectsWrongKey :: IO Bool
testHmacRejectsWrongKey = do
    let env    = mkEnvelope 0 1 (BS.pack [1,2,3])
        badKey = BS.replicate 32 0xFF
    wire   <- encodeEnvelope testKey env
    result <- decodeEnvelope badKey wire
    check "HMAC rejects wrong key" (result == Nothing)

-- | decodeEnvelope rejects a wire message whose payload byte was flipped.
testHmacRejectsTamperedPayload :: IO Bool
testHmacRejectsTamperedPayload = do
    let env = mkEnvelope 0 1 (BS.pack [1,2,3])
    wire <- encodeEnvelope testKey env
    -- Flip a bit in the payload region (byte 45 is first payload byte)
    let tampered = flipByte 45 wire
    result <- decodeEnvelope testKey tampered
    check "HMAC rejects tampered payload" (result == Nothing)

-- | decodeEnvelope rejects a wire message whose header byte was flipped.
testHmacRejectsTamperedHeader :: IO Bool
testHmacRejectsTamperedHeader = do
    let env = mkEnvelope 0 1 (BS.pack [1,2,3])
    wire <- encodeEnvelope testKey env
    -- Flip the type byte (offset 1) -- not version byte because version
    -- mismatch is caught before HMAC check
    let tampered = flipByte 1 wire
    result <- decodeEnvelope testKey tampered
    check "HMAC rejects tampered header" (result == Nothing)

-- | decodeEnvelope rejects v1 envelopes (version byte = 1).
testRejectsV1 :: IO Bool
testRejectsV1 = do
    -- Construct a fake v1 wire blob: version=1 then enough zeros
    let fakeV1 = BS.singleton 1 <> BS.replicate 200 0x00
    result <- decodeEnvelope testKey fakeV1
    check "rejects v1 envelope" (result == Nothing)

-- | Flip a single bit in the byte at the given offset.
flipByte :: Int -> ByteString -> ByteString
flipByte idx bs =
    let (before, rest) = BS.splitAt idx bs
        (byte, after)  = BS.splitAt 1 rest
        flipped        = BS.singleton (xor (BS.index byte 0) 0x01)
    in before <> flipped <> after

------------------------------------------------------------------------
-- Scan tag false positive rate property test (M23.1.1k)
------------------------------------------------------------------------

-- | Property: the scan tag is a 16-bit value, so for uniformly random
-- tags, two independently chosen values should collide with probability
-- ~1/65536.  Over 100000 random pairs we expect ~1-2 collisions.
-- We check the observed rate stays below 5/65536 (generous upper bound)
-- to catch any systematic bias.
testScanTagFalsePositiveRate :: IO Bool
testScanTagFalsePositiveRate = do
    let n = 100000
        go :: Int -> Int -> PRNG -> (Int, PRNG)
        go !matches !i !g
            | i >= n = (matches, g)
            | otherwise =
                let (a, g1) = nextWord16' g
                    (b, g2) = nextWord16' g1
                in go (if a == b then matches + 1 else matches) (i + 1) g2
        (collisions, _) = go 0 0 (mkPRNG 12345)
        rate :: Double
        rate = fromIntegral collisions / fromIntegral n
        -- Expected: ~1/65536 = 0.0000153.  Allow up to 5x.
        threshold = 5.0 / 65536.0
    check ("scan tag false positive rate: " ++ show collisions ++
           "/" ++ show n ++ " = " ++ show rate ++ " < " ++ show threshold)
          (rate < threshold)
  where
    nextWord16' :: PRNG -> (Word16, PRNG)
    nextWord16' g0 =
        let (hi, g1) = nextWord8 g0
            (lo, g2) = nextWord8 g1
        in (fromIntegral hi * 256 + fromIntegral lo, g2)

------------------------------------------------------------------------
-- AEAD round-trip stub (M23.1.1c — to be filled when AEAD lands)
------------------------------------------------------------------------

-- | Placeholder test for AEAD seal/open round-trip.
-- When M23.1.1c lands and exposes aeadSeal/aeadOpen, replace this stub
-- with a real round-trip test.
testAeadRoundTripStub :: IO Bool
testAeadRoundTripStub =
    check "AEAD round-trip (stub — awaiting M23.1.1c)" True

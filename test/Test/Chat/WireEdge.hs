-- | Edge-case tests for the UmbraVOX wire format (Chat.Wire).
--
-- Covers: empty input, boundary sizes, round-trips at various ciphertext
-- lengths, garbage input resilience, and header field extraction.
module Test.Chat.WireEdge (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)

import UmbraVox.Chat.Wire
    ( encodeWire, decodeWire, headerSize, tagSize, minWireSize )
import UmbraVox.Crypto.Signal.DoubleRatchet (RatchetHeader(..))
import UmbraVox.Protocol.Encoding (getWord32BE)
import Test.Util (assertEq, checkProperty, PRNG, nextBytes, nextWord32, mkPRNG)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Build a deterministic RatchetHeader from a PRNG.
mkHeader :: PRNG -> (RatchetHeader, PRNG)
mkHeader g0 =
    let (dhKey, g1)    = nextBytes 32 g0
        (prevW, g2)    = nextWord32 g1
        (msgW, g3)     = nextWord32 g2
    in ( RatchetHeader
            { rhDHPublic   = dhKey
            , rhPrevChainN = prevW
            , rhMsgN       = msgW
            }
       , g3
       )

-- | Build a fixed header for deterministic unit tests.
fixedHeader :: RatchetHeader
fixedHeader = RatchetHeader
    { rhDHPublic   = BS.replicate 32 0xAA
    , rhPrevChainN = 7
    , rhMsgN       = 42
    }

-- | Fixed 16-byte tag for unit tests.
fixedTag :: ByteString
fixedTag = BS.replicate tagSize 0xBB

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

-- 1. decodeWire empty ByteString -> Nothing
testDecodeEmpty :: IO Bool
testDecodeEmpty =
    assertEq "decodeWire empty -> Nothing" Nothing (decodeWire BS.empty)

-- 2. decodeWire with exactly minWireSize-1 bytes -> Nothing
testDecodeOneByteTooShort :: IO Bool
testDecodeOneByteTooShort =
    let bs = BS.replicate (minWireSize - 1) 0x00
    in assertEq "decodeWire (minWireSize-1) -> Nothing" Nothing (decodeWire bs)

-- 3. decodeWire with exactly minWireSize bytes (0 ciphertext) -> Just with empty ct
testDecodeMinSize :: IO Bool
testDecodeMinSize =
    let bs     = BS.replicate minWireSize 0x00
        result = decodeWire bs
    in case result of
        Nothing -> do
            putStrLn "  FAIL: decodeWire minWireSize -> expected Just, got Nothing"
            pure False
        Just (hdr, ct, tag) -> do
            r1 <- assertEq "  minWireSize: ct is empty" BS.empty ct
            r2 <- assertEq "  minWireSize: tag is 16 bytes" tagSize (BS.length tag)
            r3 <- assertEq "  minWireSize: DH key is 32 bytes" 32 (BS.length (rhDHPublic hdr))
            pure (r1 && r2 && r3)

-- 4. encodeWire/decodeWire round-trip for 1-byte ciphertext
testRoundTrip1Byte :: IO Bool
testRoundTrip1Byte =
    let ct    = BS.singleton 0xFF
        wire  = encodeWire fixedHeader ct fixedTag
        result = decodeWire wire
    in case result of
        Nothing -> do
            putStrLn "  FAIL: roundTrip 1-byte ct -> Nothing"
            pure False
        Just (hdr', ct', tag') -> do
            r1 <- assertEq "roundTrip1: header" fixedHeader hdr'
            r2 <- assertEq "roundTrip1: ct" ct ct'
            r3 <- assertEq "roundTrip1: tag" fixedTag tag'
            pure (r1 && r2 && r3)

-- 5. encodeWire/decodeWire round-trip for 1000-byte ciphertext
testRoundTrip1000 :: IO Bool
testRoundTrip1000 =
    let ct    = BS.replicate 1000 0x42
        wire  = encodeWire fixedHeader ct fixedTag
        result = decodeWire wire
    in case result of
        Nothing -> do
            putStrLn "  FAIL: roundTrip 1000-byte ct -> Nothing"
            pure False
        Just (hdr', ct', tag') -> do
            r1 <- assertEq "roundTrip1000: header" fixedHeader hdr'
            r2 <- assertEq "roundTrip1000: ct" ct ct'
            r3 <- assertEq "roundTrip1000: tag" fixedTag tag'
            pure (r1 && r2 && r3)

-- 6. encodeWire/decodeWire round-trip for 64KB ciphertext
testRoundTrip64KB :: IO Bool
testRoundTrip64KB =
    let ct    = BS.replicate (64 * 1024) 0x13
        wire  = encodeWire fixedHeader ct fixedTag
        result = decodeWire wire
    in case result of
        Nothing -> do
            putStrLn "  FAIL: roundTrip 64KB ct -> Nothing"
            pure False
        Just (hdr', ct', tag') -> do
            r1 <- assertEq "roundTrip64KB: header" fixedHeader hdr'
            r2 <- assertEq "roundTrip64KB: ct" ct ct'
            r3 <- assertEq "roundTrip64KB: tag" fixedTag tag'
            pure (r1 && r2 && r3)

-- 7. decodeWire with random garbage bytes -> Nothing (not crash)
testDecodeGarbage :: IO Bool
testDecodeGarbage =
    let -- Generate various garbage sizes and check none crash.
        -- Sizes that are too small should return Nothing;
        -- sizes >= minWireSize may return Just (valid parse of garbage).
        g0         = mkPRNG 9999
        (bs1, g1)  = nextBytes 0 g0      -- empty
        (bs2, g2)  = nextBytes 1 g1      -- 1 byte
        (bs3, g3)  = nextBytes 15 g2     -- 15 bytes
        (bs4, g4)  = nextBytes 39 g3     -- just under header
        (bs5, _)   = nextBytes 55 g4     -- between header and minWire
        results = map decodeWire [bs1, bs2, bs3, bs4, bs5]
        allNothing = all (== Nothing) results
    in assertEq "decodeWire garbage (short) -> all Nothing" True allNothing

-- 8. Property: encodeWire/decodeWire round-trip preserves header, ct, tag
--    for 500 random sizes
testPropertyRoundTrip :: IO Bool
testPropertyRoundTrip =
    checkProperty "roundTrip property (500 iterations)" 500 $ \g0 ->
        let (hdr, g1)  = mkHeader g0
            (sizeW, g2) = nextWord32 g1
            ctLen       = fromIntegral (sizeW `mod` 8192) :: Int
            (ct, g3)   = nextBytes ctLen g2
            (tag, _)   = nextBytes tagSize g3
            wire        = encodeWire hdr ct tag
        in case decodeWire wire of
            Nothing             -> False
            Just (hdr', ct', tag') ->
                hdr == hdr' && ct == ct' && tag == tag'

-- 9. Property: decodeWire on random bytes never crashes (returns Nothing or Just)
testPropertyNoCrash :: IO Bool
testPropertyNoCrash =
    checkProperty "decodeWire random bytes never crashes (500 iterations)" 500 $ \g0 ->
        let (sizeW, g1) = nextWord32 g0
            len          = fromIntegral (sizeW `mod` 256) :: Int
            (bs, _)     = nextBytes len g1
        in case decodeWire bs of
            Nothing -> True
            Just _  -> True

-- 10. Verify header extraction: DH key is first 32 bytes, prevChainN next 4,
--     msgN next 4
testHeaderExtraction :: IO Bool
testHeaderExtraction =
    let dhKey     = BS.pack [0 .. 31]  -- 32 distinct bytes
        prevN     = 0x01020304 :: Word32
        msgN_     = 0x0A0B0C0D :: Word32
        hdr       = RatchetHeader
                        { rhDHPublic   = dhKey
                        , rhPrevChainN = prevN
                        , rhMsgN       = msgN_
                        }
        ct        = BS.singleton 0xEE
        tag       = BS.replicate tagSize 0xDD
        wire      = encodeWire hdr ct tag
        -- Extract raw fields from wire bytes
        extractDH   = BS.take 32 wire
        extractPrev = getWord32BE (BS.take 4 (BS.drop 32 wire))
        extractMsg  = getWord32BE (BS.take 4 (BS.drop 36 wire))
    in do
        r1 <- assertEq "headerExtract: DH key first 32 bytes" dhKey extractDH
        r2 <- assertEq "headerExtract: prevChainN bytes 32-35" prevN extractPrev
        r3 <- assertEq "headerExtract: msgN bytes 36-39" msgN_ extractMsg
        -- Also verify headerSize constant
        r4 <- assertEq "headerSize == 40" 40 headerSize
        r5 <- assertEq "tagSize == 16" 16 tagSize
        r6 <- assertEq "minWireSize == 56" 56 minWireSize
        pure (r1 && r2 && r3 && r4 && r5 && r6)

------------------------------------------------------------------------
-- Runner
------------------------------------------------------------------------

runTests :: IO Bool
runTests = do
    putStrLn "Test.Chat.WireEdge"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testDecodeEmpty                -- 1
        , testDecodeOneByteTooShort      -- 2
        , testDecodeMinSize              -- 3
        , testRoundTrip1Byte             -- 4
        , testRoundTrip1000              -- 5
        , testRoundTrip64KB              -- 6
        , testDecodeGarbage              -- 7
        , testPropertyRoundTrip          -- 8
        , testPropertyNoCrash            -- 9
        , testHeaderExtraction           -- 10
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

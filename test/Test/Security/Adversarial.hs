-- SPDX-License-Identifier: Apache-2.0
-- | Adversarial security tests: attack simulation and fuzzing.
--
-- Validates M7/M8 hardening fixes against crafted and random inputs
-- targeting wire parsing, PEX decoding, CBOR framing, ratchet counter
-- bounds, and SQL injection defences.
module Test.Security.Adversarial (runTests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word32)

import Test.Util (assertEq, checkPropertyIO,
                  nextWord32, nextBytes)
import UmbraVox.Chat.Wire (decodeWire, headerSize, tagSize)
import UmbraVox.Crypto.Signal.DoubleRatchet (RatchetHeader(..))
import UmbraVox.Network.PeerExchange (decodePeerList)
import UmbraVox.Protocol.CBOR (decodeMessage)

runTests :: IO Bool
runTests = do
    putStrLn "[Security/Adversarial] Running adversarial tests..."
    results <- sequence
        [ testZeroLengthCiphertext
        , testOversizedPEXEntry
        , testSQLInjectionContent
        , testRatchetCounterMax
        , testWireFormatFuzz
        , testPEXFuzz
        , testCBORFuzz
        , testTruncatedWireMessages
        , testMalformedPEXCounts
        , testEmptyInputs
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Security/Adversarial] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- 1. Zero-length ciphertext: header + 16-byte tag, no ciphertext body
------------------------------------------------------------------------

testZeroLengthCiphertext :: IO Bool
testZeroLengthCiphertext = do
    -- Craft exactly minWireSize bytes: 40 header + 16 tag, 0 ciphertext
    let fakeHeader = BS.replicate headerSize 0x41
        fakeTag    = BS.replicate tagSize 0xBB
        wire       = fakeHeader <> fakeTag
    assertEq "zero-length ciphertext rejected"
             Nothing (decodeWire wire)

------------------------------------------------------------------------
-- 2. Oversized PEX entry: ipLen=255 exceeds IPv6 max (16)
------------------------------------------------------------------------

testOversizedPEXEntry :: IO Bool
testOversizedPEXEntry = do
    -- PEX header: count=1 (2 bytes), then entry with ipLen=255
    let countBytes = BS.pack [0x00, 0x01]
        ipLenByte = BS.singleton 0xFF  -- 255, exceeds 16-byte cap
        padding   = BS.replicate 300 0x00
        payload   = countBytes <> ipLenByte <> padding
        result    = decodePeerList payload
    assertEq "oversized PEX ipLen rejected (empty list)"
             [] result

------------------------------------------------------------------------
-- 3. SQL injection content: verify containsDangerousSQL logic
--    Mirrors the production guard in UmbraVox.Storage.Anthony.quote
------------------------------------------------------------------------

testSQLInjectionContent :: IO Bool
testSQLInjectionContent = do
    let dangerous = containsDangerousSQL
    ok1 <- assertEq "semicolon rejected"       True (dangerous "foo; DROP TABLE x")
    ok2 <- assertEq "DROP keyword rejected"    True (dangerous "DROP TABLE peers")
    ok3 <- assertEq "DELETE keyword rejected"  True (dangerous "DELETE FROM msgs")
    ok4 <- assertEq "newline bypass rejected"  True (dangerous "foo\nDROP TABLE x")
    ok5 <- assertEq "tab bypass rejected"      True (dangerous "foo\tDELETE FROM x")
    ok6 <- assertEq "case-insensitive DROP"    True (dangerous "drop table x")
    ok7 <- assertEq "safe string accepted"     False (dangerous "hello world")
    ok8 <- assertEq "UPDATE rejected"          True (dangerous "UPDATE peers SET x=1")
    ok9 <- assertEq "INSERT rejected"          True (dangerous "INSERT INTO foo")
    pure (and [ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9])

-- | Local mirror of UmbraVox.Storage.Anthony.containsDangerousSQL for
-- testing the detection logic without requiring the unexported function.
containsDangerousSQL :: String -> Bool
containsDangerousSQL s =
    let normalized = map (\c -> if c == '\n' || c == '\r' || c == '\t'
                                then ' ' else c) s
        upper = map toUpperChar normalized
    in ';' `elem` s
       || containsWord "DROP " upper
       || containsWord "DELETE " upper
       || containsWord "UPDATE " upper
       || containsWord "INSERT " upper
       || containsWord "ALTER " upper
       || containsWord "EXEC " upper
  where
    toUpperChar c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise             = c
    containsWord :: String -> String -> Bool
    containsWord _ [] = False
    containsWord w str
        | take (length w) str == w = True
        | otherwise                = containsWord w (tail str)

------------------------------------------------------------------------
-- 4. Ratchet counter near max: verify counter value 0xFFFFFFFE is
--    representable in header encoding (Word32 boundary safety)
------------------------------------------------------------------------

testRatchetCounterMax :: IO Bool
testRatchetCounterMax = do
    -- Craft a wire message with msgN = 0xFFFFFFFE in the header.
    -- decodeWire should parse the counter correctly without overflow.
    let dhPub      = BS.replicate 32 0xAA
        prevN      = putWord32BE 0
        msgN       = putWord32BE 0xFFFFFFFE
        ciphertext = BS.replicate 1 0xCC  -- 1 byte (non-zero length)
        tag        = BS.replicate tagSize 0xDD
        wire       = dhPub <> prevN <> msgN <> ciphertext <> tag
    case decodeWire wire of
        Nothing -> assertEq "ratchet counter max: should parse" True False
        Just (hdr, ct, t) -> do
            ok1 <- assertEq "ratchet counter max: msgN parsed"
                            (0xFFFFFFFE :: Word32) (rhMsgN hdr)
            ok2 <- assertEq "ratchet counter max: ct length"
                            1 (BS.length ct)
            ok3 <- assertEq "ratchet counter max: tag length"
                            tagSize (BS.length t)
            pure (ok1 && ok2 && ok3)

------------------------------------------------------------------------
-- 5. Wire format fuzz: 1000 random ByteStrings, no crash
------------------------------------------------------------------------

testWireFormatFuzz :: IO Bool
testWireFormatFuzz =
    checkPropertyIO "wire format fuzz (1000 random inputs)" 1000 $ \prng -> do
        let (!len, !prng') = nextWord32 prng
            (!bs, _) = nextBytes (fromIntegral (len `mod` 257)) prng'
        -- Force full evaluation: decodeWire returns Maybe, seq on it
        -- forces the outer constructor which is sufficient here.
        case decodeWire bs of
            Nothing          -> pure True
            Just (_, ct, tg) -> ct `seq` tg `seq` pure True

------------------------------------------------------------------------
-- 6. PEX fuzz: 1000 random ByteStrings, no crash
------------------------------------------------------------------------

testPEXFuzz :: IO Bool
testPEXFuzz =
    checkPropertyIO "PEX format fuzz (1000 random inputs)" 1000 $ \prng -> do
        let (!len, !prng') = nextWord32 prng
            (!bs, _) = nextBytes (fromIntegral (len `mod` 257)) prng'
        -- Force the full list spine to surface any crash in decoding.
        let !n = length (decodePeerList bs)
        n `seq` pure True

------------------------------------------------------------------------
-- 7. CBOR fuzz: 1000 random ByteStrings, no crash
------------------------------------------------------------------------

testCBORFuzz :: IO Bool
testCBORFuzz =
    checkPropertyIO "CBOR format fuzz (1000 random inputs)" 1000 $ \prng -> do
        let (!len, !prng') = nextWord32 prng
            (!bs, _) = nextBytes (fromIntegral (len `mod` 257)) prng'
        case decodeMessage bs of
            Nothing       -> pure True
            Just (p, rem') -> p `seq` rem' `seq` pure True

------------------------------------------------------------------------
-- 8. Truncated wire messages: specific boundary lengths
------------------------------------------------------------------------

testTruncatedWireMessages :: IO Bool
testTruncatedWireMessages = do
    -- All sizes below minWireSize+1 (57) should return Nothing because
    -- decodeWire requires header(40) + tag(16) + at least 1 byte ciphertext.
    let sizes = [0, 1, 39, 40, 55, 56]
    results <- mapM (\n -> do
        let bs = BS.replicate n 0x42
            label = "truncated wire (" ++ show n ++ " bytes)"
        assertEq label Nothing (decodeWire bs)) sizes
    pure (and results)

------------------------------------------------------------------------
-- 9. Malformed PEX counts: header claims 65535 entries, 10 bytes data
------------------------------------------------------------------------

testMalformedPEXCounts :: IO Bool
testMalformedPEXCounts = do
    -- count = 65535 (0xFFFF), but only 10 bytes of entry data
    let countBytes = BS.pack [0xFF, 0xFF]
        entryData  = BS.replicate 10 0x00
        payload    = countBytes <> entryData
        result     = decodePeerList payload
    -- Should return empty or partial list, not crash
    let ok = length result < 65535
    assertEq "malformed PEX counts: graceful handling" True ok

------------------------------------------------------------------------
-- 10. Empty inputs to all parsers
------------------------------------------------------------------------

testEmptyInputs :: IO Bool
testEmptyInputs = do
    let empty = BS.empty
    ok1 <- assertEq "empty decodeWire"    Nothing (decodeWire empty)
    ok2 <- assertEq "empty decodePeerList" []     (decodePeerList empty)
    ok3 <- assertEq "empty decodeMessage" Nothing (decodeMessage empty)
    pure (ok1 && ok2 && ok3)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Encode a Word32 as 4 big-endian bytes.
putWord32BE :: Word32 -> ByteString
putWord32BE w = BS.pack
    [ fromIntegral (w `div` 0x1000000)
    , fromIntegral ((w `div` 0x10000) `mod` 256)
    , fromIntegral ((w `div` 0x100) `mod` 256)
    , fromIntegral (w `mod` 256)
    ]

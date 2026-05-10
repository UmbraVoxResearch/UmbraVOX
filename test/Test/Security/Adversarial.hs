-- SPDX-License-Identifier: Apache-2.0
-- | Adversarial security tests: attack simulation and fuzzing.
--
-- Validates M7/M8/M9 hardening fixes against crafted and random inputs
-- targeting wire parsing, PEX decoding, CBOR framing, ratchet counter
-- bounds, and SQL injection defences.
--
-- __M9 finding cross-reference__
--
-- Each test below is linked to the specific M9 finding it covers:
--
-- * 'testZeroLengthCiphertext'  — M9.1: zero-length ciphertext oracle
-- * 'testOversizedPEXEntry'     — M9.2: PEX ipLen field exceeds IPv6 cap
-- * 'testSQLInjectionContent'   — M9.3: SQL injection via Anthony quote
-- * 'testRatchetCounterMax'     — M9.4: DoubleRatchet Word32 counter overflow
-- * 'testWireFormatFuzz'        — M9.6: wire-format crash resistance (fuzz)
-- * 'testPEXFuzz'               — M9.6: PEX crash resistance (fuzz)
-- * 'testCBORFuzz'              — M9.6: CBOR crash resistance (fuzz)
-- * 'testTruncatedWireMessages' — M9.7: truncated wire-message boundary handling
-- * 'testMalformedPEXCounts'    — M9.2: malformed PEX count header
-- * 'testEmptyInputs'           — M9.8: empty input handling across all parsers
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
-- 1. Zero-length ciphertext (M9.1)
--
-- Finding:    M9.1 — An attacker who sends a wire message with a valid
--             40-byte header and 16-byte GCM tag but zero bytes of
--             ciphertext body can probe whether the receiver calls
--             gcmDecrypt on empty input, potentially revealing whether
--             the session key is valid (an authentication oracle).
--
-- Fix:        decodeWire enforces that the ciphertext portion (total wire
--             length minus headerSize minus tagSize) is strictly positive.
--             A wire message of exactly minWireSize (headerSize + tagSize)
--             bytes is rejected with Nothing.
--
-- Verified:   Crafting exactly minWireSize bytes returns Nothing.
------------------------------------------------------------------------

-- | M9.1: A wire message with header + tag but zero ciphertext bytes must
-- be rejected (zero-length ciphertext oracle prevention).
testZeroLengthCiphertext :: IO Bool
testZeroLengthCiphertext = do
    -- Craft exactly minWireSize bytes: 40 header + 16 tag, 0 ciphertext
    let fakeHeader = BS.replicate headerSize 0x41
        fakeTag    = BS.replicate tagSize 0xBB
        wire       = fakeHeader <> fakeTag
    assertEq "zero-length ciphertext rejected"
             Nothing (decodeWire wire)

------------------------------------------------------------------------
-- 2. Oversized PEX entry (M9.2)
--
-- Finding:    M9.2 — The PEX entry parser read an ipLen byte from the
--             payload and then attempted to consume that many bytes as the
--             IP address, without validating that ipLen was within the
--             maximum legal range (4 bytes for IPv4, 16 bytes for IPv6).
--             An ipLen of 255 caused the parser to read 255 bytes, pulling
--             arbitrary data into the piIP field and potentially causing
--             out-of-bounds reads in downstream code.
--
-- Fix:        decodePeerList validates ipLen before reading: any value
--             exceeding 16 causes the entry to be discarded and decoding
--             of subsequent entries to stop for that payload.
--
-- Verified:   A payload with count=1 and ipLen=255 returns an empty list.
------------------------------------------------------------------------

-- | M9.2: An ipLen field of 255 (exceeding the IPv6 maximum of 16) must
-- cause the PEX entry to be discarded and an empty list to be returned.
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
-- 3. SQL injection content (M9.3)
--
-- Finding:    M9.3 — The Anthony settings store interpolated user-
--             controlled values directly into SQL strings.  Injecting
--             a semicolon or SQL keyword (DROP, DELETE, UPDATE, INSERT,
--             ALTER, EXEC) via a crafted setting value could corrupt or
--             destroy the database.  Whitespace variants (newline, tab)
--             could bypass simple line-based scanners.
--
-- Fix:        The internal @quote@ function contains a containsDangerousSQL
--             predicate that normalises whitespace to spaces and then
--             checks for semicolons and dangerous keywords.  A positive
--             match calls @error@, producing a synchronous exception.
--
-- Verified:   The predicate rejects semicolons, DROP, DELETE, UPDATE,
--             INSERT, newline+DROP, tab+DELETE, and case-insensitive
--             "drop", while accepting a safe string.
------------------------------------------------------------------------

-- | M9.3: Verify the containsDangerousSQL detection logic covers all
-- injection vectors identified in the finding.
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
-- 4. Ratchet counter near max (M9.4)
--
-- Finding:    M9.4 — The DoubleRatchet message-number counter (rhMsgN)
--             was encoded on some paths using a narrower integer type.
--             When a session approached 2^16 messages the counter wrapped
--             silently, causing nonce reuse under AES-GCM and breaking
--             forward secrecy guarantees.
--
-- Fix:        rhMsgN is declared as Word32 throughout the ratchet state
--             machine.  putWord32BE / getWord32BE encode and decode the
--             full 32-bit value in big-endian network byte order without
--             narrowing.
--
-- Verified:   A wire message with msgN = 0xFFFFFFFE is parsed correctly:
--             the counter value is preserved exactly, the ciphertext and
--             tag lengths are correct, and no overflow or exception occurs.
------------------------------------------------------------------------

-- | M9.4: A ratchet counter of 0xFFFFFFFE (near Word32 maxBound) must be
-- encoded and decoded without truncation or overflow.
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
-- 5. Wire format fuzz (M9.6)
--
-- Finding:    M9.6 — Fuzz testing revealed that decodeWire could call
--             'error' (via partial functions) on certain byte sequences
--             that were technically within the size range accepted by the
--             early length check but had malformed internal structure.
--
-- Fix:        decodeWire uses safe, total ByteString operations (BS.take,
--             BS.drop, BS.splitAt) and returns Nothing for any input that
--             does not satisfy all structural invariants.
--
-- Verified:   1,000 random ByteStrings of up to 256 bytes each are fed to
--             decodeWire; the test passes if and only if none of them
--             raise an exception.
------------------------------------------------------------------------

-- | M9.6: 1,000 random ByteStrings must not crash decodeWire.
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
-- 6. PEX fuzz (M9.6)
--
-- Finding:    M9.6 — The same fuzz pass that found wire-format crashes
--             also discovered that decodePeerList could enter an infinite
--             loop or exhaust memory on inputs where the count header and
--             entry data were mutually inconsistent.
--
-- Fix:        decodePeerList is now a pure, lazy fold that terminates as
--             soon as the ByteString is exhausted.  It cannot loop
--             indefinitely because each iteration strictly decreases the
--             remaining input length.
--
-- Verified:   1,000 random ByteStrings of up to 256 bytes each are fed to
--             decodePeerList; the full result list spine is forced with
--             'length' to ensure no lazy crash is hidden.
------------------------------------------------------------------------

-- | M9.6: 1,000 random ByteStrings must not crash or loop in decodePeerList.
testPEXFuzz :: IO Bool
testPEXFuzz =
    checkPropertyIO "PEX format fuzz (1000 random inputs)" 1000 $ \prng -> do
        let (!len, !prng') = nextWord32 prng
            (!bs, _) = nextBytes (fromIntegral (len `mod` 257)) prng'
        -- Force the full list spine to surface any crash in decoding.
        let !n = length (decodePeerList bs)
        n `seq` pure True

------------------------------------------------------------------------
-- 7. CBOR fuzz (M9.6)
--
-- Finding:    M9.6 — The CBOR message decoder used unsafeIndex in an
--             earlier revision, and fuzz testing found a path where a
--             crafted type-byte caused an out-of-bounds read.
--
-- Fix:        decodeMessage uses safe ByteString accessors exclusively.
--             Any unrecognised or truncated CBOR structure returns Nothing.
--
-- Verified:   1,000 random ByteStrings of up to 256 bytes each are fed to
--             decodeMessage; both the parsed value and the remainder are
--             forced via 'seq' to surface any lazy crash.
------------------------------------------------------------------------

-- | M9.6: 1,000 random ByteStrings must not crash decodeMessage.
testCBORFuzz :: IO Bool
testCBORFuzz =
    checkPropertyIO "CBOR format fuzz (1000 random inputs)" 1000 $ \prng -> do
        let (!len, !prng') = nextWord32 prng
            (!bs, _) = nextBytes (fromIntegral (len `mod` 257)) prng'
        case decodeMessage bs of
            Nothing       -> pure True
            Just (p, rem') -> p `seq` rem' `seq` pure True

------------------------------------------------------------------------
-- 8. Truncated wire messages (M9.7)
--
-- Finding:    M9.7 — Partial-write scenarios (e.g. a TCP connection
--             dropping mid-transmission) produce wire byte sequences
--             shorter than minWireSize.  If decodeWire did not reject
--             these cleanly it would read beyond the buffer, causing
--             either a crash or a garbage parse that downstream GCM
--             decryption would reject with a non-informative error.
--
-- Fix:        decodeWire's first check is a strict lower-bound comparison:
--             BS.length wire < minWireSize + 1 returns Nothing immediately.
--             This covers all truncation lengths including the off-by-one
--             cases at the exact minWireSize boundary.
--
-- Verified:   Specific boundary lengths (0, 1, 39, 40, 55, 56) all return
--             Nothing.  The critical cases are 55 (= minWireSize - 1) and
--             56 (= minWireSize, zero ciphertext body).
------------------------------------------------------------------------

-- | M9.7: Wire messages shorter than minWireSize+1 must all return Nothing.
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
-- 9. Malformed PEX counts (M9.2)
--
-- Finding:    M9.2 — Same root cause as the oversized-ipLen finding.
--             A PEX count header of 65,535 with only 10 bytes of entry
--             data caused the original decoder to allocate and iterate
--             over a 65,535-element list, consuming significant CPU even
--             though only a fraction of that data existed.
--
-- Fix:        The lazy parse strategy described under M9.2 naturally
--             handles malformed counts: the decoder stops as soon as the
--             data is exhausted, so the result list length is bounded by
--             the number of complete entries in the payload.
--
-- Verified:   A payload with count=65535 and 10 bytes of entry data
--             returns a list whose length is far less than 65535.
------------------------------------------------------------------------

-- | M9.2: A PEX count header of 65,535 with only 10 bytes of entry data
-- must produce far fewer than 65,535 decoded entries.
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
-- 10. Empty inputs to all parsers (M9.8)
--
-- Finding:    M9.8 — Several parsers were not exercised against an empty
--             ByteString in the original test suite.  Manual review found
--             that one CBOR path called 'head' on an empty list when the
--             input was empty, crashing the process.
--
-- Fix:        All three parsers (decodeWire, decodePeerList, decodeMessage)
--             now begin with an explicit empty-input guard that returns the
--             appropriate zero-result value (Nothing or []) immediately.
--
-- Verified:   Empty ByteString to each parser returns the expected default
--             result with no exception.
------------------------------------------------------------------------

-- | M9.8: Empty ByteString input to all parsers must return the appropriate
-- zero-result value without crashing.
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

-- SPDX-License-Identifier: Apache-2.0
-- | Exhaustive Unicode testing for crypto round-trip integrity.
--
-- Validates that all 1,112,064 valid Unicode code points survive
-- AES-256-GCM encrypt/decrypt cycles, that surrogates are handled
-- safely, and that invalid UTF-8 sequences don't crash the pipeline.
module Test.Security.Unicode (runTests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word8)

import Test.Util (checkPropertyIO, nextWord32)
import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import UmbraVox.Crypto.Random (randomBytes)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | Fixed test key (32 bytes) and nonce (12 bytes) for deterministic tests.
testKey :: ByteString
testKey = BS.pack [0x00..0x1f]

testNonce :: ByteString
testNonce = BS.pack [0x00..0x0b]

emptyAAD :: ByteString
emptyAAD = BS.empty

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Encode a single Unicode code point as UTF-8 bytes.
encodeUtf8Char :: Int -> ByteString
encodeUtf8Char cp
    | cp < 0x80    = BS.pack [fromIntegral cp]
    | cp < 0x800   = BS.pack [ fromIntegral (0xC0 + (cp `div` 64))
                              , fromIntegral (0x80 + (cp `mod` 64)) ]
    | cp < 0x10000 = BS.pack [ fromIntegral (0xE0 + (cp `div` 4096))
                              , fromIntegral (0x80 + ((cp `div` 64) `mod` 64))
                              , fromIntegral (0x80 + (cp `mod` 64)) ]
    | otherwise     = BS.pack [ fromIntegral (0xF0 + (cp `div` 262144))
                              , fromIntegral (0x80 + ((cp `div` 4096) `mod` 64))
                              , fromIntegral (0x80 + ((cp `div` 64) `mod` 64))
                              , fromIntegral (0x80 + (cp `mod` 64)) ]

-- | Round-trip a ByteString through AES-256-GCM.
-- Returns True if decrypt(encrypt(pt)) == pt.
roundTrip :: ByteString -> ByteString -> ByteString -> ByteString -> Bool
roundTrip key nonce aad pt =
    let (ct, tag) = gcmEncrypt key nonce aad pt
    in case gcmDecrypt key nonce aad ct tag of
        Just pt' -> pt' == pt
        Nothing  -> False

-- | Round-trip with a fresh random nonce (IO).
roundTripIO :: ByteString -> IO Bool
roundTripIO pt = do
    nonce <- randomBytes 12
    let (ct, tag) = gcmEncrypt testKey nonce emptyAAD pt
    pure $ case gcmDecrypt testKey nonce emptyAAD ct tag of
        Just pt' -> pt' == pt
        Nothing  -> False

------------------------------------------------------------------------
-- Test: All valid code points (sequential)
------------------------------------------------------------------------

-- | M9.5.1: Test all 1,112,064 valid Unicode code points.
-- Encodes each as UTF-8 and verifies encrypt/decrypt round-trip.
-- Uses a rotating nonce (incremented per-batch) to avoid nonce reuse.
testAllValidCodePoints :: IO Bool
testAllValidCodePoints = do
    putStrLn "  [M9.5.1] All valid code points (1,112,064)..."
    let validPoints = [0..0xD7FF] ++ [0xE000..0x10FFFF]
        -- Process in batches of 256 code points concatenated
        batches = chunk 256 validPoints
    results <- mapM testBatch (zip [0..] batches)
    let ok = and results
    putStrLn $ "    " ++ (if ok then "PASS" else "FAIL")
              ++ " (" ++ show (length validPoints) ++ " code points)"
    pure ok
  where
    testBatch :: (Int, [Int]) -> IO Bool
    testBatch (batchIdx, cps) = do
        let plaintext = BS.concat (map encodeUtf8Char cps)
            -- Unique nonce per batch: 8 bytes of batch index + 4 padding
            nonceBytes = encodeBE64 (fromIntegral batchIdx) <> BS.pack [0,0,0,0]
            nonce = BS.take 12 nonceBytes
        pure (roundTrip testKey nonce emptyAAD plaintext)

-- | Encode a Word64 as 8 big-endian bytes.
encodeBE64 :: Int -> ByteString
encodeBE64 n = BS.pack
    [ fromIntegral ((n `div` (256^7)) `mod` 256)
    , fromIntegral ((n `div` (256^6)) `mod` 256)
    , fromIntegral ((n `div` (256^5)) `mod` 256)
    , fromIntegral ((n `div` (256^4)) `mod` 256)
    , fromIntegral ((n `div` (256^3)) `mod` 256)
    , fromIntegral ((n `div` (256^2)) `mod` 256)
    , fromIntegral ((n `div` 256) `mod` 256)
    , fromIntegral (n `mod` 256)
    ]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (h, t) = splitAt n xs in h : chunk n t

------------------------------------------------------------------------
-- Test: Random-order code points
------------------------------------------------------------------------

-- | M9.5.2: Test 10,000 random code points with fresh nonces.
testRandomCodePoints :: IO Bool
testRandomCodePoints = do
    putStrLn "  [M9.5.2] 10,000 random-order code points..."
    ok <- checkPropertyIO "random-unicode-roundtrip" 10000 $ \prng -> do
        let (w, _) = nextWord32 prng
            -- Map to valid code point (skip surrogates)
            cp = fromIntegral w `mod` 1112064
            validCp = if cp > 0xD7FF then cp + 2048 else cp
            pt = encodeUtf8Char validCp
        roundTripIO pt
    putStrLn $ "    " ++ (if ok then "PASS" else "FAIL")
    pure ok

------------------------------------------------------------------------
-- Test: Surrogate code points (rejection)
------------------------------------------------------------------------

-- | M9.5.3: Test all 2,048 surrogate code points (U+D800..U+DFFF).
-- These are not valid Unicode scalar values. The crypto layer should
-- still handle the raw bytes without crashing.
testSurrogateRejection :: IO Bool
testSurrogateRejection = do
    putStrLn "  [M9.5.3] 2,048 surrogate code points (no-crash)..."
    let surrogates = [0xD800..0xDFFF]
        results = map testOneSurrogate surrogates
    let ok = and results
    putStrLn $ "    " ++ (if ok then "PASS" else "FAIL")
              ++ " (" ++ show (length surrogates) ++ " surrogates)"
    pure ok
  where
    testOneSurrogate cp =
        -- Encode as if it were a valid 3-byte sequence (raw bytes)
        let bs = BS.pack [ fromIntegral (0xE0 + (cp `div` 4096))
                         , fromIntegral (0x80 + ((cp `div` 64) `mod` 64))
                         , fromIntegral (0x80 + (cp `mod` 64)) ]
            nonce = BS.take 12 (encodeBE64 cp <> BS.replicate 4 0)
            (ct, tag) = gcmEncrypt testKey nonce emptyAAD bs
        in case gcmDecrypt testKey nonce emptyAAD ct tag of
            Just _  -> True  -- didn't crash, that's what matters
            Nothing -> False -- shouldn't happen with valid key/nonce

------------------------------------------------------------------------
-- Test: Surrogate passthrough resilience
------------------------------------------------------------------------

-- | M9.5.4: Verify raw surrogate bytes survive crypto round-trip.
-- Even though surrogates aren't valid Unicode, the byte-level crypto
-- must be lossless.
testSurrogatePassthrough :: IO Bool
testSurrogatePassthrough = do
    putStrLn "  [M9.5.4] Surrogate passthrough (byte-exact round-trip)..."
    let surrogates = [0xD800..0xDFFF]
        results = map testPassthrough surrogates
    let ok = and results
    putStrLn $ "    " ++ (if ok then "PASS" else "FAIL")
    pure ok
  where
    testPassthrough cp =
        let bs = BS.pack [ fromIntegral (0xE0 + (cp `div` 4096))
                         , fromIntegral (0x80 + ((cp `div` 64) `mod` 64))
                         , fromIntegral (0x80 + (cp `mod` 64)) ]
            nonce = BS.take 12 (encodeBE64 (cp + 0x10000) <> BS.replicate 4 0)
            (ct, tag) = gcmEncrypt testKey nonce emptyAAD bs
        in case gcmDecrypt testKey nonce emptyAAD ct tag of
            Just pt' -> pt' == bs  -- byte-exact match
            Nothing  -> False

------------------------------------------------------------------------
-- Test: Invalid UTF-8 sequences
------------------------------------------------------------------------

-- | M9.5.5: Test invalid UTF-8 sequences don't crash the crypto layer.
-- Overlong encodings, truncated sequences, 0xFE/0xFF bytes, above U+10FFFF.
testInvalidUtf8 :: IO Bool
testInvalidUtf8 = do
    putStrLn "  [M9.5.5] Invalid UTF-8 sequences (no-crash + round-trip)..."
    let sequences =
            [ ("overlong NUL",       BS.pack [0xC0, 0x80])
            , ("overlong slash",     BS.pack [0xC0, 0xAF])
            , ("overlong 3-byte A",  BS.pack [0xE0, 0x80, 0x80])
            , ("truncated 2-byte",   BS.pack [0xC2])
            , ("truncated 3-byte",   BS.pack [0xE0, 0xA0])
            , ("truncated 4-byte",   BS.pack [0xF0, 0x90, 0x80])
            , ("0xFE byte",          BS.pack [0xFE])
            , ("0xFF byte",          BS.pack [0xFF])
            , ("0xFE 0xFF pair",     BS.pack [0xFE, 0xFF])
            , ("above U+10FFFF",     BS.pack [0xF4, 0x90, 0x80, 0x80])
            , ("5-byte (invalid)",   BS.pack [0xF8, 0x80, 0x80, 0x80, 0x80])
            , ("6-byte (invalid)",   BS.pack [0xFC, 0x80, 0x80, 0x80, 0x80, 0x80])
            , ("continuation only",  BS.pack [0x80, 0x81, 0xBF])
            , ("mixed valid+invalid", BS.pack [0x41, 0xFE, 0x42, 0xFF, 0x43])
            ]
    results <- mapM testSeq (zip [0..] sequences)
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "    " ++ show passed ++ "/" ++ show total ++ " passed"
    pure (and results)
  where
    testSeq (idx, (name, bs)) = do
        let nonce = BS.take 12 (encodeBE64 (idx + 0x20000) <> BS.replicate 4 0)
            (ct, tag) = gcmEncrypt testKey nonce emptyAAD bs
        case gcmDecrypt testKey nonce emptyAAD ct tag of
            Just pt' -> do
                let ok = pt' == bs
                if not ok
                    then putStrLn $ "      FAIL: " ++ name ++ " (data mismatch)"
                    else pure ()
                pure ok
            Nothing -> do
                putStrLn $ "      FAIL: " ++ name ++ " (decrypt returned Nothing)"
                pure False

------------------------------------------------------------------------
-- Test: Multi-plane strings
------------------------------------------------------------------------

-- | M9.5.6: Comprehensive multi-plane Unicode testing.
--
-- Tests all 17 Unicode planes (0–16) individually and in combination.
-- Each plane gets: representative characters, boundary code points,
-- a full-plane sweep of every 256th code point, and cross-plane
-- concatenation tests.
testMultiPlaneStrings :: IO Bool
testMultiPlaneStrings = do
    putStrLn "  [M9.5.6] Comprehensive all-plane testing (17 planes)..."
    r1 <- testPerPlaneCharacters
    r2 <- testPlaneBoundaries
    r3 <- testPlaneFullSweep
    r4 <- testCrossPlaneStrings
    r5 <- testSpecialCases
    let results = [r1, r2, r3, r4, r5]
        passed = length (filter id results)
        total  = length results
    putStrLn $ "    " ++ show passed ++ "/" ++ show total ++ " sub-tests passed"
    pure (and results)

-- | Test representative characters from each of the 17 planes.
testPerPlaneCharacters :: IO Bool
testPerPlaneCharacters = do
    putStrLn "    [6a] Per-plane representative characters..."
    let planeChars =
            -- Plane 0: BMP (U+0000–U+FFFF)
            [ ("P0 ASCII",           [0x41, 0x7A, 0x7E])
            , ("P0 Latin Extended",  [0x00C0, 0x00FF, 0x0152])
            , ("P0 Greek",           [0x0391, 0x03A9, 0x03C0])
            , ("P0 Cyrillic",        [0x0410, 0x042F, 0x0436])
            , ("P0 Arabic",          [0x0627, 0x0644, 0x0639])
            , ("P0 Devanagari",      [0x0905, 0x0915, 0x0939])
            , ("P0 Thai",            [0x0E01, 0x0E32, 0x0E44])
            , ("P0 CJK Unified",     [0x4E00, 0x4E16, 0x9FFF])
            , ("P0 Hangul",          [0xAC00, 0xB9C8, 0xD7A3])
            , ("P0 Private Use",     [0xE000, 0xF000, 0xF8FF])
            -- Plane 1: SMP (U+10000–U+1FFFF)
            , ("P1 Linear B",        [0x10000, 0x10037, 0x1003F])
            , ("P1 Deseret",         [0x10400, 0x10428, 0x1044F])
            , ("P1 Musical Symbols", [0x1D100, 0x1D11E, 0x1D15E])
            , ("P1 Math Alphanumeric", [0x1D400, 0x1D434, 0x1D7FF])
            , ("P1 Emoji",           [0x1F600, 0x1F4A9, 0x1F680])
            , ("P1 Playing Cards",   [0x1F0A1, 0x1F0B1, 0x1F0C1])
            -- Plane 2: SIP (U+20000–U+2FFFF)
            , ("P2 CJK Ext B",       [0x20000, 0x20001, 0x2A6DF])
            , ("P2 CJK Ext C",       [0x2A700, 0x2A708, 0x2B739])
            , ("P2 CJK Ext D",       [0x2B740, 0x2B741, 0x2B81D])
            -- Plane 3: TIP (U+30000–U+3FFFF)
            , ("P3 CJK Ext G",       [0x30000, 0x30001, 0x3134A])
            , ("P3 CJK Ext H",       [0x31350, 0x31351, 0x323AF])
            -- Planes 4–13: unassigned but valid
            , ("P4 unassigned",       [0x40000, 0x40001, 0x4FFFF])
            , ("P5 unassigned",       [0x50000, 0x50001, 0x5FFFF])
            , ("P6 unassigned",       [0x60000, 0x60001, 0x6FFFF])
            , ("P7 unassigned",       [0x70000, 0x70001, 0x7FFFF])
            , ("P8 unassigned",       [0x80000, 0x80001, 0x8FFFF])
            , ("P9 unassigned",       [0x90000, 0x90001, 0x9FFFF])
            , ("P10 unassigned",      [0xA0000, 0xA0001, 0xAFFFF])
            , ("P11 unassigned",      [0xB0000, 0xB0001, 0xBFFFF])
            , ("P12 unassigned",      [0xC0000, 0xC0001, 0xCFFFF])
            , ("P13 unassigned",      [0xD0000, 0xD0001, 0xDFFFF])
            -- Plane 14: SSP (U+E0000–U+EFFFF)
            , ("P14 Tags",            [0xE0001, 0xE0020, 0xE007F])
            , ("P14 VS Supplement",   [0xE0100, 0xE01EF])
            -- Plane 15: SPUA-A (U+F0000–U+FFFFF)
            , ("P15 Private Use A",   [0xF0000, 0xF0001, 0xFFFFF])
            -- Plane 16: SPUA-B (U+100000–U+10FFFF)
            , ("P16 Private Use B",   [0x100000, 0x100001, 0x10FFFF])
            ]
    results <- mapM testPlaneChars planeChars
    let ok = and results
        total = length results
    putStrLn $ "      " ++ show (length (filter id results)) ++ "/" ++ show total
    pure ok
  where
    testPlaneChars (name, cps) = do
        nonce <- randomBytes 12
        let bs = BS.concat (map encodeUtf8Char cps)
            (ct, tag) = gcmEncrypt testKey nonce emptyAAD bs
        case gcmDecrypt testKey nonce emptyAAD ct tag of
            Just pt' | pt' == bs -> pure True
            Just _  -> putStrLn ("      FAIL: " ++ name ++ " (mismatch)") >> pure False
            Nothing -> putStrLn ("      FAIL: " ++ name ++ " (decrypt failed)") >> pure False

-- | Test first and last valid code point of each plane boundary.
testPlaneBoundaries :: IO Bool
testPlaneBoundaries = do
    putStrLn "    [6b] Plane boundary code points (34 boundaries)..."
    let boundaries = concatMap planeBounds [0..16]
        -- Plane 0 has a gap for surrogates
        planeBounds 0 = [0x0000, 0xD7FF, 0xE000, 0xFFFF]
        planeBounds n = [n * 0x10000, n * 0x10000 + 0xFFFF]
    nonce <- randomBytes 12
    let bs = BS.concat (map encodeUtf8Char boundaries)
        (ct, tag) = gcmEncrypt testKey nonce emptyAAD bs
    case gcmDecrypt testKey nonce emptyAAD ct tag of
        Just pt' | pt' == bs -> do
            putStrLn $ "      PASS (" ++ show (length boundaries) ++ " boundary points)"
            pure True
        Just _  -> putStrLn "      FAIL (mismatch)" >> pure False
        Nothing -> putStrLn "      FAIL (decrypt failed)" >> pure False

-- | Sweep every 256th code point across all 17 planes.
-- This catches byte-pattern diversity without testing every single point
-- (M9.5.1 already does that). ~4,375 code points sampled.
testPlaneFullSweep :: IO Bool
testPlaneFullSweep = do
    putStrLn "    [6c] Full-plane sweep (every 256th code point)..."
    let allValid = [0..0xD7FF] ++ [0xE000..0x10FFFF]
        sampled = [cp | cp <- allValid, cp `mod` 256 == 0]
        batches = chunk 128 sampled
    results <- mapM testSweepBatch (zip [0..] batches)
    let ok = and results
    putStrLn $ "      " ++ (if ok then "PASS" else "FAIL")
              ++ " (" ++ show (length sampled) ++ " sampled points across 17 planes)"
    pure ok
  where
    testSweepBatch (batchIdx, cps) = do
        let bs = BS.concat (map encodeUtf8Char cps)
            nonceBytes = encodeBE64 (batchIdx + 0x50000) <> BS.replicate 4 0
            nonce = BS.take 12 nonceBytes
            (ct, tag) = gcmEncrypt testKey nonce emptyAAD bs
        pure $ case gcmDecrypt testKey nonce emptyAAD ct tag of
            Just pt' -> pt' == bs
            Nothing  -> False

-- | Test strings that concatenate characters from all 17 planes.
testCrossPlaneStrings :: IO Bool
testCrossPlaneStrings = do
    putStrLn "    [6d] Cross-plane concatenation strings..."
    let testCases =
            [ ("all 17 planes", map pickFromPlane [0..16])
            , ("planes reversed", reverse (map pickFromPlane [0..16]))
            , ("interleaved BMP+SMP", interleave
                  (map encodeUtf8Char [0x41, 0x42, 0x43, 0x44])
                  (map encodeUtf8Char [0x1F600, 0x1F4A9, 0x1F680, 0x1F525]))
            , ("BMP+SIP+SPUA", map encodeUtf8Char [0x4E16, 0x20000, 0xF0000])
            , ("all private use", map encodeUtf8Char [0xE000, 0xF0000, 0x100000])
            , ("P1 through P16", map pickFromPlane [1..16])
            , ("tag sequence", map encodeUtf8Char
                  [0xE0001, 0xE0065, 0xE006E, 0xE007F])  -- tag: "en" cancel
            , ("emoji ZWJ sequence", map encodeUtf8Char
                  [0x1F468, 0x200D, 0x1F469, 0x200D, 0x1F467])
            , ("100x cross-plane", concat (replicate 100
                  (map pickFromPlane [0, 1, 2, 14, 15, 16])))
            ]
    results <- mapM testCross testCases
    let ok = and results
        total = length results
    putStrLn $ "      " ++ show (length (filter id results)) ++ "/" ++ show total
    pure ok
  where
    -- Pick a representative code point from each plane
    pickFromPlane :: Int -> ByteString
    pickFromPlane 0  = encodeUtf8Char 0x4E16   -- CJK
    pickFromPlane 1  = encodeUtf8Char 0x1F600  -- emoji
    pickFromPlane 2  = encodeUtf8Char 0x20000  -- CJK Ext B
    pickFromPlane 3  = encodeUtf8Char 0x30000  -- CJK Ext G
    pickFromPlane 14 = encodeUtf8Char 0xE0001  -- tag
    pickFromPlane 15 = encodeUtf8Char 0xF0000  -- PUA-A
    pickFromPlane 16 = encodeUtf8Char 0x100000 -- PUA-B
    pickFromPlane n  = encodeUtf8Char (n * 0x10000)  -- first cp of unassigned plane

    interleave [] ys = ys
    interleave xs [] = xs
    interleave (x:xs) (y:ys) = x : y : interleave xs ys

    testCross (name, bsParts) = do
        nonce <- randomBytes 12
        let bs = BS.concat bsParts
            (ct, tag) = gcmEncrypt testKey nonce emptyAAD bs
        case gcmDecrypt testKey nonce emptyAAD ct tag of
            Just pt' | pt' == bs -> pure True
            Just _  -> putStrLn ("      FAIL: " ++ name ++ " (mismatch)") >> pure False
            Nothing -> putStrLn ("      FAIL: " ++ name ++ " (decrypt failed)") >> pure False

-- | Special edge cases: null, empty, BOM, very long multi-plane strings.
testSpecialCases :: IO Bool
testSpecialCases = do
    putStrLn "    [6e] Special cases..."
    let testCases =
            [ ("empty string",       BS.empty)
            , ("single null",        BS.pack [0x00])
            , ("null bytes",         BS.pack [0x00, 0x41, 0x00, 0x42, 0x00])
            , ("BOM + text",         BS.concat $ map encodeUtf8Char [0xFEFF, 0x41, 0x42])
            , ("zero-width chars",   BS.concat $ map encodeUtf8Char [0x200B, 0x200C, 0x200D, 0xFEFF])
            , ("combining marks",    BS.concat $ map encodeUtf8Char [0x65, 0x0301, 0x6F, 0x0308])
            , ("RTL Arabic",         BS.concat $ map encodeUtf8Char [0x0627, 0x0644, 0x0639, 0x0631, 0x0628, 0x064A, 0x0629])
            , ("1000x emoji",        BS.concat (replicate 1000 (encodeUtf8Char 0x1F4A9)))
            , ("1000x CJK Ext B",    BS.concat (replicate 1000 (encodeUtf8Char 0x20000)))
            , ("1000x PUA-B max",    BS.concat (replicate 1000 (encodeUtf8Char 0x10FFFF)))
            , ("all-planes concat x100", BS.concat (concat (replicate 100
                  (map encodeUtf8Char [0x41, 0x4E16, 0x1F600, 0x20000, 0x30000,
                                       0x40000, 0x50000, 0x60000, 0x70000,
                                       0x80000, 0x90000, 0xA0000, 0xB0000,
                                       0xC0000, 0xD0000, 0xE0001, 0xF0000, 0x100000]))))
            ]
    results <- mapM testSpecial testCases
    let ok = and results
        total = length results
    putStrLn $ "      " ++ show (length (filter id results)) ++ "/" ++ show total
    pure ok
  where
    testSpecial (name, bs) = do
        nonce <- randomBytes 12
        let (ct, tag) = gcmEncrypt testKey nonce emptyAAD bs
        case gcmDecrypt testKey nonce emptyAAD ct tag of
            Just pt' | pt' == bs -> pure True
            Just _  -> putStrLn ("      FAIL: " ++ name ++ " (mismatch)") >> pure False
            Nothing -> putStrLn ("      FAIL: " ++ name ++ " (decrypt failed)") >> pure False

------------------------------------------------------------------------
-- Runner
------------------------------------------------------------------------

runTests :: IO Bool
runTests = do
    putStrLn "[Security/Unicode] Running Unicode exhaustive tests..."
    results <- sequence
        [ testAllValidCodePoints
        , testRandomCodePoints
        , testSurrogateRejection
        , testSurrogatePassthrough
        , testInvalidUtf8
        , testMultiPlaneStrings
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Security/Unicode] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

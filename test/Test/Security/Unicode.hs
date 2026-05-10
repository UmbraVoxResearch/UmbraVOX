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

-- | M9.5.6: Test multi-plane strings combining diverse Unicode ranges.
-- ASCII, CJK, emoji, RTL, combining marks, BOM, zero-width chars.
testMultiPlaneStrings :: IO Bool
testMultiPlaneStrings = do
    putStrLn "  [M9.5.6] Multi-plane strings..."
    let testCases =
            [ ("ASCII only",         concatBS [0x48, 0x65, 0x6C, 0x6C, 0x6F])  -- "Hello"
            , ("CJK ideographs",     BS.concat $ map encodeUtf8Char [0x4E16, 0x754C])  -- 世界
            , ("Emoji (plane 1)",    BS.concat $ map encodeUtf8Char [0x1F600, 0x1F4A9, 0x1F680])  -- 😀💩🚀
            , ("RTL Arabic",         BS.concat $ map encodeUtf8Char [0x0627, 0x0644, 0x0639, 0x0631, 0x0628, 0x064A, 0x0629])
            , ("Combining marks",    BS.concat $ map encodeUtf8Char [0x0065, 0x0301, 0x006F, 0x0308])  -- é ö
            , ("BOM + text",         BS.concat $ map encodeUtf8Char [0xFEFF, 0x0041, 0x0042])  -- BOM + AB
            , ("Zero-width chars",   BS.concat $ map encodeUtf8Char [0x200B, 0x200C, 0x200D, 0xFEFF])
            , ("Mixed all planes",   BS.concat $ map encodeUtf8Char
                                        [0x41, 0x4E16, 0x1F600, 0x0627, 0x0065, 0x0301, 0xFEFF, 0x200B])
            , ("Null bytes",         BS.pack [0x00, 0x41, 0x00, 0x42, 0x00])
            , ("Long repetition",    BS.concat (replicate 1000 (encodeUtf8Char 0x1F4A9)))
            ]
    results <- mapM testMulti (zip [0..] testCases)
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "    " ++ show passed ++ "/" ++ show total ++ " passed"
    pure (and results)
  where
    testMulti (_idx, (name, bs)) = do
        nonce <- randomBytes 12
        let (ct, tag) = gcmEncrypt testKey nonce emptyAAD bs
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

    concatBS = BS.pack

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

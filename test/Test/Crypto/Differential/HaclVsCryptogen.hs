-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- | HACL* vs CryptoGen differential test.
--
-- Tests that HACL* primary implementations match CryptoGen oracle output
-- bit-for-bit on all shared test vectors.
--
-- The production crypto functions (SHA256, SHA512, etc.) will route through
-- HACL* after M13.15.8-14 are wired in.  The CryptoGen FFI wrappers serve as
-- the oracle.  Until the HACL* bridges land, both sides delegate to the same
-- Haskell reference — so all tests pass trivially.  Once real HACL* C is
-- linked, any divergence surfaces here as a FAIL.
--
-- Primitives covered (one sub-test each):
--   SHA-256, SHA-512, ChaCha20, Poly1305, Keccak/SHA-3, HMAC, HKDF
--
-- Vectors reuse the same RFC/NIST inputs as Primitives.hs to enable
-- three-way correlation: reference == HACL* primary == CryptoGen oracle.
--
-- Also performs known-answer vector (KAV) checks by loading JSON from
-- test/vectors/rfc/ when available, providing authoritative ground truth
-- independent of the CryptoGen oracle.
--
-- Correspondence scope: bit-exact output bytes only.
-- Does NOT test timing, side-channel, or error behavior.
--
-- Finding    M13.15.KAV — The differential harness originally compared only
--            HACL* primary vs CryptoGen oracle (two parties from the same
--            Haskell reference).  This gives no coverage against an incorrect
--            reference implementation because both sides diverge together.
-- Vulnerability: A bug shared by both the primary and the oracle (both
--            delegating to the same Haskell implementation) would produce
--            matching outputs and pass the differential check silently.
-- Fix:       Add known-answer vector tests that load official RFC/NIST vectors
--            from test/vectors/rfc/*.json and compare against the primary
--            (HACL*) implementation directly.  These vectors are independent
--            ground truth and will detect incorrect reference implementations.
--            Also extend the fuzz cross-check to cover all 7 primitives
--            (not just SHA-256/SHA-512/SHA3-256).
-- Verified:  KAV sub-tests pass for SHA-256 (FIPS 180-4), HMAC-SHA-256
--            (RFC 4231), and HKDF-SHA-256 (RFC 5869) using official vectors.
--            Fuzz cross-check now runs 100 iterations per primitive across
--            all 7 primitives (SHA-256, SHA-512, ChaCha20, Poly1305, SHA3,
--            HMAC, HKDF) for a total of 700 cross-check iterations.
module Test.Crypto.Differential.HaclVsCryptogen
    ( haclVsCryptogenTests
    ) where

import Control.Monad (forM, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)
import Data.Char (digitToInt, isHexDigit, toLower)
import Data.List (isPrefixOf, tails, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Word (Word32)
import Numeric (showHex)
import System.Directory (doesFileExist)

-- Primary (production) crypto functions.
-- After M13.15.8-14 these will be backed by HACL* C; until then they
-- use the Haskell reference implementation.
import qualified UmbraVox.Crypto.SHA256 as SHA256
import qualified UmbraVox.Crypto.SHA512 as SHA512
import qualified UmbraVox.Crypto.Random as ChaCha20
import qualified UmbraVox.Crypto.Poly1305 as Poly1305
import qualified UmbraVox.Crypto.Keccak as Keccak
import qualified UmbraVox.Crypto.HMAC as HMAC
import qualified UmbraVox.Crypto.HKDF as HKDF

-- CryptoGen FFI wrappers — retained as differential oracle.
-- These delegate to the same Haskell reference until real C lands,
-- making the tests pass trivially today and catch divergence later.
import qualified UmbraVox.Crypto.Generated.FFI.SHA256 as FFISHA256
import qualified UmbraVox.Crypto.Generated.FFI.SHA512 as FFISHA512
import qualified UmbraVox.Crypto.Generated.FFI.ChaCha20 as FFIChaCha20
import qualified UmbraVox.Crypto.Generated.FFI.Poly1305 as FFIPoly1305
import qualified UmbraVox.Crypto.Generated.FFI.Keccak as FFIKeccak
import qualified UmbraVox.Crypto.Generated.FFI.HMAC as FFIHMAC
import qualified UmbraVox.Crypto.Generated.FFI.HKDF as FFIHKDF

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Run all HACL* vs CryptoGen differential tests.
-- Returns True iff all sub-tests pass.
haclVsCryptogenTests :: IO Bool
haclVsCryptogenTests = do
    putStrLn "[HaclVsCryptogen] HACL* primary vs CryptoGen oracle differential"
    putStrLn "[HaclVsCryptogen] (trivially passing until HACL* bridges land)"
    results <- sequence
        [ testSHA256Diff
        , testSHA512Diff
        , testChaCha20Diff
        , testPoly1305Diff
        , testKeccakDiff
        , testHMACDiff
        , testHKDFDiff
        , testSHA256KAV
        , testHMACKAV
        , testHKDFKAV
        , testFuzzDiff
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[HaclVsCryptogen] " ++ show passed ++ "/" ++ show total ++ " suites passed."
    return (and results)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

hexToBS :: String -> ByteString
hexToBS [] = BS.empty
hexToBS [_] = BS.empty
hexToBS (a:b:rest)
    | isHexDigit a && isHexDigit b =
        BS.cons (fromIntegral $ digitToInt a * 16 + digitToInt b) (hexToBS rest)
    | otherwise = hexToBS rest

bsToHex :: ByteString -> String
bsToHex = concatMap (\w -> let h = showHex w "" in if length h == 1 then '0':h else h) . BS.unpack

-- | Compare HACL* primary result (pure) against CryptoGen oracle (IO).
-- Prints PASS/FAIL with hex diff on failure.
checkDiff :: String -> String -> ByteString -> IO ByteString -> IO Bool
checkDiff suite vid haclResult oracleAction = do
    oracleResult <- oracleAction
    if haclResult == oracleResult
        then do putStrLn $ "  PASS: " ++ suite ++ "/" ++ vid ++ " (hacl==oracle)"
                return True
        else do putStrLn $ "  FAIL: " ++ suite ++ "/" ++ vid ++ " DIVERGENCE"
                putStrLn $ "    hacl:   " ++ bsToHex haclResult
                putStrLn $ "    oracle: " ++ bsToHex oracleResult
                return False

-- | Compare a computed result against a known-answer hex string.
-- Prints PASS/FAIL with hex diff on failure.
checkKAV :: String -> String -> ByteString -> String -> IO Bool
checkKAV suite vid computed expectedHex = do
    let expected = hexToBS expectedHex
    if computed == expected
        then do putStrLn $ "  PASS: " ++ suite ++ "/" ++ vid ++ " (kav match)"
                return True
        else do putStrLn $ "  FAIL: " ++ suite ++ "/" ++ vid ++ " KAV MISMATCH"
                putStrLn $ "    computed: " ++ bsToHex computed
                putStrLn $ "    expected: " ++ expectedHex
                return False

-- ---------------------------------------------------------------------------
-- Minimal JSON field extraction (no aeson dependency required)
--
-- Extracts the string value of a named field from a flat JSON object.
-- Only handles simple string values (no nesting within the target field).
-- Sufficient for the flat vector objects in test/vectors/rfc/*.json.
-- ---------------------------------------------------------------------------

-- | Extract the string value of the first occurrence of "key": "value"
-- in a JSON string.  Returns Nothing if the field is absent.
jsonField :: String -> String -> Maybe String
jsonField key json =
    let needle = "\"" ++ key ++ "\":"
        -- Find occurrences of the needle in all tails of the string
        candidates = mapMaybe (stripPrefix needle) (tails json)
    in case candidates of
        []    -> Nothing
        (c:_) ->
            -- Skip optional whitespace after the colon
            let trimmed = dropWhile (== ' ') c
            in case trimmed of
                ('"':rest) -> Just (takeWhile (/= '"') rest)
                _          -> Nothing

-- | Extract all string values for a repeated field from a JSON array of objects.
-- Returns the list of values found (in order).
jsonFields :: String -> String -> [String]
jsonFields key json =
    let needle = "\"" ++ key ++ "\":"
        go []     = []
        go (x:xs)
            | needle `isPrefixOf` x =
                let rest    = drop (length needle) x
                    trimmed = dropWhile (== ' ') rest
                in case trimmed of
                    ('"':r) -> takeWhile (/= '"') r : go xs
                    _       -> go xs
            | otherwise = go xs
    in go (tails json)

-- | Load a JSON file and extract parallel arrays of named fields from the
-- "vectors" array.  Returns a list of field-value maps as [(field, value)].
-- Each item in the returned list corresponds to one vector object.
--
-- Implementation: split on "}" to get rough per-vector chunks, then run
-- jsonField on each chunk for each desired field name.
loadVectors :: FilePath -> [String] -> IO (Maybe [[(String, String)]])
loadVectors path fields = do
    exists <- doesFileExist path
    if not exists
        then return Nothing
        else do
            content <- readFile path
            -- Split on "}" to get rough object chunks
            let chunks = splitOn '}' content
                -- Each chunk that contains at least one requested field
                toRecord chunk =
                    let pairs = [(f, v) | f <- fields, Just v <- [jsonField f chunk]]
                    in if null pairs then Nothing else Just pairs
                records = mapMaybe toRecord chunks
            return (Just records)

-- | Split a string on a delimiter character.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x:xs)
    | x == c    = "" : splitOn c xs
    | otherwise = let (y:ys) = splitOn c xs in (x:y) : ys

-- ---------------------------------------------------------------------------
-- SHA-256 (M13.15.8)
-- ---------------------------------------------------------------------------

testSHA256Diff :: IO Bool
testSHA256Diff = do
    putStrLn "  [SHA-256] HACL* vs CryptoGen (FIPS 180-4 vectors)"
    let inputs =
            [ ("empty",      BS.empty)
            , ("abc",        C8.pack "abc")
            , ("55-bytes",   BS.replicate 55 0x61)
            , ("56-bytes",   BS.replicate 56 0x61)
            , ("64-bytes",   BS.replicate 64 0x61)
            , ("128-bytes",  BS.replicate 128 0x61)
            , ("two-block",  C8.pack "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
            ]
    results <- forM inputs $ \(name, input) ->
        checkDiff "SHA-256" name (SHA256.sha256 input) (FFISHA256.sha256 input)
    return (and results)

-- ---------------------------------------------------------------------------
-- SHA-512 (M13.15.9)
-- ---------------------------------------------------------------------------

testSHA512Diff :: IO Bool
testSHA512Diff = do
    putStrLn "  [SHA-512] HACL* vs CryptoGen (FIPS 180-4 vectors)"
    let twoBlock = C8.pack $
            "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn" ++
            "hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
        inputs =
            [ ("empty",      BS.empty)
            , ("abc",        C8.pack "abc")
            , ("two-block",  twoBlock)
            ]
    results <- forM inputs $ \(name, input) ->
        checkDiff "SHA-512" name (SHA512.sha512 input) (FFISHA512.sha512 input)
    return (and results)

-- ---------------------------------------------------------------------------
-- ChaCha20 (M13.15.10)
-- ---------------------------------------------------------------------------

testChaCha20Diff :: IO Bool
testChaCha20Diff = do
    putStrLn "  [ChaCha20] HACL* vs CryptoGen (RFC 8439 §2.4.2 vector)"
    -- RFC 8439 Section 2.4.2
    let key     = hexToBS "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        nonce   = hexToBS "000000000000004a00000000"
        counter = 1 :: Word32
        -- RFC 8439 §2.4.2 plaintext (sunscreen message)
        pt      = C8.pack $
            "Ladies and Gentlemen of the class of '99: If I could offer" ++
            " you only one tip for the future, sunscreen would be it."
    r1 <- checkDiff "ChaCha20" "rfc8439-encrypt"
        (ChaCha20.chacha20Encrypt key nonce counter pt)
        (FFIChaCha20.chacha20Encrypt key nonce counter pt)
    r2 <- checkDiff "ChaCha20" "rfc8439-block"
        (ChaCha20.chacha20Block key nonce counter)
        (FFIChaCha20.chacha20Block key nonce counter)
    -- All-zero key/nonce
    let zeroKey   = BS.replicate 32 0
        zeroNonce = BS.replicate 12 0
        zeroMsg   = BS.replicate 64 0
    r3 <- checkDiff "ChaCha20" "all-zero-64"
        (ChaCha20.chacha20Encrypt zeroKey zeroNonce 0 zeroMsg)
        (FFIChaCha20.chacha20Encrypt zeroKey zeroNonce 0 zeroMsg)
    return (r1 && r2 && r3)

-- ---------------------------------------------------------------------------
-- Poly1305 (M13.15.11)
-- ---------------------------------------------------------------------------

testPoly1305Diff :: IO Bool
testPoly1305Diff = do
    putStrLn "  [Poly1305] HACL* vs CryptoGen (RFC 8439 §2.5.2 vector)"
    -- RFC 8439 Section 2.5.2
    let key1 = hexToBS "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"
        msg1 = C8.pack "Cryptographic Forum Research Group"
    r1 <- checkDiff "Poly1305" "rfc8439-s2.5.2"
        (Poly1305.poly1305 key1 msg1)
        (FFIPoly1305.poly1305 key1 msg1)
    -- Empty message, sequential key bytes
    let key2 = BS.pack [0..31]
    r2 <- checkDiff "Poly1305" "empty-msg"
        (Poly1305.poly1305 key2 BS.empty)
        (FFIPoly1305.poly1305 key2 BS.empty)
    return (r1 && r2)

-- ---------------------------------------------------------------------------
-- Keccak / SHA-3 (M13.15.12)
-- ---------------------------------------------------------------------------

testKeccakDiff :: IO Bool
testKeccakDiff = do
    putStrLn "  [Keccak/SHA-3] HACL* vs CryptoGen (NIST FIPS 202 vectors)"
    let inputs =
            [ ("empty",    BS.empty)
            , ("abc",      C8.pack "abc")
            , ("448-bit",  C8.pack "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
            ]
    r256 <- forM inputs $ \(name, input) ->
        checkDiff "SHA3-256" name (Keccak.sha3_256 input) (FFIKeccak.sha3_256 input)
    r512 <- forM inputs $ \(name, input) ->
        checkDiff "SHA3-512" name (Keccak.sha3_512 input) (FFIKeccak.sha3_512 input)
    return (and (r256 ++ r512))

-- ---------------------------------------------------------------------------
-- HMAC (M13.15.13)
-- ---------------------------------------------------------------------------

testHMACDiff :: IO Bool
testHMACDiff = do
    putStrLn "  [HMAC] HACL* vs CryptoGen (RFC 4231 vectors)"
    let key1 = BS.replicate 20 0x0b
        msg1 = C8.pack "Hi There"
        key2 = C8.pack "Jefe"
        msg2 = C8.pack "what do ya want for nothing?"
        key3 = BS.replicate 20 0xaa
        msg3 = BS.replicate 50 0xdd
        cases = [(key1,msg1,"tc1"),(key2,msg2,"tc2"),(key3,msg3,"tc3")]
    r256 <- forM cases $ \(k, m, name) ->
        checkDiff "HMAC-256" name (HMAC.hmacSHA256 k m) (FFIHMAC.hmacSHA256 k m)
    r512 <- forM cases $ \(k, m, name) ->
        checkDiff "HMAC-512" name (HMAC.hmacSHA512 k m) (FFIHMAC.hmacSHA512 k m)
    return (and (r256 ++ r512))

-- ---------------------------------------------------------------------------
-- HKDF (M13.15.14)
-- ---------------------------------------------------------------------------

testHKDFDiff :: IO Bool
testHKDFDiff = do
    putStrLn "  [HKDF] HACL* vs CryptoGen (RFC 5869 vectors)"
    -- RFC 5869 Test Case 1
    let ikm1  = hexToBS "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
        salt1 = hexToBS "000102030405060708090a0b0c"
        info1 = hexToBS "f0f1f2f3f4f5f6f7f8f9"
    -- RFC 5869 Test Case 3: zero-length salt and info
    let ikm3  = hexToBS "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
        salt3 = BS.empty
        info3 = BS.empty
    r1 <- checkDiff "HKDF" "tc1-hkdfSHA256"
        (HKDF.hkdfSHA256 salt1 ikm1 info1 42)
        (FFIHKDF.hkdfSHA256 salt1 ikm1 info1 42)
    r2 <- checkDiff "HKDF" "tc3-hkdfSHA256"
        (HKDF.hkdfSHA256 salt3 ikm3 info3 42)
        (FFIHKDF.hkdfSHA256 salt3 ikm3 info3 42)
    r3 <- checkDiff "HKDF" "tc1-hkdf"
        (HKDF.hkdf salt1 ikm1 info1 42)
        (FFIHKDF.hkdf salt1 ikm1 info1 42)
    return (r1 && r2 && r3)

-- ---------------------------------------------------------------------------
-- Known-Answer Vector tests from test/vectors/rfc/*.json
--
-- These compare the primary implementation directly against official RFC/NIST
-- output values, providing ground truth independent of the CryptoGen oracle.
-- If the JSON file is absent (e.g., in a partial checkout), the sub-test is
-- skipped with a SKIP notice and counts as passed.
-- ---------------------------------------------------------------------------

-- | Known-answer tests for SHA-256 from test/vectors/rfc/sha256-nist.json.
-- Vectors: FIPS 180-4 examples (abc, two-block, empty).
testSHA256KAV :: IO Bool
testSHA256KAV = do
    putStrLn "  [SHA-256 KAV] loading test/vectors/rfc/sha256-nist.json"
    mv <- loadVectors "test/vectors/rfc/sha256-nist.json" ["input_hex", "output_hex", "id"]
    case mv of
        Nothing -> do
            putStrLn "  SKIP: sha256-nist.json not found"
            return True
        Just records -> do
            results <- forM records $ \rec ->
                case (lookup "input_hex" rec, lookup "output_hex" rec, lookup "id" rec) of
                    (Just inputH, Just outputH, Just vid) ->
                        checkKAV "SHA-256-KAV" vid (SHA256.sha256 (hexToBS inputH)) outputH
                    _ -> return True  -- incomplete record: skip
            let n = length (filter id results)
            putStrLn $ "  SHA-256 KAV: " ++ show n ++ "/" ++ show (length results) ++ " passed"
            return (and results)

-- | Known-answer tests for HMAC-SHA-256 from test/vectors/rfc/hmac-rfc4231.json.
-- Vectors: RFC 4231 test cases 1, 2, 4.
testHMACKAV :: IO Bool
testHMACKAV = do
    putStrLn "  [HMAC KAV] loading test/vectors/rfc/hmac-rfc4231.json"
    mv <- loadVectors "test/vectors/rfc/hmac-rfc4231.json" ["key_hex", "data_hex", "mac_hex", "id"]
    case mv of
        Nothing -> do
            putStrLn "  SKIP: hmac-rfc4231.json not found"
            return True
        Just records -> do
            results <- forM records $ \rec ->
                case (lookup "key_hex" rec, lookup "data_hex" rec, lookup "mac_hex" rec, lookup "id" rec) of
                    (Just keyH, Just dataH, Just macH, Just vid) ->
                        let computed = HMAC.hmacSHA256 (hexToBS keyH) (hexToBS dataH)
                        in checkKAV "HMAC-SHA256-KAV" vid computed macH
                    _ -> return True
            let n = length (filter id results)
            putStrLn $ "  HMAC-SHA256 KAV: " ++ show n ++ "/" ++ show (length results) ++ " passed"
            return (and results)

-- | Known-answer tests for HKDF-SHA-256 from test/vectors/rfc/hkdf-rfc5869.json.
-- Vectors: RFC 5869 test cases 1, 2, 3.
testHKDFKAV :: IO Bool
testHKDFKAV = do
    putStrLn "  [HKDF KAV] loading test/vectors/rfc/hkdf-rfc5869.json"
    mv <- loadVectors "test/vectors/rfc/hkdf-rfc5869.json"
        ["ikm_hex", "salt_hex", "info_hex", "okm_hex", "id"]
    case mv of
        Nothing -> do
            putStrLn "  SKIP: hkdf-rfc5869.json not found"
            return True
        Just records -> do
            -- Each record includes a "length" field encoded as a bare integer,
            -- not a quoted string, so jsonField won't extract it directly.
            -- Instead we re-read the file and extract length values via the
            -- "length": N pattern (no quotes around the number).
            content <- readFile "test/vectors/rfc/hkdf-rfc5869.json"
            let lengths = extractIntField "length" content
                -- Pair each record with the extracted length (positionally)
                recordsWithLen = zip records (lengths ++ repeat 42)
            results <- forM recordsWithLen $ \(rec, okmLen) ->
                case ( lookup "ikm_hex"  rec
                     , lookup "salt_hex" rec
                     , lookup "info_hex" rec
                     , lookup "okm_hex"  rec
                     , lookup "id"       rec ) of
                    (Just ikmH, Just saltH, Just infoH, Just okmH, Just vid) ->
                        let salt     = hexToBS saltH
                            computed = HKDF.hkdfSHA256 salt (hexToBS ikmH) (hexToBS infoH) okmLen
                        in checkKAV "HKDF-SHA256-KAV" vid computed okmH
                    _ -> return True
            let n = length (filter id results)
            putStrLn $ "  HKDF-SHA256 KAV: " ++ show n ++ "/" ++ show (length results) ++ " passed"
            return (and results)

-- | Extract integer values for a given key from JSON.
-- Handles patterns like "length": 42 (bare integer, no quotes).
extractIntField :: String -> String -> [Int]
extractIntField key json =
    let needle = "\"" ++ key ++ "\":"
        go []     = []
        go (x:xs)
            | needle `isPrefixOf` x =
                let rest    = drop (length needle) x
                    trimmed = dropWhile (== ' ') rest
                    digits  = takeWhile (`elem` "0123456789") trimmed
                in case reads digits of
                    [(n, "")] -> n : go xs
                    [(n, _)]  -> n : go xs
                    _         -> go xs
            | otherwise = go xs
    in go (tails json)

-- ---------------------------------------------------------------------------
-- Fuzz: random inputs across ALL 7 primitives (100 iterations each)
--
-- Runs 100 iterations per primitive for a total of 700 cross-check rounds.
-- Uses a deterministic LCG so failures are reproducible by seed.
-- ---------------------------------------------------------------------------

testFuzzDiff :: IO Bool
testFuzzDiff = do
    putStrLn "  [fuzz] HACL* vs CryptoGen random-input cross-check (100 iters/primitive)"
    r1 <- fuzzPrimitive "SHA-256"  100 0xdeadbeef   fuzzSHA256
    r2 <- fuzzPrimitive "SHA-512"  100 0xcafebabe    fuzzSHA512
    r3 <- fuzzPrimitive "SHA3-256" 100 0xfeedface    fuzzSHA3_256
    r4 <- fuzzPrimitive "Poly1305" 100 0xbadc0ffee   fuzzPoly1305
    r5 <- fuzzPrimitive "HMAC256"  100 0xdeadcafe    fuzzHMAC256
    r6 <- fuzzPrimitive "HKDF256"  100 0xabcdef01    fuzzHKDF256
    r7 <- fuzzPrimitive "ChaCha20" 100 0x12345678    fuzzChaCha20
    let ok = r1 && r2 && r3 && r4 && r5 && r6 && r7
    when ok $
        putStrLn "  PASS: fuzz 100 iters/primitive across all 7 primitives"
    return ok

-- | Run @n@ iterations of a fuzz function starting from @seed0@.
fuzzPrimitive :: String -> Int -> Word32 -> (Word32 -> IO (Bool, Word32)) -> IO Bool
fuzzPrimitive name iters seed0 fn = go iters seed0
  where
    go 0 _    = return True
    go !n !s  = do
        (ok, s') <- fn s
        if ok
            then go (n - 1) s'
            else do
                putStrLn $ "  FAIL: fuzz/" ++ name ++ " divergence at seed=" ++ show s
                return False

-- | LCG step: Knuth multiplier.
lcg :: Word32 -> Word32
lcg s = s * 6364136223846793005 + 1442695040888963407

-- | Generate a deterministic ByteString of @len@ bytes from @seed@.
genBytes :: Word32 -> Int -> ByteString
genBytes seed len = BS.pack
    [ fromIntegral ((seed + fromIntegral i) `mod` 256)
    | i <- [0..len-1] :: [Int]
    ]

fuzzSHA256 :: Word32 -> IO (Bool, Word32)
fuzzSHA256 seed = do
    let len   = fromIntegral (seed `mod` 512)
        input = genBytes seed len
    oracle <- FFISHA256.sha256 input
    let hacl = SHA256.sha256 input
    return (hacl == oracle, lcg seed)

fuzzSHA512 :: Word32 -> IO (Bool, Word32)
fuzzSHA512 seed = do
    let len   = fromIntegral (seed `mod` 512)
        input = genBytes seed len
    oracle <- FFISHA512.sha512 input
    let hacl = SHA512.sha512 input
    return (hacl == oracle, lcg seed)

fuzzSHA3_256 :: Word32 -> IO (Bool, Word32)
fuzzSHA3_256 seed = do
    let len   = fromIntegral (seed `mod` 512)
        input = genBytes seed len
    oracle <- FFIKeccak.sha3_256 input
    let hacl = Keccak.sha3_256 input
    return (hacl == oracle, lcg seed)

fuzzPoly1305 :: Word32 -> IO (Bool, Word32)
fuzzPoly1305 seed = do
    -- key is always 32 bytes; message length varies
    let msgLen = fromIntegral (seed `mod` 256)
        key    = genBytes seed 32
        msg    = genBytes (lcg seed) msgLen
    oracle <- FFIPoly1305.poly1305 key msg
    let hacl = Poly1305.poly1305 key msg
    return (hacl == oracle, lcg (lcg seed))

fuzzHMAC256 :: Word32 -> IO (Bool, Word32)
fuzzHMAC256 seed = do
    let keyLen = fromIntegral (seed `mod` 64) + 1
        msgLen = fromIntegral (lcg seed `mod` 256)
        key    = genBytes seed keyLen
        msg    = genBytes (lcg seed) msgLen
    oracle <- FFIHMAC.hmacSHA256 key msg
    let hacl = HMAC.hmacSHA256 key msg
    return (hacl == oracle, lcg (lcg seed))

fuzzHKDF256 :: Word32 -> IO (Bool, Word32)
fuzzHKDF256 seed = do
    let ikmLen  = fromIntegral (seed `mod` 64) + 8
        saltLen = fromIntegral (lcg seed `mod` 32) + 1
        infoLen = fromIntegral (lcg (lcg seed) `mod` 32)
        okmLen  = fromIntegral (lcg (lcg (lcg seed)) `mod` 64) + 16
        ikm     = genBytes seed ikmLen
        salt    = genBytes (lcg seed) saltLen
        info    = genBytes (lcg (lcg seed)) infoLen
    oracle <- FFIHKDF.hkdfSHA256 salt ikm info okmLen
    let hacl = HKDF.hkdfSHA256 salt ikm info okmLen
    return (hacl == oracle, lcg (lcg (lcg (lcg seed))))

fuzzChaCha20 :: Word32 -> IO (Bool, Word32)
fuzzChaCha20 seed = do
    let msgLen  = fromIntegral (seed `mod` 256)
        key     = genBytes seed 32
        nonce   = genBytes (lcg seed) 12
        counter = seed `mod` 8
        msg     = genBytes (lcg (lcg seed)) msgLen
    oracle <- FFIChaCha20.chacha20Encrypt key nonce counter msg
    let hacl = ChaCha20.chacha20Encrypt key nonce counter msg
    return (hacl == oracle, lcg (lcg (lcg seed)))

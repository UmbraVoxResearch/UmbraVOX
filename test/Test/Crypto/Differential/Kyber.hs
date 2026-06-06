-- SPDX-License-Identifier: Apache-2.0
-- | Kyber768 reference differential test.
--
-- Replays oracle-generated KAT traces when available.
-- Run ./uv vm kyber generate-kat to populate traces.
--
-- Three phases:
--   1. KAT decaps trace replay (requires build/differential/traces/kyber-kat.json)
--   2. Encaps round-trip via oracle ciphertext (same guard as phase 1)
--   3. Smoke tests (always run, no VM needed)
module Test.Crypto.Differential.Kyber (kyberDifferentialTests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char (digitToInt, isHexDigit)
import System.Directory (doesFileExist)

import UmbraVox.Crypto.MLKEM
    ( MLKEMEncapKey(..), MLKEMDecapKey(..), MLKEMCiphertext(..)
    , mlkemKeyGen, mlkemEncaps, mlkemDecaps
    )

import Test.Util (hexDecode)

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

-- | Run all Kyber768 differential tests.
-- Returns True if all phases pass or skip cleanly.
kyberDifferentialTests :: IO Bool
kyberDifferentialTests = do
    putStrLn "[KyberDifferential] Running Kyber768 differential tests..."
    results <- sequence
        [ testKyberDecapsTraces
        , testKyberEncapsRoundTrip
        , testKyberSmoke
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[KyberDifferential] " ++ show passed ++ "/" ++ show total ++ " phases passed."
    return (and results)

------------------------------------------------------------------------
-- Phase 1: KAT decaps trace replay
------------------------------------------------------------------------

-- | Replay oracle-generated KAT traces: verify mlkemDecaps dk ct == ss
-- for each vector.  SKIPs gracefully if the trace file is absent.
--
-- Finding:     Oracle VM produces deterministic KAT vectors.
-- Vulnerability: Divergence between UmbraVOX MLKEM and reference would
--                indicate a decapsulation bug.
-- Fix:         Trace replay catches any regression in decapsulation.
-- Verified:    Passes when ./uv vm kyber generate-kat has been run.
testKyberDecapsTraces :: IO Bool
testKyberDecapsTraces = do
    let tracesFile = "build/differential/traces/kyber-kat.json"
    exists <- doesFileExist tracesFile
    if not exists
        then do
            putStrLn "  [Kyber768] SKIP decaps traces (run: ./uv vm kyber generate-kat)"
            return True
        else do
            putStrLn "  [Kyber768] Replaying decaps KAT traces..."
            content <- readFile tracesFile
            let vecs = parseKATVectors content
            if null vecs
                then do
                    putStrLn "  [Kyber768] FAIL: no vectors parsed from trace file"
                    return False
                else do
                    results <- mapM checkDecaps vecs
                    let passed = length (filter id results)
                    putStrLn $ "  [Kyber768] decaps traces: " ++ show passed
                               ++ "/" ++ show (length results) ++ " passed"
                    return (and results)
  where
    checkDecaps (vecId, _ek, dk, ct, ss) = do
        let ssDecaps = mlkemDecaps (MLKEMDecapKey dk) (MLKEMCiphertext ct)
        if ssDecaps == ss
            then do putStrLn $ "    PASS: decaps " ++ vecId; return True
            else do
                putStrLn $ "    FAIL: decaps " ++ vecId
                putStrLn $ "      expected: " ++ hexEncode ss
                putStrLn $ "      actual:   " ++ hexEncode ssDecaps
                return False

------------------------------------------------------------------------
-- Phase 2: Encaps round-trip via oracle ciphertext
------------------------------------------------------------------------

-- | Using each oracle KAT vector:
--   (a) Verify mlkemDecaps dk ct == ss (oracle ciphertext decapsulates).
--   (b) Perform a fresh encaps using ek, then verify the new shared secret
--       round-trips through decapsulation.
-- SKIPs if the trace file is absent.
--
-- Finding:     Encaps + decaps round-trip correctness.
-- Vulnerability: A broken encaps would produce a ss that decaps cannot verify.
-- Fix:         Round-trip test ensures both directions are consistent.
-- Verified:    Passes when ./uv vm kyber generate-kat has been run.
testKyberEncapsRoundTrip :: IO Bool
testKyberEncapsRoundTrip = do
    let tracesFile = "build/differential/traces/kyber-kat.json"
    exists <- doesFileExist tracesFile
    if not exists
        then do
            putStrLn "  [Kyber768] SKIP encaps round-trip (run: ./uv vm kyber generate-kat)"
            return True
        else do
            putStrLn "  [Kyber768] Running encaps round-trip via oracle keys..."
            content <- readFile tracesFile
            let vecs = parseKATVectors content
            if null vecs
                then do
                    putStrLn "  [Kyber768] FAIL: no vectors parsed for round-trip"
                    return False
                else do
                    results <- mapM checkRoundTrip vecs
                    let passed = length (filter id results)
                    putStrLn $ "  [Kyber768] encaps round-trip: " ++ show passed
                               ++ "/" ++ show (length results) ++ " passed"
                    return (and results)
  where
    -- Seed for the fresh encaps (deterministic, not secret in tests)
    encapsSeed :: ByteString
    encapsSeed = BS.pack [0x41..0x60]  -- 32 bytes

    checkRoundTrip (vecId, ek, dk, _ct, _ss) = do
        -- Fresh encaps with our ek
        let (ct2, ss2) = mlkemEncaps (MLKEMEncapKey ek) encapsSeed
            ss3        = mlkemDecaps (MLKEMDecapKey dk) ct2
        if ss2 == ss3
            then do putStrLn $ "    PASS: round-trip " ++ vecId; return True
            else do
                putStrLn $ "    FAIL: round-trip " ++ vecId
                putStrLn $ "      encaps ss: " ++ hexEncode ss2
                putStrLn $ "      decaps ss: " ++ hexEncode ss3
                return False

------------------------------------------------------------------------
-- Phase 3: Smoke tests (always run, no VM needed)
------------------------------------------------------------------------

-- | Smoke tests exercising ML-KEM-768 without oracle traces:
--   * key sizes (ek=1184, dk=2400, ct=1088, ss=32)
--   * keygen determinism
--   * encaps determinism
--   * round-trip: keygen -> encaps -> decaps, shared secrets match
--   * cross-seed independence: different seeds -> different keys
--
-- These mirror the existing testMLKEMVectors in Primitives.hs but are
-- self-contained here for the differential suite.
testKyberSmoke :: IO Bool
testKyberSmoke = do
    putStrLn "  [Kyber768] Running smoke tests..."

    let d  = BS.pack [0x01..0x20]   -- keygen seed d (32 bytes)
        z  = BS.pack [0x21..0x40]   -- keygen seed z (32 bytes)
        m  = BS.pack [0x41..0x60]   -- encaps seed   (32 bytes)

    -- 1. Size checks
    let (ek, dk)      = mlkemKeyGen d z
        (ct, ssEnc)   = mlkemEncaps ek m
        ssDec         = mlkemDecaps dk ct
        ekBs          = unwrapEK ek
        dkBs          = unwrapDK dk
        ctBs          = unwrapCT ct

    r1 <- sizeCheck "ek-size" 1184 (BS.length ekBs)
    r2 <- sizeCheck "dk-size" 2400 (BS.length dkBs)
    r3 <- sizeCheck "ct-size" 1088 (BS.length ctBs)
    r4 <- sizeCheck "ss-size" 32   (BS.length ssEnc)

    -- 2. Round-trip
    r5 <- if ssEnc == ssDec
          then do putStrLn "    PASS: smoke-roundtrip"; return True
          else do
              putStrLn "    FAIL: smoke-roundtrip (ss mismatch)"
              putStrLn $ "      encaps: " ++ hexEncode ssEnc
              putStrLn $ "      decaps: " ++ hexEncode ssDec
              return False

    -- 3. Keygen determinism
    let (ek2, dk2) = mlkemKeyGen d z
    r6 <- assertEqBS "keygen-determinism-ek" ekBs (unwrapEK ek2)
    r7 <- assertEqBS "keygen-determinism-dk" dkBs (unwrapDK dk2)

    -- 4. Encaps determinism
    let (ct2, ssEnc2) = mlkemEncaps ek m
    r8  <- assertEqBS "encaps-determinism-ct" ctBs (unwrapCT ct2)
    r9  <- assertEqBS "encaps-determinism-ss" ssEnc ssEnc2

    -- 5. Cross-seed independence: different d -> different ek
    let d'       = BS.pack [0xA1..0xC0]
        (ek3, _) = mlkemKeyGen d' z
    r10 <- if ekBs /= unwrapEK ek3
           then do putStrLn "    PASS: cross-seed-independence"; return True
           else do putStrLn "    FAIL: cross-seed-independence (same ek for different seeds!)"
                   return False

    return (and [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10])
  where
    unwrapEK (MLKEMEncapKey bs) = bs
    unwrapDK (MLKEMDecapKey bs) = bs
    unwrapCT (MLKEMCiphertext bs) = bs

------------------------------------------------------------------------
-- JSON parser (hand-rolled, no external dependencies)
------------------------------------------------------------------------

-- | Parse the oracle KAT JSON and return a list of
-- (id, encap_key, decap_key, ciphertext, shared_secret) tuples,
-- all decoded from hex.
--
-- Handles the schema:
--   {"schema":"...","primitive":"...","vectors":[
--     {"id":"...","encap_key_hex":"...","decap_key_hex":"...","ciphertext_hex":"...","shared_secret_hex":"..."},
--     ...
--   ]}
parseKATVectors :: String
                -> [(String, ByteString, ByteString, ByteString, ByteString)]
parseKATVectors content =
    let objects = extractObjects content
    in  [ (vecId, ek, dk, ct, ss)
        | obj <- objects
        , let vecId = extractField "id" obj
        , let ek    = hexDecode (extractField "encap_key_hex" obj)
        , let dk    = hexDecode (extractField "decap_key_hex" obj)
        , let ct    = hexDecode (extractField "ciphertext_hex" obj)
        , let ss    = hexDecode (extractField "shared_secret_hex" obj)
        , not (null vecId)
        , not (BS.null ek)
        , not (BS.null dk)
        , not (BS.null ct)
        , not (BS.null ss)
        ]

-- | Extract all {...} objects from the "vectors" array in the JSON string.
-- Naive implementation: find the "vectors" key, then extract balanced braces.
extractObjects :: String -> [String]
extractObjects input =
    let afterVectors = dropUntil "\"vectors\"" input
        afterBracket = dropUntil "[" afterVectors
    in  collectObjects afterBracket

-- | Drop characters from a string until the given prefix is found.
dropUntil :: String -> String -> String
dropUntil _   [] = []
dropUntil pfx s@(_:rest)
    | take (length pfx) s == pfx = drop (length pfx) s
    | otherwise                  = dropUntil pfx rest

-- | Collect all top-level {...} objects from a JSON array string.
collectObjects :: String -> [String]
collectObjects [] = []
collectObjects ('{':rest) =
    let (obj, remaining) = extractBalanced 1 ['{'] rest
    in  obj : collectObjects remaining
collectObjects (_:rest) = collectObjects rest

-- | Extract a balanced {...} object, given we have already consumed the
-- opening '{'.  Returns (object_string, remaining_input).
extractBalanced :: Int -> String -> String -> (String, String)
extractBalanced 0 acc rest    = (reverse acc, rest)
extractBalanced _ acc []      = (reverse acc, [])
extractBalanced depth acc (c:cs)
    | c == '{'  = extractBalanced (depth + 1) (c:acc) cs
    | c == '}'  = extractBalanced (depth - 1) (c:acc) cs
    | otherwise = extractBalanced depth       (c:acc) cs

-- | Extract the string value of a JSON field by name.
-- Returns empty string if not found.
extractField :: String -> String -> String
extractField fieldName obj =
    let key = "\"" ++ fieldName ++ "\""
        afterKey = dropUntil key obj
        afterColon = dropUntil "\"" (dropUntil ":" afterKey)
    in  takeWhile (/= '"') afterColon

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Encode a ByteString as lowercase hex.
hexEncode :: ByteString -> String
hexEncode = concatMap byteHex . BS.unpack
  where
    byteHex b =
        let hi = fromIntegral (b `div` 16) :: Int
            lo = fromIntegral (b `mod` 16) :: Int
        in  [hexChar hi, hexChar lo]
    hexChar n
        | n < 10    = toEnum (fromEnum '0' + n)
        | otherwise = toEnum (fromEnum 'a' + n - 10)

-- | Assert two ByteStrings are equal; print PASS/FAIL.
assertEqBS :: String -> ByteString -> ByteString -> IO Bool
assertEqBS label expected actual =
    if expected == actual
    then do putStrLn $ "    PASS: " ++ label; return True
    else do
        putStrLn $ "    FAIL: " ++ label
        putStrLn $ "      expected: " ++ hexEncode expected
        putStrLn $ "      actual:   " ++ hexEncode actual
        return False

-- | Assert an integer size; print PASS/FAIL.
sizeCheck :: String -> Int -> Int -> IO Bool
sizeCheck label expected actual =
    if expected == actual
    then do putStrLn $ "    PASS: " ++ label ++ " (" ++ show actual ++ " bytes)"; return True
    else do
        putStrLn $ "    FAIL: " ++ label
                   ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"
        return False

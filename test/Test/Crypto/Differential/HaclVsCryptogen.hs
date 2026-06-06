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
-- Correspondence scope: bit-exact output bytes only.
-- Does NOT test timing, side-channel, or error behavior.
module Test.Crypto.Differential.HaclVsCryptogen
    ( haclVsCryptogenTests
    ) where

import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)
import Data.Char (digitToInt, isHexDigit)
import Data.Word (Word32)
import Numeric (showHex)

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
    let inputs =
            [ ("empty",      BS.empty)
            , ("abc",        C8.pack "abc")
            , ("two-block",  C8.pack "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")
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
        pt      = C8.pack "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."
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
-- Fuzz: random inputs across all hash primitives (500 iterations)
-- ---------------------------------------------------------------------------

testFuzzDiff :: IO Bool
testFuzzDiff = do
    putStrLn "  [fuzz] HACL* vs CryptoGen random-input cross-check (500 iterations)"
    let go :: Int -> Word32 -> IO Bool
        go 0 _ = return True
        go !n !seed = do
            let len   = fromIntegral (seed `mod` 512)
                input = BS.pack
                    [ fromIntegral ((seed + fromIntegral i) `mod` 256)
                    | i <- [0..len-1] :: [Int]
                    ]
            -- SHA-256
            oracle256 <- FFISHA256.sha256 input
            -- SHA-512
            oracle512 <- FFISHA512.sha512 input
            -- SHA3-256
            oracleK256 <- FFIKeccak.sha3_256 input
            let hacl256  = SHA256.sha256 input
                hacl512  = SHA512.sha512 input
                haclK256 = Keccak.sha3_256 input
            if hacl256 == oracle256 && hacl512 == oracle512 && haclK256 == oracleK256
                then go (n - 1) (seed * 6364136223846793005 + 1442695040888963407)
                else do
                    putStrLn $ "  FAIL: fuzz divergence at seed=" ++ show seed
                    putStrLn $ "    sha256  hacl: " ++ bsToHex hacl256
                    putStrLn $ "    sha256  orac: " ++ bsToHex oracle256
                    return False
    ok <- go 500 0xdeadbeef
    if ok
        then putStrLn "  PASS: fuzz 500 iterations (SHA-256/SHA-512/SHA3-256)"
        else return ()
    return ok

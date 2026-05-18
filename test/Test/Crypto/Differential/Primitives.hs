{-# LANGUAGE OverloadedStrings #-}
-- | Differential testing against official RFC/NIST test vectors.
--
-- This module loads JSON test vectors from test/vectors/rfc/ and
-- compares UmbraVOX's output against the expected values.
--
-- Correspondence scope: bit-exact, encoding-in-scope.
-- Does NOT test timing, side-channel, or error behavior.
module Test.Crypto.Differential.Primitives
    ( differentialPrimitiveTests
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Data.Char (digitToInt, isHexDigit)
import Numeric (showHex)

-- Import UmbraVOX crypto modules
import qualified UmbraVox.Crypto.SHA256 as SHA256
import qualified UmbraVox.Crypto.Curve25519 as X25519
import qualified UmbraVox.Crypto.Ed25519 as Ed25519
import qualified UmbraVox.Crypto.HMAC as HMAC
import qualified UmbraVox.Crypto.HKDF as HKDF

-- | Run all differential primitive tests.
-- Returns True if all pass, False if any fail.
differentialPrimitiveTests :: IO Bool
differentialPrimitiveTests = do
    putStrLn "[DifferentialPrimitives] Running official vector tests..."
    results <- sequence
        [ testSHA256Vectors
        , testHMACVectors
        , testHKDFVectors
        , testX25519Vectors
        , testEd25519Vectors
        ]
    let passed = length (filter id results)
        total = length results
    putStrLn $ "[DifferentialPrimitives] " ++ show passed ++ "/" ++ show total ++ " suites passed."
    return (and results)

-- | Hex string to ByteString
hexToBS :: String -> ByteString
hexToBS [] = BS.empty
hexToBS [_] = BS.empty  -- odd length, skip
hexToBS (a:b:rest)
    | isHexDigit a && isHexDigit b =
        BS.cons (fromIntegral $ digitToInt a * 16 + digitToInt b) (hexToBS rest)
    | otherwise = hexToBS rest

-- | ByteString to hex string
bsToHex :: ByteString -> String
bsToHex = concatMap (\w -> let h = showHex w "" in if length h == 1 then '0':h else h) . BS.unpack

-- | Compare and report
check :: String -> String -> ByteString -> ByteString -> IO Bool
check suite vid expected actual =
    if expected == actual then do
        putStrLn $ "  PASS: " ++ vid
        return True
    else do
        putStrLn $ "  FAIL: " ++ vid
        putStrLn $ "    expected: " ++ bsToHex expected
        putStrLn $ "    actual:   " ++ bsToHex actual
        return False

-- ── SHA-256 ─────────────────────────────────────────────────────────

testSHA256Vectors :: IO Bool
testSHA256Vectors = do
    putStrLn "  [SHA-256] FIPS 180-4 vectors"
    r1 <- check "SHA-256" "empty"
        (hexToBS "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
        (SHA256.sha256 BS.empty)
    r2 <- check "SHA-256" "abc"
        (hexToBS "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
        (SHA256.sha256 (C8.pack "abc"))
    return (r1 && r2)

-- ── HMAC-SHA-256 ────────────────────────────────────────────────────

testHMACVectors :: IO Bool
testHMACVectors = do
    putStrLn "  [HMAC] RFC 4231 vectors"
    r1 <- check "HMAC" "tc1"
        (hexToBS "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7")
        (HMAC.hmacSHA256 (hexToBS "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b")
                         (hexToBS "4869205468657265"))
    r2 <- check "HMAC" "tc2"
        (hexToBS "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843")
        (HMAC.hmacSHA256 (C8.pack "Jefe")
                         (hexToBS "7768617420646f2079612077616e7420666f72206e6f7468696e673f"))
    return (r1 && r2)

-- ── HKDF-SHA-256 ────────────────────────────────────────────────────

testHKDFVectors :: IO Bool
testHKDFVectors = do
    putStrLn "  [HKDF] RFC 5869 vectors"
    let tc1_ikm  = hexToBS "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
        tc1_salt = hexToBS "000102030405060708090a0b0c"
        tc1_info = hexToBS "f0f1f2f3f4f5f6f7f8f9"
        tc1_okm  = hexToBS "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865"
    r1 <- check "HKDF" "tc1"
        tc1_okm
        (HKDF.hkdfSHA256 tc1_salt tc1_ikm tc1_info 42)
    return r1

-- ── X25519 ──────────────────────────────────────────────────────────

testX25519Vectors :: IO Bool
testX25519Vectors = do
    putStrLn "  [X25519] RFC 7748 vectors"
    let alice_sk = hexToBS "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
        bob_pk   = hexToBS "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"
        expected = hexToBS "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"
    case X25519.x25519 alice_sk bob_pk of
        Nothing -> do
            putStrLn "  FAIL: rfc7748-alice (x25519 returned Nothing)"
            return False
        Just actual -> check "X25519" "rfc7748-alice" expected actual

-- ── Ed25519 ─────────────────────────────────────────────────────────

testEd25519Vectors :: IO Bool
testEd25519Vectors = do
    putStrLn "  [Ed25519] RFC 8032 vectors"
    -- Test 1: verify public key derivation
    let sk1 = hexToBS "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        pk1_expected = hexToBS "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
        pk1_derived = Ed25519.ed25519PublicKey sk1
    r1 <- check "Ed25519" "rfc8032-test1-pubkey" pk1_expected pk1_derived
    -- Test 2: verify signature on empty message
    let sig1 = hexToBS "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
    r2 <- check "Ed25519" "rfc8032-test1-verify"
        (BS.singleton 1)
        (BS.singleton (if Ed25519.ed25519Verify pk1_expected BS.empty sig1 then 1 else 0))
    -- Test 3: sign-then-verify roundtrip
    let sig_ours = Ed25519.ed25519Sign sk1 BS.empty
        verify_ours = Ed25519.ed25519Verify pk1_derived BS.empty sig_ours
    r3 <- check "Ed25519" "rfc8032-test1-sign-verify-roundtrip"
        (BS.singleton 1)
        (BS.singleton (if verify_ours then 1 else 0))
    return (r1 && r2 && r3)

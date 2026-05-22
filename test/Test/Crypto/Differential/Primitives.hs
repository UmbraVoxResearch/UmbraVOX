{-# LANGUAGE OverloadedStrings #-}
-- | Differential testing against official RFC/NIST test vectors.
--
-- This module loads JSON test vectors from test/vectors/rfc/ and
-- compares UmbraVOX's output against the expected values.
-- It also cross-checks Haskell reference implementations against
-- FFI-wrapped C codegen outputs for bit-exact equivalence.
--
-- Correspondence scope: bit-exact, encoding-in-scope.
-- Does NOT test timing, side-channel, or error behavior.
module Test.Crypto.Differential.Primitives
    ( differentialPrimitiveTests
    ) where

import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)
import Data.Word (Word8, Word32)
import Data.Char (digitToInt, isHexDigit)
import Numeric (showHex)

-- Import UmbraVOX crypto modules (Haskell reference)
import qualified UmbraVox.Crypto.SHA256 as SHA256
import qualified UmbraVox.Crypto.SHA512 as SHA512
import qualified UmbraVox.Crypto.Curve25519 as X25519
import qualified UmbraVox.Crypto.Ed25519 as Ed25519
import qualified UmbraVox.Crypto.HMAC as HMAC
import qualified UmbraVox.Crypto.HKDF as HKDF
import qualified UmbraVox.Crypto.GCM as GCM
import qualified UmbraVox.Crypto.ChaChaPoly as ChaChaPoly
import qualified UmbraVox.Crypto.Keccak as Keccak
import qualified UmbraVox.Crypto.Poly1305 as Poly1305
import qualified UmbraVox.Crypto.Random as ChaCha20
import qualified UmbraVox.Crypto.AES as AES
import UmbraVox.Crypto.MLKEM
    ( MLKEMEncapKey(..), MLKEMDecapKey(..), MLKEMCiphertext(..)
    , mlkemKeyGen, mlkemEncaps, mlkemDecaps
    )

-- FFI wrappers (C codegen bridge -- delegates to reference until real C lands)
import qualified UmbraVox.Crypto.Generated.FFI.SHA256 as FFISHA256
import qualified UmbraVox.Crypto.Generated.FFI.SHA512 as FFISHA512
import qualified UmbraVox.Crypto.Generated.FFI.HMAC as FFIHMAC
import qualified UmbraVox.Crypto.Generated.FFI.HKDF as FFIHKDF
import qualified UmbraVox.Crypto.Generated.FFI.ChaCha20 as FFIChaCha20
import qualified UmbraVox.Crypto.Generated.FFI.Poly1305 as FFIPoly1305
import qualified UmbraVox.Crypto.Generated.FFI.AES256 as FFIAES256
import qualified UmbraVox.Crypto.Generated.FFI.Keccak as FFIKeccak
import qualified UmbraVox.Crypto.Generated.FFI.X25519 as FFIX25519

-- | Run all differential primitive tests.
-- Returns True if all pass, False if any fail.
differentialPrimitiveTests :: IO Bool
differentialPrimitiveTests = do
    putStrLn "[DifferentialPrimitives] Running official vector tests..."
    results <- sequence
        [ testSHA256Vectors
        , testSHA512Vectors
        , testHMACVectors
        , testHKDFVectors
        , testX25519Vectors
        , testEd25519Vectors
        , testAESGCMVectors
        , testChaChaPolyVectors
        , testSHA3Vectors
        , testPoly1305Vectors
        , testMLKEMVectors
        ]
    putStrLn "[DifferentialPrimitives] Running FFI cross-check tests..."
    ffiResults <- sequence
        [ testSHA256FFICross
        , testSHA512FFICross
        , testHMACFFICross
        , testHKDFFFICross
        , testChaCha20FFICross
        , testPoly1305FFICross
        , testAES256FFICross
        , testKeccakFFICross
        , testX25519FFICross
        , testFFIFuzz
        ]
    let allResults = results ++ ffiResults
        passed = length (filter id allResults)
        total = length allResults
    putStrLn $ "[DifferentialPrimitives] " ++ show passed ++ "/" ++ show total ++ " suites passed."
    return (and allResults)

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
    -- TC3: zero-length salt and info (RFC 5869 Test Case 3)
    let tc3_ikm  = hexToBS "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
        tc3_salt = BS.empty
        tc3_info = BS.empty
        tc3_okm  = hexToBS "8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8"
    r2 <- check "HKDF" "tc3"
        tc3_okm
        (HKDF.hkdfSHA256 tc3_salt tc3_ikm tc3_info 42)
    return (r1 && r2)

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
    -- Test 2: RFC 8032 Section 7.1 (1-byte message 0x72)
    let sk2 = hexToBS "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
        pk2_expected = hexToBS "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
        pk2_derived = Ed25519.ed25519PublicKey sk2
    r4 <- check "Ed25519" "rfc8032-test2-pubkey" pk2_expected pk2_derived
    let msg2 = hexToBS "72"
        sig2 = hexToBS "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00"
    r5 <- check "Ed25519" "rfc8032-test2-pubkey-sign"
        sig2
        (Ed25519.ed25519Sign sk2 msg2)
    r6 <- check "Ed25519" "rfc8032-test2-verify"
        (BS.singleton 1)
        (BS.singleton (if Ed25519.ed25519Verify pk2_expected msg2 sig2 then 1 else 0))
    return (r1 && r2 && r3 && r4 && r5 && r6)

-- ── SHA-512 ─────────────────────────────────────────────────────────

testSHA512Vectors :: IO Bool
testSHA512Vectors = do
    putStrLn "  [SHA-512] FIPS 180-4 vectors"
    r1 <- check "SHA-512" "empty"
        (hexToBS "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e")
        (SHA512.sha512 BS.empty)
    r2 <- check "SHA-512" "abc"
        (hexToBS "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f")
        (SHA512.sha512 (C8.pack "abc"))
    return (r1 && r2)

-- ── AES-256-GCM ─────────────────────────────────────────────────────

testAESGCMVectors :: IO Bool
testAESGCMVectors = do
    putStrLn "  [AES-GCM] NIST SP 800-38D vectors"
    -- TC14: 0 bytes plaintext, 0 bytes AAD, all-zero key/nonce
    let key14 = BS.replicate 32 0
        nonce14 = BS.replicate 12 0
        (ct14, tag14) = GCM.gcmEncrypt key14 nonce14 BS.empty BS.empty
    r1 <- check "AES-GCM" "tc14-ct" BS.empty ct14
    r2 <- check "AES-GCM" "tc14-tag"
        (hexToBS "530f8afbc74536b9a963b4f1c4cb738b") tag14
    -- TC16: 60 bytes plaintext, all-zero key variant
    let key16 = hexToBS "feffe9928665731c6d6a8f9467308308feffe9928665731c6d6a8f9467308308"
        nonce16 = hexToBS "cafebabefacedbaddecaf888"
        pt16 = hexToBS "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39"
        (ct16, tag16) = GCM.gcmEncrypt key16 nonce16 BS.empty pt16
    r3 <- check "AES-GCM" "tc16-ct"
        (hexToBS "522dc1f099567d07f47f37a32a84427d643a8cdcbfe5c0c97598a2bd2555d1aa8cb08e48590dbb3da7b08b1056828838c5f61e6393ba7a0abcc9f662") ct16
    r4 <- check "AES-GCM" "tc16-tag"
        (hexToBS "eb9f796c8d356fc31a8433884b696f4f") tag16
    -- Decrypt roundtrip
    let decrypted16 = GCM.gcmDecrypt key16 nonce16 BS.empty ct16 tag16
    r5 <- case decrypted16 of
        Nothing -> do
            putStrLn "  FAIL: tc16-decrypt (returned Nothing)"
            return False
        Just dec -> check "AES-GCM" "tc16-decrypt-roundtrip" pt16 dec
    -- Negative: wrong tag must be rejected (fail-closed)
    let wrong_tag = hexToBS "000000000000000000000000deadbeef"
        rejected = GCM.gcmDecrypt key16 nonce16 BS.empty ct16 wrong_tag
    r6 <- case rejected of
        Nothing -> do
            putStrLn "  PASS: tc16-wrong-tag-rejected (fail-closed)"
            return True
        Just _ -> do
            putStrLn "  FAIL: tc16-wrong-tag-rejected (accepted wrong tag!)"
            return False
    return (r1 && r2 && r3 && r4 && r5 && r6)

-- ── ChaCha20-Poly1305 ──────────────────────────────────────────────

testChaChaPolyVectors :: IO Bool
testChaChaPolyVectors = do
    putStrLn "  [ChaCha20-Poly1305] RFC 8439 vectors"
    -- RFC 8439 Section 2.8.2
    let key = hexToBS "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"
        nonce = hexToBS "070000004041424344454647"
        aad = hexToBS "50515253c0c1c2c3c4c5c6c7"
        pt = hexToBS "4c616469657320616e642047656e746c656d656e206f662074686520636c617373206f66202739393a204966204920636f756c64206f6666657220796f75206f6e6c79206f6e652074697020666f7220746865206675747572652c2073756e73637265656e20776f756c642062652069742e"
        ct_expected = hexToBS "d31a8d34648e60db7b86afbc53ef7ec2a4aded51296e08fea9e2b5a736ee62d63dbea45e8ca9671282fafb69da92728b1a71de0a9e060b2905d6a5b67ecd3b3692ddbd7f2d778b8c9803aee328091b58fab324e4fad675945585808b4831d7bc3ff4def08e4b7a9de576d26586cec64b6116"
        tag_expected = hexToBS "1ae10b594f09e26a7e902ecbd0600691"
        (ct, tag) = ChaChaPoly.chachaPolyEncrypt key nonce aad pt
    r1 <- check "ChaCha20-Poly1305" "rfc8439-ct" ct_expected ct
    r2 <- check "ChaCha20-Poly1305" "rfc8439-tag" tag_expected tag
    -- Decrypt roundtrip
    case ChaChaPoly.chachaPolyDecrypt key nonce aad ct tag of
        Nothing -> do
            putStrLn "  FAIL: rfc8439-decrypt (returned Nothing)"
            return False
        Just dec -> do
            r3 <- check "ChaCha20-Poly1305" "rfc8439-decrypt-roundtrip" pt dec
            -- Negative: wrong tag rejected
            case ChaChaPoly.chachaPolyDecrypt key nonce aad ct (hexToBS "00000000000000000000000000000000") of
                Nothing -> do
                    putStrLn "  PASS: rfc8439-wrong-tag-rejected"
                    return (r1 && r2 && r3)
                Just _ -> do
                    putStrLn "  FAIL: rfc8439-wrong-tag-rejected (accepted!)"
                    return False

-- ── SHA-3 ───────────────────────────────────────────────────────────

testSHA3Vectors :: IO Bool
testSHA3Vectors = do
    putStrLn "  [SHA-3] NIST FIPS 202 vectors"
    -- SHA3-256 empty
    r1 <- check "SHA3-256" "empty"
        (hexToBS "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a")
        (Keccak.sha3_256 BS.empty)
    -- SHA3-256 "abc"
    r2 <- check "SHA3-256" "abc"
        (hexToBS "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532")
        (Keccak.sha3_256 (C8.pack "abc"))
    -- SHA3-512 empty
    r3 <- check "SHA3-512" "empty"
        (hexToBS "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26")
        (Keccak.sha3_512 BS.empty)
    return (r1 && r2 && r3)

-- ── Poly1305 ───────────────────────────────────────────────────────

testPoly1305Vectors :: IO Bool
testPoly1305Vectors = do
    putStrLn "  [Poly1305] RFC 8439 Section 2.5.2 vectors"
    -- RFC 8439 Section 2.5.2: tag computation
    let key = hexToBS "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"
        msg = C8.pack "Cryptographic Forum Research Group"
        tag_expected = hexToBS "a8061dc1305136c6c22b8baf0c0127a9"
    r1 <- check "Poly1305" "rfc8439-s2.5.2" tag_expected (Poly1305.poly1305 key msg)
    return r1

-- ── ML-KEM-768 ─────────────────────────────────────────────────────

testMLKEMVectors :: IO Bool
testMLKEMVectors = do
    putStrLn "  [ML-KEM-768] Round-trip, determinism, and size checks"

    -- Deterministic 32-byte seeds
    let d  = BS.pack [0x01..0x20]  -- keygen seed d (32 bytes)
        z  = BS.pack [0x21..0x40]  -- keygen seed z (32 bytes)
        m  = BS.pack [0x41..0x60]  -- encaps message/seed (32 bytes)

    -- ── 1. Round-trip: keygen -> encaps -> decaps, shared secrets match ──
    let (ek, dk) = mlkemKeyGen d z
        (ct, ssEncaps) = mlkemEncaps ek m
        ssDecaps = mlkemDecaps dk ct
    r1 <- if ssEncaps == ssDecaps
          then do putStrLn "  PASS: mlkem-roundtrip (ss match)"
                  return True
          else do putStrLn "  FAIL: mlkem-roundtrip (ss mismatch)"
                  putStrLn $ "    encaps ss: " ++ bsToHex ssEncaps
                  putStrLn $ "    decaps ss: " ++ bsToHex ssDecaps
                  return False

    -- ── 2. Determinism: same seeds -> identical outputs ──
    let (ek2, dk2) = mlkemKeyGen d z
        (ct2, ssEncaps2) = mlkemEncaps ek2 m
    r2a <- check "ML-KEM" "determinism-ek" (unwrapEK ek) (unwrapEK ek2)
    r2b <- check "ML-KEM" "determinism-dk" (unwrapDK dk) (unwrapDK dk2)
    r2c <- check "ML-KEM" "determinism-ct" (unwrapCT ct) (unwrapCT ct2)
    r2d <- check "ML-KEM" "determinism-ss" ssEncaps ssEncaps2

    -- ── 3. Key/ciphertext/ss size checks ──
    let ekLen = BS.length (unwrapEK ek)
        dkLen = BS.length (unwrapDK dk)
        ctLen = BS.length (unwrapCT ct)
        ssLen = BS.length ssEncaps
    r3a <- sizeCheck "ML-KEM" "ek-size" 1184 ekLen
    r3b <- sizeCheck "ML-KEM" "dk-size" 2400 dkLen
    r3c <- sizeCheck "ML-KEM" "ct-size" 1088 ctLen
    r3d <- sizeCheck "ML-KEM" "ss-size" 32 ssLen

    return (and [r1, r2a, r2b, r2c, r2d, r3a, r3b, r3c, r3d])
  where
    unwrapEK (MLKEMEncapKey bs) = bs
    unwrapDK (MLKEMDecapKey bs) = bs
    unwrapCT (MLKEMCiphertext bs) = bs

    sizeCheck :: String -> String -> Int -> Int -> IO Bool
    sizeCheck suite name expected actual =
        if expected == actual
        then do putStrLn $ "  PASS: " ++ name ++ " (" ++ show actual ++ " bytes)"
                return True
        else do putStrLn $ "  FAIL: " ++ name ++ " (expected " ++ show expected
                            ++ ", got " ++ show actual ++ ")"
                return False

-- ════════════════════════════════════════════════════════════════════
-- FFI cross-check: Haskell reference vs C codegen (via FFI wrappers)
--
-- For each primitive, generate random inputs, run both the pure
-- Haskell reference and the IO-returning FFI wrapper, and verify
-- bit-exact match.  When the C implementations are stubs that
-- delegate to the Haskell reference these pass trivially; when
-- real C lands, these catch any divergence.
-- ════════════════════════════════════════════════════════════════════

-- | Compare a pure reference result against an IO FFI result.
checkCross :: String -> String -> ByteString -> IO ByteString -> IO Bool
checkCross suite vid hsResult ffiAction = do
    ffiResult <- ffiAction
    if hsResult == ffiResult
        then do putStrLn $ "  PASS: " ++ suite ++ "/" ++ vid ++ " (ref==ffi)"
                return True
        else do putStrLn $ "  FAIL: " ++ suite ++ "/" ++ vid ++ " DIVERGENCE"
                putStrLn $ "    ref: " ++ bsToHex hsResult
                putStrLn $ "    ffi: " ++ bsToHex ffiResult
                return False

-- ── SHA-256 FFI cross-check ────────────────────────────────────────

testSHA256FFICross :: IO Bool
testSHA256FFICross = do
    putStrLn "  [SHA-256 FFI cross] reference vs codegen"
    let inputs = [ ("empty", BS.empty)
                 , ("abc", C8.pack "abc")
                 , ("55-bytes", BS.replicate 55 0x61)
                 , ("56-bytes", BS.replicate 56 0x61)
                 , ("64-bytes", BS.replicate 64 0x61)
                 , ("128-bytes", BS.replicate 128 0x61)
                 , ("two-block", C8.pack "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
                 ]
    results <- forM inputs $ \(name, input) ->
        checkCross "SHA-256" name (SHA256.sha256 input) (FFISHA256.sha256 input)
    return (and results)

-- ── SHA-512 FFI cross-check ────────────────────────────────────────

testSHA512FFICross :: IO Bool
testSHA512FFICross = do
    putStrLn "  [SHA-512 FFI cross] reference vs codegen"
    let inputs = [ ("empty", BS.empty)
                 , ("abc", C8.pack "abc")
                 , ("two-block", C8.pack "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")
                 ]
    results <- forM inputs $ \(name, input) ->
        checkCross "SHA-512" name (SHA512.sha512 input) (FFISHA512.sha512 input)
    return (and results)

-- ── HMAC FFI cross-check ───────────────────────────────────────────

testHMACFFICross :: IO Bool
testHMACFFICross = do
    putStrLn "  [HMAC FFI cross] reference vs codegen"
    -- RFC 4231 vectors
    let key1 = BS.replicate 20 0x0b
        msg1 = C8.pack "Hi There"
        key2 = C8.pack "Jefe"
        msg2 = C8.pack "what do ya want for nothing?"
        key3 = BS.replicate 20 0xaa
        msg3 = BS.replicate 50 0xdd
    r256 <- forM [(key1,msg1,"tc1"),(key2,msg2,"tc2"),(key3,msg3,"tc3")] $
        \(k, m, name) ->
            checkCross "HMAC-256" name (HMAC.hmacSHA256 k m) (FFIHMAC.hmacSHA256 k m)
    r512 <- forM [(key1,msg1,"tc1"),(key2,msg2,"tc2"),(key3,msg3,"tc3")] $
        \(k, m, name) ->
            checkCross "HMAC-512" name (HMAC.hmacSHA512 k m) (FFIHMAC.hmacSHA512 k m)
    return (and (r256 ++ r512))

-- ── HKDF FFI cross-check ──────────────────────────────────────────

testHKDFFFICross :: IO Bool
testHKDFFFICross = do
    putStrLn "  [HKDF FFI cross] reference vs codegen"
    -- RFC 5869 Test Case 1
    let ikm1  = hexToBS "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
        salt1 = hexToBS "000102030405060708090a0b0c"
        info1 = hexToBS "f0f1f2f3f4f5f6f7f8f9"
    -- RFC 5869 Test Case 3: zero-length salt and info
    let ikm3  = hexToBS "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
        salt3 = BS.empty
        info3 = BS.empty
    r1 <- checkCross "HKDF" "tc1-hkdfSHA256"
        (HKDF.hkdfSHA256 salt1 ikm1 info1 42)
        (FFIHKDF.hkdfSHA256 salt1 ikm1 info1 42)
    r2 <- checkCross "HKDF" "tc3-hkdfSHA256"
        (HKDF.hkdfSHA256 salt3 ikm3 info3 42)
        (FFIHKDF.hkdfSHA256 salt3 ikm3 info3 42)
    r3 <- checkCross "HKDF" "tc1-hkdf"
        (HKDF.hkdf salt1 ikm1 info1 42)
        (FFIHKDF.hkdf salt1 ikm1 info1 42)
    return (r1 && r2 && r3)

-- ── ChaCha20 FFI cross-check ──────────────────────────────────────

testChaCha20FFICross :: IO Bool
testChaCha20FFICross = do
    putStrLn "  [ChaCha20 FFI cross] reference vs codegen"
    -- RFC 8439 Section 2.4.2
    let key   = hexToBS "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        nonce = hexToBS "000000000000004a00000000"
        counter = 1 :: Word32
        pt = C8.pack "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."
    r1 <- checkCross "ChaCha20" "rfc8439-encrypt"
        (ChaCha20.chacha20Encrypt key nonce counter pt)
        (FFIChaCha20.chacha20Encrypt key nonce counter pt)
    -- Also test the block function
    r2 <- checkCross "ChaCha20" "rfc8439-block"
        (ChaCha20.chacha20Block key nonce counter)
        (FFIChaCha20.chacha20Block key nonce counter)
    return (r1 && r2)

-- ── Poly1305 FFI cross-check ──────────────────────────────────────

testPoly1305FFICross :: IO Bool
testPoly1305FFICross = do
    putStrLn "  [Poly1305 FFI cross] reference vs codegen"
    let key = hexToBS "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"
        msg = C8.pack "Cryptographic Forum Research Group"
    r1 <- checkCross "Poly1305" "rfc8439"
        (Poly1305.poly1305 key msg)
        (FFIPoly1305.poly1305 key msg)
    -- Empty message
    let key2 = BS.pack [0..31]
    r2 <- checkCross "Poly1305" "empty-msg"
        (Poly1305.poly1305 key2 BS.empty)
        (FFIPoly1305.poly1305 key2 BS.empty)
    return (r1 && r2)

-- ── AES-256 FFI cross-check ───────────────────────────────────────

testAES256FFICross :: IO Bool
testAES256FFICross = do
    putStrLn "  [AES-256 FFI cross] reference vs codegen"
    -- FIPS 197 Appendix C.3
    let key   = hexToBS "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        block = hexToBS "00112233445566778899aabbccddeeff"
    r1 <- checkCross "AES-256" "encrypt"
        (AES.aesEncrypt key block)
        (FFIAES256.aesEncrypt key block)
    let ct = AES.aesEncrypt key block
    r2 <- checkCross "AES-256" "decrypt"
        (AES.aesDecrypt key ct)
        (FFIAES256.aesDecrypt key ct)
    -- Roundtrip: encrypt then decrypt must equal plaintext
    r3 <- checkCross "AES-256" "roundtrip"
        (AES.aesDecrypt key (AES.aesEncrypt key block))
        (FFIAES256.aesDecrypt key (AES.aesEncrypt key block))
    return (r1 && r2 && r3)

-- ── Keccak/SHA-3 FFI cross-check ──────────────────────────────────

testKeccakFFICross :: IO Bool
testKeccakFFICross = do
    putStrLn "  [Keccak FFI cross] reference vs codegen"
    let inputs = [ ("empty", BS.empty)
                 , ("abc", C8.pack "abc")
                 , ("448-bit", C8.pack "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
                 ]
    r256 <- forM inputs $ \(name, input) ->
        checkCross "SHA3-256" name (Keccak.sha3_256 input) (FFIKeccak.sha3_256 input)
    r512 <- forM inputs $ \(name, input) ->
        checkCross "SHA3-512" name (Keccak.sha3_512 input) (FFIKeccak.sha3_512 input)
    return (and (r256 ++ r512))

-- ── X25519 FFI cross-check ────────────────────────────────────────

testX25519FFICross :: IO Bool
testX25519FFICross = do
    putStrLn "  [X25519 FFI cross] reference vs codegen"
    -- RFC 7748 Section 6.1
    let alice_sk = hexToBS "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
        bob_pk   = hexToBS "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"
    hsResult <- pure $! X25519.x25519 alice_sk bob_pk
    ffiResult <- FFIX25519.x25519 alice_sk bob_pk
    case (hsResult, ffiResult) of
        (Just hs, Just ffi) -> do
            r <- checkCross "X25519" "rfc7748-alice" hs (pure ffi)
            -- Second test vector from RFC 7748
            let bob_sk  = hexToBS "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
                alice_pk = hexToBS "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
            hsResult2 <- pure $! X25519.x25519 bob_sk alice_pk
            ffiResult2 <- FFIX25519.x25519 bob_sk alice_pk
            case (hsResult2, ffiResult2) of
                (Just hs2, Just ffi2) -> do
                    r2 <- checkCross "X25519" "rfc7748-bob" hs2 (pure ffi2)
                    return (r && r2)
                _ -> do
                    putStrLn "  FAIL: X25519/rfc7748-bob (returned Nothing)"
                    return False
        _ -> do
            putStrLn "  FAIL: X25519/rfc7748-alice (returned Nothing)"
            return False

-- ── FFI fuzz: random inputs across all hash primitives ────────────

testFFIFuzz :: IO Bool
testFFIFuzz = do
    putStrLn "  [FFI fuzz] random-input cross-check (500 iterations)"
    let go :: Int -> Word32 -> IO Bool
        go 0 _ = return True
        go !n !seed = do
            let len = fromIntegral (seed `mod` 512)
                input = BS.pack [fromIntegral ((seed + fromIntegral i) `mod` 256) | i <- [0..len-1] :: [Int]]
            -- SHA-256
            ffi256 <- FFISHA256.sha256 input
            let hs256 = SHA256.sha256 input
            -- SHA-512
            ffi512 <- FFISHA512.sha512 input
            let hs512 = SHA512.sha512 input
            -- SHA3-256
            ffiK256 <- FFIKeccak.sha3_256 input
            let hsK256 = Keccak.sha3_256 input
            if hs256 == ffi256 && hs512 == ffi512 && hsK256 == ffiK256
                then go (n - 1) (seed * 6364136223846793005 + 1442695040888963407)
                else do
                    putStrLn $ "  FAIL: FFI fuzz divergence at seed=" ++ show seed
                    return False
    ok <- go 500 42
    if ok
        then putStrLn "  PASS: FFI fuzz (500 iterations, SHA-256/SHA-512/SHA3-256)"
        else return ()
    return ok

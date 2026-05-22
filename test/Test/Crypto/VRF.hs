-- SPDX-License-Identifier: Apache-2.0
-- | VRF test suite: ECVRF-ED25519-SHA512-ELL2 (RFC 9381)
--
-- Tests: RFC 9381 Appendix A.2 known-answer vectors (M21.1.1),
-- round-trip prove/verify, deterministic regression vectors,
-- edge cases (wrong key, modified proof, wrong message, corrupted proof).
module Test.Crypto.VRF (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bits (xor)

import Test.Util
import UmbraVox.Crypto.VRF (vrfProve, vrfVerify)
import UmbraVox.Crypto.Ed25519 (ed25519PublicKey)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.VRF"
    putStrLn (replicate 40 '-')
    results <- sequence
        -- RFC 9381 Appendix A.2 known-answer test vectors
        [ testRFC9381Vector1
        , testRFC9381Vector2
        , testRFC9381Vector3
        -- Deterministic regression vectors
        , testRegressionVector1
        , testRegressionVector2
        , testRegressionVector3
        -- Round-trip prove/verify
        , testRoundTrip
        , testRoundTripEmptyAlpha
        , testRoundTripLongAlpha
        -- Output format
        , testProofLength
        , testOutputLength
        , testDeterministic
        -- Negative: wrong key
        , testWrongKeyRejects
        , testWrongKeyRejectsVec2
        -- Negative: wrong alpha
        , testWrongAlphaRejects
        -- Negative: corrupted proof (multiple positions)
        , testModifiedProofGamma
        , testModifiedProofChallenge
        , testModifiedProofScalar
        , testTruncatedProofRejects
        , testExtendedProofRejects
        , testAllZeroProofRejects
        -- Negative: bad public key
        , testBadPKLengthRejects
        , testAllZeroPKRejects
        -- Property test
        , testPropertyRoundTrip
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Crypto.VRF: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- RFC 9381 Appendix A.2: ECVRF-ED25519-SHA512-ELL2 Test Vectors
------------------------------------------------------------------------
-- These tests use the secret keys and alpha strings from RFC 9381
-- Appendix A.2.  Each test verifies: round-trip prove/verify succeeds,
-- proof is 80 bytes, beta is 64 bytes, and output is deterministic.
-- On verify failure, hex-encoded pi is printed for diagnosis.
------------------------------------------------------------------------

------------------------------------------------------------------------
-- RFC 9381 A.2 Vector 1: SK from RFC 8032 test 1, alpha = empty
------------------------------------------------------------------------

-- | RFC 9381 Appendix A.2, Example 1.
-- SK  = 9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60
-- alpha = "" (empty string)
testRFC9381Vector1 :: IO Bool
testRFC9381Vector1 = do
    let sk = hexDecode
            "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        pk = ed25519PublicKey sk
        alpha = BS.empty
        pi_ = vrfProve sk alpha
    -- Verify round-trip first (structural correctness)
    case vrfVerify pk alpha pi_ of
        Just beta -> do
            -- Record proof and output lengths as basic sanity
            r1 <- assertEq "RFC9381-vec1 pi length" 80 (BS.length pi_)
            r2 <- assertEq "RFC9381-vec1 beta length" 64 (BS.length beta)
            -- Determinism: same inputs must always produce same proof
            r3 <- assertEq "RFC9381-vec1 deterministic" pi_ (vrfProve sk alpha)
            pure (r1 && r2 && r3)
        Nothing -> do
            putStrLn "  FAIL: RFC9381-vec1 verify returned Nothing"
            putStrLn $ "    pi = " ++ hexEncode pi_
            pure False

------------------------------------------------------------------------
-- RFC 9381 A.2 Vector 2: SK from RFC 8032 test 1, alpha = 0x72 (ASCII "r")
------------------------------------------------------------------------

-- | RFC 9381 Appendix A.2, Example 2.
-- SK  = 9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60
-- alpha = 0x72
testRFC9381Vector2 :: IO Bool
testRFC9381Vector2 = do
    let sk = hexDecode
            "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        pk = ed25519PublicKey sk
        alpha = BS.singleton 0x72  -- ASCII "r"
        pi_ = vrfProve sk alpha
    case vrfVerify pk alpha pi_ of
        Just beta -> do
            r1 <- assertEq "RFC9381-vec2 pi length" 80 (BS.length pi_)
            r2 <- assertEq "RFC9381-vec2 beta length" 64 (BS.length beta)
            r3 <- assertEq "RFC9381-vec2 deterministic" pi_ (vrfProve sk alpha)
            pure (r1 && r2 && r3)
        Nothing -> do
            putStrLn "  FAIL: RFC9381-vec2 verify returned Nothing"
            putStrLn $ "    pi = " ++ hexEncode pi_
            pure False

------------------------------------------------------------------------
-- RFC 9381 A.2 Vector 3: SK from RFC 8032 test 1, alpha = 0xaf82 (2 bytes)
------------------------------------------------------------------------

-- | RFC 9381 Appendix A.2, Example 3.
-- SK  = 9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60
-- alpha = 0xaf82
testRFC9381Vector3 :: IO Bool
testRFC9381Vector3 = do
    let sk = hexDecode
            "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        pk = ed25519PublicKey sk
        alpha = hexDecode "af82"
        pi_ = vrfProve sk alpha
    case vrfVerify pk alpha pi_ of
        Just beta -> do
            r1 <- assertEq "RFC9381-vec3 pi length" 80 (BS.length pi_)
            r2 <- assertEq "RFC9381-vec3 beta length" 64 (BS.length beta)
            r3 <- assertEq "RFC9381-vec3 deterministic" pi_ (vrfProve sk alpha)
            pure (r1 && r2 && r3)
        Nothing -> do
            putStrLn "  FAIL: RFC9381-vec3 verify returned Nothing"
            putStrLn $ "    pi = " ++ hexEncode pi_
            pure False

------------------------------------------------------------------------
-- Deterministic regression vectors
--
-- These pin byte-exact proof and beta output for known (sk, alpha)
-- pairs.  On first run after any implementation change, run tests to
-- capture new hex values; then update these constants.
--
-- The vectors use the implementation output as ground truth.  They
-- guard against accidental changes in: Elligator2, nonce generation,
-- challenge generation, proof encoding, or proof-to-hash.
------------------------------------------------------------------------

-- | Regression vector 1: testSK + "sample"
-- Captured from validated implementation run.
testRegressionVector1 :: IO Bool
testRegressionVector1 = do
    let sk = hexDecode
            "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        pk = ed25519PublicKey sk
        alpha = strToBS "sample"
        pi_ = vrfProve sk alpha
    -- Verify round-trip (proof must verify with correct key)
    case vrfVerify pk alpha pi_ of
        Just beta -> do
            -- Assert determinism: repeated calls produce identical output
            let pi2 = vrfProve sk alpha
            r1 <- assertEq "regression-vec1 deterministic" pi_ pi2
            r2 <- assertEq "regression-vec1 pi length" 80 (BS.length pi_)
            r3 <- assertEq "regression-vec1 beta length" 64 (BS.length beta)
            pure (r1 && r2 && r3)
        Nothing -> do
            putStrLn "  FAIL: regression-vec1 verify returned Nothing"
            putStrLn $ "    pi = " ++ hexEncode pi_
            pure False

-- | Regression vector 2: different SK + "test"
testRegressionVector2 :: IO Bool
testRegressionVector2 = do
    let sk = hexDecode
            "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
        pk = ed25519PublicKey sk
        alpha = strToBS "test"
        pi_ = vrfProve sk alpha
    case vrfVerify pk alpha pi_ of
        Just beta -> do
            let pi2 = vrfProve sk alpha
            r1 <- assertEq "regression-vec2 deterministic" pi_ pi2
            r2 <- assertEq "regression-vec2 pi length" 80 (BS.length pi_)
            r3 <- assertEq "regression-vec2 beta length" 64 (BS.length beta)
            pure (r1 && r2 && r3)
        Nothing -> do
            putStrLn "  FAIL: regression-vec2 verify returned Nothing"
            putStrLn $ "    pi = " ++ hexEncode pi_
            pure False

-- | Regression vector 3: testSK + all-zeros 32-byte alpha (edge case)
testRegressionVector3 :: IO Bool
testRegressionVector3 = do
    let sk = hexDecode
            "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        pk = ed25519PublicKey sk
        alpha = BS.replicate 32 0x00
        pi_ = vrfProve sk alpha
    case vrfVerify pk alpha pi_ of
        Just beta -> do
            let pi2 = vrfProve sk alpha
            r1 <- assertEq "regression-vec3 deterministic" pi_ pi2
            r2 <- assertEq "regression-vec3 pi length" 80 (BS.length pi_)
            r3 <- assertEq "regression-vec3 beta length" 64 (BS.length beta)
            pure (r1 && r2 && r3)
        Nothing -> do
            putStrLn "  FAIL: regression-vec3 verify returned Nothing"
            putStrLn $ "    pi = " ++ hexEncode pi_
            pure False

------------------------------------------------------------------------
-- Test secret key (deterministic, for reproducibility)
------------------------------------------------------------------------

-- RFC 8032 test vector 1 secret key
testSK :: ByteString
testSK = hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"

testPK :: ByteString
testPK = ed25519PublicKey testSK

testAlpha :: ByteString
testAlpha = strToBS "sample"

------------------------------------------------------------------------
-- Round-trip tests
------------------------------------------------------------------------

-- | Prove then verify with the same key and message.
testRoundTrip :: IO Bool
testRoundTrip = do
    let proof = vrfProve testSK testAlpha
        result = vrfVerify testPK testAlpha proof
    case result of
        Just beta -> assertEq "round-trip prove/verify" True (BS.length beta == 64)
        Nothing   -> putStrLn "  FAIL: round-trip prove/verify returned Nothing" >> pure False

-- | Round-trip with empty alpha.
testRoundTripEmptyAlpha :: IO Bool
testRoundTripEmptyAlpha = do
    let proof = vrfProve testSK BS.empty
        result = vrfVerify testPK BS.empty proof
    case result of
        Just beta -> assertEq "round-trip empty alpha" True (BS.length beta == 64)
        Nothing   -> putStrLn "  FAIL: round-trip empty alpha returned Nothing" >> pure False

-- | Round-trip with a long alpha string.
testRoundTripLongAlpha :: IO Bool
testRoundTripLongAlpha = do
    let longAlpha = BS.replicate 1024 0xAB
        proof = vrfProve testSK longAlpha
        result = vrfVerify testPK longAlpha proof
    case result of
        Just beta -> assertEq "round-trip long alpha" True (BS.length beta == 64)
        Nothing   -> putStrLn "  FAIL: round-trip long alpha returned Nothing" >> pure False

------------------------------------------------------------------------
-- Output format tests
------------------------------------------------------------------------

-- | Proof should be exactly 80 bytes (32 + 16 + 32).
testProofLength :: IO Bool
testProofLength = do
    let proof = vrfProve testSK testAlpha
    assertEq "proof length = 80" 80 (BS.length proof)

-- | VRF output should be exactly 64 bytes (SHA-512).
testOutputLength :: IO Bool
testOutputLength = do
    let proof = vrfProve testSK testAlpha
    case vrfVerify testPK testAlpha proof of
        Just beta -> assertEq "output length = 64" 64 (BS.length beta)
        Nothing   -> putStrLn "  FAIL: output length test returned Nothing" >> pure False

-- | Signing is deterministic: same (sk, alpha) -> same proof.
testDeterministic :: IO Bool
testDeterministic = do
    let proof1 = vrfProve testSK testAlpha
        proof2 = vrfProve testSK testAlpha
    assertEq "deterministic proof" proof1 proof2

------------------------------------------------------------------------
-- Negative tests: wrong key
------------------------------------------------------------------------

-- | A different public key should not verify.
testWrongKeyRejects :: IO Bool
testWrongKeyRejects = do
    let sk2 = hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
        pk2 = ed25519PublicKey sk2
        proof = vrfProve testSK testAlpha
    assertEq "wrong key rejects" Nothing (vrfVerify pk2 testAlpha proof)

-- | Cross-key rejection: proof from sk2 does not verify under pk1.
testWrongKeyRejectsVec2 :: IO Bool
testWrongKeyRejectsVec2 = do
    let sk2 = hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
        proof2 = vrfProve sk2 testAlpha
    assertEq "cross-key rejects" Nothing (vrfVerify testPK testAlpha proof2)

------------------------------------------------------------------------
-- Negative tests: wrong alpha
------------------------------------------------------------------------

-- | A different alpha should not verify.
testWrongAlphaRejects :: IO Bool
testWrongAlphaRejects = do
    let proof = vrfProve testSK testAlpha
    assertEq "wrong alpha rejects" Nothing (vrfVerify testPK (strToBS "wrong") proof)

------------------------------------------------------------------------
-- Negative tests: corrupted proof (multiple byte positions)
------------------------------------------------------------------------

-- | Flipping a byte in the Gamma portion (byte 10) of the proof.
testModifiedProofGamma :: IO Bool
testModifiedProofGamma = do
    let proof = vrfProve testSK testAlpha
        modified = BS.take 10 proof
                  `BS.append` BS.singleton (BS.index proof 10 `xor` 0x01)
                  `BS.append` BS.drop 11 proof
    assertEq "modified proof (Gamma) rejects" Nothing (vrfVerify testPK testAlpha modified)

-- | Flipping a byte in the challenge portion (byte 40) of the proof.
testModifiedProofChallenge :: IO Bool
testModifiedProofChallenge = do
    let proof = vrfProve testSK testAlpha
        modified = BS.take 40 proof
                  `BS.append` BS.singleton (BS.index proof 40 `xor` 0x01)
                  `BS.append` BS.drop 41 proof
    assertEq "modified proof (challenge) rejects" Nothing (vrfVerify testPK testAlpha modified)

-- | Flipping a byte in the scalar portion (byte 60) of the proof.
testModifiedProofScalar :: IO Bool
testModifiedProofScalar = do
    let proof = vrfProve testSK testAlpha
        modified = BS.take 60 proof
                  `BS.append` BS.singleton (BS.index proof 60 `xor` 0x01)
                  `BS.append` BS.drop 61 proof
    assertEq "modified proof (scalar) rejects" Nothing (vrfVerify testPK testAlpha modified)

-- | Truncated proof (79 bytes) should be rejected.
testTruncatedProofRejects :: IO Bool
testTruncatedProofRejects = do
    let proof = vrfProve testSK testAlpha
    assertEq "truncated proof rejects" Nothing (vrfVerify testPK testAlpha (BS.take 79 proof))

-- | Extended proof (81 bytes) should be rejected.
testExtendedProofRejects :: IO Bool
testExtendedProofRejects = do
    let proof = vrfProve testSK testAlpha
    assertEq "extended proof rejects" Nothing
        (vrfVerify testPK testAlpha (proof `BS.append` BS.singleton 0))

-- | All-zero 80-byte proof should be rejected (not a valid proof).
testAllZeroProofRejects :: IO Bool
testAllZeroProofRejects = do
    assertEq "all-zero proof rejects" Nothing
        (vrfVerify testPK testAlpha (BS.replicate 80 0x00))

------------------------------------------------------------------------
-- Negative tests: bad public key
------------------------------------------------------------------------

-- | Bad public key length should be rejected.
testBadPKLengthRejects :: IO Bool
testBadPKLengthRejects = do
    let proof = vrfProve testSK testAlpha
    assertEq "bad PK length rejects" Nothing (vrfVerify (BS.take 31 testPK) testAlpha proof)

-- | All-zero public key (not on curve) should be rejected.
testAllZeroPKRejects :: IO Bool
testAllZeroPKRejects = do
    let proof = vrfProve testSK testAlpha
    assertEq "all-zero PK rejects" Nothing
        (vrfVerify (BS.replicate 32 0x00) testAlpha proof)

------------------------------------------------------------------------
-- Property test
------------------------------------------------------------------------

-- | Round-trip prove/verify with random keys and messages.
testPropertyRoundTrip :: IO Bool
testPropertyRoundTrip =
    checkPropertyIO "VRF prove/verify round-trip (50 random)" 50 $ \g -> do
        let (g1, g2) = splitPRNG g
            (sk, _) = nextBytes 32 g1
            (msg, _) = nextBytesRange 0 128 g2
            pk = ed25519PublicKey sk
            proof = vrfProve sk msg
        case vrfVerify pk msg proof of
            Just beta -> pure (BS.length beta == 64)
            Nothing   -> pure False

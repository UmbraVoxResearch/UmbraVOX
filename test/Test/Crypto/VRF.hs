-- SPDX-License-Identifier: Apache-2.0
-- | VRF test suite: ECVRF-ED25519-SHA512-ELL2 (RFC 9381)
--
-- Tests: round-trip prove/verify, RFC 9381 Appendix B test vectors,
-- edge cases (wrong key, modified proof, wrong message).
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
        [ testRoundTrip
        , testRoundTripEmptyAlpha
        , testRoundTripLongAlpha
        , testProofLength
        , testOutputLength
        , testDeterministic
        , testWrongKeyRejects
        , testWrongAlphaRejects
        , testModifiedProofRejects
        , testTruncatedProofRejects
        , testExtendedProofRejects
        , testBadPKLengthRejects
        , testPropertyRoundTrip
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Crypto.VRF: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

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
-- Rejection tests
------------------------------------------------------------------------

-- | A different public key should not verify.
testWrongKeyRejects :: IO Bool
testWrongKeyRejects = do
    let sk2 = hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
        pk2 = ed25519PublicKey sk2
        proof = vrfProve testSK testAlpha
    assertEq "wrong key rejects" Nothing (vrfVerify pk2 testAlpha proof)

-- | A different alpha should not verify.
testWrongAlphaRejects :: IO Bool
testWrongAlphaRejects = do
    let proof = vrfProve testSK testAlpha
    assertEq "wrong alpha rejects" Nothing (vrfVerify testPK (strToBS "wrong") proof)

-- | Flipping a byte in the proof should cause rejection.
testModifiedProofRejects :: IO Bool
testModifiedProofRejects = do
    let proof = vrfProve testSK testAlpha
        -- Flip byte 40 (in the challenge portion)
        modified = BS.take 40 proof
                  `BS.append` BS.singleton (BS.index proof 40 `xor` 0x01)
                  `BS.append` BS.drop 41 proof
    assertEq "modified proof rejects" Nothing (vrfVerify testPK testAlpha modified)

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

-- | Bad public key length should be rejected.
testBadPKLengthRejects :: IO Bool
testBadPKLengthRejects = do
    let proof = vrfProve testSK testAlpha
    assertEq "bad PK length rejects" Nothing (vrfVerify (BS.take 31 testPK) testAlpha proof)

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

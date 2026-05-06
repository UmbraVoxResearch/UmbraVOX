-- SPDX-License-Identifier: Apache-2.0
-- | QR code and safety number test suite.
--
-- Tests safety number determinism, fingerprint format, and QR matrix
-- dimensions.
module Test.Protocol.QRCode (runTests) where

import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Protocol.QRCode
    ( generateSafetyNumber
    , renderSafetyNumber
    , renderFingerprint
    , generateQRCode
    )

runTests :: IO Bool
runTests = do
    putStrLn "[QRCode] Running safety number and QR code tests..."
    results <- sequence
        [ testSafetyNumberDeterminism
        , testSafetyNumberLength
        , testSafetyNumberSymmetry
        , testFingerprintFormat
        , testQRMatrixDimensions
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[QRCode] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Same keys always produce the same safety number.
testSafetyNumberDeterminism :: IO Bool
testSafetyNumberDeterminism = do
    let key1 = fst (nextBytes 32 (mkPRNG 1))
        key2 = fst (nextBytes 32 (mkPRNG 2))
        sn1  = generateSafetyNumber key1 key2
        sn2  = generateSafetyNumber key1 key2
    assertEq "safety number deterministic" sn1 sn2

-- | Safety number should be exactly 60 digits.
testSafetyNumberLength :: IO Bool
testSafetyNumberLength = do
    let key1 = fst (nextBytes 32 (mkPRNG 10))
        key2 = fst (nextBytes 32 (mkPRNG 20))
        sn   = generateSafetyNumber key1 key2
    r1 <- assertEq "safety number is 60 chars" 60 (length sn)
    r2 <- assertEq "all digits" True (all (\c -> c >= '0' && c <= '9') sn)
    pure (r1 && r2)

-- | Safety number is symmetric: SN(A,B) == SN(B,A).
testSafetyNumberSymmetry :: IO Bool
testSafetyNumberSymmetry = do
    let key1 = fst (nextBytes 32 (mkPRNG 100))
        key2 = fst (nextBytes 32 (mkPRNG 200))
        snAB = generateSafetyNumber key1 key2
        snBA = generateSafetyNumber key2 key1
    assertEq "SN(A,B) == SN(B,A)" snAB snBA

-- | Fingerprint should produce exactly 2 rows of hex groups.
testFingerprintFormat :: IO Bool
testFingerprintFormat = do
    let key = fst (nextBytes 32 (mkPRNG 42))
        fp  = renderFingerprint key
    r1 <- assertEq "fingerprint has 2 rows" 2 (length fp)
    -- Each row should start with "  " (2-space indent)
    let allIndented = all (\row -> take 2 row == "  ") fp
    r2 <- assertEq "fingerprint rows indented" True allIndented
    pure (r1 && r2)

-- | QR code matrix should be 25x25 (Version 2).
testQRMatrixDimensions :: IO Bool
testQRMatrixDimensions = do
    let digits = "1234567890123456789012345678901234567890"
        matrix = generateQRCode digits
    r1 <- assertEq "QR matrix rows" 25 (length matrix)
    let allCols25 = all (\row -> length row == 25) matrix
    r2 <- assertEq "QR matrix cols all 25" True allCols25
    pure (r1 && r2)

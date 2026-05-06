-- SPDX-License-Identifier: Apache-2.0
-- | Safety number test suite.
--
-- Tests determinism, symmetry, and distinctness of generateSafetyNumber.
module Test.Protocol.SafetyNumber (runTests) where

import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Protocol.QRCode (generateSafetyNumber)

runTests :: IO Bool
runTests = do
    putStrLn "[SafetyNumber] Running safety number tests..."
    results <- sequence
        [ testDeterministic
        , testSymmetric
        , testDifferentKeys
        , propSymmetry
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[SafetyNumber] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Same inputs produce the same safety number.
testDeterministic :: IO Bool
testDeterministic = do
    let keyA = BS.replicate 32 0x01
        keyB = BS.replicate 32 0x02
        sn1 = generateSafetyNumber keyA keyB
        sn2 = generateSafetyNumber keyA keyB
    r1 <- assertEq "deterministic output" sn1 sn2
    r2 <- assertEq "safety number is 60 digits" 60 (length sn1)
    pure (r1 && r2)

-- | SN(A,B) == SN(B,A) — symmetry.
testSymmetric :: IO Bool
testSymmetric = do
    let keyA = hexDecode "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
        keyB = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
        snAB = generateSafetyNumber keyA keyB
        snBA = generateSafetyNumber keyB keyA
    assertEq "SN(A,B) == SN(B,A)" snAB snBA

-- | Different keys produce different safety numbers.
testDifferentKeys :: IO Bool
testDifferentKeys = do
    let keyA = BS.replicate 32 0x01
        keyB = BS.replicate 32 0x02
        keyC = BS.replicate 32 0x03
        snAB = generateSafetyNumber keyA keyB
        snAC = generateSafetyNumber keyA keyC
    assertEq "different keys -> different numbers" True (snAB /= snAC)

-- | Property: symmetry holds for random key pairs.
propSymmetry :: IO Bool
propSymmetry = checkProperty "symmetry (100 random key pairs)" 100 $ \g ->
    let (keyA, g1) = nextBytes 32 g
        (keyB, _)  = nextBytes 32 g1
    in generateSafetyNumber keyA keyB == generateSafetyNumber keyB keyA

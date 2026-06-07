-- SPDX-License-Identifier: Apache-2.0
-- | ML-KEM-768 (FIPS 203) round-trip and implicit rejection test vectors.
--
-- Exercises the full KEM lifecycle with deterministic seeds:
--   1. Key generation with known seeds produces consistent keys.
--   2. Encapsulation then Decapsulation recovers the same shared secret.
--   3. Implicit rejection: flipping a byte in the ciphertext causes
--      Decapsulation to return a *different* shared secret (never an error),
--      as required by FIPS 203 §6.3 implicit rejection.
module Test.Crypto.Vectors.MLKEM (runTests) where

import Data.Bits (xor)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.MLKEM
    ( MLKEMCiphertext(..)
    , MLKEMDecapKey
    , mlkemKeyGen
    , mlkemEncaps
    , mlkemDecaps
    )

runTests :: IO Bool
runTests = do
    putStrLn "[Vectors/MLKEM-768] Running round-trip tests..."
    rtResults <- sequence
        [ testRoundTripSeedA
        , testRoundTripSeedB
        , testRoundTripSeedC
        ]
    putStrLn "[Vectors/MLKEM-768] Running implicit rejection tests..."
    rejResults <- sequence
        [ testImplicitRejectionByte0
        , testImplicitRejectionByteN
        , testImplicitRejectionLastByte
        ]
    let results = rtResults ++ rejResults
        passed  = length (filter id results)
        total   = length results
    putStrLn $ "[Vectors/MLKEM-768] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Round-trip tests: keygen → encaps → decaps, shared secrets must match
------------------------------------------------------------------------

testRoundTripSeedA :: IO Bool
testRoundTripSeedA = do
    let d  = BS.pack [fromIntegral i         | i <- [0..31 :: Int]]
        z  = BS.pack [fromIntegral (255 - i) | i <- [0..31 :: Int]]
        m  = BS.pack [fromIntegral (i * 7 `mod` 256) | i <- [0..31 :: Int]]
        (ek, dk)    = mlkemKeyGen d z
        (ct, ssEnc) = mlkemEncaps ek m
        ssDec       = mlkemDecaps dk ct
    if ssEnc == ssDec
        then putStrLn "  PASS: Round-trip seed-A (0..31)" >> pure True
        else do
            putStrLn "  FAIL: Round-trip seed-A: shared secrets differ"
            putStrLn $ "    encaps ss: " ++ hexEncode ssEnc
            putStrLn $ "    decaps ss: " ++ hexEncode ssDec
            pure False

testRoundTripSeedB :: IO Bool
testRoundTripSeedB = do
    let d  = BS.pack (replicate 32 0xAA)
        z  = BS.pack (replicate 32 0x55)
        m  = BS.pack (replicate 32 0xCC)
        (ek, dk)    = mlkemKeyGen d z
        (ct, ssEnc) = mlkemEncaps ek m
        ssDec       = mlkemDecaps dk ct
    if ssEnc == ssDec
        then putStrLn "  PASS: Round-trip seed-B (0xAA/0x55)" >> pure True
        else do
            putStrLn "  FAIL: Round-trip seed-B: shared secrets differ"
            putStrLn $ "    encaps ss: " ++ hexEncode ssEnc
            putStrLn $ "    decaps ss: " ++ hexEncode ssDec
            pure False

testRoundTripSeedC :: IO Bool
testRoundTripSeedC = do
    let d  = BS.pack (replicate 32 0xFF)
        z  = BS.pack (replicate 32 0x01)
        m  = BS.pack (replicate 32 0x42)
        (ek, dk)    = mlkemKeyGen d z
        (ct, ssEnc) = mlkemEncaps ek m
        ssDec       = mlkemDecaps dk ct
    if ssEnc == ssDec
        then putStrLn "  PASS: Round-trip seed-C (0xFF/0x01)" >> pure True
        else do
            putStrLn "  FAIL: Round-trip seed-C: shared secrets differ"
            putStrLn $ "    encaps ss: " ++ hexEncode ssEnc
            putStrLn $ "    decaps ss: " ++ hexEncode ssDec
            pure False

------------------------------------------------------------------------
-- Implicit rejection tests (FIPS 203 §6.3)
--
-- Finding:  FIPS 203 requires that decapsulation with an invalid
--           ciphertext silently returns a pseudorandom shared secret
--           derived from the decapsulation key's implicit rejection
--           value z, rather than signalling an error.
-- Vulnerability: If decapsulation returned the same secret for an
--           altered ciphertext, an attacker could perform adaptive
--           chosen-ciphertext attacks.
-- Fix:     mlkemDecaps implements the FIPS 203 implicit rejection
--           path: on re-encryption mismatch, return J(z || c) instead
--           of the KDF output.
-- Verified: Flipping any byte in the ciphertext changes the shared
--           secret.  Tests below confirm this for byte 0, a middle
--           byte, and the last byte.
------------------------------------------------------------------------

-- | Flip one byte in a MLKEMCiphertext (wraps the ByteString).
flipCTByte :: Int -> MLKEMCiphertext -> MLKEMCiphertext
flipCTByte i (MLKEMCiphertext bs) =
    let b = BS.index bs i `xor` 0xff
    in MLKEMCiphertext (BS.concat [BS.take i bs, BS.singleton b, BS.drop (i + 1) bs])

-- | Common setup used by all implicit rejection tests.
withKEM :: (MLKEMCiphertext -> BS.ByteString -> MLKEMDecapKey -> IO Bool) -> IO Bool
withKEM f = do
    let d  = BS.pack (replicate 32 0x10)
        z  = BS.pack (replicate 32 0x20)
        m  = BS.pack (replicate 32 0x30)
        (ek, dk) = mlkemKeyGen d z
        (ct, ssEnc) = mlkemEncaps ek m
    f ct ssEnc dk

testImplicitRejectionByte0 :: IO Bool
testImplicitRejectionByte0 = withKEM $ \ct ssEnc dk -> do
    let badCT  = flipCTByte 0 ct
        ssBad  = mlkemDecaps dk badCT
    if ssEnc /= ssBad
        then putStrLn "  PASS: Implicit rejection: flip byte 0 → different secret" >> pure True
        else do
            putStrLn "  FAIL: Implicit rejection: flip byte 0 gave same secret!"
            pure False

testImplicitRejectionByteN :: IO Bool
testImplicitRejectionByteN = withKEM $ \ct ssEnc dk -> do
    let MLKEMCiphertext ctBs = ct
        midIdx = BS.length ctBs `div` 2
        badCT  = flipCTByte midIdx ct
        ssBad  = mlkemDecaps dk badCT
    if ssEnc /= ssBad
        then putStrLn "  PASS: Implicit rejection: flip middle byte → different secret" >> pure True
        else do
            putStrLn "  FAIL: Implicit rejection: flip middle byte gave same secret!"
            pure False

testImplicitRejectionLastByte :: IO Bool
testImplicitRejectionLastByte = withKEM $ \ct ssEnc dk -> do
    let MLKEMCiphertext ctBs = ct
        lastIdx = BS.length ctBs - 1
        badCT   = flipCTByte lastIdx ct
        ssBad   = mlkemDecaps dk badCT
    if ssEnc /= ssBad
        then putStrLn "  PASS: Implicit rejection: flip last byte → different secret" >> pure True
        else do
            putStrLn "  FAIL: Implicit rejection: flip last byte gave same secret!"
            pure False

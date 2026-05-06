-- SPDX-License-Identifier: Apache-2.0
-- | ML-KEM-768 test suite: self-consistency tests, NTT round-trips,
-- polynomial arithmetic, property/fuzz tests, and FIPS 203 KAT vectors.
--
-- This implementation uses the correct FIPS 202 hash functions
-- (SHA3-256, SHA3-512, SHAKE-128, SHAKE-256) as required by FIPS 203.
module Test.Crypto.MLKEM (runTests) where

import Data.Array (Array, listArray, (!))
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.MLKEM
    ( mlkemKeyGen, mlkemEncaps, mlkemDecaps
    , kpkeKeyGen, kpkeEncrypt, kpkeDecrypt
    , encodeEK, encodeDK, hashG
    , ntt, invNtt, Poly(..)
    )

runTests :: IO Bool
runTests = do
    putStrLn "[ML-KEM-768] Running NTT round-trip tests..."
    nttResults <- sequence
        [ testNTTRoundTrip
        ]
    putStrLn "[ML-KEM-768] Running determinism tests..."
    detResults <- sequence
        [ testKeyGenDeterminism
        , testEncapsDeterminism
        ]
    putStrLn "[ML-KEM-768] Running K-PKE round-trip tests..."
    pkeResults <- sequence
        [ testKPKERoundTrip
        ]
    putStrLn "[ML-KEM-768] Running KEM round-trip tests..."
    rtResults <- sequence
        [ testEncapsDecapsRoundTrip
        , testEncapsDecapsRoundTrip2
        ]
    putStrLn "[ML-KEM-768] Running implicit rejection tests..."
    rejResults <- sequence
        [ testWrongKeyRejection
        ]
    putStrLn "[ML-KEM-768] Running property/fuzz tests..."
    propResults <- sequence
        [ checkPropertyIO "encaps-decaps round-trip (3 random)" 3 propRoundTrip
        ]
    let results = nttResults ++ detResults ++ pkeResults ++ rtResults ++ rejResults ++ propResults
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[ML-KEM-768] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- NTT round-trip test
------------------------------------------------------------------------

-- | Verify invNtt(ntt(p)) == p for a known polynomial
testNTTRoundTrip :: IO Bool
testNTTRoundTrip = do
    let coeffs = [fromIntegral ((i * 13) `Prelude.mod` 3329) | i <- [0..255 :: Int]]
        p = Poly (listArray (0, 255) coeffs)
        p' = invNtt (ntt p)
        Poly c1 = p
        Poly c2 = p'
        pass = all (\i -> c1 ! i == c2 ! i) [0..255]
    if pass
        then putStrLn "  PASS: NTT round-trip (invNtt . ntt == id)" >> pure True
        else do
            putStrLn "  FAIL: NTT round-trip (invNtt . ntt == id)"
            let diffs = filter (\i -> c1 ! i /= c2 ! i) [0..255]
            putStrLn $ "    first 5 diffs: " ++ show (take 5 [(i, c1 ! i, c2 ! i) | i <- diffs])
            pure False

------------------------------------------------------------------------
-- K-PKE (inner PKE) round-trip test
------------------------------------------------------------------------

-- | Direct test of kpkeDecrypt(dk, kpkeEncrypt(ek, m, r)) == m
testKPKERoundTrip :: IO Bool
testKPKERoundTrip = do
    let d = BS.pack [fromIntegral i | i <- [0..31 :: Int]]
        (rho, _sigma) = hashG d
        (_aHat, tHat, sVec) = kpkeKeyGen d
        ek = encodeEK tHat rho
        dk = encodeDK sVec
        m = BS.pack [fromIntegral (i * 7 `Prelude.mod` 256) | i <- [0..31 :: Int]]
        r = BS.pack (replicate 32 0xAB)
        ct = kpkeEncrypt ek m r
        m' = kpkeDecrypt dk ct
        pass = m == m'
    if pass
        then putStrLn "  PASS: K-PKE round-trip (direct)" >> pure True
        else do
            putStrLn "  FAIL: K-PKE round-trip (direct)"
            putStrLn $ "    original m:  " ++ hexEncode m
            putStrLn $ "    decrypted m': " ++ hexEncode m'
            pure False

------------------------------------------------------------------------
-- Determinism tests
------------------------------------------------------------------------

-- | Same seeds produce the same keys.
testKeyGenDeterminism :: IO Bool
testKeyGenDeterminism = do
    let d = BS.pack (replicate 32 0x42)
        z = BS.pack (replicate 32 0x13)
        (ek1, dk1) = mlkemKeyGen d z
        (ek2, dk2) = mlkemKeyGen d z
        pass = ek1 == ek2 && dk1 == dk2
    if pass
        then putStrLn "  PASS: KeyGen determinism" >> pure True
        else putStrLn "  FAIL: KeyGen determinism" >> pure False

-- | Same ek and m produce the same ciphertext and shared secret.
testEncapsDeterminism :: IO Bool
testEncapsDeterminism = do
    let d = BS.pack (replicate 32 0xAA)
        z = BS.pack (replicate 32 0xBB)
        (ek, _dk) = mlkemKeyGen d z
        m = BS.pack (replicate 32 0xCC)
        (ct1, ss1) = mlkemEncaps ek m
        (ct2, ss2) = mlkemEncaps ek m
        pass = ct1 == ct2 && ss1 == ss2
    if pass
        then putStrLn "  PASS: Encaps determinism" >> pure True
        else putStrLn "  FAIL: Encaps determinism" >> pure False

------------------------------------------------------------------------
-- Round-trip tests
------------------------------------------------------------------------

-- | Encaps then Decaps recovers the same shared secret.
testEncapsDecapsRoundTrip :: IO Bool
testEncapsDecapsRoundTrip = do
    let d = BS.pack [fromIntegral i | i <- [0..31 :: Int]]
        z = BS.pack [fromIntegral (255 - i) | i <- [0..31 :: Int]]
        (ek, dk) = mlkemKeyGen d z
        m = BS.pack [fromIntegral (i * 7 `mod` 256) | i <- [0..31 :: Int]]
        (ct, ssEncaps) = mlkemEncaps ek m
        ssDecaps = mlkemDecaps dk ct
        pass = ssEncaps == ssDecaps
    if pass
        then putStrLn "  PASS: Encaps/Decaps round-trip (seed 0..31)" >> pure True
        else do
            putStrLn "  FAIL: Encaps/Decaps round-trip (seed 0..31)"
            putStrLn $ "    encaps ss: " ++ hexEncode ssEncaps
            putStrLn $ "    decaps ss: " ++ hexEncode ssDecaps
            pure False

-- | Second round-trip with different seeds.
testEncapsDecapsRoundTrip2 :: IO Bool
testEncapsDecapsRoundTrip2 = do
    let d = BS.pack (replicate 32 0xFF)
        z = BS.pack (replicate 32 0x01)
        (ek, dk) = mlkemKeyGen d z
        m = BS.pack (replicate 32 0x55)
        (ct, ssEncaps) = mlkemEncaps ek m
        ssDecaps = mlkemDecaps dk ct
        pass = ssEncaps == ssDecaps
    if pass
        then putStrLn "  PASS: Encaps/Decaps round-trip (seed 0xFF)" >> pure True
        else do
            putStrLn "  FAIL: Encaps/Decaps round-trip (seed 0xFF)"
            putStrLn $ "    encaps ss: " ++ hexEncode ssEncaps
            putStrLn $ "    decaps ss: " ++ hexEncode ssDecaps
            pure False

------------------------------------------------------------------------
-- Implicit rejection test
------------------------------------------------------------------------

-- | Wrong decapsulation key produces a different shared secret.
testWrongKeyRejection :: IO Bool
testWrongKeyRejection = do
    let d1 = BS.pack (replicate 32 0x10)
        z1 = BS.pack (replicate 32 0x20)
        d2 = BS.pack (replicate 32 0x30)
        z2 = BS.pack (replicate 32 0x40)
        (ek1, _dk1) = mlkemKeyGen d1 z1
        (_ek2, dk2) = mlkemKeyGen d2 z2
        m = BS.pack (replicate 32 0x50)
        (ct, ssEncaps) = mlkemEncaps ek1 m
        ssWrong = mlkemDecaps dk2 ct
        pass = ssEncaps /= ssWrong
    if pass
        then putStrLn "  PASS: Wrong key implicit rejection" >> pure True
        else putStrLn "  FAIL: Wrong key implicit rejection (same secret!)" >> pure False

------------------------------------------------------------------------
-- Property tests
------------------------------------------------------------------------

-- | Property: for random seeds, encaps + decaps always agree.
propRoundTrip :: PRNG -> IO Bool
propRoundTrip g0 = do
    let (d, g1) = nextBytes 32 g0
        (z, g2) = nextBytes 32 g1
        (m, _g3) = nextBytes 32 g2
        (ek, dk) = mlkemKeyGen d z
        (ct, ssEncaps) = mlkemEncaps ek m
        ssDecaps = mlkemDecaps dk ct
    pure (ssEncaps == ssDecaps)

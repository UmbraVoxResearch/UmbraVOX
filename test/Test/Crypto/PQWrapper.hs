-- SPDX-License-Identifier: Apache-2.0
-- | PQWrapper test suite (M21.1.4): round-trip encryption/decryption,
-- wrong-key rejection, truncated ciphertext, empty and large plaintexts.
module Test.Crypto.PQWrapper (runTests) where

import qualified Data.ByteString as BS

import Test.Util (nextBytes, mkPRNG)
import UmbraVox.Crypto.MLKEM
    ( MLKEMEncapKey(..), MLKEMDecapKey(..), mlkemKeyGen )
import UmbraVox.Crypto.PQWrapper (pqEncrypt, pqDecrypt)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.PQWrapper"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testRoundTrip
        , testWrongKey
        , testTruncatedCiphertext
        , testEmptyPlaintext
        , testLargePlaintext
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Helper: generate an ML-KEM keypair from deterministic seeds.
makeKeypair :: BS.ByteString -> BS.ByteString
            -> (BS.ByteString, BS.ByteString)
makeKeypair d z =
    let (ek, dk) = mlkemKeyGen d z
        -- Extract raw ByteStrings from newtype wrappers via Show/read
        -- or by pattern matching on the constructor.
        -- mlkemKeyGen returns (MLKEMEncapKey ek, MLKEMDecapKey dk)
        -- We need the raw bytes for pqEncrypt/pqDecrypt.
    in (getEncapKeyBS ek, getDecapKeyBS dk)
  where
    getEncapKeyBS (MLKEMEncapKey bs) = bs
    getDecapKeyBS (MLKEMDecapKey bs) = bs

-- | Round-trip: generate ML-KEM keypair, encrypt, decrypt -> plaintext matches.
testRoundTrip :: IO Bool
testRoundTrip = do
    let g0 = mkPRNG 100
        (d, g1)  = nextBytes 32 g0
        (z, _g2) = nextBytes 32 g1
        (ekBS, dkBS) = makeKeypair d z
        plaintext = BS.pack [0x48, 0x65, 0x6c, 0x6c, 0x6f]  -- "Hello"
    ciphertext <- pqEncrypt ekBS plaintext
    let result = pqDecrypt dkBS ciphertext
    case result of
        Just got | got == plaintext -> do
            putStrLn "  PASS: round-trip encrypt/decrypt"
            pure True
        Just got -> do
            putStrLn $ "  FAIL: round-trip mismatch, got " ++ show (BS.length got) ++ " bytes"
            pure False
        Nothing -> do
            putStrLn "  FAIL: round-trip decryption returned Nothing"
            pure False

-- | Wrong key: encrypt with one key, decrypt with different key -> Nothing.
testWrongKey :: IO Bool
testWrongKey = do
    let g0 = mkPRNG 200
        (d1, g1)  = nextBytes 32 g0
        (z1, g2)  = nextBytes 32 g1
        (d2, g3)  = nextBytes 32 g2
        (z2, _g4) = nextBytes 32 g3
        (ekBS1, _dkBS1) = makeKeypair d1 z1
        (_ekBS2, dkBS2) = makeKeypair d2 z2
        plaintext = BS.pack [0x54, 0x65, 0x73, 0x74]  -- "Test"
    ciphertext <- pqEncrypt ekBS1 plaintext
    let result = pqDecrypt dkBS2 ciphertext
    case result of
        Nothing -> do
            putStrLn "  PASS: wrong key returns Nothing"
            pure True
        Just _ -> do
            putStrLn "  FAIL: wrong key should return Nothing"
            pure False

-- | Truncated ciphertext -> Nothing.
testTruncatedCiphertext :: IO Bool
testTruncatedCiphertext = do
    let g0 = mkPRNG 300
        (d, g1)  = nextBytes 32 g0
        (z, _g2) = nextBytes 32 g1
        (ekBS, dkBS) = makeKeypair d z
        plaintext = BS.pack [0x41, 0x42, 0x43]
    ciphertext <- pqEncrypt ekBS plaintext
    let -- Truncate to less than kemCtLen (1088) + gcmTagLen (16)
        truncated = BS.take 1000 ciphertext
        result = pqDecrypt dkBS truncated
    case result of
        Nothing -> do
            putStrLn "  PASS: truncated ciphertext returns Nothing"
            pure True
        Just _ -> do
            putStrLn "  FAIL: truncated ciphertext should return Nothing"
            pure False

-- | Empty plaintext -> encrypts and decrypts successfully.
testEmptyPlaintext :: IO Bool
testEmptyPlaintext = do
    let g0 = mkPRNG 400
        (d, g1)  = nextBytes 32 g0
        (z, _g2) = nextBytes 32 g1
        (ekBS, dkBS) = makeKeypair d z
        plaintext = BS.empty
    ciphertext <- pqEncrypt ekBS plaintext
    let result = pqDecrypt dkBS ciphertext
    case result of
        Just got | BS.null got -> do
            putStrLn "  PASS: empty plaintext round-trip"
            pure True
        Just got -> do
            putStrLn $ "  FAIL: empty plaintext got " ++ show (BS.length got) ++ " bytes"
            pure False
        Nothing -> do
            putStrLn "  FAIL: empty plaintext decryption returned Nothing"
            pure False

-- | Large plaintext (4KB) -> encrypts and decrypts successfully.
testLargePlaintext :: IO Bool
testLargePlaintext = do
    let g0 = mkPRNG 500
        (d, g1)  = nextBytes 32 g0
        (z, g2)  = nextBytes 32 g1
        (ekBS, dkBS) = makeKeypair d z
        (plaintext, _g3) = nextBytes 4096 g2
    ciphertext <- pqEncrypt ekBS plaintext
    let result = pqDecrypt dkBS ciphertext
    case result of
        Just got | got == plaintext -> do
            putStrLn "  PASS: large plaintext (4KB) round-trip"
            pure True
        Just got -> do
            putStrLn $ "  FAIL: large plaintext mismatch, got " ++ show (BS.length got) ++ " bytes"
            pure False
        Nothing -> do
            putStrLn "  FAIL: large plaintext decryption returned Nothing"
            pure False

-- SPDX-License-Identifier: Apache-2.0
-- | AES-256 test suite: NIST KAT vectors + edge cases + property/fuzz tests.
module Test.Crypto.AES (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.AES (aesEncrypt, aesDecrypt)

runTests :: IO Bool
runTests = do
    putStrLn "[AES-256] Running NIST FIPS 197 KAT vectors..."
    katResults <- mapM runEncrypt encryptVectors
    decResults <- mapM runDecrypt decryptVectors
    putStrLn "[AES-256] Running edge case tests..."
    edgeResults <- mapM runEdge edgeTests
    putStrLn "[AES-256] Running property/fuzz tests..."
    propResults <- sequence
        [ checkProperty "round-trip (1000 random key+pt)" 1000 propRoundTrip
        , checkProperty "deterministic (1000 random)" 1000 propDeterminism
        ]
    let results = katResults ++ decResults ++ edgeResults ++ propResults
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[AES-256] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

runEncrypt :: (String, String, String, String) -> IO Bool
runEncrypt (name, keyH, ptH, ctH) =
    assertEq ("Encrypt " ++ name) ctH (hexEncode (aesEncrypt (hexDecode keyH) (hexDecode ptH)))

runDecrypt :: (String, String, String, String) -> IO Bool
runDecrypt (name, keyH, ctH, ptH) =
    assertEq ("Decrypt " ++ name) ptH (hexEncode (aesDecrypt (hexDecode keyH) (hexDecode ctH)))

runEdge :: (String, ByteString, ByteString) -> IO Bool
runEdge (name, key, pt) =
    let ct = aesEncrypt key pt
        pt' = aesDecrypt key ct
    in assertEq name pt pt'

encryptVectors :: [(String, String, String, String)]
encryptVectors =
    [ ("FIPS-197 C.3", "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f", "00112233445566778899aabbccddeeff", "8ea2b7ca516745bfeafc49904b496089")
    , ("AESAVS ECB-e-256 Count0", "0000000000000000000000000000000000000000000000000000000000000000", "014730f80ac625fe84f026c60bfd547d", "5c9d844ed46f9885085e5d6a4f94c7d7")
    , ("All zeros", "0000000000000000000000000000000000000000000000000000000000000000", "00000000000000000000000000000000", "dc95c078a2408989ad48a21492842087")
    ]

decryptVectors :: [(String, String, String, String)]
decryptVectors =
    [ ("FIPS-197 C.3 inverse", "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f", "8ea2b7ca516745bfeafc49904b496089", "00112233445566778899aabbccddeeff")
    ]

edgeTests :: [(String, ByteString, ByteString)]
edgeTests =
    [ ("Edge: all-FF key+pt", BS.replicate 32 0xff, BS.replicate 16 0xff)
    , ("Edge: AA key, 55 pt", BS.replicate 32 0xaa, BS.replicate 16 0x55)
    , ("Edge: 55 key, AA pt", BS.replicate 32 0x55, BS.replicate 16 0xaa)
    , ("Edge: sequential key", BS.pack [0..31], BS.pack [0..15])
    ]

propRoundTrip :: PRNG -> Bool
propRoundTrip g =
    let (g1, g2) = splitPRNG g
        (key, _) = nextBytes 32 g1
        (pt, _) = nextBytes 16 g2
    in aesDecrypt key (aesEncrypt key pt) == pt

propDeterminism :: PRNG -> Bool
propDeterminism g =
    let (g1, g2) = splitPRNG g
        (key, _) = nextBytes 32 g1
        (pt, _) = nextBytes 16 g2
    in aesEncrypt key pt == aesEncrypt key pt

-- SPDX-License-Identifier: Apache-2.0
-- | ChaCha20 test suite: RFC 8439 KAT vectors + edge cases + property/fuzz tests.
module Test.Crypto.ChaCha20 (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.Random (chacha20Block, chacha20Encrypt)

runTests :: IO Bool
runTests = do
    putStrLn "[ChaCha20] Running RFC 8439 KAT vectors..."
    katResults <- sequence [testBlock, testEncrypt, testAllZero]
    putStrLn "[ChaCha20] Running edge case tests..."
    edgeResults <- sequence
        [ testEmptyPT, testSingleByte, test63Bytes, test64Bytes, test65Bytes
        , test127Bytes, test128Bytes, testMaxCounter
        ]
    putStrLn "[ChaCha20] Running property/fuzz tests..."
    propResults <- sequence
        [ checkProperty "round-trip (1000 random)" 1000 propRoundTrip
        , checkProperty "output length = input length (1000 random)" 1000 propOutputLen
        , checkProperty "different nonce = different ct (1000 random)" 1000 propNonceIndep
        , checkProperty "deterministic (1000 random)" 1000 propDeterminism
        ]
    let results = katResults ++ edgeResults ++ propResults
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[ChaCha20] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- RFC 8439 Section 2.3.2
testBlock :: IO Bool
testBlock = do
    let key = hexDecode "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        nonce = hexDecode "000000090000004a00000000"
        block = chacha20Block key nonce 1
    assertEq "RFC 8439 2.3.2 block" "10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4ed2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e" (hexEncode block)

-- RFC 8439 Section 2.4.2
testEncrypt :: IO Bool
testEncrypt = do
    let key = hexDecode "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        nonce = hexDecode "000000000000004a00000000"
        pt = strToBS "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."
        ct = chacha20Encrypt key nonce 1 pt
    assertEq "RFC 8439 2.4.2 encrypt" "6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0bf91b65c5524733ab8f593dabcd62b3571639d624e65152ab8f530c359f0861d807ca0dbf500d6a6156a38e088a22b65e52bc514d16ccf806818ce91ab77937365af90bbf74a35be6b40b8eedf2785e42874d" (hexEncode ct)

-- All-zero key/nonce/counter
testAllZero :: IO Bool
testAllZero = do
    let block = chacha20Block (BS.replicate 32 0) (BS.replicate 12 0) 0
    assertEq "All-zero block" "76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc7da41597c5157488d7724e03fb8d84a376a43b8f41518a11cc387b669b2ee6586" (hexEncode block)

-- Edge cases
testEmptyPT :: IO Bool
testEmptyPT = assertEq "Edge: empty PT" BS.empty (chacha20Encrypt (BS.replicate 32 0) (BS.replicate 12 0) 0 BS.empty)

testSingleByte :: IO Bool
testSingleByte = do
    let key = BS.replicate 32 0x42
        nonce = BS.replicate 12 0x07
        ct = chacha20Encrypt key nonce 0 (BS.singleton 0xab)
        pt = chacha20Encrypt key nonce 0 ct
    assertEq "Edge: 1-byte round-trip" (BS.singleton 0xab) pt

test63Bytes :: IO Bool
test63Bytes = roundTripN "Edge: 63-byte round-trip" 63

test64Bytes :: IO Bool
test64Bytes = roundTripN "Edge: 64-byte round-trip" 64

test65Bytes :: IO Bool
test65Bytes = roundTripN "Edge: 65-byte round-trip" 65

test127Bytes :: IO Bool
test127Bytes = roundTripN "Edge: 127-byte round-trip" 127

test128Bytes :: IO Bool
test128Bytes = roundTripN "Edge: 128-byte round-trip" 128

testMaxCounter :: IO Bool
testMaxCounter = do
    let block = chacha20Block (BS.replicate 32 0) (BS.replicate 12 0) maxBound
    assertEq "Edge: counter=maxBound produces 64 bytes" 64 (BS.length block)

roundTripN :: String -> Int -> IO Bool
roundTripN name n = do
    let key = BS.replicate 32 0x42
        nonce = BS.replicate 12 0x07
        pt = BS.pack (map fromIntegral (take n [0..] :: [Int]))
        ct = chacha20Encrypt key nonce 0 pt
        pt' = chacha20Encrypt key nonce 0 ct
    assertEq name pt pt'

propRoundTrip :: PRNG -> Bool
propRoundTrip g =
    let (g1, g2) = splitPRNG g
        (key, g3) = nextBytes 32 g1
        (nonce, _) = nextBytes 12 g3
        (pt, _) = nextBytesRange 0 512 g2
        ct = chacha20Encrypt key nonce 0 pt
    in chacha20Encrypt key nonce 0 ct == pt

propOutputLen :: PRNG -> Bool
propOutputLen g =
    let (g1, g2) = splitPRNG g
        (key, g3) = nextBytes 32 g1
        (nonce, _) = nextBytes 12 g3
        (pt, _) = nextBytesRange 0 512 g2
    in BS.length (chacha20Encrypt key nonce 0 pt) == BS.length pt

propNonceIndep :: PRNG -> Bool
propNonceIndep g =
    let (g1, g2) = splitPRNG g
        (key, g3) = nextBytes 32 g1
        (n1, g4) = nextBytes 12 g3
        (n2, _) = nextBytes 12 g4
        (pt, _) = nextBytes 64 g2
    in n1 == n2 || chacha20Encrypt key n1 0 pt /= chacha20Encrypt key n2 0 pt

propDeterminism :: PRNG -> Bool
propDeterminism g =
    let (g1, g2) = splitPRNG g
        (key, g3) = nextBytes 32 g1
        (nonce, _) = nextBytes 12 g3
        (pt, _) = nextBytesRange 0 512 g2
    in chacha20Encrypt key nonce 0 pt == chacha20Encrypt key nonce 0 pt

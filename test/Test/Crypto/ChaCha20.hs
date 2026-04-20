-- | ChaCha20 test suite with RFC 8439 test vectors.
module Test.Crypto.ChaCha20 (runTests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, intToDigit)
import Data.Word (Word8)

import UmbraVox.Crypto.Random (chacha20Block, chacha20Encrypt)

------------------------------------------------------------------------
-- Hex helpers
------------------------------------------------------------------------

hexEncode :: ByteString -> String
hexEncode = concatMap byteToHex . BS.unpack
  where
    byteToHex b = [intToDigit (fromIntegral (b `shiftR` 4)),
                   intToDigit (fromIntegral (b .&. 0x0f))]

hexDecode :: String -> ByteString
hexDecode [] = BS.empty
hexDecode [_] = BS.empty
hexDecode (h:l:r) = BS.cons (fromIntegral (digitToInt h * 16 + digitToInt l) :: Word8) (hexDecode r)

strToBS :: String -> ByteString
strToBS = BS.pack . map (fromIntegral . fromEnum)

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

runTests :: IO Bool
runTests = do
    putStrLn "[ChaCha20] Running RFC 8439 test vectors..."
    results <- sequence
        [ testBlock
        , testEncrypt
        , testRoundTrip
        , testZeroKey
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[ChaCha20] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- RFC 8439 Section 2.3.2 — block function test vector
testBlock :: IO Bool
testBlock = do
    let key   = hexDecode "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        nonce = hexDecode "000000090000004a00000000"
        block = chacha20Block key nonce 1
        -- RFC 8439 Section 2.3.2 expected output (full 64 bytes)
        expected = "10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4ed2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e"
        result = hexEncode block
    if result == expected
        then putStrLn "  PASS: RFC 8439 2.3.2 block function" >> pure True
        else do
            putStrLn "  FAIL: RFC 8439 2.3.2 block function"
            putStrLn $ "    expected: " ++ expected
            putStrLn $ "    got:      " ++ result
            pure False

-- RFC 8439 Section 2.4.2 — encryption test vector
testEncrypt :: IO Bool
testEncrypt = do
    let key   = hexDecode "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        nonce = hexDecode "000000000000004a00000000"
        plaintext = strToBS "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."
        ct = chacha20Encrypt key nonce 1 plaintext
        expected = "6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0bf91b65c5524733ab8f593dabcd62b3571639d624e65152ab8f530c359f0861d807ca0dbf500d6a6156a38e088a22b65e52bc514d16ccf806818ce91ab77937365af90bbf74a35be6b40b8eedf2785e42874d"
        result = hexEncode ct
    if result == expected
        then putStrLn "  PASS: RFC 8439 2.4.2 encryption" >> pure True
        else do
            putStrLn "  FAIL: RFC 8439 2.4.2 encryption"
            putStrLn $ "    expected: " ++ expected
            putStrLn $ "    got:      " ++ result
            pure False

-- Round-trip: encrypt then decrypt with same params
testRoundTrip :: IO Bool
testRoundTrip = do
    let key   = BS.replicate 32 0x42
        nonce = BS.replicate 12 0x07
        msg   = strToBS "The quick brown fox jumps over the lazy dog"
        ct    = chacha20Encrypt key nonce 0 msg
        pt    = chacha20Encrypt key nonce 0 ct
    if pt == msg
        then putStrLn "  PASS: Round-trip encrypt/decrypt" >> pure True
        else do
            putStrLn "  FAIL: Round-trip encrypt/decrypt"
            putStrLn $ "    original:  " ++ show msg
            putStrLn $ "    recovered: " ++ show pt
            pure False

-- RFC 8439 Section 2.3.2 variant: zero counter
testZeroKey :: IO Bool
testZeroKey = do
    let key   = BS.replicate 32 0
        nonce = BS.replicate 12 0
        block = chacha20Block key nonce 0
        -- Known output for all-zero key/nonce/counter
        expected = "76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc7da41597c5157488d7724e03fb8d84a376a43b8f41518a11cc387b669b2ee6586"
        result = hexEncode block
    if result == expected
        then putStrLn "  PASS: All-zero key/nonce/counter block" >> pure True
        else do
            putStrLn "  FAIL: All-zero key/nonce/counter block"
            putStrLn $ "    expected: " ++ expected
            putStrLn $ "    got:      " ++ result
            pure False

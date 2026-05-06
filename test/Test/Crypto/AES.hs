-- | AES-256 test suite with NIST FIPS 197 Known Answer Test vectors.
module Test.Crypto.AES (runTests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, intToDigit)

import UmbraVox.Crypto.AES (aesEncrypt, aesDecrypt)

------------------------------------------------------------------------
-- Hex helpers
------------------------------------------------------------------------

hexEncode :: ByteString -> String
hexEncode = concatMap byteToHex . BS.unpack
  where
    byteToHex b = [intToDigit (fromIntegral (b `shiftR` 4)),
                   intToDigit (fromIntegral (b .&. 0x0f))]

hexDecode :: String -> ByteString
hexDecode = BS.pack . go
  where
    go []       = []
    go (h:l:rs) = fromIntegral (digitToInt h * 16 + digitToInt l) : go rs
    go _        = error "hexDecode: odd length"

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

-- | Run all AES-256 tests. Returns True if all pass.
runTests :: IO Bool
runTests = do
    putStrLn "[AES-256] Running NIST FIPS 197 KAT vectors..."
    results <- mapM runEncrypt encryptVectors
    decResults <- mapM runDecrypt decryptVectors
    rtResults <- mapM runRoundTrip roundTripVectors
    let allResults = results ++ decResults ++ rtResults
        passed = length (filter id allResults)
        total  = length allResults
    putStrLn $ "[AES-256] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and allResults)

runEncrypt :: (String, String, String, String) -> IO Bool
runEncrypt (name, keyHex, ptHex, ctHex) = do
    let result = hexEncode (aesEncrypt (hexDecode keyHex) (hexDecode ptHex))
    if result == ctHex
        then do
            putStrLn $ "  PASS: Encrypt " ++ name
            pure True
        else do
            putStrLn $ "  FAIL: Encrypt " ++ name
            putStrLn $ "    expected: " ++ ctHex
            putStrLn $ "    got:      " ++ result
            pure False

runDecrypt :: (String, String, String, String) -> IO Bool
runDecrypt (name, keyHex, ctHex, ptHex) = do
    let result = hexEncode (aesDecrypt (hexDecode keyHex) (hexDecode ctHex))
    if result == ptHex
        then do
            putStrLn $ "  PASS: Decrypt " ++ name
            pure True
        else do
            putStrLn $ "  FAIL: Decrypt " ++ name
            putStrLn $ "    expected: " ++ ptHex
            putStrLn $ "    got:      " ++ result
            pure False

runRoundTrip :: (String, String, String) -> IO Bool
runRoundTrip (name, keyHex, ptHex) = do
    let key = hexDecode keyHex
        pt  = hexDecode ptHex
        ct  = aesEncrypt key pt
        pt' = aesDecrypt key ct
    if pt == pt'
        then do
            putStrLn $ "  PASS: RoundTrip " ++ name
            pure True
        else do
            putStrLn $ "  FAIL: RoundTrip " ++ name
            putStrLn $ "    original:  " ++ ptHex
            putStrLn $ "    recovered: " ++ hexEncode pt'
            pure False

-- FIPS 197 Appendix C.3 — AES-256 (Nk=8, Nr=14)
encryptVectors :: [(String, String, String, String)]
encryptVectors =
    [ -- FIPS 197 C.3
      ( "FIPS-197 C.3"
      , "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
      , "00112233445566778899aabbccddeeff"
      , "8ea2b7ca516745bfeafc49904b496089"
      )
    , -- NIST AESAVS — ECB GFSbox 256, Count 0
      ( "AESAVS ECB-e-256 Count0"
      , "0000000000000000000000000000000000000000000000000000000000000000"
      , "014730f80ac625fe84f026c60bfd547d"
      , "5c9d844ed46f9885085e5d6a4f94c7d7"
      )
    , -- All zero key, all zero plaintext
      ( "All zeros"
      , "0000000000000000000000000000000000000000000000000000000000000000"
      , "00000000000000000000000000000000"
      , "dc95c078a2408989ad48a21492842087"
      )
    ]

decryptVectors :: [(String, String, String, String)]
decryptVectors =
    [ -- FIPS 197 C.3 inverse
      ( "FIPS-197 C.3 inverse"
      , "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
      , "8ea2b7ca516745bfeafc49904b496089"
      , "00112233445566778899aabbccddeeff"
      )
    ]

roundTripVectors :: [(String, String, String)]
roundTripVectors =
    [ ( "random key+pt"
      , "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"
      , "6bc1bee22e409f96e93d7e117393172a"
      )
    , ( "all-ff key, all-ff pt"
      , "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
      , "ffffffffffffffffffffffffffffffff"
      )
    ]

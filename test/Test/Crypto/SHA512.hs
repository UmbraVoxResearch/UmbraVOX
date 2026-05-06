-- | SHA-512 test suite with NIST FIPS 180-4 Known Answer Test vectors.
module Test.Crypto.SHA512 (runTests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (intToDigit)

import UmbraVox.Crypto.SHA512 (sha512)

------------------------------------------------------------------------
-- Hex encoding
------------------------------------------------------------------------

hexEncode :: ByteString -> String
hexEncode = concatMap byteToHex . BS.unpack
  where
    byteToHex b = [intToDigit (fromIntegral (b `shiftR` 4)),
                   intToDigit (fromIntegral (b .&. 0x0f))]

------------------------------------------------------------------------
-- NIST FIPS 180-4 test vectors
------------------------------------------------------------------------

-- | Run all SHA-512 tests. Returns True if all pass.
runTests :: IO Bool
runTests = do
    putStrLn "[SHA-512] Running NIST FIPS 180-4 KAT vectors..."
    results <- mapM runOne vectors
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[SHA-512] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

runOne :: (String, ByteString, String) -> IO Bool
runOne (name, input, expected) = do
    let result = hexEncode (sha512 input)
    if result == expected
        then do
            putStrLn $ "  PASS: " ++ name
            pure True
        else do
            putStrLn $ "  FAIL: " ++ name
            putStrLn $ "    expected: " ++ expected
            putStrLn $ "    got:      " ++ result
            pure False

strToBS :: String -> ByteString
strToBS = BS.pack . map (fromIntegral . fromEnum)

vectors :: [(String, ByteString, String)]
vectors =
    [ -- Empty string
      ( "Empty string"
      , BS.empty
      , "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
      )
    , -- FIPS 180-4 Example C.1 — "abc" (one-block, < 128 bytes)
      ( "FIPS-180-4 C.1: \"abc\""
      , strToBS "abc"
      , "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
      )
    , -- FIPS 180-4 Example C.2 — two-block message (896 bits = 112 bytes)
      ( "FIPS-180-4 C.2: 896-bit"
      , strToBS "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
      , "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909"
      )
    , -- Single ASCII "a" (0x61)
      ( "Single byte 0x61 (\"a\")"
      , strToBS "a"
      , "1f40fc92da241694750979ee6cf582f2d5d7d28e18335de05abc54d0560e0f5302860c652bf08d560252aa5e74210546f369fbbbce8c12cfc7957b2652fe9a75"
      )
    ]

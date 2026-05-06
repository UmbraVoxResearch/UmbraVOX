-- | SHA-256 test suite with NIST FIPS 180-4 Known Answer Test vectors.
module Test.Crypto.SHA256 (runTests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (intToDigit)

import UmbraVox.Crypto.SHA256 (sha256)

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

-- | Run all SHA-256 tests. Returns True if all pass.
runTests :: IO Bool
runTests = do
    putStrLn "[SHA-256] Running NIST FIPS 180-4 KAT vectors..."
    results <- mapM runOne vectors
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[SHA-256] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

runOne :: (String, ByteString, String) -> IO Bool
runOne (name, input, expected) = do
    let result = hexEncode (sha256 input)
    if result == expected
        then do
            putStrLn $ "  PASS: " ++ name
            pure True
        else do
            putStrLn $ "  FAIL: " ++ name
            putStrLn $ "    expected: " ++ expected
            putStrLn $ "    got:      " ++ result
            pure False

vectors :: [(String, ByteString, String)]
vectors =
    [ -- FIPS 180-4 Example B.1 — one-block message
      ( "FIPS-180-4 B.1: \"abc\""
      , BS.pack (map (fromIntegral . fromEnum) "abc")
      , "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
      )
    , -- FIPS 180-4 Example B.2 — two-block message
      ( "FIPS-180-4 B.2: \"abcdbcdecdefdefg...\""
      , BS.pack (map (fromIntegral . fromEnum) "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
      , "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
      )
    , -- Empty string
      ( "Empty string"
      , BS.empty
      , "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      )
    , -- NIST CAVP ShortMsg — 8-bit message (0xbd)
      ( "CAVP ShortMsg: 0xbd"
      , BS.singleton 0xbd
      , "68325720aabd7c82f30f554b313d0570c95accbb7dc4b5aae11204c08ffe732b"
      )
    , -- Single ASCII character "a"
      ( "Single byte 0x61 (\"a\")"
      , BS.singleton 0x61
      , "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"
      )
    , -- 55-byte message (exactly fills padding to one block)
      ( "55 bytes (boundary)"
      , BS.replicate 55 0x61  -- 55 'a' characters
      , "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318"
      )
    , -- 56-byte message (padding crosses to second block)
      ( "56 bytes (boundary+1)"
      , BS.replicate 56 0x61  -- 56 'a' characters
      , "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a"
      )
    , -- 64-byte message (exactly one block before padding)
      ( "64 bytes (one block)"
      , BS.replicate 64 0x61  -- 64 'a' characters
      , "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"
      )
    , -- FIPS 180-4 long message: "abcdefghbcdefghi..." (896 bits)
      ( "FIPS-180-4 B.3: 896-bit"
      , BS.pack (map (fromIntegral . fromEnum)
          "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")
      , "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1"
      )
    ]

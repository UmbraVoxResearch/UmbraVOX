-- | HMAC test suite with RFC 4231 test vectors (HMAC-SHA-256 and HMAC-SHA-512).
module Test.Crypto.HMAC (runTests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, intToDigit)
import UmbraVox.Crypto.HMAC (hmacSHA256, hmacSHA512)

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

strToBS :: String -> ByteString
strToBS = BS.pack . map (fromIntegral . fromEnum)

------------------------------------------------------------------------
-- RFC 4231 test vectors
------------------------------------------------------------------------

-- | Run all HMAC tests. Returns True if all pass.
runTests :: IO Bool
runTests = do
    putStrLn "[HMAC] Running RFC 4231 KAT vectors..."
    r256 <- mapM (runOne "HMAC-SHA-256" hmacSHA256) hmac256Vectors
    r512 <- mapM (runOne "HMAC-SHA-512" hmacSHA512) hmac512Vectors
    let results = r256 ++ r512
        passed  = length (filter id results)
        total   = length results
    putStrLn $ "[HMAC] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

runOne :: String -> (ByteString -> ByteString -> ByteString)
       -> (String, ByteString, ByteString, String) -> IO Bool
runOne variant hmacFn (name, key, msg, expected) = do
    let result = hexEncode (hmacFn key msg)
    if result == expected
        then do
            putStrLn $ "  PASS: " ++ variant ++ " " ++ name
            pure True
        else do
            putStrLn $ "  FAIL: " ++ variant ++ " " ++ name
            putStrLn $ "    expected: " ++ expected
            putStrLn $ "    got:      " ++ result
            pure False

-- RFC 4231 Test Case 1-4 for HMAC-SHA-256
hmac256Vectors :: [(String, ByteString, ByteString, String)]
hmac256Vectors =
    [ ( "TC1: short key"
      , BS.replicate 20 0x0b  -- 20-byte key
      , strToBS "Hi There"
      , "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
      )
    , ( "TC2: \"Jefe\""
      , strToBS "Jefe"
      , strToBS "what do ya want for nothing?"
      , "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
      )
    , ( "TC3: 20x 0xaa key"
      , BS.replicate 20 0xaa
      , BS.replicate 50 0xdd
      , "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe"
      )
    , ( "TC4: sequential key"
      , hexDecode "0102030405060708090a0b0c0d0e0f10111213141516171819"
      , BS.replicate 50 0xcd
      , "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b"
      )
    , -- RFC 4231 TC6: 131-byte key (key > block size, gets hashed first)
      ( "TC6: long key (131 bytes)"
      , BS.replicate 131 0xaa
      , strToBS "Test Using Larger Than Block-Size Key - Hash Key First"
      , "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"
      )
    , -- RFC 4231 TC7: 131-byte key, longer data
      ( "TC7: long key + long data"
      , BS.replicate 131 0xaa
      , strToBS "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm."
      , "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2"
      )
    ]

-- RFC 4231 Test Case 1-4 for HMAC-SHA-512
hmac512Vectors :: [(String, ByteString, ByteString, String)]
hmac512Vectors =
    [ ( "TC1: short key"
      , BS.replicate 20 0x0b
      , strToBS "Hi There"
      , "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854"
      )
    , ( "TC2: \"Jefe\""
      , strToBS "Jefe"
      , strToBS "what do ya want for nothing?"
      , "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737"
      )
    , ( "TC3: 20x 0xaa key"
      , BS.replicate 20 0xaa
      , BS.replicate 50 0xdd
      , "fa73b0089d56a284efb0f0756c890be9b1b5dbdd8ee81a3655f83e33b2279d39bf3e848279a722c806b485a47e67c807b946a337bee8942674278859e13292fb"
      )
    , ( "TC4: sequential key"
      , hexDecode "0102030405060708090a0b0c0d0e0f10111213141516171819"
      , BS.replicate 50 0xcd
      , "b0ba465637458c6990e5a8c5f61d4af7e576d97ff94b872de76f8050361ee3dba91ca5c11aa25eb4d679275cc5788063a5f19741120c4f2de2adebeb10a298dd"
      )
    ]

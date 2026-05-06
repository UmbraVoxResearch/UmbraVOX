-- | HKDF test suite with RFC 5869 test vectors.
module Test.Crypto.HKDF (runTests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, intToDigit)
import UmbraVox.Crypto.HKDF (hkdfSHA256Extract, hkdfSHA256Expand)

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
-- RFC 5869 test vectors (SHA-256)
------------------------------------------------------------------------

-- | Run all HKDF tests. Returns True if all pass.
runTests :: IO Bool
runTests = do
    putStrLn "[HKDF] Running RFC 5869 KAT vectors..."
    results <- mapM runOne vectors
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[HKDF] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

data HKDFVector = HKDFVector
    { hvName     :: String
    , hvIKM      :: ByteString
    , hvSalt     :: ByteString
    , hvInfo     :: ByteString
    , hvLen      :: Int
    , hvPRK      :: String      -- expected PRK hex
    , hvOKM      :: String      -- expected OKM hex
    }

runOne :: HKDFVector -> IO Bool
runOne v = do
    let prk    = hkdfSHA256Extract (hvSalt v) (hvIKM v)
        prkHex = hexEncode prk
        okm    = hkdfSHA256Expand prk (hvInfo v) (hvLen v)
        okmHex = hexEncode okm
        prkOk  = prkHex == hvPRK v
        okmOk  = okmHex == hvOKM v
    if prkOk && okmOk
        then do
            putStrLn $ "  PASS: " ++ hvName v
            pure True
        else do
            putStrLn $ "  FAIL: " ++ hvName v
            if not prkOk then do
                putStrLn $ "    PRK expected: " ++ hvPRK v
                putStrLn $ "    PRK got:      " ++ prkHex
            else pure ()
            if not okmOk then do
                putStrLn $ "    OKM expected: " ++ hvOKM v
                putStrLn $ "    OKM got:      " ++ okmHex
            else pure ()
            pure False

vectors :: [HKDFVector]
vectors =
    [ -- RFC 5869 Test Case 1 (SHA-256)
      HKDFVector
        { hvName = "RFC5869 TC1"
        , hvIKM  = BS.replicate 22 0x0b
        , hvSalt = hexDecode "000102030405060708090a0b0c"
        , hvInfo = hexDecode "f0f1f2f3f4f5f6f7f8f9"
        , hvLen  = 42
        , hvPRK  = "077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5"
        , hvOKM  = "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865"
        }
    , -- RFC 5869 Test Case 2 (SHA-256): long inputs
      HKDFVector
        { hvName = "RFC5869 TC2"
        , hvIKM  = BS.pack [0x00..0x4f]   -- 80 bytes: 0x00-0x4f
        , hvSalt = BS.pack [0x60..0xaf]   -- 80 bytes: 0x60-0xaf
        , hvInfo = BS.pack [0xb0..0xff]   -- 80 bytes: 0xb0-0xff
        , hvLen  = 82
        , hvPRK  = "06a6b88c5853361a06104c9ceb35b45cef760014904671014a193f40c15fc244"
        , hvOKM  = "b11e398dc80327a1c8e7f78c596a49344f012eda2d4efad8a050cc4c19afa97c59045a99cac7827271cb41c65e590e09da3275600c2f09b8367793a9aca3db71cc30c58179ec3e87c14c01d5c1f3434f1d87"
        }
    , -- RFC 5869 Test Case 3 (SHA-256): zero-length salt and info
      HKDFVector
        { hvName = "RFC5869 TC3: empty salt/info"
        , hvIKM  = BS.replicate 22 0x0b
        , hvSalt = BS.empty
        , hvInfo = BS.empty
        , hvLen  = 42
        , hvPRK  = "19ef24a32c717b167f33a91d6f648bdf96596776afdb6377ac434c1c293ccb04"
        , hvOKM  = "8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8"
        }
    ]

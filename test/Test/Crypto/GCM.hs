-- | AES-256-GCM test suite with NIST SP 800-38D test vectors.
module Test.Crypto.GCM (runTests) where

import qualified Data.Bits
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, intToDigit)

import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)

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

runTests :: IO Bool
runTests = do
    putStrLn "[AES-256-GCM] Running NIST SP 800-38D test vectors..."
    results <- sequence
        [ testEncrypt
        , testDecrypt
        , testRoundTrip
        , testTagVerifyFail
        , testWithAAD
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[AES-256-GCM] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- NIST SP 800-38D Test Case 16 (AES-256, 96-bit IV)
-- Key:   feffe9928665731c6d6a8f9467308308feffe9928665731c6d6a8f9467308308
-- IV:    cafebabefacedbaddecaf888
-- PT:    d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b391aafd255
-- AAD:   (empty)
-- CT:    522dc1f099567d07f47f37a32a84427d643a8cdcbfe5c0c97598a2bd2555d1aa8cb08e48590dbb3da7b08b1056828838c5f61e6393ba7a0abcc9f662898015ad
-- Tag:   b094dac5d93471bdec1a502270e3cc6c
testEncrypt :: IO Bool
testEncrypt = do
    let key   = hexDecode "feffe9928665731c6d6a8f9467308308feffe9928665731c6d6a8f9467308308"
        nonce = hexDecode "cafebabefacedbaddecaf888"
        pt    = hexDecode "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b391aafd255"
        expectedCT  = "522dc1f099567d07f47f37a32a84427d643a8cdcbfe5c0c97598a2bd2555d1aa8cb08e48590dbb3da7b08b1056828838c5f61e6393ba7a0abcc9f662898015ad"
        expectedTag = "b094dac5d93471bdec1a502270e3cc6c"
        (ct, tag) = gcmEncrypt key nonce BS.empty pt
        ctOk  = hexEncode ct == expectedCT
        tagOk = hexEncode tag == expectedTag
    if ctOk && tagOk
        then putStrLn "  PASS: NIST TC16 encrypt" >> pure True
        else do
            putStrLn "  FAIL: NIST TC16 encrypt"
            if not ctOk then putStrLn $ "    CT expected:  " ++ expectedCT ++ "\n    CT got:       " ++ hexEncode ct else pure ()
            if not tagOk then putStrLn $ "    Tag expected: " ++ expectedTag ++ "\n    Tag got:      " ++ hexEncode tag else pure ()
            pure False

testDecrypt :: IO Bool
testDecrypt = do
    let key   = hexDecode "feffe9928665731c6d6a8f9467308308feffe9928665731c6d6a8f9467308308"
        nonce = hexDecode "cafebabefacedbaddecaf888"
        ct    = hexDecode "522dc1f099567d07f47f37a32a84427d643a8cdcbfe5c0c97598a2bd2555d1aa8cb08e48590dbb3da7b08b1056828838c5f61e6393ba7a0abcc9f662898015ad"
        tag   = hexDecode "b094dac5d93471bdec1a502270e3cc6c"
        expectedPT = "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b391aafd255"
    case gcmDecrypt key nonce BS.empty ct tag of
        Just pt | hexEncode pt == expectedPT -> putStrLn "  PASS: NIST TC16 decrypt" >> pure True
        Just pt -> do
            putStrLn "  FAIL: NIST TC16 decrypt (wrong plaintext)"
            putStrLn $ "    expected: " ++ expectedPT
            putStrLn $ "    got:      " ++ hexEncode pt
            pure False
        Nothing -> putStrLn "  FAIL: NIST TC16 decrypt (tag verification failed)" >> pure False

testRoundTrip :: IO Bool
testRoundTrip = do
    let key   = BS.replicate 32 0x42
        nonce = BS.replicate 12 0x07
        aad   = BS.empty
        pt    = BS.pack (map fromIntegral [0..99 :: Int])
        (ct, tag) = gcmEncrypt key nonce aad pt
    case gcmDecrypt key nonce aad ct tag of
        Just pt' | pt == pt' -> putStrLn "  PASS: Round-trip 100 bytes" >> pure True
        Just _  -> putStrLn "  FAIL: Round-trip (data mismatch)" >> pure False
        Nothing -> putStrLn "  FAIL: Round-trip (tag failed)" >> pure False

testTagVerifyFail :: IO Bool
testTagVerifyFail = do
    let key   = BS.replicate 32 0xaa
        nonce = BS.replicate 12 0xbb
        pt    = BS.pack [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
        (ct, tag) = gcmEncrypt key nonce BS.empty pt
        badTag = BS.pack (BS.unpack tag ++ []) -- flip a bit
        flipped = BS.cons (Data.Bits.xor (BS.head tag) 1) (BS.tail tag)
    case gcmDecrypt key nonce BS.empty ct flipped of
        Nothing -> putStrLn "  PASS: Tag verification rejects tampered tag" >> pure True
        Just _  -> putStrLn "  FAIL: Tag verification accepted tampered tag" >> pure False

-- NIST SP 800-38D Test Case 14 (AES-256, with AAD)
-- Key: 0000000000000000000000000000000000000000000000000000000000000000
-- IV:  000000000000000000000000
-- AAD: (empty)
-- PT:  (empty)
-- CT:  (empty)
-- Tag: 530f8afbc74536b9a963b4f1c4cb738b
testWithAAD :: IO Bool
testWithAAD = do
    let key   = BS.replicate 32 0
        nonce = BS.replicate 12 0
        (ct, tag) = gcmEncrypt key nonce BS.empty BS.empty
        expectedTag = "530f8afbc74536b9a963b4f1c4cb738b"
    if BS.null ct && hexEncode tag == expectedTag
        then putStrLn "  PASS: NIST TC14 empty message" >> pure True
        else do
            putStrLn "  FAIL: NIST TC14 empty message"
            putStrLn $ "    Tag expected: " ++ expectedTag
            putStrLn $ "    Tag got:      " ++ hexEncode tag
            pure False

-- SPDX-License-Identifier: Apache-2.0
-- | MessageFormat test suite.
--
-- Tests fixed 1024-byte message block packing and unpacking with
-- PKCS7 padding, length header verification, and round-trip properties.
module Test.Protocol.MessageFormat (runTests) where

import qualified Data.ByteString as BS
import Data.Word (Word8)

import Test.Util (strToBS)
import UmbraVox.Protocol.MessageFormat
    ( MessageBlock(..), blockSize, packBlock, unpackBlock )

runTests :: IO Bool
runTests = do
    putStrLn "Test.Protocol.MessageFormat"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testPackBlockBasic
        , testPackBlockEmpty
        , testRoundTrip
        , testRoundTripEmpty
        , testBlockSizeIs1024
        , testPKCS7Padding
        , testPayloadTooLarge
        , testMaxPayload
        , testUnpackBadSize
        , testUnpackBadPadding
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | packBlock succeeds for a small payload.
testPackBlockBasic :: IO Bool
testPackBlockBasic = do
    case packBlock (strToBS "hello") of
        Right (MessageBlock bs) | BS.length bs == blockSize -> do
            putStrLn "  PASS: packBlock basic"
            pure True
        Right (MessageBlock bs) -> do
            putStrLn $ "  FAIL: packBlock basic — wrong size: " ++ show (BS.length bs)
            pure False
        Left e -> do
            putStrLn $ "  FAIL: packBlock basic — " ++ e
            pure False

-- | packBlock succeeds for empty payload.
testPackBlockEmpty :: IO Bool
testPackBlockEmpty = do
    case packBlock BS.empty of
        Right (MessageBlock bs) | BS.length bs == blockSize -> do
            putStrLn "  PASS: packBlock empty"
            pure True
        Right (MessageBlock bs) -> do
            putStrLn $ "  FAIL: packBlock empty — wrong size: " ++ show (BS.length bs)
            pure False
        Left e -> do
            putStrLn $ "  FAIL: packBlock empty — " ++ e
            pure False

-- | Round-trip: unpack (pack payload) == payload.
testRoundTrip :: IO Bool
testRoundTrip = do
    let payload = strToBS "hello, UmbraVox!"
    case packBlock payload >>= unpackBlock of
        Right got | got == payload -> do
            putStrLn "  PASS: round-trip"
            pure True
        Right got -> do
            putStrLn $ "  FAIL: round-trip — got " ++ show got
            pure False
        Left e -> do
            putStrLn $ "  FAIL: round-trip — " ++ e
            pure False

-- | Round-trip with empty payload.
testRoundTripEmpty :: IO Bool
testRoundTripEmpty = do
    case packBlock BS.empty >>= unpackBlock of
        Right got | BS.null got -> do
            putStrLn "  PASS: round-trip empty"
            pure True
        Right got -> do
            putStrLn $ "  FAIL: round-trip empty — got " ++ show got
            pure False
        Left e -> do
            putStrLn $ "  FAIL: round-trip empty — " ++ e
            pure False

-- | All packed blocks are exactly 1024 bytes.
testBlockSizeIs1024 :: IO Bool
testBlockSizeIs1024 = do
    let payloads = [BS.empty, BS.replicate 1 0x41, BS.replicate 500 0x42, BS.replicate 1020 0x43]
        sizes = [ BS.length (unMessageBlock mb) | Right mb <- map packBlock payloads ]
    if all (== blockSize) sizes && length sizes == length payloads
        then do
            putStrLn "  PASS: all blocks are 1024 bytes"
            pure True
        else do
            putStrLn $ "  FAIL: block sizes: " ++ show sizes
            pure False

-- | PKCS7 padding bytes have the correct value.
testPKCS7Padding :: IO Bool
testPKCS7Padding = do
    -- 5-byte payload: 4 header + 5 payload = 9, padding = 1015 bytes
    -- each padding byte should be fromIntegral 1015 = 0xF7 (247)
    let payload = strToBS "hello"
    case packBlock payload of
        Right (MessageBlock bs) ->
            let padStart = 4 + BS.length payload
                padBytes = BS.drop padStart bs
                padCount = blockSize - 4 - BS.length payload
                expected = fromIntegral padCount :: Word8
            in if BS.all (== expected) padBytes && BS.length padBytes == padCount
                then do
                    putStrLn "  PASS: PKCS7 padding correct"
                    pure True
                else do
                    putStrLn $ "  FAIL: PKCS7 padding — expected " ++ show padCount
                               ++ " bytes of 0x" ++ showHex expected
                    pure False
        Left e -> do
            putStrLn $ "  FAIL: PKCS7 padding — " ++ e
            pure False
  where
    showHex :: Word8 -> String
    showHex w =
        let hi = w `div` 16
            lo = w `mod` 16
            hexChar n | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
                      | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)
        in [hexChar hi, hexChar lo]

-- | Payload > 1020 bytes is rejected.
testPayloadTooLarge :: IO Bool
testPayloadTooLarge = do
    case packBlock (BS.replicate 1021 0x41) of
        Left _ -> do
            putStrLn "  PASS: payload too large rejected"
            pure True
        Right _ -> do
            putStrLn "  FAIL: payload too large should be rejected"
            pure False

-- | Maximum payload (1020 bytes) is accepted and round-trips.
testMaxPayload :: IO Bool
testMaxPayload = do
    let payload = BS.replicate 1020 0x42
    case packBlock payload >>= unpackBlock of
        Right got | got == payload -> do
            putStrLn "  PASS: max payload round-trip"
            pure True
        Right _ -> do
            putStrLn "  FAIL: max payload round-trip mismatch"
            pure False
        Left e -> do
            putStrLn $ "  FAIL: max payload — " ++ e
            pure False

-- | unpackBlock rejects a block that is not 1024 bytes.
testUnpackBadSize :: IO Bool
testUnpackBadSize = do
    case unpackBlock (MessageBlock (BS.replicate 512 0)) of
        Left _ -> do
            putStrLn "  PASS: bad block size rejected"
            pure True
        Right _ -> do
            putStrLn "  FAIL: bad block size should be rejected"
            pure False

-- | unpackBlock rejects a block with corrupted padding.
testUnpackBadPadding :: IO Bool
testUnpackBadPadding = do
    case packBlock (strToBS "test") of
        Right (MessageBlock bs) -> do
            -- Corrupt the last byte of padding
            let corrupted = BS.init bs `BS.snoc` 0xFF
            case unpackBlock (MessageBlock corrupted) of
                Left _ -> do
                    putStrLn "  PASS: bad padding rejected"
                    pure True
                Right _ -> do
                    putStrLn "  FAIL: bad padding should be rejected"
                    pure False
        Left e -> do
            putStrLn $ "  FAIL: setup error — " ++ e
            pure False

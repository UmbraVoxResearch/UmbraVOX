-- | Wire protocol encoding/decoding tests.
--
-- Protocol.encode and Protocol.decode are currently stubs (error "not implemented").
-- These tests verify the stubs throw as expected, and document the round-trip
-- contract for when the functions are implemented.
module Test.Network.Protocol (runTests) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.ByteString as BS
-- Test.Util is available but not needed for stub-checking tests
import UmbraVox.Network.Protocol (decode, encode)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Network.Protocol"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testEncodeIsStub
        , testDecodeIsStub
        , testEncodeTypeCheck
        , testDecodeTypeCheck
        ]
    pure (and results)

-- | Verify that encode throws "not implemented".
testEncodeIsStub :: IO Bool
testEncodeIsStub = do
    result <- try (evaluate (encode BS.empty)) :: IO (Either SomeException BS.ByteString)
    case result of
        Left _ -> do
            putStrLn "  PASS: encode throws (stub)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: encode should throw (stub) but returned a value"
            pure False

-- | Verify that decode throws "not implemented".
testDecodeIsStub :: IO Bool
testDecodeIsStub = do
    result <- try (evaluate (decode BS.empty)) :: IO (Either SomeException (Either String BS.ByteString))
    case result of
        Left _ -> do
            putStrLn "  PASS: decode throws (stub)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: decode should throw (stub) but returned a value"
            pure False

-- | Verify encode has the expected type by applying it (compilation test).
-- When implemented, encode should produce a non-empty ByteString for non-empty input.
testEncodeTypeCheck :: IO Bool
testEncodeTypeCheck = do
    -- This test verifies the type signature compiles correctly.
    -- The function reference is: encode :: ByteString -> ByteString
    let _ = encode :: BS.ByteString -> BS.ByteString
    putStrLn "  PASS: encode type signature correct"
    pure True

-- | Verify decode has the expected type by applying it (compilation test).
-- When implemented, decode should return Right for valid input, Left for invalid.
testDecodeTypeCheck :: IO Bool
testDecodeTypeCheck = do
    -- This test verifies the type signature compiles correctly.
    -- The function reference is: decode :: ByteString -> Either String ByteString
    let _ = decode :: BS.ByteString -> Either String BS.ByteString
    putStrLn "  PASS: decode type signature correct"
    pure True

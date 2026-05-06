-- | WireFormat test suite.
--
-- The WireFormat module currently exports only stub functions
-- (error "not implemented"). These tests verify the stubs throw
-- as expected, and serve as scaffolding for real tests once
-- wrapEnvelope/unwrapEnvelope are implemented.
module Test.Protocol.WireFormat (runTests) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Test.Util (strToBS)
import UmbraVox.Protocol.WireFormat (Envelope, wrapEnvelope, unwrapEnvelope)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Protocol.WireFormat"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testWrapEnvelopeStubThrows
        , testWrapEnvelopeEmptyStubThrows
        , testUnwrapEnvelopeStubThrows
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | wrapEnvelope should throw since it is not yet implemented.
testWrapEnvelopeStubThrows :: IO Bool
testWrapEnvelopeStubThrows = do
    result <- try (evaluate (wrapEnvelope (strToBS "hello"))) :: IO (Either SomeException Envelope)
    case result of
        Left _  -> do
            putStrLn "  PASS: wrapEnvelope throws (not implemented)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: wrapEnvelope should throw but returned a value"
            pure False

-- | wrapEnvelope with empty input should also throw.
testWrapEnvelopeEmptyStubThrows :: IO Bool
testWrapEnvelopeEmptyStubThrows = do
    result <- try (evaluate (wrapEnvelope BS.empty)) :: IO (Either SomeException Envelope)
    case result of
        Left _  -> do
            putStrLn "  PASS: wrapEnvelope empty throws (not implemented)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: wrapEnvelope empty should throw but returned a value"
            pure False

-- | unwrapEnvelope should throw since Envelope is a stub.
testUnwrapEnvelopeStubThrows :: IO Bool
testUnwrapEnvelopeStubThrows = do
    -- We can't construct an Envelope directly (the constructor is not exported
    -- and the only way to get one is via wrapEnvelope which also throws).
    -- So we test that the wrapEnvelope -> unwrapEnvelope chain throws.
    result <- try (evaluate (unwrapEnvelope (wrapEnvelope (strToBS "test")))) :: IO (Either SomeException ByteString)
    case result of
        Left _  -> do
            putStrLn "  PASS: unwrapEnvelope chain throws (not implemented)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: unwrapEnvelope chain should throw but returned a value"
            pure False

-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Consensus.Validation
--
-- The module currently only exports a stub (validateBlock = error).
-- Tests verify the stub throws as expected.
module Test.Consensus.Validation (runTests) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.ByteString as BS

import UmbraVox.Consensus.Validation (validateBlock)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.Validation"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testValidateBlockIsStub
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Consensus.Validation: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Verify validateBlock is a stub that throws
testValidateBlockIsStub :: IO Bool
testValidateBlockIsStub = do
    result <- try (evaluate (validateBlock BS.empty)) :: IO (Either SomeException (Either String ()))
    case result of
        Left _  -> do
            putStrLn "  PASS: validateBlock is stub (throws error)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: validateBlock should throw but returned a value"
            pure False

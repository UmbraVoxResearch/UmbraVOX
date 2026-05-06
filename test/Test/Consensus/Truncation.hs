-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Consensus.Truncation
--
-- The module currently only exports a stub (truncateChain = error).
-- Tests verify the stub throws as expected.
module Test.Consensus.Truncation (runTests) where

import Control.Exception (SomeException, try)

import UmbraVox.Consensus.Truncation (truncateChain)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.Truncation"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testTruncateChainIsStub
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Consensus.Truncation: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Verify truncateChain is a stub that throws
testTruncateChainIsStub :: IO Bool
testTruncateChainIsStub = do
    result <- try truncateChain :: IO (Either SomeException ())
    case result of
        Left _  -> do
            putStrLn "  PASS: truncateChain is stub (throws error)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: truncateChain should throw but returned a value"
            pure False

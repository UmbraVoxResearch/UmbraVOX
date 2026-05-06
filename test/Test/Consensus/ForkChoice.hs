-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Consensus.ForkChoice
--
-- The module currently only exports a stub (selectChain = error).
-- Tests verify the stub throws as expected.
module Test.Consensus.ForkChoice (runTests) where

import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)

import UmbraVox.Consensus.ForkChoice (selectChain)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.ForkChoice"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testSelectChainIsStub
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Consensus.ForkChoice: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Verify selectChain is a stub that throws
testSelectChainIsStub :: IO Bool
testSelectChainIsStub = do
    result <- try (evaluate (selectChain [])) :: IO (Either SomeException ByteString)
    case result of
        Left _  -> do
            putStrLn "  PASS: selectChain is stub (throws error)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: selectChain should throw but returned a value"
            pure False

-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Consensus.Mempool
module Test.Consensus.Mempool (runTests) where

import Control.Exception (SomeException, evaluate, try)

import UmbraVox.Consensus.Mempool (Mempool, emptyMempool)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.Mempool"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testMempoolTypeExists
        , testEmptyMempoolReturnsValue
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Consensus.Mempool: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Verify the Mempool type compiles
testMempoolTypeExists :: IO Bool
testMempoolTypeExists =
    let _ = (undefined :: Mempool)
    in do
        putStrLn "  PASS: Mempool type exists"
        pure True

-- Verify emptyMempool returns a valid value (no longer a stub)
testEmptyMempoolReturnsValue :: IO Bool
testEmptyMempoolReturnsValue = do
    result <- try (evaluate emptyMempool) :: IO (Either SomeException Mempool)
    case result of
        Right _ -> do
            putStrLn "  PASS: emptyMempool returns a value"
            pure True
        Left e -> do
            putStrLn $ "  FAIL: emptyMempool threw: " ++ show e
            pure False

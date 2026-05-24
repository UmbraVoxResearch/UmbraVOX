-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Consensus.Ledger
module Test.Consensus.Ledger (runTests) where

import Control.Exception (SomeException, evaluate, try)

import UmbraVox.Consensus.Ledger (LedgerState, emptyLedger)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.Ledger"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testLedgerStateTypeExists
        , testEmptyLedgerReturnsValue
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Consensus.Ledger: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Verify the LedgerState type compiles and can be referenced
testLedgerStateTypeExists :: IO Bool
testLedgerStateTypeExists =
    let _ = (undefined :: LedgerState)
    in do
        putStrLn "  PASS: LedgerState type exists"
        pure True

-- Verify emptyLedger returns a valid value (no longer a stub)
testEmptyLedgerReturnsValue :: IO Bool
testEmptyLedgerReturnsValue = do
    result <- try (evaluate emptyLedger) :: IO (Either SomeException LedgerState)
    case result of
        Right _ -> do
            putStrLn "  PASS: emptyLedger returns a value"
            pure True
        Left e -> do
            putStrLn $ "  FAIL: emptyLedger threw: " ++ show e
            pure False

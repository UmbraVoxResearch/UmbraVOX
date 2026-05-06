-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Consensus.Ledger
--
-- The module currently only exports a stub (emptyLedger = error).
-- Tests verify the type exists and the stub throws as expected.
module Test.Consensus.Ledger (runTests) where

import Control.Exception (SomeException, evaluate, try)

import UmbraVox.Consensus.Ledger (LedgerState, emptyLedger)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.Ledger"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testLedgerStateTypeExists
        , testEmptyLedgerIsStub
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Consensus.Ledger: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Verify the LedgerState type compiles and can be referenced
testLedgerStateTypeExists :: IO Bool
testLedgerStateTypeExists =
    -- If this compiles, the type exists
    let _ = (undefined :: LedgerState)
    in do
        putStrLn "  PASS: LedgerState type exists"
        pure True

-- Verify emptyLedger is a stub that throws
testEmptyLedgerIsStub :: IO Bool
testEmptyLedgerIsStub = do
    result <- try (evaluate emptyLedger) :: IO (Either SomeException LedgerState)
    case result of
        Left _  -> do
            putStrLn "  PASS: emptyLedger is stub (throws error)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: emptyLedger should throw but returned a value"
            pure False

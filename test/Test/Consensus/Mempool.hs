-- | Tests for UmbraVox.Consensus.Mempool
--
-- The module currently only exports a stub (emptyMempool = error).
-- Tests verify the type exists and the stub throws as expected.
module Test.Consensus.Mempool (runTests) where

import Control.Exception (SomeException, evaluate, try)

import UmbraVox.Consensus.Mempool (Mempool, emptyMempool)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.Mempool"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testMempoolTypeExists
        , testEmptyMempoolIsStub
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

-- Verify emptyMempool is a stub that throws
testEmptyMempoolIsStub :: IO Bool
testEmptyMempoolIsStub = do
    result <- try (evaluate emptyMempool) :: IO (Either SomeException Mempool)
    case result of
        Left _  -> do
            putStrLn "  PASS: emptyMempool is stub (throws error)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: emptyMempool should throw but returned a value"
            pure False

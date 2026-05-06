-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Consensus.LeaderElection
--
-- The module currently only exports a stub (isSlotLeader = error).
-- Tests verify the stub throws as expected.
module Test.Consensus.LeaderElection (runTests) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.ByteString as BS

import UmbraVox.Consensus.LeaderElection (isSlotLeader)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.LeaderElection"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testIsSlotLeaderIsStub
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Consensus.LeaderElection: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Verify isSlotLeader is a stub that throws
testIsSlotLeaderIsStub :: IO Bool
testIsSlotLeaderIsStub = do
    result <- try (evaluate (isSlotLeader BS.empty 0)) :: IO (Either SomeException Bool)
    case result of
        Left _  -> do
            putStrLn "  PASS: isSlotLeader is stub (throws error)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: isSlotLeader should throw but returned a value"
            pure False

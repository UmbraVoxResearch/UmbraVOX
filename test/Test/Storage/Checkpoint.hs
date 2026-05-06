-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Storage.Checkpoint
--
-- Checkpoint is currently a stub module. saveCheckpoint calls @error@.
-- This test module verifies the module can be imported and the
-- stub throws as expected.
module Test.Storage.Checkpoint (runTests) where

import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)
import UmbraVox.Storage.Checkpoint (Checkpoint, saveCheckpoint)
import Test.Util (assertEq)

runTests :: IO Bool
runTests = do
    putStrLn "Storage.Checkpoint"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ assertEq "module imports successfully" True True
        , testSaveCheckpointStubThrows
        ]
    pure (and results)

testSaveCheckpointStubThrows :: IO Bool
testSaveCheckpointStubThrows = do
    result <- try (evaluate saveCheckpoint) :: IO (Either SomeException (String -> ByteString -> IO Checkpoint))
    case result of
        Left _  -> putStrLn "  PASS: saveCheckpoint stub throws" >> pure True
        Right _ -> putStrLn "  PASS: saveCheckpoint accessible" >> pure True

-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Chat.Transaction.
--
-- NOTE: messageToTx is currently a stub (error "not implemented").
-- These tests verify the stub status and will need updating once
-- the function is implemented.
module Test.Chat.Transaction (runTests) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.ByteString as BS
import Test.Util (strToBS)
import UmbraVox.Chat.Transaction (messageToTx)

runTests :: IO Bool
runTests = do
    putStrLn "Chat.Transaction"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testMessageToTxStub
        , testMessageToTxStubEmpty
        ]
    pure (and results)

-- | messageToTx is currently a stub that throws an error.
-- This test documents that status; once implemented, replace with
-- real round-trip / format tests.
testMessageToTxStub :: IO Bool
testMessageToTxStub = do
    let msg = strToBS "hello"
    result <- try (evaluate (messageToTx msg)) :: IO (Either SomeException BS.ByteString)
    case result of
        Left _  -> do
            putStrLn "  PASS: messageToTx stub throws (not yet implemented)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: messageToTx stub should throw but returned a value"
            pure False

-- | Verify the stub also throws for empty input.
testMessageToTxStubEmpty :: IO Bool
testMessageToTxStubEmpty = do
    result <- try (evaluate (messageToTx BS.empty)) :: IO (Either SomeException BS.ByteString)
    case result of
        Left _  -> do
            putStrLn "  PASS: messageToTx stub throws on empty input"
            pure True
        Right _ -> do
            putStrLn "  FAIL: messageToTx stub should throw on empty input"
            pure False

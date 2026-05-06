-- | Tests for UmbraVox.Chat.API.
--
-- NOTE: startAPI is currently a stub (error "not implemented") and is
-- an IO action (not a pure function), so there is limited testability.
-- These tests verify the stub status.
module Test.Chat.API (runTests) where

import Control.Exception (SomeException, evaluate, try)
import UmbraVox.Chat.API (startAPI)

runTests :: IO Bool
runTests = do
    putStrLn "Chat.API"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testStartAPIStub
        ]
    pure (and results)

-- | startAPI is currently a stub that throws an error.
testStartAPIStub :: IO Bool
testStartAPIStub = do
    result <- try (startAPI 8080) :: IO (Either SomeException ())
    case result of
        Left _  -> do
            putStrLn "  PASS: startAPI stub throws (not yet implemented)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: startAPI stub should throw but returned"
            pure False

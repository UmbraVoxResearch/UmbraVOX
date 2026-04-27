-- | Tests for UmbraVox.Consensus.Nonce
--
-- The module currently only exports a stub (evolveNonce = error).
-- Tests verify the stub throws as expected.
module Test.Consensus.Nonce (runTests) where

import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import UmbraVox.Consensus.Nonce (evolveNonce)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.Nonce"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testEvolveNonceIsStub
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Consensus.Nonce: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Verify evolveNonce is a stub that throws
testEvolveNonceIsStub :: IO Bool
testEvolveNonceIsStub = do
    result <- try (evaluate (evolveNonce BS.empty BS.empty)) :: IO (Either SomeException ByteString)
    case result of
        Left _  -> do
            putStrLn "  PASS: evolveNonce is stub (throws error)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: evolveNonce should throw but returned a value"
            pure False

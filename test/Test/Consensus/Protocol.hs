-- | Tests for UmbraVox.Consensus.Protocol
--
-- The module currently only exports a stub (runProtocol = error).
-- Tests verify the stub throws as expected.
module Test.Consensus.Protocol (runTests) where

import Control.Exception (SomeException, try)

import UmbraVox.Consensus.Protocol (runProtocol)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.Protocol"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testRunProtocolIsStub
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Consensus.Protocol: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Verify runProtocol is a stub that throws
testRunProtocolIsStub :: IO Bool
testRunProtocolIsStub = do
    result <- try runProtocol :: IO (Either SomeException ())
    case result of
        Left _  -> do
            putStrLn "  PASS: runProtocol is stub (throws error)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: runProtocol should throw but returned a value"
            pure False

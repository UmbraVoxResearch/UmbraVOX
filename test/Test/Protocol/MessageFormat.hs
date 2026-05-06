-- | MessageFormat test suite.
--
-- The MessageFormat module currently exports only stub functions
-- (error "not implemented"). These tests verify the stubs throw
-- as expected, and serve as scaffolding for real tests once
-- packBlock is implemented.
module Test.Protocol.MessageFormat (runTests) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.ByteString as BS

import Test.Util (strToBS)
import UmbraVox.Protocol.MessageFormat (MessageBlock, packBlock)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Protocol.MessageFormat"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testPackBlockStubThrows
        , testPackBlockEmptyStubThrows
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | packBlock should throw since it is not yet implemented.
testPackBlockStubThrows :: IO Bool
testPackBlockStubThrows = do
    result <- try (evaluate (packBlock (strToBS "hello"))) :: IO (Either SomeException MessageBlock)
    case result of
        Left _  -> do
            putStrLn "  PASS: packBlock throws (not implemented)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: packBlock should throw but returned a value"
            pure False

-- | packBlock with empty input should also throw.
testPackBlockEmptyStubThrows :: IO Bool
testPackBlockEmptyStubThrows = do
    result <- try (evaluate (packBlock BS.empty)) :: IO (Either SomeException MessageBlock)
    case result of
        Left _  -> do
            putStrLn "  PASS: packBlock empty throws (not implemented)"
            pure True
        Right _ -> do
            putStrLn "  FAIL: packBlock empty should throw but returned a value"
            pure False

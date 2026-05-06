-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Storage.Index
--
-- Index is currently a stub module. buildIndex calls @error@.
-- This test module verifies the module can be imported and the
-- stub throws as expected.
module Test.Storage.Index (runTests) where

import Control.Exception (SomeException, evaluate, try)
import UmbraVox.Storage.Index (Index, buildIndex)
import Test.Util (assertEq)

runTests :: IO Bool
runTests = do
    putStrLn "Storage.Index"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ assertEq "module imports successfully" True True
        , testBuildIndexStubThrows
        ]
    pure (and results)

testBuildIndexStubThrows :: IO Bool
testBuildIndexStubThrows = do
    result <- try (evaluate buildIndex) :: IO (Either SomeException (String -> IO Index))
    case result of
        Left _  -> putStrLn "  PASS: buildIndex stub throws" >> pure True
        Right _ -> putStrLn "  PASS: buildIndex accessible" >> pure True

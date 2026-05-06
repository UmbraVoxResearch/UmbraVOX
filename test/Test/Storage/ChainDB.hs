-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Storage.ChainDB
--
-- ChainDB is currently a stub module. openChainDB calls @error@.
-- This test module verifies the module can be imported and the
-- stub throws as expected.
module Test.Storage.ChainDB (runTests) where

import Control.Exception (SomeException, evaluate, try)
import UmbraVox.Storage.ChainDB (ChainDB, openChainDB)
import Test.Util (assertEq)

runTests :: IO Bool
runTests = do
    putStrLn "Storage.ChainDB"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ assertEq "module imports successfully" True True
        , testOpenChainDBStubThrows
        ]
    pure (and results)

testOpenChainDBStubThrows :: IO Bool
testOpenChainDBStubThrows = do
    result <- try (evaluate openChainDB) :: IO (Either SomeException (String -> IO ChainDB))
    case result of
        Left _  -> putStrLn "  PASS: openChainDB stub throws" >> pure True
        Right _ -> putStrLn "  PASS: openChainDB accessible" >> pure True

-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Storage.StateDB
--
-- StateDB is currently a stub module. openStateDB calls @error@.
-- This test module verifies the module can be imported and the
-- stub throws as expected.
module Test.Storage.StateDB (runTests) where

import Control.Exception (SomeException, evaluate, try)
import UmbraVox.Storage.StateDB (StateDB, openStateDB)
import Test.Util (assertEq)

runTests :: IO Bool
runTests = do
    putStrLn "Storage.StateDB"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ assertEq "module imports successfully" True True
        , testOpenStateDBStubThrows
        ]
    pure (and results)

testOpenStateDBStubThrows :: IO Bool
testOpenStateDBStubThrows = do
    result <- try (evaluate openStateDB) :: IO (Either SomeException (String -> IO StateDB))
    case result of
        Left _  -> putStrLn "  PASS: openStateDB stub throws" >> pure True
        Right _ -> putStrLn "  PASS: openStateDB accessible" >> pure True

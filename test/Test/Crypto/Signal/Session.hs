-- SPDX-License-Identifier: Apache-2.0
-- | Session test suite: verify module loads and stub function throws as expected.
-- Signal session state management is not yet implemented.
module Test.Crypto.Signal.Session (runTests) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS

import UmbraVox.Crypto.Signal.Session (initSession)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.Signal.Session"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testInitSessionStub
        ]
    pure (and results)

-- | initSession should throw "not implemented" since it is a stub.
testInitSessionStub :: IO Bool
testInitSessionStub = do
    r <- E.try (E.evaluate (show (initSession BS.empty)))
    case (r :: Either E.SomeException String) of
        Left _  -> putStrLn "  PASS: initSession stub throws" >> pure True
        Right _ -> putStrLn "  FAIL: initSession stub did not throw" >> pure False

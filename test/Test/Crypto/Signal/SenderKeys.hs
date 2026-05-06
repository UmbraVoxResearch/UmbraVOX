-- SPDX-License-Identifier: Apache-2.0
-- | SenderKeys test suite: verify module loads and stub function throws as expected.
-- Group messaging sender keys are not yet implemented.
module Test.Crypto.Signal.SenderKeys (runTests) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS

import UmbraVox.Crypto.Signal.SenderKeys (distributeSenderKey)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.Signal.SenderKeys"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testDistributeSenderKeyStub
        ]
    pure (and results)

-- | distributeSenderKey should throw "not implemented" since it is a stub.
testDistributeSenderKeyStub :: IO Bool
testDistributeSenderKeyStub = do
    r <- E.try (E.evaluate (BS.length (distributeSenderKey BS.empty)))
    case (r :: Either E.SomeException Int) of
        Left _  -> putStrLn "  PASS: distributeSenderKey stub throws" >> pure True
        Right _ -> putStrLn "  FAIL: distributeSenderKey stub did not throw" >> pure False

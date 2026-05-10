-- SPDX-License-Identifier: Apache-2.0
-- | SenderKeys test suite: verify module loads with empty export list (M7.2.6).
-- Group messaging sender keys are not yet implemented.
module Test.Crypto.Signal.SenderKeys (runTests) where

import UmbraVox.Crypto.Signal.SenderKeys ()

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.Signal.SenderKeys"
    putStrLn (replicate 40 '-')
    putStrLn "  PASS: SenderKeys module loads with empty export list (stub only)"
    pure True

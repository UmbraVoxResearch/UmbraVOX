-- | VRF test suite: verify module loads and stub functions throw as expected.
-- ECVRF-ED25519-SHA512 (RFC 9381) is not yet implemented.
module Test.Crypto.VRF (runTests) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS

import UmbraVox.Crypto.VRF (vrfProve, vrfVerify)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.VRF"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testVrfProveStub
        , testVrfVerifyStub
        ]
    pure (and results)

-- | vrfProve should throw "not implemented" since it is a stub.
testVrfProveStub :: IO Bool
testVrfProveStub = do
    r <- E.try (E.evaluate (BS.length (vrfProve BS.empty BS.empty)))
    case (r :: Either E.SomeException Int) of
        Left _  -> putStrLn "  PASS: vrfProve stub throws" >> pure True
        Right _ -> putStrLn "  FAIL: vrfProve stub did not throw" >> pure False

-- | vrfVerify should throw "not implemented" since it is a stub.
testVrfVerifyStub :: IO Bool
testVrfVerifyStub = do
    r <- E.try (E.evaluate (vrfVerify BS.empty BS.empty BS.empty))
    case (r :: Either E.SomeException (Maybe BS.ByteString)) of
        Left _  -> putStrLn "  PASS: vrfVerify stub throws" >> pure True
        Right _ -> putStrLn "  FAIL: vrfVerify stub did not throw" >> pure False

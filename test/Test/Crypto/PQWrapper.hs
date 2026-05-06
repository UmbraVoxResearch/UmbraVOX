-- | PQWrapper test suite: verify module loads and stub functions throw as expected.
-- Post-quantum outer encryption layer is not yet implemented.
module Test.Crypto.PQWrapper (runTests) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS

import UmbraVox.Crypto.PQWrapper (pqEncrypt, pqDecrypt)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.PQWrapper"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testPqEncryptStub
        , testPqDecryptStub
        ]
    pure (and results)

-- | pqEncrypt should throw "not implemented" since it is a stub.
testPqEncryptStub :: IO Bool
testPqEncryptStub = do
    r <- E.try (E.evaluate (BS.length (pqEncrypt BS.empty BS.empty)))
    case (r :: Either E.SomeException Int) of
        Left _  -> putStrLn "  PASS: pqEncrypt stub throws" >> pure True
        Right _ -> putStrLn "  FAIL: pqEncrypt stub did not throw" >> pure False

-- | pqDecrypt should throw "not implemented" since it is a stub.
testPqDecryptStub :: IO Bool
testPqDecryptStub = do
    r <- E.try (E.evaluate (pqDecrypt BS.empty BS.empty))
    case (r :: Either E.SomeException (Maybe BS.ByteString)) of
        Left _  -> putStrLn "  PASS: pqDecrypt stub throws" >> pure True
        Right _ -> putStrLn "  FAIL: pqDecrypt stub did not throw" >> pure False

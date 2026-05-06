-- | KeyStore test suite: verify module loads and stub functions throw as expected.
-- Encrypted-at-rest key management is not yet implemented.
module Test.Crypto.KeyStore (runTests) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS

import UmbraVox.Crypto.KeyStore (KeyStore, openKeyStore)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.KeyStore"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testOpenKeyStoreStub
        ]
    pure (and results)

-- | openKeyStore should throw "not implemented" since it is a stub.
testOpenKeyStoreStub :: IO Bool
testOpenKeyStoreStub = do
    r <- E.try (openKeyStore "/tmp/nonexistent-keystore" BS.empty)
    case (r :: Either E.SomeException KeyStore) of
        Left _  -> putStrLn "  PASS: openKeyStore stub throws" >> pure True
        Right _ -> putStrLn "  FAIL: openKeyStore stub did not throw" >> pure False

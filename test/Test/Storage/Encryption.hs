-- SPDX-License-Identifier: Apache-2.0
-- | Tests for at-rest persistence encryption via UmbraVox.Storage.Encryption.
--
-- Anthony integration tests are deferred until upstream patches land.
module Test.Storage.Encryption (runTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import UmbraVox.Storage.Encryption
    (StorageKey, testStorageKey, encryptField, decryptField, isEncryptedField, deriveStorageKey)
import Test.Util (assertEq)

runTests :: IO Bool
runTests = do
    putStrLn "  Storage Encryption Tests"
    putStrLn "  ========================"
    results <- sequence
        [ testRoundTrip
        , testMigrationSafe
        , testIsEncrypted
        , testDifferentKeys
        , testEmptyString
        , testLargeContent
        , testDeriveKeyDeterministic
        , testEncryptedPrefix
        ]
    pure (and results)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

key1 :: StorageKey
key1 = testStorageKey

key2 :: StorageKey
key2 = deriveStorageKey (BS.replicate 32 0x01) (BS.pack [32..63])

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

testRoundTrip :: IO Bool
testRoundTrip = do
    let msg = "Hello, encrypted world!"
    ct <- encryptField key1 msg
    let result = decryptField key1 ct
    assertEq "roundTrip: encrypt then decrypt" (Just msg) result

testMigrationSafe :: IO Bool
testMigrationSafe = do
    -- M10.3.7: passthrough removed — non-UVENC1-prefixed values must yield
    -- Nothing so that unencrypted or injected plaintext is never silently
    -- accepted.
    let plain = "legacy plaintext value"
        result = decryptField key1 plain
    assertEq "migrationSafe: non-encrypted value yields Nothing" Nothing result

testIsEncrypted :: IO Bool
testIsEncrypted = do
    a <- assertEq "isEncrypted: UVENC1 prefix" True (isEncryptedField "UVENC1:abcd")
    b <- assertEq "isEncrypted: plain string" False (isEncryptedField "hello")
    pure (a && b)

testDifferentKeys :: IO Bool
testDifferentKeys = do
    ct <- encryptField key1 "secret data"
    let result = decryptField key2 ct
    assertEq "differentKeys: wrong key yields Nothing" Nothing result

testEmptyString :: IO Bool
testEmptyString = do
    ct <- encryptField key1 ""
    let result = decryptField key1 ct
    assertEq "emptyString: encrypt/decrypt empty" (Just "") result

testLargeContent :: IO Bool
testLargeContent = do
    let big = replicate 10000 'X'
    ct <- encryptField key1 big
    let result = decryptField key1 ct
    assertEq "largeContent: 10000-char round-trip" (Just big) result

testDeriveKeyDeterministic :: IO Bool
testDeriveKeyDeterministic = do
    let salt = BS.replicate 32 0xAB
        k1 = deriveStorageKey salt (BS.pack [0..31])
        k2 = deriveStorageKey salt (BS.pack [0..31])
    assertEq "deriveKey: deterministic" k1 k2

testEncryptedPrefix :: IO Bool
testEncryptedPrefix = do
    ct <- encryptField key1 "test"
    assertEq "encryptedPrefix: starts with UVENC1:" True (isEncryptedField ct)

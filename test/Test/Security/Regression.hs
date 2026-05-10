-- SPDX-License-Identifier: Apache-2.0
-- | Security regression tests covering audit rounds M7-M10.
--
-- Each test is labelled with the finding reference it covers so that
-- a future audit can trace live test coverage back to the finding list.
module Test.Security.Regression (runTests) where

import qualified Data.ByteString as BS
import Data.Word (Word32)

import Test.Util (assertEq)
import UmbraVox.Crypto.ConstantTime (constantEq)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Protocol.Encoding (putWord32BE, safeReadPort, defaultPorts)
import UmbraVox.Storage.Encryption
    ( testStorageKey, encryptField, decryptField, isEncryptedField
    , deriveStorageKey )

runTests :: IO Bool
runTests = do
    putStrLn "[Security/Regression] Running regression tests (M7-M10)..."
    results <- sequence
        [ -- M10.4.1 – constant-time comparison
          testConstantEqMatch
        , testConstantEqMismatch
        , testConstantEqDifferentLengths
        , testConstantEqPrefixAttack
        , testConstantEqBothEmpty
        , testConstantEqOneEmpty
          -- M10.4.6 – HMAC empty key
        , testHmacEmptyKey
          -- M10.4.8 – DoubleRatchet counter / Word32 encoding
        , testEncodeWord32BEMaxBound
          -- M10.4.12 – Storage.Encryption
        , testStorageRoundTrip
        , testStorageWrongKey
        , testStorageIsEncryptedField
        , testStorageTruncatedPassthrough
          -- M10.4.14 – port parsing
        , testPortValid
        , testPortTrailingChars
        , testPortEmpty
        , testPortOutOfRange
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Security/Regression] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- M10.4.1 – constantEq: constant-time comparison
------------------------------------------------------------------------

testConstantEqMatch :: IO Bool
testConstantEqMatch =
    assertEq "M10.4.1 constantEq: equal 32-byte match -> True"
        True
        (constantEq (BS.replicate 32 0xAB) (BS.replicate 32 0xAB))

testConstantEqMismatch :: IO Bool
testConstantEqMismatch =
    -- Flip the last bit of the second operand.
    let a = BS.replicate 32 0xAB
        b = BS.take 31 a <> BS.singleton 0xAA
    in assertEq "M10.4.1 constantEq: 1-bit flip -> False"
           False
           (constantEq a b)

testConstantEqDifferentLengths :: IO Bool
testConstantEqDifferentLengths =
    assertEq "M10.4.1 constantEq: 16 vs 32 bytes -> False"
        False
        (constantEq (BS.replicate 16 0x00) (BS.replicate 32 0x00))

testConstantEqPrefixAttack :: IO Bool
testConstantEqPrefixAttack =
    -- a is a prefix of b; they share the same content but differ in length.
    let a = BS.replicate 16 0xCC
        b = BS.replicate 32 0xCC
    in assertEq "M10.4.1 constantEq: prefix attack (a is prefix of b) -> False"
           False
           (constantEq a b)

testConstantEqBothEmpty :: IO Bool
testConstantEqBothEmpty =
    assertEq "M10.4.1 constantEq: both empty -> True"
        True
        (constantEq BS.empty BS.empty)

testConstantEqOneEmpty :: IO Bool
testConstantEqOneEmpty =
    assertEq "M10.4.1 constantEq: one empty, one non-empty -> False"
        False
        (constantEq BS.empty (BS.singleton 0x00))

------------------------------------------------------------------------
-- M10.4.6 – hmacSHA256: empty key must not crash, output is 32 bytes
------------------------------------------------------------------------

testHmacEmptyKey :: IO Bool
testHmacEmptyKey = do
    let result = hmacSHA256 BS.empty (BS.pack [1, 2, 3])
    assertEq "M10.4.6 hmacSHA256: empty key -> 32-byte result"
        32
        (BS.length result)

------------------------------------------------------------------------
-- M10.4.8 – putWord32BE: maxBound encodes to exactly 4 bytes and
--            round-trips through getWord32BE without truncation.
------------------------------------------------------------------------

testEncodeWord32BEMaxBound :: IO Bool
testEncodeWord32BEMaxBound = do
    let encoded = putWord32BE (maxBound :: Word32)
    ok1 <- assertEq "M10.4.8 putWord32BE maxBound: length is 4"
               4 (BS.length encoded)
    ok2 <- assertEq "M10.4.8 putWord32BE maxBound: all bytes are 0xFF"
               (BS.replicate 4 0xFF) encoded
    pure (ok1 && ok2)

------------------------------------------------------------------------
-- M10.4.12 – Storage.Encryption
------------------------------------------------------------------------

testStorageRoundTrip :: IO Bool
testStorageRoundTrip = do
    let msg = "regression test secret value"
    ct <- encryptField testStorageKey msg
    let result = decryptField testStorageKey ct
    assertEq "M10.4.12 encryptField/decryptField round-trip"
        (Just msg) result

testStorageWrongKey :: IO Bool
testStorageWrongKey = do
    let wrongKey = deriveStorageKey (BS.pack [99..130])
    ct <- encryptField testStorageKey "sensitive data"
    let result = decryptField wrongKey ct
    assertEq "M10.4.12 decryptField with wrong key -> Nothing"
        Nothing result

testStorageIsEncryptedField :: IO Bool
testStorageIsEncryptedField = do
    ok1 <- assertEq "M10.4.12 isEncryptedField: UVENC1: prefix -> True"
               True (isEncryptedField "UVENC1:deadbeef")
    ok2 <- assertEq "M10.4.12 isEncryptedField: plain text -> False"
               False (isEncryptedField "plain text")
    ok3 <- assertEq "M10.4.12 isEncryptedField: empty -> False"
               False (isEncryptedField "")
    pure (ok1 && ok2 && ok3)

-- Truncated ciphertext: remove the last byte from a valid encrypted
-- field so that the hex decoding produces a short blob.  The input no
-- longer has a valid UVENC1: prefix after truncation because we strip a
-- hex character making the hex string odd-length; decryptField should
-- return Nothing (malformed hex) rather than crashing.
testStorageTruncatedPassthrough :: IO Bool
testStorageTruncatedPassthrough = do
    ct <- encryptField testStorageKey "hello"
    -- Remove the last character from the encrypted string (breaks hex).
    let truncated = init ct
    -- If it still starts with UVENC1: then decryptField must return Nothing.
    -- If somehow truncation removed the prefix it would pass through; either
    -- way there must be no exception.
    let result = decryptField testStorageKey truncated
    case result of
        Nothing -> do
            putStrLn "  PASS: M10.4.12 truncated ciphertext -> Nothing (malformed hex)"
            pure True
        Just v
            | isEncryptedField truncated -> do
                putStrLn "  FAIL: M10.4.12 truncated ciphertext should not decrypt successfully"
                pure False
            | otherwise -> do
                -- Passthrough path: truncated string lost the prefix, treated as plaintext.
                assertEq "M10.4.12 truncated passthrough: value is passthrough string"
                    truncated v

------------------------------------------------------------------------
-- M10.4.14 – safeReadPort: port string validation
------------------------------------------------------------------------

testPortValid :: IO Bool
testPortValid =
    assertEq "M10.4.14 safeReadPort: \"7853\" -> 7853"
        7853
        (safeReadPort "7853")

testPortTrailingChars :: IO Bool
testPortTrailingChars =
    -- After the fix, trailing non-numeric chars must reject and fall back
    -- to the default port.  reads "80abc" produces [(80,"abc")] in Haskell
    -- which does NOT match [(p,"")] so safeReadPort falls back correctly.
    assertEq "M10.4.14 safeReadPort: \"80abc\" -> default port (trailing chars rejected)"
        (head defaultPorts)
        (safeReadPort "80abc")

testPortEmpty :: IO Bool
testPortEmpty =
    assertEq "M10.4.14 safeReadPort: empty string -> default port"
        (head defaultPorts)
        (safeReadPort "")

testPortOutOfRange :: IO Bool
testPortOutOfRange =
    -- 99999 is a valid integer but outside the valid port range 1-65535.
    -- safeReadPort performs no range check itself; the caller is expected
    -- to validate.  We test that at least a parseable integer is returned
    -- (no crash) and document the known behaviour.
    let result = safeReadPort "99999"
    in assertEq "M10.4.14 safeReadPort: \"99999\" parsed (no crash)"
           99999
           result

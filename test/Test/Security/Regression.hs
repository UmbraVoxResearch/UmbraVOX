-- SPDX-License-Identifier: Apache-2.0
-- | Security regression tests for M10 crypto and storage findings.
--
-- This module provides a regression harness that ensures every discrete
-- vulnerability identified during audit round M10 remains fixed.  Each
-- test is labelled with the exact finding reference (e.g. M10.4.1) so
-- that future auditors can trace live test coverage back to the finding
-- list without needing to read the full audit report.
--
-- __Scope__
--
-- * M10.4.1  – Constant-time comparison (timing-oracle prevention)
-- * M10.4.6  – HMAC with an empty key (crash / weak-MAC prevention)
-- * M10.4.8  – DoubleRatchet counter encoding (Word32 big-endian)
-- * M10.4.12 – Storage field encryption round-trip and error paths
-- * M10.4.14 – Port-string parsing (injection / crash prevention)
--
-- __How to read these tests__
--
-- Every test name begins with the finding reference.  The comment block
-- immediately above each test explains:
--
-- 1. Which finding it covers.
-- 2. What the original vulnerability was.
-- 3. How the production fix works.
-- 4. What property this test verifies.
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
--
-- Finding:    The original MAC verification used (==) on ByteStrings,
--             which Haskell evaluates left-to-right and short-circuits on
--             the first differing byte.  An attacker who can submit
--             crafted MACs and measure response latency can recover the
--             expected MAC one byte at a time (timing oracle).
--
-- Fix:        UmbraVox.Crypto.ConstantTime.constantEq XORs all bytes
--             together and only branches on the accumulated result, so
--             execution time is independent of where the first difference
--             occurs.  Length mismatch is detected via a separate
--             length-equality check that is also evaluated without early
--             exit.
--
-- Verified:   (a) equal inputs return True, (b) a single-bit flip
--             returns False, (c) different lengths always return False
--             even when the shorter value is a prefix of the longer,
--             (d) both-empty and one-empty edge cases are handled.
------------------------------------------------------------------------

-- | M10.4.1: Two identical 32-byte values must compare as equal.
testConstantEqMatch :: IO Bool
testConstantEqMatch =
    assertEq "M10.4.1 constantEq: equal 32-byte match -> True"
        True
        (constantEq (BS.replicate 32 0xAB) (BS.replicate 32 0xAB))

-- | M10.4.1: A single-bit difference in the last byte must return False.
-- This exercises the full XOR reduction path rather than the length
-- short-circuit.
testConstantEqMismatch :: IO Bool
testConstantEqMismatch =
    -- Flip the last bit of the second operand.
    let a = BS.replicate 32 0xAB
        b = BS.take 31 a <> BS.singleton 0xAA
    in assertEq "M10.4.1 constantEq: 1-bit flip -> False"
           False
           (constantEq a b)

-- | M10.4.1: Inputs of different lengths must always return False,
-- even when all byte values in the shorter operand match the prefix
-- of the longer operand.
testConstantEqDifferentLengths :: IO Bool
testConstantEqDifferentLengths =
    assertEq "M10.4.1 constantEq: 16 vs 32 bytes -> False"
        False
        (constantEq (BS.replicate 16 0x00) (BS.replicate 32 0x00))

-- | M10.4.1: Prefix-attack variant — 'a' is a proper prefix of 'b'.
-- If length were not checked, the XOR of the overlapping region would
-- be zero (all bytes match), and the comparison could mistakenly return
-- True.  constantEq must reject this.
testConstantEqPrefixAttack :: IO Bool
testConstantEqPrefixAttack =
    -- a is a prefix of b; they share the same content but differ in length.
    let a = BS.replicate 16 0xCC
        b = BS.replicate 32 0xCC
    in assertEq "M10.4.1 constantEq: prefix attack (a is prefix of b) -> False"
           False
           (constantEq a b)

-- | M10.4.1: Both-empty edge case — two empty ByteStrings are equal.
testConstantEqBothEmpty :: IO Bool
testConstantEqBothEmpty =
    assertEq "M10.4.1 constantEq: both empty -> True"
        True
        (constantEq BS.empty BS.empty)

-- | M10.4.1: One-empty edge case — empty vs. non-empty must return False.
testConstantEqOneEmpty :: IO Bool
testConstantEqOneEmpty =
    assertEq "M10.4.1 constantEq: one empty, one non-empty -> False"
        False
        (constantEq BS.empty (BS.singleton 0x00))

------------------------------------------------------------------------
-- M10.4.6 – hmacSHA256: empty key must not crash, output is 32 bytes
--
-- Finding:    The HMAC implementation used an unsafe call that panicked
--             when given a zero-length key, because it attempted to index
--             the key ByteString unconditionally.  An attacker who can
--             trigger HMAC computation with a crafted (empty) session key
--             could cause the process to crash (denial of service).
--
-- Fix:        hmacSHA256 now accepts a key of any length, including zero.
--             The underlying HMAC-SHA256 standard (RFC 2104) already
--             defines the empty-key case: the key is padded with zeros to
--             the block size before being used.
--
-- Verified:   hmacSHA256 with an empty key does not crash and produces
--             a 32-byte digest (the standard SHA-256 output size).
------------------------------------------------------------------------

-- | M10.4.6: An empty HMAC key must not crash; the digest must be 32 bytes.
testHmacEmptyKey :: IO Bool
testHmacEmptyKey = do
    let result = hmacSHA256 BS.empty (BS.pack [1, 2, 3])
    assertEq "M10.4.6 hmacSHA256: empty key -> 32-byte result"
        32
        (BS.length result)

------------------------------------------------------------------------
-- M10.4.8 – putWord32BE: DoubleRatchet counter encoding
--
-- Finding:    The DoubleRatchet message-number counter (rhMsgN) was
--             encoded using a narrow integer type on some paths, causing
--             silent truncation when the counter exceeded 0xFFFF.  A
--             long-running session could wrap the counter back to a
--             previously-used value, enabling a nonce-reuse attack.
--
-- Fix:        putWord32BE encodes the full 32-bit counter in network
--             (big-endian) byte order, and getWord32BE decodes it back
--             without loss.  The counter is declared as Word32 throughout
--             the DoubleRatchet state machine so narrowing cannot occur.
--
-- Verified:   Encoding maxBound (0xFFFFFFFF) produces exactly 4 bytes
--             of value 0xFF each, confirming that the full value range
--             is preserved without truncation or overflow.
------------------------------------------------------------------------

-- | M10.4.8: maxBound :: Word32 must encode to 4 bytes of 0xFF (no truncation).
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
--
-- Finding:    Settings stored in the Anthony SQLite database were written
--             in plaintext.  An attacker with read access to the database
--             file (e.g. via a local privilege escalation or a backup
--             leak) could recover private keys, peer identities, and
--             session state without needing the application's credentials.
--
-- Fix:        Sensitive fields are now encrypted with AES-256-GCM before
--             being written to the database, using a key derived from a
--             machine-local secret via HKDF.  Encrypted fields are
--             prefixed with "UVENC1:" to distinguish them from legacy
--             plaintext rows.  Decryption authenticates the ciphertext
--             with the GCM tag; any tampered or truncated value returns
--             Nothing rather than producing garbage plaintext.
--
-- Verified:   (a) encrypt then decrypt with the correct key returns the
--             original string, (b) decryption with a different key
--             returns Nothing, (c) isEncryptedField recognises the
--             "UVENC1:" prefix correctly, (d) a truncated ciphertext
--             (odd-length hex) returns Nothing instead of crashing.
------------------------------------------------------------------------

-- | M10.4.12: Correct-key round-trip must recover the original plaintext.
testStorageRoundTrip :: IO Bool
testStorageRoundTrip = do
    key <- testStorageKey
    let msg = "regression test secret value"
    ct <- encryptField key msg
    result <- decryptField key ct
    assertEq "M10.4.12 encryptField/decryptField round-trip"
        (Just msg) result

-- | M10.4.12: Decryption with a different key must return Nothing (GCM
-- authentication failure), not a garbage plaintext.
testStorageWrongKey :: IO Bool
testStorageWrongKey = do
    key    <- testStorageKey
    wrongKey <- deriveStorageKey (BS.replicate 32 0xFF) (BS.pack [99..130])
    ct <- encryptField key "sensitive data"
    result <- decryptField wrongKey ct
    assertEq "M10.4.12 decryptField with wrong key -> Nothing"
        Nothing result

-- | M10.4.12: isEncryptedField must correctly distinguish encrypted
-- fields (prefixed "UVENC1:") from plaintext rows and empty strings.
testStorageIsEncryptedField :: IO Bool
testStorageIsEncryptedField = do
    ok1 <- assertEq "M10.4.12 isEncryptedField: UVENC1: prefix -> True"
               True (isEncryptedField "UVENC1:deadbeef")
    ok2 <- assertEq "M10.4.12 isEncryptedField: plain text -> False"
               False (isEncryptedField "plain text")
    ok3 <- assertEq "M10.4.12 isEncryptedField: empty -> False"
               False (isEncryptedField "")
    pure (ok1 && ok2 && ok3)

-- | M10.4.12: A truncated ciphertext (last hex character removed, making
-- the hex string odd-length) must return Nothing rather than crashing.
-- This guards against partial-write corruption in the database.
--
-- The input still starts with "UVENC1:" after removing the final character,
-- so decryptField must reject it cleanly through its hex-decode error path.
testStorageTruncatedPassthrough :: IO Bool
testStorageTruncatedPassthrough = do
    key <- testStorageKey
    ct <- encryptField key "hello"
    -- Remove the last character from the encrypted string (breaks hex).
    let truncated = init ct
    -- If it still starts with UVENC1: then decryptField must return Nothing.
    -- If somehow truncation removed the prefix it would pass through; either
    -- way there must be no exception.
    result <- decryptField key truncated
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
--
-- Finding:    The original port-string parser used 'read' directly, which
--             throws an exception on non-numeric input (crashing the
--             listener thread) and accepted strings like "80abc" by
--             ignoring trailing characters, allowing port-injection via
--             crafted configuration values or mDNS announcements.
--
-- Fix:        safeReadPort uses 'reads' and only accepts a parse result
--             where the unconsumed remainder is the empty string, i.e.
--             the full input was a valid integer.  Invalid input falls
--             back to the first entry in 'defaultPorts' rather than
--             crashing.  Range validation (1–65535) is left to the
--             caller and is documented as a known limitation.
--
-- Verified:   (a) a well-formed port number parses correctly, (b) a
--             string with trailing non-numeric characters falls back to
--             the default port, (c) an empty string falls back to the
--             default port, (d) an out-of-range number does not crash
--             (the test documents the known no-range-check behaviour).
------------------------------------------------------------------------

-- | M10.4.14: A well-formed port number string must parse to the expected
-- integer value.
testPortValid :: IO Bool
testPortValid =
    assertEq "M10.4.14 safeReadPort: \"7853\" -> 7853"
        7853
        (safeReadPort "7853")

-- | M10.4.14: A string with trailing non-numeric characters must fall back
-- to the default port.  Before the fix, "reads" was not used and trailing
-- garbage was silently ignored, enabling port injection.
testPortTrailingChars :: IO Bool
testPortTrailingChars =
    -- After the fix, trailing non-numeric chars must reject and fall back
    -- to the default port.  reads "80abc" produces [(80,"abc")] in Haskell
    -- which does NOT match [(p,"")] so safeReadPort falls back correctly.
    assertEq "M10.4.14 safeReadPort: \"80abc\" -> default port (trailing chars rejected)"
        (head defaultPorts)
        (safeReadPort "80abc")

-- | M10.4.14: An empty string must fall back to the default port without
-- crashing (the pre-fix 'read "" :: Int' throws an exception).
testPortEmpty :: IO Bool
testPortEmpty =
    assertEq "M10.4.14 safeReadPort: empty string -> default port"
        (head defaultPorts)
        (safeReadPort "")

-- | M10.4.14: A number outside the valid TCP port range (1–65535) is
-- accepted as an integer but not range-checked by safeReadPort itself.
-- This test documents that behaviour and verifies no crash occurs.
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

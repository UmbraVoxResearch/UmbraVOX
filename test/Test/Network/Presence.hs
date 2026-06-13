-- SPDX-License-Identifier: Apache-2.0
-- | Presence record freshness regression suite (M40.41a).
--
-- Tests that 'verifyPresenceRecord' rejects expired and implausibly
-- future-dated records in addition to verifying the Ed25519 signature.
module Test.Network.Presence (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util (assertEq)
import qualified UmbraVox.Crypto.Generated.FFI.Ed25519Extended as Ed25519FFI
import UmbraVox.Network.Presence
    ( PresenceRecord(..)
    , createPresenceRecord
    , verifyPresenceRecord
    )

runTests :: IO Bool
runTests = do
    putStrLn "[Presence] Running presence freshness tests..."
    results <- sequence
        [ testFreshRecordVerifies
        , testExpiredRecordRejected
        , testFutureDatedRejected
        , testFutureSkewBoundaryAccepted
        , testWrongKeyRejected
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Presence] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Build a signed presence record plus the signer's Ed25519 public key.
--
-- 'createPresenceRecord' stamps the record with the current POSIX time and
-- the default 24h TTL; the test probes 'verifyPresenceRecord' with @now@
-- values computed relative to the record's own 'prTimestamp', so the test
-- is independent of the wall clock.
mkRecord :: IO (ByteString, PresenceRecord)
mkRecord = do
    let edSecret    = BS.replicate 32 7
        scanKey     = BS.replicate 32 9
        identityPub = BS.replicate 32 11
        contactInfo = BS.pack [1, 2, 3, 4, 5]
    pubKey <- Ed25519FFI.ed25519PublicKey edSecret
    eRec   <- createPresenceRecord edSecret scanKey identityPub contactInfo
    case eRec of
        Left err -> error ("mkRecord: createPresenceRecord failed: " ++ err)
        Right pr -> pure (pubKey, pr)

-- | Test 1: a record verified at its own timestamp passes.
--
-- Finding:     M40.41a — 'verifyPresenceRecord' validated only the Ed25519
--              signature, ignoring 'prTimestamp'/'prTTL'.
-- Fix:         A freshness window (not expired, not implausibly future-dated)
--              is now enforced alongside the signature check.
-- Verified:    A record checked at @now == prTimestamp@ is well within its
--              TTL and verifies.
testFreshRecordVerifies :: IO Bool
testFreshRecordVerifies = do
    (pubKey, pr) <- mkRecord
    ok <- verifyPresenceRecord pubKey (prTimestamp pr) pr
    assertEq "fresh record verifies" True ok

-- | Test 2: a record older than its TTL is rejected.
--
-- Vulnerability: A captured but validly-signed record stays signature-valid
--              forever, letting an attacker resurrect a stale contact address
--              long after key rotation.
-- Verified:    Checking at @now = prTimestamp + prTTL + 1@ (one second past
--              expiry) returns False.
testExpiredRecordRejected :: IO Bool
testExpiredRecordRejected = do
    (pubKey, pr) <- mkRecord
    let expiredNow = prTimestamp pr + fromIntegral (prTTL pr) + 1
    ok <- verifyPresenceRecord pubKey expiredNow pr
    assertEq "expired record rejected" False ok

-- | Test 3: a record dated beyond the clock-skew allowance is rejected.
--
-- Vulnerability: A record stamped far in the future would win "newest wins"
--              relay-selection races indefinitely.
-- Verified:    With the verifier's clock at @prTimestamp - 301@ (the record
--              is 301s in the future, exceeding the 300s skew bound), the
--              record returns False.
testFutureDatedRejected :: IO Bool
testFutureDatedRejected = do
    (pubKey, pr) <- mkRecord
    -- now is 301s behind the record's timestamp -> record is 301s in the future.
    let nowBehind = prTimestamp pr - 301
    ok <- verifyPresenceRecord pubKey nowBehind pr
    assertEq "future-dated record (beyond skew) rejected" False ok

-- | Test 4: a record exactly at the future-skew boundary is accepted.
--
-- Verified:    With the verifier's clock at @prTimestamp - 300@ (the record
--              is exactly 300s in the future, the maximum tolerated skew),
--              the record verifies.
testFutureSkewBoundaryAccepted :: IO Bool
testFutureSkewBoundaryAccepted = do
    (pubKey, pr) <- mkRecord
    let nowAtBoundary = prTimestamp pr - 300
    ok <- verifyPresenceRecord pubKey nowAtBoundary pr
    assertEq "record at future-skew boundary accepted" True ok

-- | Test 5: a fresh record with the wrong public key is rejected.
--
-- Verified:    Freshness alone is not sufficient; the signature must still
--              verify against the supplied key. A mismatched key returns
--              False even for an in-window record.
testWrongKeyRejected :: IO Bool
testWrongKeyRejected = do
    (_pubKey, pr) <- mkRecord
    wrongKey <- Ed25519FFI.ed25519PublicKey (BS.replicate 32 99)
    ok <- verifyPresenceRecord wrongKey (prTimestamp pr) pr
    assertEq "fresh record with wrong key rejected" False ok

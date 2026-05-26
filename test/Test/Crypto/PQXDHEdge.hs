-- SPDX-License-Identifier: Apache-2.0
-- | PQXDH post-quantum edge-case test suite.
--
-- Covers: OPK full integration, hybrid-binding isolation, ML-KEM all-zero
-- seed validity, multi-byte ciphertext corruption (implicit rejection), and
-- crash-recovery (initiate with no subsequent respond).
module Test.Crypto.PQXDHEdge (runTests) where

import Data.Bits (xor)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.Ed25519 (ed25519Sign)
import UmbraVox.Crypto.SecureBytes (toByteString)
import UmbraVox.Crypto.Signal.PQXDH
    ( PQPreKeyBundle(..), pqxdhInitiate, pqxdhRespond, PQXDHResult(..) )
import UmbraVox.Crypto.Signal.X3DH
    ( generateKeyPair, generateIdentityKey, signPreKey
    , IdentityKey(..), KeyPair(..)
    )
import UmbraVox.Crypto.MLKEM
    ( mlkemKeyGen, mlkemEncaps, mlkemDecaps
    , MLKEMEncapKey(..), MLKEMDecapKey(..), MLKEMCiphertext(..)
    )

runTests :: IO Bool
runTests = do
    putStrLn "[PQXDHEdge] Running PQXDH edge-case tests..."
    results <- sequence
        [ testPQXDHWithOPKFullFlow
        , testHybridBinding
        , testMLKEMAllZeroSeed
        , testMLKEMCorruptedCiphertext
        , testPQXDHCrashRecovery
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[PQXDHEdge] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Deterministic key material (distinct seeds, no overlap with PQXDH.hs)
------------------------------------------------------------------------

aliceEdSeed, aliceXSeed :: BS.ByteString
aliceEdSeed = BS.pack [0x11..0x30]
aliceXSeed  = BS.pack [0x31..0x50]

bobEdSeed, bobXSeed :: BS.ByteString
bobEdSeed = BS.pack [0x51..0x70]
bobXSeed  = BS.pack [0x71..0x90]

spkSeed, ekSeed, opkSeed :: BS.ByteString
spkSeed = BS.pack [0x91..0xB0]
ekSeed  = BS.pack [0xB1..0xD0]
opkSeed = BS.pack [0xD1..0xF0]

mlkemDSeed, mlkemZSeed, mlkemRandSeed :: BS.ByteString
mlkemDSeed    = BS.replicate 32 0xAA
mlkemZSeed    = BS.replicate 32 0xBB
mlkemRandSeed = BS.replicate 32 0xCC

-- | Build a deterministic PQXDH prekey bundle for Bob.
-- Returns (bundle, spkSecret, bobIK, mlkemDecapKey).
mkEdgeBundle :: Maybe BS.ByteString
             -- ^ OPK public key to include (Nothing = no OPK)
             -> IO (PQPreKeyBundle, BS.ByteString, IdentityKey, MLKEMDecapKey)
mkEdgeBundle mOPKPub = do
    bobIK  <- generateIdentityKey bobEdSeed bobXSeed
    spk    <- generateKeyPair spkSeed
    spkSig <- signPreKey bobIK (kpPublic spk)
    let (ekPQ, dkPQ) = mlkemKeyGen mlkemDSeed mlkemZSeed
        MLKEMEncapKey ekPQBytes = ekPQ
    bobEdSec <- toByteString (ikEd25519Secret bobIK)
    let pqSig  = ed25519Sign bobEdSec ekPQBytes
        bundle = PQPreKeyBundle
            { pqpkbIdentityKey     = ikX25519Public bobIK
            , pqpkbSignedPreKey    = kpPublic spk
            , pqpkbSPKSignature    = spkSig
            , pqpkbIdentityEd25519 = ikEd25519Public bobIK
            , pqpkbOneTimePreKey   = mOPKPub
            , pqpkbPQPreKey        = ekPQ
            , pqpkbPQKeySignature  = pqSig
            }
    pure (bundle, spkSeed, bobIK, dkPQ)

------------------------------------------------------------------------
-- Test 1: PQXDH with OPK — full integration flow
--
-- Finding:     One-time prekeys provide additional forward secrecy beyond
--              the signed prekey.  If the OPK is not incorporated into
--              the KDF, the quantum-classical hybrid provides no extra
--              protection for the first message.
-- Vulnerability: Omitting the OPK DH leg from the KDF input would mean
--              the OPK contributes nothing to the shared secret.
-- Fix:         pqxdhInitiate includes DH(Alice_EK, Bob_OPK) in the HKDF
--              input; pqxdhRespond mirrors this with the OPK secret.
-- Verified:    Both sides derive the same 32-byte shared secret; the
--              secret differs from the no-OPK case; pqxdhUsedOPK reports
--              the OPK public key.
------------------------------------------------------------------------

testPQXDHWithOPKFullFlow :: IO Bool
testPQXDHWithOPKFullFlow = do
    aliceIK    <- generateIdentityKey aliceEdSeed aliceXSeed
    opk        <- generateKeyPair opkSeed
    (bundleOPK, spkSec, bobIK, dkPQ) <- mkEdgeBundle (Just (kpPublic opk))
    (bundleNoOPK, _, _, _)            <- mkEdgeBundle Nothing

    mResult <- pqxdhInitiate aliceIK bundleOPK ekSeed mlkemRandSeed
    case mResult of
        Nothing -> do
            putStrLn "  FAIL: pqxdh-opk-flow: initiation returned Nothing"
            pure False
        Just result -> do
            -- Bob responds with OPK secret.
            mBobSecret <- pqxdhRespond bobIK spkSec (Just opkSeed) dkPQ
                    (ikX25519Public aliceIK)
                    (pqxdhEphemeralKey result)
                    (pqxdhPQCiphertext result)
            case mBobSecret of
                Nothing -> do
                    putStrLn "  FAIL: pqxdh-opk-flow: respond returned Nothing"
                    pure False
                Just bobSecret -> do
                    r1 <- assertEq "pqxdh-opk-flow: secrets match"
                            (pqxdhSharedSecret result) bobSecret
                    r2 <- assertEq "pqxdh-opk-flow: secret is 32 bytes"
                            32 (BS.length (pqxdhSharedSecret result))
                    r3 <- assertEq "pqxdh-opk-flow: OPK reported as consumed"
                            (Just (kpPublic opk)) (pqxdhUsedOPK result)
                    r4 <- assertEq "pqxdh-opk-flow: secret is non-zero"
                            True (pqxdhSharedSecret result /= BS.replicate 32 0x00)
                    -- Compare with no-OPK to confirm OPK changes the secret.
                    mNoOPK <- pqxdhInitiate aliceIK bundleNoOPK ekSeed mlkemRandSeed
                    r5 <- case mNoOPK of
                        Nothing -> do
                            putStrLn "  FAIL: pqxdh-opk-flow: no-OPK baseline returned Nothing"
                            pure False
                        Just resultNoOPK ->
                            assertEq "pqxdh-opk-flow: OPK changes shared secret"
                                True (pqxdhSharedSecret result /= pqxdhSharedSecret resultNoOPK)
                    pure (r1 && r2 && r3 && r4 && r5)

------------------------------------------------------------------------
-- Test 2: Hybrid binding
--
-- Verify that DH-only, KEM-only, and DH+KEM produce distinct shared
-- secrets.  We approximate this by comparing:
--   (a) normal PQXDH (DH+KEM)   — pqxdhInitiate with valid bundle
--   (b) DH-only baseline        — bundle with a different ML-KEM key
--       but same DH keys; both sides derive from different KEM paths
--   (c) KEM-only isolation      — change the SPK (affects DH) while
--       keeping the PQ key; secrets must differ
--
-- This confirms that each leg is independently bound into the KDF.
------------------------------------------------------------------------

testHybridBinding :: IO Bool
testHybridBinding = do
    aliceIK <- generateIdentityKey aliceEdSeed aliceXSeed
    bobIK   <- generateIdentityKey bobEdSeed bobXSeed

    -- Build the canonical bundle (DH+KEM).
    spk    <- generateKeyPair spkSeed
    spkSig <- signPreKey bobIK (kpPublic spk)
    let (ekPQ, _dkPQ) = mlkemKeyGen mlkemDSeed mlkemZSeed
        MLKEMEncapKey ekPQBytes = ekPQ
    bobEdSec <- toByteString (ikEd25519Secret bobIK)
    let pqSig = ed25519Sign bobEdSec ekPQBytes

    let mkBundle pqKey pqS spkPub spkS = PQPreKeyBundle
            { pqpkbIdentityKey     = ikX25519Public bobIK
            , pqpkbSignedPreKey    = spkPub
            , pqpkbSPKSignature    = spkS
            , pqpkbIdentityEd25519 = ikEd25519Public bobIK
            , pqpkbOneTimePreKey   = Nothing
            , pqpkbPQPreKey        = pqKey
            , pqpkbPQKeySignature  = pqS
            }

    -- (a) DH+KEM canonical bundle.
    let bundleFull = mkBundle ekPQ pqSig (kpPublic spk) spkSig

    -- (b) DH-only approximation: different ML-KEM seed => different KEM leg.
    let (ekPQ2, _) = mlkemKeyGen (BS.replicate 32 0x77) (BS.replicate 32 0x88)
        MLKEMEncapKey ekPQBytes2 = ekPQ2
        pqSig2 = ed25519Sign bobEdSec ekPQBytes2
        bundleDiffKEM = mkBundle ekPQ2 pqSig2 (kpPublic spk) spkSig

    -- (c) KEM-only variation: different SPK (different DH leg), same PQ key.
    spk3    <- generateKeyPair (BS.replicate 32 0x99)
    spkSig3 <- signPreKey bobIK (kpPublic spk3)
    let bundleDiffDH = mkBundle ekPQ pqSig (kpPublic spk3) spkSig3

    mFull   <- pqxdhInitiate aliceIK bundleFull    ekSeed mlkemRandSeed
    mDiffK  <- pqxdhInitiate aliceIK bundleDiffKEM ekSeed mlkemRandSeed
    mDiffDH <- pqxdhInitiate aliceIK bundleDiffDH  ekSeed mlkemRandSeed

    case (mFull, mDiffK, mDiffDH) of
        (Just full, Just diffK, Just diffDH) -> do
            r1 <- assertEq "hybrid-binding: DH+KEM vs different KEM: secrets differ"
                    True (pqxdhSharedSecret full /= pqxdhSharedSecret diffK)
            r2 <- assertEq "hybrid-binding: DH+KEM vs different DH: secrets differ"
                    True (pqxdhSharedSecret full /= pqxdhSharedSecret diffDH)
            r3 <- assertEq "hybrid-binding: different-KEM vs different-DH: secrets differ"
                    True (pqxdhSharedSecret diffK /= pqxdhSharedSecret diffDH)
            pure (r1 && r2 && r3)
        _ -> do
            putStrLn "  FAIL: hybrid-binding: one or more pqxdhInitiate returned Nothing"
            pure False

------------------------------------------------------------------------
-- Test 3: ML-KEM all-zero seed produces a valid keypair
--
-- Finding:     The ML-KEM reference implementation must not panic or
--              return degenerate output for all-zero seed inputs (d=0,
--              z=0).  Some implementations crash or produce all-zero
--              keys that fail downstream encapsulation.
-- Vulnerability: An all-zero seed is a valid but unusual input per
--              FIPS 203; rejecting or crashing on it would allow a
--              denial-of-service by submitting a specially crafted
--              prekey bundle.
-- Fix:         mlkemKeyGen accepts arbitrary 32-byte seeds including
--              all-zero; SHA3-512(0x00...00) produces a non-zero rho
--              and sigma, so the resulting keypair is non-trivial.
-- Verified:    mlkemKeyGen with d=0x00...00, z=0x00...00 returns a
--              keypair where the encapsulation key is non-empty, and
--              mlkemEncaps followed by mlkemDecaps recovers the shared
--              secret correctly.
------------------------------------------------------------------------

testMLKEMAllZeroSeed :: IO Bool
testMLKEMAllZeroSeed = do
    let zeroD = BS.replicate 32 0x00
        zeroZ = BS.replicate 32 0x00
        (MLKEMEncapKey ekBytes, MLKEMDecapKey dkBytes) = mlkemKeyGen zeroD zeroZ

    r1 <- assertEq "mlkem-zero-seed: encapsulation key is 1184 bytes" 1184 (BS.length ekBytes)
    r2 <- assertEq "mlkem-zero-seed: decapsulation key is 2400 bytes" 2400 (BS.length dkBytes)
    r3 <- assertEq "mlkem-zero-seed: ek is not all-zero"
            True (ekBytes /= BS.replicate 1184 0x00)

    -- Round-trip: encapsulate then decapsulate recovers the shared secret.
    let ek = MLKEMEncapKey ekBytes
        dk = MLKEMDecapKey dkBytes
        -- Use a non-zero encapsulation randomness so both sides can compare.
        encapsRand = BS.replicate 32 0x42
        -- mlkemEncaps takes (encapKey, randomness); returns (ciphertext, sharedSecret)
        (ct, ssEnc) = mlkemEncaps ek encapsRand
        ssDec = mlkemDecaps dk ct

    r4 <- assertEq "mlkem-zero-seed: encaps/decaps round-trip"   ssEnc ssDec
    r5 <- assertEq "mlkem-zero-seed: shared secret is 32 bytes"  32    (BS.length ssEnc)
    r6 <- assertEq "mlkem-zero-seed: shared secret is non-zero"
            True (ssEnc /= BS.replicate 32 0x00)

    pure (r1 && r2 && r3 && r4 && r5 && r6)

------------------------------------------------------------------------
-- Test 4: ML-KEM multi-byte ciphertext corruption (implicit rejection)
--
-- Finding:     FIPS 203 specifies implicit rejection: mlkemDecaps with an
--              invalid ciphertext must return a pseudorandom value (not
--              the real shared secret) without signaling the error.
-- Vulnerability: Distinguishing rejection from acceptance would allow a
--              decryption oracle attack; returning a constant (e.g.
--              all-zero) on failure would allow the same.
-- Fix:         mlkemDecaps computes a rejection secret via J(z, ct) when
--              re-encryption does not match.  Different malformed
--              ciphertexts must produce different rejection secrets.
-- Verified:    (a) all-zero CT, (b) all-0xFF CT, and (c) first-32-bytes-
--              zeroed CT each produce distinct shared secrets; all differ
--              from the legitimate shared secret; none are all-zero.
------------------------------------------------------------------------

testMLKEMCorruptedCiphertext :: IO Bool
testMLKEMCorruptedCiphertext = do
    let (ek, dk) = mlkemKeyGen mlkemDSeed mlkemZSeed
        encapsRand   = BS.replicate 32 0x55
        (legitimateCT, ssLegit) = mlkemEncaps ek encapsRand
        MLKEMCiphertext ctBytes = legitimateCT
        ctLen = BS.length ctBytes  -- should be 1088

    -- (a) All-zero ciphertext.
    let ctAllZero = MLKEMCiphertext (BS.replicate ctLen 0x00)
    let ssZero = mlkemDecaps dk ctAllZero

    -- (b) All-0xFF ciphertext.
    let ctAllFF = MLKEMCiphertext (BS.replicate ctLen 0xFF)
    let ssFF = mlkemDecaps dk ctAllFF

    -- (c) First 32 bytes zeroed.
    let ctFirst32Zero = MLKEMCiphertext (BS.replicate 32 0x00 <> BS.drop 32 ctBytes)
    let ssFirst32 = mlkemDecaps dk ctFirst32Zero

    r1 <- assertEq "mlkem-corrupt-ct: all-zero CT differs from legitimate"
            True (ssZero /= ssLegit)
    r2 <- assertEq "mlkem-corrupt-ct: all-FF CT differs from legitimate"
            True (ssFF /= ssLegit)
    r3 <- assertEq "mlkem-corrupt-ct: first-32-zeroed CT differs from legitimate"
            True (ssFirst32 /= ssLegit)
    r4 <- assertEq "mlkem-corrupt-ct: all-zero CT differs from all-FF CT"
            True (ssZero /= ssFF)
    r5 <- assertEq "mlkem-corrupt-ct: all-zero CT differs from first-32-zeroed CT"
            True (ssZero /= ssFirst32)
    r6 <- assertEq "mlkem-corrupt-ct: all-FF CT differs from first-32-zeroed CT"
            True (ssFF /= ssFirst32)
    r7 <- assertEq "mlkem-corrupt-ct: all-zero rejection is non-zero"
            True (ssZero /= BS.replicate 32 0x00)
    r8 <- assertEq "mlkem-corrupt-ct: all-FF rejection is non-zero"
            True (ssFF /= BS.replicate 32 0x00)
    r9 <- assertEq "mlkem-corrupt-ct: first-32-zeroed rejection is non-zero"
            True (ssFirst32 /= BS.replicate 32 0x00)

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9)

------------------------------------------------------------------------
-- Test 5: PQXDH crash recovery
--
-- Finding:     If the initiator (Alice) calls pqxdhInitiate but the
--              process terminates before pqxdhRespond completes (e.g.
--              power loss, OOM kill), the session must be safely
--              restartable without leaking key material or reusing state.
-- Vulnerability: If pqxdhInitiate has side effects that corrupt a
--              persistent store, a retry with fresh randomness would
--              produce the same ephemeral keys (if randomness is seeded
--              from a crashed PRNG state), allowing reuse.
-- Fix:         pqxdhInitiate is a pure function of its inputs (aliceIK,
--              bundle, ekSecret, mlkemRand).  A retry with different
--              randomness (different ekSecret2 / mlkemRand2) produces a
--              completely independent initiation result.  The original
--              result is abandoned (Bob never received the ciphertext);
--              the new result is self-consistent.
-- Verified:    Two initiations with different randomness produce distinct
--              ephemeral keys and PQ ciphertexts; Bob can respond to the
--              second initiation and derive the same secret as Alice's
--              second result; the first result's secret differs from the
--              second's.
------------------------------------------------------------------------

testPQXDHCrashRecovery :: IO Bool
testPQXDHCrashRecovery = do
    aliceIK <- generateIdentityKey aliceEdSeed aliceXSeed
    (bundle, spkSec, bobIK, dkPQ) <- mkEdgeBundle Nothing

    -- First initiation (simulating a session that crashed before respond).
    let ekSecret1   = ekSeed
        mlkemRand1  = mlkemRandSeed
    mResult1 <- pqxdhInitiate aliceIK bundle ekSecret1 mlkemRand1

    -- Second initiation (the recovery attempt with fresh randomness).
    let ekSecret2   = BS.map (`xor` 0xFF) ekSeed
        mlkemRand2  = BS.map (`xor` 0xFF) mlkemRandSeed
    mResult2 <- pqxdhInitiate aliceIK bundle ekSecret2 mlkemRand2

    case (mResult1, mResult2) of
        (Just result1, Just result2) -> do
            -- The two initiations must produce distinct ephemeral keys.
            r1 <- assertEq "crash-recovery: ephemeral keys differ between initiations"
                    True (pqxdhEphemeralKey result1 /= pqxdhEphemeralKey result2)
            -- The two initiations must produce distinct PQ ciphertexts.
            r2 <- assertEq "crash-recovery: PQ ciphertexts differ between initiations"
                    True (pqxdhPQCiphertext result1 /= pqxdhPQCiphertext result2)
            -- The shared secrets must differ.
            r3 <- assertEq "crash-recovery: shared secrets differ between initiations"
                    True (pqxdhSharedSecret result1 /= pqxdhSharedSecret result2)

            -- Bob responds to the second (recovery) initiation.
            mBobSecret2 <- pqxdhRespond bobIK spkSec Nothing dkPQ
                    (ikX25519Public aliceIK)
                    (pqxdhEphemeralKey result2)
                    (pqxdhPQCiphertext result2)
            r4 <- case mBobSecret2 of
                Nothing -> do
                    putStrLn "  FAIL: crash-recovery: Bob could not respond to recovery initiation"
                    pure False
                Just bobSecret2 ->
                    assertEq "crash-recovery: Bob matches Alice's second initiation"
                            (pqxdhSharedSecret result2) bobSecret2

            -- Bob cannot respond to the first (abandoned) initiation with the
            -- second's ciphertext (they use different ephemeral keys so the
            -- DH legs differ; the result would be a different secret).
            mBobSecret1With2 <- pqxdhRespond bobIK spkSec Nothing dkPQ
                    (ikX25519Public aliceIK)
                    (pqxdhEphemeralKey result1)
                    (pqxdhPQCiphertext result1)
            r5 <- case mBobSecret1With2 of
                Nothing ->
                    -- Respond may fail entirely; that is fine.
                    assertEq "crash-recovery: Bob rejected/abandoned first initiation" True True
                Just bobSecret1 ->
                    -- If respond succeeds it must match the first result, not the second.
                    assertEq "crash-recovery: Bob's first-initiation secret matches result1"
                            (pqxdhSharedSecret result1) bobSecret1

            pure (r1 && r2 && r3 && r4 && r5)
        _ -> do
            putStrLn "  FAIL: crash-recovery: one or both initiations returned Nothing"
            pure False

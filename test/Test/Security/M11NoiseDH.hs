-- SPDX-License-Identifier: Apache-2.0
-- | Noise IK DH-leg test vectors and negative tests — M10.1.9 / M10.1.10.
--
-- These tests operate directly on the Noise IK key-derivation chain,
-- bypassing the transport layer so that deterministic fixed keypairs can be
-- used as test vectors.  Each DH leg (ee, es, ss, se) is exercised explicitly
-- and negative tests verify that wrong or swapped DH terms cause detectable
-- handshake failure.
--
-- __Findings covered__
--
-- * DH-001 — Four DH outputs (ee, es, ss, se) are all distinct.
-- * DH-002 — Both sides derive the same final session keys.
-- * DH-003 — Encrypt on one side decrypts on the other.
-- * DH-004 — Replacing @se@ with a duplicate of @es@ (the old bug) causes
--            the two sides to derive DIFFERENT session keys.
-- * DH-005 — Omitting the @ee@ leg (zeroing its DH input) causes decryption
--            to fail.
-- * DH-006 — Swapping initiator and responder static keys causes handshake
--            failure.
-- * DH-007 — Using a different responder ephemeral than the one in msg2
--            causes decryption to fail.
module Test.Security.M11NoiseDH (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)

import Test.Util (assertEq, hexDecode)
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Network.Noise (NoiseState(..), noiseEncrypt, noiseDecrypt)
import UmbraVox.Network.Noise.Handshake
    ( hkdfCK
    , splitKeys
    , encryptAndTag
    , initHash
    , initCK
    , mixHash
    )
import UmbraVox.Network.Noise.State (prologue)

------------------------------------------------------------------------
-- Top-level runner
------------------------------------------------------------------------

runTests :: IO Bool
runTests = do
    putStrLn "[Security/M11NoiseDH] Running Noise IK DH-leg vector and negative tests..."
    results <- sequence
        [ -- M10.1.9 — DH leg test vectors
          testDH001FourLegsDistinct
        , testDH002BothSidesSameSessionKeys
        , testDH003EncryptDecryptRoundTrip

          -- M10.1.10 — Negative tests
        , testDH004DuplicateESAsSEKeysDiverge
        , testDH005ZeroEEDecryptionFails
        , testDH006SwappedStaticKeysFail
        , testDH007WrongResponderEphemeralFails
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Security/M11NoiseDH] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Fixed test keypairs (deterministic, RFC 7748 §6 vectors extended)
------------------------------------------------------------------------

-- Initiator static keypair
iStaticSec :: ByteString
iStaticSec = hexDecode "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"

iStaticPub :: ByteString
iStaticPub = case x25519 iStaticSec x25519Basepoint of
    Just p  -> p
    Nothing -> error "iStaticPub: impossible with test vector"

-- Responder static keypair
rStaticSec :: ByteString
rStaticSec = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"

rStaticPub :: ByteString
rStaticPub = case x25519 rStaticSec x25519Basepoint of
    Just p  -> p
    Nothing -> error "rStaticPub: impossible with test vector"

-- Initiator ephemeral keypair (fixed for determinism)
iEphSec :: ByteString
iEphSec = hexDecode "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"

iEphPub :: ByteString
iEphPub = case x25519 iEphSec x25519Basepoint of
    Just p  -> p
    Nothing -> error "iEphPub: impossible with test vector"

-- Responder ephemeral keypair (fixed for determinism)
rEphSec :: ByteString
rEphSec = hexDecode "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"

rEphPub :: ByteString
rEphPub = case x25519 rEphSec x25519Basepoint of
    Just p  -> p
    Nothing -> error "rEphPub: impossible with test vector"

------------------------------------------------------------------------
-- Reference DH computation
--
-- Replicate the full Noise IK chain from both sides using the fixed
-- keypairs above.  This mirrors noiseHandshakeInitiator /
-- noiseHandshakeResponder but works without a transport.
------------------------------------------------------------------------

-- | Compute all four DH legs and the final chaining key from the
-- initiator's perspective using the fixed test keypairs.
--
-- Returns (dhEE, dhES, dhSS, dhSE, ck4) where ck4 is the final
-- chaining key used to split session keys.
computeInitiatorChain
    :: ByteString   -- ^ Initiator static secret
    -> ByteString   -- ^ Initiator static public
    -> ByteString   -- ^ Responder static public
    -> ByteString   -- ^ Initiator ephemeral secret
    -> ByteString   -- ^ Initiator ephemeral public
    -> ByteString   -- ^ Responder ephemeral public
    -> Maybe (ByteString, ByteString, ByteString, ByteString, ByteString)
computeInitiatorChain isSec _isPub rsPub ieSec _iePub rePub = do
    -- Initialize hash and chaining key
    let !h0 = initHash
        !h1 = mixHash h0 prologue
        !h2 = mixHash h1 rsPub
        !ck0 = initCK

    -- -> e: mix initiator ephemeral into hash
    let !h3 = mixHash h2 iEphPub

    -- -> es: DH(e_i, s_r)
    dhES <- x25519 ieSec rsPub
    let (!ck1, !k1) = hkdfCK ck0 dhES

    -- -> s: encrypt initiator static public key
    let !encStaticPub = encryptAndTag k1 h3 _isPub
        !h4 = mixHash h3 encStaticPub

    -- -> ss: DH(s_i, s_r)
    dhSS <- x25519 isSec rsPub
    let (!ck2, !_k2) = hkdfCK ck1 dhSS

    -- <- e: mix responder ephemeral into hash
    let !h5 = mixHash h4 rePub
    _ <- pure h5

    -- <- ee: DH(e_i, e_r)
    dhEE <- x25519 ieSec rePub
    let (!ck3, !_k3) = hkdfCK ck2 dhEE

    -- <- se: DH(s_i, e_r)
    dhSE <- x25519 isSec rePub
    let (!ck4, !_k4) = hkdfCK ck3 dhSE

    pure (dhEE, dhES, dhSS, dhSE, ck4)

-- | Compute the full chain from the responder's perspective.
-- Returns (dhEE, dhES, dhSS, dhSE, ck4).
--
-- In these tests @isPub@ is supplied directly (as if the responder
-- successfully decrypted the initiator's static public key from msg1).
-- We compute @encStaticPub@ using the same @encryptAndTag@ the initiator
-- used, to reproduce the exact handshake hash h4 without going through
-- @decryptWithKey@ (which would require the framing macLen to match the
-- handshake-phase HMAC length, a concern separate from the DH chain).
computeResponderChain
    :: ByteString   -- ^ Responder static secret
    -> ByteString   -- ^ Responder static public
    -> ByteString   -- ^ Initiator static public (known in advance for test)
    -> ByteString   -- ^ Initiator ephemeral public
    -> ByteString   -- ^ Responder ephemeral secret
    -> ByteString   -- ^ Responder ephemeral public
    -> Maybe (ByteString, ByteString, ByteString, ByteString, ByteString)
computeResponderChain rsSec rsPub isPub iePub reSec rePub = do
    -- Initialize hash and chaining key
    let !h0 = initHash
        !h1 = mixHash h0 prologue
        !h2 = mixHash h1 rsPub
        !ck0 = initCK

    -- -> e: mix initiator ephemeral into hash
    let !h3 = mixHash h2 iePub

    -- -> es: DH(s_r, e_i)
    dhES <- x25519 rsSec iePub
    let (!ck1, !k1) = hkdfCK ck0 dhES

    -- -> s: reproduce encStaticPub that the initiator sent, to hash it
    -- (same formula as encryptAndTag in the initiator path)
    let !encStaticPub = encryptAndTag k1 h3 isPub
        !h4 = mixHash h3 encStaticPub

    -- -> ss: DH(s_r, s_i)
    dhSS <- x25519 rsSec isPub
    let (!ck2, !_k2) = hkdfCK ck1 dhSS

    -- <- e: mix responder ephemeral into hash
    let !_h5 = mixHash h4 rePub

    -- <- ee: DH(e_r, e_i)
    dhEE <- x25519 reSec iePub
    let (!ck3, !_k3) = hkdfCK ck2 dhEE

    -- <- se: DH(e_r, s_i)
    dhSE <- x25519 reSec isPub
    let (!ck4, !_k4) = hkdfCK ck3 dhSE

    pure (dhEE, dhES, dhSS, dhSE, ck4)

------------------------------------------------------------------------
-- DH-001 — Four DH outputs are distinct from each other
--
-- Finding:       DH-001
-- Vulnerability: If any two DH legs produce the same output, one DH
--                contribution is effectively absorbed into another.  An
--                adversary who learns one DH secret (e.g. the long-term
--                static key) also learns the value used for the duplicate
--                leg, collapsing the chain's independence.
-- Fix:           The Noise IK pattern uses four independent key-pairs:
--                (e_i, s_r), (s_i, s_r), (e_i, e_r), (s_i, e_r).
--                For generic keys these four DH outputs are
--                cryptographically independent.
-- Verified:      Compute all four DH outputs with fixed test keypairs and
--                assert pairwise inequality.
------------------------------------------------------------------------

testDH001FourLegsDistinct :: IO Bool
testDH001FourLegsDistinct = do
    case computeInitiatorChain iStaticSec iStaticPub rStaticPub
             iEphSec iEphPub rEphPub of
      Nothing ->
          putStrLn "  FAIL: DH-001 could not compute DH chain" >> pure False
      Just (dhEE, dhES, dhSS, dhSE, _ck4) -> do
          let pairs = [ ("ee vs es", dhEE, dhES)
                      , ("ee vs ss", dhEE, dhSS)
                      , ("ee vs se", dhEE, dhSE)
                      , ("es vs ss", dhES, dhSS)
                      , ("es vs se", dhES, dhSE)
                      , ("ss vs se", dhSS, dhSE)
                      ]
          let allDistinct = all (\(_, a, b) -> a /= b) pairs
          if allDistinct
            then putStrLn "  PASS: DH-001 all four DH outputs are distinct" >> pure True
            else do
              let dupes = [ label | (label, a, b) <- pairs, a == b ]
              putStrLn $ "  FAIL: DH-001 duplicate DH outputs: " ++ show dupes
              pure False

------------------------------------------------------------------------
-- DH-002 — Both sides derive the same final session keys
--
-- Finding:       DH-002
-- Vulnerability: If the initiator and responder derive different session
--                keys, every subsequent encrypted message is silently
--                corrupted.  This would indicate a key-derivation asymmetry.
-- Fix:           Both noiseHandshakeInitiator and noiseHandshakeResponder
--                apply the same four DH legs (in the same order) and feed
--                the results through the same hkdfCK chain, ending at ck4.
--                splitKeys(ck4) produces the same four sub-keys on both sides.
-- Verified:      Compute ck4 independently from initiator's and responder's
--                perspectives with fixed keypairs and assert equality.
------------------------------------------------------------------------

testDH002BothSidesSameSessionKeys :: IO Bool
testDH002BothSidesSameSessionKeys = do
    let mInit = computeInitiatorChain iStaticSec iStaticPub rStaticPub
                    iEphSec iEphPub rEphPub
        mResp = computeResponderChain rStaticSec rStaticPub iStaticPub
                    iEphPub rEphSec rEphPub
    case (mInit, mResp) of
      (Nothing, _) ->
          putStrLn "  FAIL: DH-002 initiator chain failed" >> pure False
      (_, Nothing) ->
          putStrLn "  FAIL: DH-002 responder chain failed" >> pure False
      (Just (_,_,_,_, ck4I), Just (_,_,_,_, ck4R)) ->
          assertEq "DH-002 both sides derive same ck4" ck4I ck4R

------------------------------------------------------------------------
-- DH-003 — Encrypt on one side decrypts on the other
--
-- Finding:       DH-003
-- Vulnerability: If session keys are correct but the send/recv key
--                assignment is inverted between sides, encryption succeeds
--                on the initiator but decryption returns Nothing on the
--                responder (or vice versa), silently breaking the channel.
-- Fix:           splitKeys returns (sendEncKey, recvEncKey) from the
--                initiator's perspective; the responder swaps these:
--                nsSendEncKey = iRecvEncKey, nsRecvEncKey = iSendEncKey.
-- Verified:      Build NoiseState structs from ck4 for both sides, encrypt
--                a test message from the initiator, decrypt on the responder
--                and assert equality with the original plaintext.
------------------------------------------------------------------------

testDH003EncryptDecryptRoundTrip :: IO Bool
testDH003EncryptDecryptRoundTrip = do
    let mInit = computeInitiatorChain iStaticSec iStaticPub rStaticPub
                    iEphSec iEphPub rEphPub
        mResp = computeResponderChain rStaticSec rStaticPub iStaticPub
                    iEphPub rEphSec rEphPub
    case (mInit, mResp) of
      (Nothing, _) ->
          putStrLn "  FAIL: DH-003 initiator chain failed" >> pure False
      (_, Nothing) ->
          putStrLn "  FAIL: DH-003 responder chain failed" >> pure False
      (Just (_,_,_,_, ck4I), Just (_,_,_,_, ck4R)) -> do
          let !h5I = computeH5 iEphPub rEphPub
              !h5R = h5I   -- both sides hash the same transcript
              (!iSendEncKey, !iRecvEncKey) = splitKeys ck4I
              (!rSendEncKey, !rRecvEncKey) = splitKeys ck4R
              -- Initiator: send keys as-is
              iState = NoiseState
                  { nsSendEncKey   = iSendEncKey
                  , nsRecvEncKey   = iRecvEncKey
                  , nsSendN        = 0
                  , nsRecvN        = 0
                  , nsHandshakeHash = h5I
                  }
              -- Responder: swap send/recv
              rState = NoiseState
                  { nsSendEncKey   = rRecvEncKey
                  , nsRecvEncKey   = rSendEncKey
                  , nsSendN        = 0
                  , nsRecvN        = 0
                  , nsHandshakeHash = h5R
                  }
              msg = BS.pack [0x68, 0x65, 0x6c, 0x6c, 0x6f]  -- "hello"
              Just (_, ct) = noiseEncrypt iState msg
          case noiseDecrypt rState ct of
            Nothing ->
                putStrLn "  FAIL: DH-003 responder could not decrypt initiator ciphertext" >> pure False
            Just (_, pt) ->
                assertEq "DH-003 encrypt/decrypt round-trip" msg pt

------------------------------------------------------------------------
-- DH-004 — Replacing se with a duplicate of es causes key divergence
--
-- Finding:       DH-004
-- Vulnerability: The original bug in noiseHandshakeInitiator computed
--                se = DH(e_i, s_r) instead of se = DH(s_i, e_r), which
--                is the same as the es leg (DH commutes on the same pair).
--                This collapsed two independent DH contributions into one.
-- Fix:           noiseHandshakeInitiator now computes se = x25519 iStaticSec
--                rEPub (Handshake.hs lines 155-158), which the responder
--                mirrors as x25519 eSec iStaticPub.
-- Verified:      Manually wire a "buggy" initiator that uses dhES in place of
--                dhSE and a correct responder; assert their ck4 values differ.
------------------------------------------------------------------------

testDH004DuplicateESAsSEKeysDiverge :: IO Bool
testDH004DuplicateESAsSEKeysDiverge = do
    -- Correct responder chain
    let mResp = computeResponderChain rStaticSec rStaticPub iStaticPub
                    iEphPub rEphSec rEphPub
    -- Buggy initiator chain: substitute es output in place of se
    let mBuggy = computeBuggyInitiatorChain iStaticSec iStaticPub rStaticPub
                     iEphSec iEphPub rEphPub
    case (mBuggy, mResp) of
      (Nothing, _) ->
          putStrLn "  FAIL: DH-004 buggy chain computation failed" >> pure False
      (_, Nothing) ->
          putStrLn "  FAIL: DH-004 responder chain computation failed" >> pure False
      (Just ck4Buggy, Just (_, _, _, _, ck4Resp)) ->
          if ck4Buggy /= ck4Resp
            then putStrLn "  PASS: DH-004 duplicated es-as-se causes key divergence" >> pure True
            else putStrLn "  FAIL: DH-004 buggy and correct chains must produce different ck4" >> pure False

-- | Buggy initiator chain: uses dhES again in place of dhSE.
computeBuggyInitiatorChain
    :: ByteString -> ByteString -> ByteString
    -> ByteString -> ByteString -> ByteString
    -> Maybe ByteString
computeBuggyInitiatorChain isSec _isPub rsPub ieSec _iePub rePub = do
    let !h0 = initHash
        !h1 = mixHash h0 prologue
        !h2 = mixHash h1 rsPub
        !ck0 = initCK
    let !h3 = mixHash h2 iEphPub

    dhES <- x25519 ieSec rsPub
    let (!ck1, !k1) = hkdfCK ck0 dhES

    let !encStaticPub = encryptAndTag k1 h3 _isPub
        !h4 = mixHash h3 encStaticPub

    dhSS <- x25519 isSec rsPub
    let (!ck2, !_k2) = hkdfCK ck1 dhSS

    let !h5 = mixHash h4 rePub
    _ <- pure h5

    dhEE <- x25519 ieSec rePub
    let (!ck3, !_k3) = hkdfCK ck2 dhEE

    -- BUG: reuse dhES here instead of computing dhSE = x25519 isSec rePub
    let (!ck4, !_k4) = hkdfCK ck3 dhES

    pure ck4

------------------------------------------------------------------------
-- DH-005 — Omitting ee (zero DH input) causes decryption failure
--
-- Finding:       DH-005
-- Vulnerability: If the ee leg is zeroed (e.g. because the implementation
--                forgot to include it, or the ephemeral key was never
--                generated), the chaining key ck3 is derived from a known
--                constant instead of a fresh ephemeral contribution.  This
--                removes ephemeral forward secrecy from the derived keys.
-- Fix:           The production handshake always computes
--                ee = x25519 eSec rEPub (or x25519 reSec iEPub on the
--                responder side) and rejects all-zero DH output (Nothing).
-- Verified:      Build an initiator state with zero injected for dhEE and
--                a correct responder state; verify that decryption fails.
------------------------------------------------------------------------

testDH005ZeroEEDecryptionFails :: IO Bool
testDH005ZeroEEDecryptionFails = do
    let mResp = computeResponderChain rStaticSec rStaticPub iStaticPub
                    iEphPub rEphSec rEphPub
    -- Initiator with zero ee term
    let mBuggy = computeZeroEEInitiator iStaticSec iStaticPub rStaticPub
                     iEphSec iEphPub rEphPub
    case (mBuggy, mResp) of
      (Nothing, _) ->
          -- If zero-ee causes Nothing directly, that's also a pass
          -- (the DH guard fires)
          putStrLn "  PASS: DH-005 zero ee -> chain rejected (Nothing)" >> pure True
      (_, Nothing) ->
          putStrLn "  FAIL: DH-005 responder chain failed" >> pure False
      (Just ck4Buggy, Just (_, _, _, _, ck4Resp)) -> do
          let !h5I = computeH5 iEphPub rEphPub
              !h5R = h5I
              (!iSendEncKey, _) = splitKeys ck4Buggy
              (!rSendEncKey, !rRecvEncKey) = splitKeys ck4Resp
              iState = NoiseState
                  { nsSendEncKey   = iSendEncKey
                  , nsRecvEncKey   = BS.replicate 32 0  -- doesn't matter
                  , nsSendN        = 0
                  , nsRecvN        = 0
                  , nsHandshakeHash = h5I
                  }
              rState = NoiseState
                  { nsSendEncKey   = rRecvEncKey
                  , nsRecvEncKey   = rSendEncKey
                  , nsSendN        = 0
                  , nsRecvN        = 0
                  , nsHandshakeHash = h5R
                  }
              msg = BS.pack [0x74, 0x65, 0x73, 0x74]  -- "test"
              Just (_, ct) = noiseEncrypt iState msg
          case noiseDecrypt rState ct of
            Nothing ->
                putStrLn "  PASS: DH-005 zero ee -> decryption correctly fails" >> pure True
            Just _ ->
                putStrLn "  FAIL: DH-005 zero ee must cause decryption failure" >> pure False

-- | Initiator chain with zero bytes substituted for dhEE.
computeZeroEEInitiator
    :: ByteString -> ByteString -> ByteString
    -> ByteString -> ByteString -> ByteString
    -> Maybe ByteString
computeZeroEEInitiator isSec _isPub rsPub ieSec _iePub rePub = do
    let !h0 = initHash
        !h1 = mixHash h0 prologue
        !h2 = mixHash h1 rsPub
        !ck0 = initCK
    let !h3 = mixHash h2 iEphPub

    dhES <- x25519 ieSec rsPub
    let (!ck1, !k1) = hkdfCK ck0 dhES

    let !encStaticPub = encryptAndTag k1 h3 _isPub
        !h4 = mixHash h3 encStaticPub

    dhSS <- x25519 isSec rsPub
    let (!ck2, !_k2) = hkdfCK ck1 dhSS

    let !h5 = mixHash h4 rePub
    _ <- pure h5

    -- Zero out dhEE instead of computing the real DH
    let zeroDhEE = BS.replicate 32 0x00
        (!ck3, !_k3) = hkdfCK ck2 zeroDhEE

    dhSE <- x25519 isSec rePub
    let (!ck4, !_k4) = hkdfCK ck3 dhSE

    pure ck4

------------------------------------------------------------------------
-- DH-006 — Swapping initiator and responder static keys causes failure
--
-- Finding:       DH-006
-- Vulnerability: If the initiator uses the wrong static key (e.g. the
--                responder's static key as its own), the es/ss/se DH
--                computations will yield different values on each side.
--                The session keys will diverge and all messages will fail
--                to decrypt.
-- Fix:           Both sides compute DH using their own key material;
--                agreement is enforced by the DH commutativity property
--                only when each side uses the correct role-specific key.
-- Verified:      Run the initiator chain with swapped static keys and the
--                responder chain with correct keys; assert ck4 values differ.
------------------------------------------------------------------------

testDH006SwappedStaticKeysFail :: IO Bool
testDH006SwappedStaticKeysFail = do
    -- Initiator uses rStaticSec/rStaticPub instead of iStaticSec/iStaticPub
    let mSwapped = computeInitiatorChain rStaticSec rStaticPub rStaticPub
                       iEphSec iEphPub rEphPub
        mCorrect = computeResponderChain rStaticSec rStaticPub iStaticPub
                       iEphPub rEphSec rEphPub
    case (mSwapped, mCorrect) of
      (Nothing, _) ->
          putStrLn "  PASS: DH-006 swapped static keys -> chain rejected (Nothing)" >> pure True
      (_, Nothing) ->
          putStrLn "  FAIL: DH-006 correct responder chain failed" >> pure False
      (Just (_, _, _, _, ck4Swapped), Just (_, _, _, _, ck4Correct)) ->
          if ck4Swapped /= ck4Correct
            then putStrLn "  PASS: DH-006 swapped static keys cause session key divergence" >> pure True
            else putStrLn "  FAIL: DH-006 swapped static keys must cause key divergence" >> pure False

------------------------------------------------------------------------
-- DH-007 — Wrong responder ephemeral causes decryption failure
--
-- Finding:       DH-007
-- Vulnerability: The ee and se legs both depend on the responder's
--                ephemeral public key rEPub sent in msg2.  If the
--                initiator uses a different (e.g. stale or attacker-
--                injected) ephemeral for the DH computation, both legs
--                are wrong and the derived session keys differ from the
--                responder's view.
-- Fix:           The initiator reads rEPub exclusively from the received
--                msg2 frame; there is no cached or fallback value.  An
--                injected or replayed rEPub causes MAC verification to
--                fail on the first transport message.
-- Verified:      Build an initiator state using an alternate ephemeral
--                (rEphPubAlt) and the correct responder state; verify
--                that decryption fails.
------------------------------------------------------------------------

testDH007WrongResponderEphemeralFails :: IO Bool
testDH007WrongResponderEphemeralFails = do
    -- Alternate ephemeral for the responder (wrong from initiator's view)
    let rEphSecAlt = hexDecode "b8b4e236805318e93f48bfbb365656ec1d068bf3d8cabb64dd1ba4523cec3a2a"
        rEphPubAlt = case x25519 rEphSecAlt x25519Basepoint of
                         Just p  -> p
                         Nothing -> error "rEphPubAlt: impossible"

    -- Initiator uses the wrong rEPub (alt) for DH; responder uses the correct one
    let mWrong = computeInitiatorChain iStaticSec iStaticPub rStaticPub
                     iEphSec iEphPub rEphPubAlt
        mCorrect = computeResponderChain rStaticSec rStaticPub iStaticPub
                       iEphPub rEphSec rEphPub
    case (mWrong, mCorrect) of
      (Nothing, _) ->
          putStrLn "  PASS: DH-007 wrong ephemeral -> chain rejected (Nothing)" >> pure True
      (_, Nothing) ->
          putStrLn "  FAIL: DH-007 correct responder chain failed" >> pure False
      (Just (_, _, _, _, ck4Wrong), Just (_, _, _, _, ck4Correct)) -> do
          let !h5W = computeH5 iEphPub rEphPubAlt
              !h5R = computeH5 iEphPub rEphPub
              (!iSendEncKey, _) = splitKeys ck4Wrong
              (!rSendEncKey, !rRecvEncKey) = splitKeys ck4Correct
              iState = NoiseState
                  { nsSendEncKey   = iSendEncKey
                  , nsRecvEncKey   = BS.replicate 32 0
                  , nsSendN        = 0
                  , nsRecvN        = 0
                  , nsHandshakeHash = h5W
                  }
              rState = NoiseState
                  { nsSendEncKey   = rRecvEncKey
                  , nsRecvEncKey   = rSendEncKey
                  , nsSendN        = 0
                  , nsRecvN        = 0
                  , nsHandshakeHash = h5R
                  }
              msg = BS.pack [0x70, 0x72, 0x6f, 0x62, 0x65]  -- "probe"
              Just (_, ct) = noiseEncrypt iState msg
          case noiseDecrypt rState ct of
            Nothing ->
                putStrLn "  PASS: DH-007 wrong responder ephemeral -> decryption fails" >> pure True
            Just _ ->
                putStrLn "  FAIL: DH-007 wrong responder ephemeral must cause decryption failure" >> pure False

------------------------------------------------------------------------
-- Internal helper: compute the final handshake hash h5
------------------------------------------------------------------------

-- | Reproduce the h5 computation (after the four DH legs, both sides
-- hash the same transcript up to and including the responder's ephemeral
-- public key).
computeH5 :: ByteString -> ByteString -> ByteString
computeH5 iePub rePub =
    let !h0 = initHash
        !h1 = mixHash h0 prologue
        !h2 = mixHash h1 rStaticPub
        !h3 = mixHash h2 iePub
        -- After es we encrypt the initiator's static public; approximate
        -- the hash contribution using the real encStaticPub.
        !ck0 = initCK
        !dhES = case x25519 iEphSec rStaticPub of
                    Just v -> v
                    Nothing -> error "computeH5: impossible dhES"
        (!_ck1, !k1) = hkdfCK ck0 dhES
        !encStaticPub = encryptAndTag k1 h3 iStaticPub
        !h4 = mixHash h3 encStaticPub
        !h5 = mixHash h4 rePub
    in h5

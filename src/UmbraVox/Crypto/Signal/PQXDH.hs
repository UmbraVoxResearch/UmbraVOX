-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "SIGNAL-002" #-} PQXDH (Post-Quantum Extended Triple Diffie-Hellman)
--
-- Hybrid classical + post-quantum key agreement combining X3DH with ML-KEM-768.
-- Security holds if EITHER the classical CDH problem OR the ML-KEM (Module-LWE)
-- problem is hard. See: attic/doc-legacy-2026-04-28/03-cryptography.md
--
-- Finding:     M10.2.1 — PQ prekey lacked an Ed25519 signature.  An active
--              adversary who can substitute the ML-KEM encapsulation key in
--              transit would supply their own decapsulation key and forward a
--              KEM ciphertext of their choosing to Bob.  Because the initiator
--              encapsulates under the adversary's key, the adversary learns
--              the ML-KEM shared secret pqSS, breaking the quantum-hard
--              component of PQXDH.
-- Vulnerability: Without a signature covering pqpkbPQPreKey, any
--              man-in-the-middle can swap the ML-KEM encapsulation key with
--              one for which they hold the decapsulation key.  The session
--              key derivation then depends entirely on the classical X25519
--              DH terms, degrading PQXDH to ordinary X3DH.
-- Fix:         Added pqpkbPQKeySignature :: !ByteString — an Ed25519
--              signature produced by the responder's identity key over the
--              raw bytes of the ML-KEM encapsulation key.  pqxdhInitiate
--              now verifies this signature using pqpkbIdentityEd25519 before
--              calling mlkemEncaps.  If verification fails the function
--              returns Nothing, aborting the handshake.
-- Verified:    testPQXDHPQKeySigVerification (Test.Crypto.PQXDH): a zeroed
--              PQ prekey signature is rejected (initiate returns Nothing);
--              a valid signature passes and both sides derive the same secret.
module UmbraVox.Crypto.Signal.PQXDH
    ( PQPreKeyBundle(..)
    , PQXDHResult(..)
    , pqxdhInitiate
    , pqxdhRespond
    ) where

import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)

import UmbraVox.Crypto.ConstantTime (constantEq)
import qualified UmbraVox.Crypto.Generated.FFI.Ed25519Extended as Ed25519FFI
import qualified UmbraVox.Crypto.Generated.FFI.HKDF as HKDFFFI
import qualified UmbraVox.Crypto.Generated.FFI.SHA256 as SHA256FFI
import qualified UmbraVox.Crypto.Generated.FFI.X25519 as X25519FFI
import UmbraVox.Crypto.MLKEM (MLKEMEncapKey(..), MLKEMDecapKey, MLKEMCiphertext(..),
                               mlkemEncaps, mlkemDecaps)
import UmbraVox.Crypto.SecureBytes (toByteString)
import UmbraVox.Crypto.Signal.X3DH (KeyPair(..), IdentityKey(..), generateKeyPair)

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

-- | Bob's prekey bundle extended with a post-quantum prekey (ML-KEM-768).
data PQPreKeyBundle = PQPreKeyBundle
    { pqpkbIdentityKey     :: !ByteString        -- ^ Bob's X25519 identity public key
    , pqpkbSignedPreKey    :: !ByteString        -- ^ Bob's SPK public key
    , pqpkbSPKSignature    :: !ByteString        -- ^ Ed25519 signature over SPK
    , pqpkbIdentityEd25519 :: !ByteString        -- ^ Bob's Ed25519 identity public key
    , pqpkbOneTimePreKey   :: !(Maybe ByteString) -- ^ Bob's OPK public key (if available)
    , pqpkbPQPreKey        :: !MLKEMEncapKey      -- ^ Bob's ML-KEM-768 encapsulation key
    , pqpkbPQKeySignature  :: !ByteString        -- ^ Ed25519 signature over the PQ encap key bytes
    }

-- | Result of the PQXDH initiation (Alice's side).
data PQXDHResult = PQXDHResult
    { pqxdhSharedSecret  :: !ByteString        -- ^ 32-byte master secret
    , pqxdhEphemeralKey  :: !ByteString        -- ^ Alice's ephemeral public key
    , pqxdhPQCiphertext  :: !MLKEMCiphertext   -- ^ ML-KEM ciphertext for Bob
    , pqxdhUsedOPK       :: !(Maybe ByteString) -- ^ Which OPK was consumed
    }

------------------------------------------------------------------------
-- Protocol info string
------------------------------------------------------------------------

pqxdhInfo :: ByteString
pqxdhInfo = BS.pack (map (fromIntegral . fromEnum) "UmbraVox_PQXDH_v1")

-- | M27.6.10: Prepend a 4-byte big-endian length prefix to a key.
-- Prevents concatenation collisions when variable-length keys are
-- used in HKDF info strings under protocol evolution.
lenPrefix :: ByteString -> ByteString
lenPrefix bs =
    let !len = fromIntegral (BS.length bs) :: Word32
    in BS.pack [ fromIntegral (len `shiftR` 24)
               , fromIntegral (len `shiftR` 16)
               , fromIntegral (len `shiftR` 8)
               , fromIntegral len
               ] <> bs

------------------------------------------------------------------------
-- Shared secret derivation
------------------------------------------------------------------------

-- | Derive the PQXDH master secret from DH outputs and ML-KEM shared secret.
-- ikm = 0xFF*32 || dh1 || dh2 || dh3 || [dh4] || pq_ss || SHA256(pq_ct)
-- salt = 0x00*32
-- info = "UmbraVox_PQXDH_v1" || len(IK_A_pub) || IK_A_pub || len(IK_B_pub) || IK_B_pub
-- output = 32 bytes
--
-- M27.6.10: Each key in the info string is now preceded by its 4-byte
-- big-endian length to prevent concatenation collisions under protocol
-- evolution.
--
-- The SHA256(pq_ct) binding ensures both parties committed to the same
-- PQ ciphertext, preventing substitution attacks on the KEM exchange.
--
-- SecureBytes migration (M15.3.3): the @pq_ss@ argument is the raw ML-KEM
-- shared secret — the most sensitive intermediate value in PQXDH.  Once
-- 'mlkemEncaps' / 'mlkemDecaps' can return their shared secret wrapped in
-- 'UmbraVox.Crypto.SecureBytes.SecureBytes', this function should accept
-- @SecureBytes@ for @pq_ss@ (and for the DH outputs) and zero them
-- immediately after HKDF expansion.  Coordinate this with the MLKEM module.
derivePQSecret :: ByteString   -- ^ dh1
               -> ByteString   -- ^ dh2
               -> ByteString   -- ^ dh3
               -> Maybe ByteString  -- ^ dh4 (optional)
               -> ByteString   -- ^ pq_ss (ML-KEM shared secret)
                               -- TODO(M15.3): accept SecureBytes here (see note above)
               -> MLKEMCiphertext  -- ^ pq_ct (ML-KEM ciphertext, hashed into ikm)
               -> ByteString   -- ^ Initiator (Alice) X25519 identity public key
               -> ByteString   -- ^ Responder (Bob) X25519 identity public key
               -> IO ByteString   -- ^ 32-byte master secret
derivePQSecret !dh1 !dh2 !dh3 !mDh4 !pqSS !pqCt !aliceIKPub !bobIKPub = do
    let MLKEMCiphertext ctBytes = pqCt
    !ctHash <- SHA256FFI.sha256 ctBytes
    let !pad  = BS.replicate 32 0xff
        !salt = BS.replicate 32 0x00
        !ikm  = BS.concat $ [pad, dh1, dh2, dh3]
                          ++ maybe [] (:[]) mDh4
                          ++ [pqSS, ctHash]
        -- M27.6.10: length-prefixed keys in info string
        !info = pqxdhInfo <> lenPrefix aliceIKPub <> lenPrefix bobIKPub
    HKDFFFI.hkdfSHA512 salt ikm info 32

------------------------------------------------------------------------
-- PQXDH Initiation (Alice's side)
------------------------------------------------------------------------

-- | Alice initiates PQXDH to Bob.
--
-- Takes Alice's identity key, Bob's prekey bundle, a 32-byte ephemeral
-- secret for generating the ephemeral X25519 keypair, and a 32-byte
-- randomness value for ML-KEM encapsulation.
-- Returns Nothing if SPK signature verification fails or any DH yields
-- an all-zero output (low-order point attack rejection).
pqxdhInitiate :: IdentityKey      -- ^ Alice's identity key
              -> PQPreKeyBundle   -- ^ Bob's prekey bundle
              -> ByteString       -- ^ 32-byte ephemeral secret
              -> ByteString       -- ^ 32-byte ML-KEM randomness
              -> IO (Maybe PQXDHResult)
pqxdhInitiate aliceIK bundle ekSecret mlkemRand = do
    -- Step 1: Verify SPK signature (M23.3.1: message = ikEd25519 || spkPub)
    spkOk <- Ed25519FFI.ed25519Verify (pqpkbIdentityEd25519 bundle)
                 (pqpkbIdentityEd25519 bundle <> pqpkbSignedPreKey bundle)
                 (pqpkbSPKSignature bundle)
    if not spkOk then pure Nothing
    else do
        -- Step 2: Verify PQ prekey signature (M10.2.1)
        -- The ML-KEM encapsulation key bytes are signed by the responder's Ed25519
        -- identity key.  Verification ensures the encap key was not substituted in
        -- transit; a MITM without the responder's Ed25519 secret cannot forge this.
        let MLKEMEncapKey pqBytes = pqpkbPQPreKey bundle
        pqOk <- Ed25519FFI.ed25519Verify (pqpkbIdentityEd25519 bundle)
                    pqBytes
                    (pqpkbPQKeySignature bundle)
        if not pqOk then pure Nothing
        else initSession aliceIK bundle ekSecret mlkemRand

-- TODO(M15.8): CRITICAL — dh1/dh2/dh3/dh4 (lines ~182-187) and pqSS (line ~191)
-- are all plain ByteString values on the GC heap once extracted from SecureBytes
-- (toByteString creates an unprotected copy).  None are explicitly zeroed after
-- derivePQSecret completes.  Blast radius: heap dump or swap file between the
-- X25519 calls and HKDF completion exposes all DH outputs and the ML-KEM shared
-- secret; combined with intercepted ciphertexts this allows retroactive session
-- decryption.  Fix (M15.8): wrap each DH output in SecureBytes immediately after
-- X25519FFI.x25519; use withSecureKey for each; concatenate into a single zeroed
-- SecureBytes ikm buffer; pass pqSS as SecureBytes; zero all intermediates after
-- HKDF.  Coordinate with mlkemEncaps returning SecureBytes for pqSS.

-- | Compute the PQXDH session after SPK verification succeeds.
-- Returns Nothing if any DH output is all-zero (low-order point).
initSession :: IdentityKey -> PQPreKeyBundle -> ByteString -> ByteString -> IO (Maybe PQXDHResult)
initSession aliceIK bundle ekSecret mlkemRand = do
    -- Generate ephemeral keypair
    ek <- generateKeyPair ekSecret
    -- Extract secrets from SecureBytes for DH operations
    xSecret   <- toByteString (ikX25519Secret aliceIK)
    ekSecret' <- toByteString (kpSecret ek)
    -- Compute DH values; abort if any yields all-zero (low-order point)
    mDh1 <- X25519FFI.x25519 xSecret   (pqpkbSignedPreKey bundle)
    mDh2 <- X25519FFI.x25519 ekSecret' (pqpkbIdentityKey bundle)
    mDh3 <- X25519FFI.x25519 ekSecret' (pqpkbSignedPreKey bundle)
    mDh4 <- case pqpkbOneTimePreKey bundle of
                 Nothing  -> pure (Just Nothing)
                 Just opk -> fmap (fmap Just) (X25519FFI.x25519 ekSecret' opk)
    case (mDh1, mDh2, mDh3, mDh4) of
        (Just dh1, Just dh2, Just dh3, Just mDh4') -> do
            let -- ML-KEM encapsulation
                !(pqCt, pqSS) = mlkemEncaps (pqpkbPQPreKey bundle) mlkemRand
            -- M23.3.2: validate ML-KEM encapsulation output
            if BS.length pqSS /= 32 || constantEq pqSS (BS.replicate 32 0)
                then pure Nothing
                else do
                    masterSecret <- derivePQSecret dh1 dh2 dh3 mDh4' pqSS pqCt
                                        (ikX25519Public aliceIK) (pqpkbIdentityKey bundle)
                    pure $ Just PQXDHResult
                        { pqxdhSharedSecret = masterSecret
                        , pqxdhEphemeralKey = kpPublic ek
                        , pqxdhPQCiphertext = pqCt
                        , pqxdhUsedOPK      = pqpkbOneTimePreKey bundle
                        }
        _ -> pure Nothing

------------------------------------------------------------------------
-- PQXDH Response (Bob's side)
------------------------------------------------------------------------

-- | Bob responds to Alice's PQXDH initiation.
--
-- Computes the same DH values as Alice (from Bob's perspective),
-- decapsulates the ML-KEM ciphertext, and derives the same master secret.
pqxdhRespond :: IdentityKey       -- ^ Bob's identity key
             -> ByteString        -- ^ Bob's SPK secret key
             -> Maybe ByteString  -- ^ Bob's OPK secret key (if used)
             -> MLKEMDecapKey     -- ^ Bob's ML-KEM decapsulation key
             -> ByteString        -- ^ Alice's X25519 identity public key
             -> ByteString        -- ^ Alice's ephemeral public key
             -> MLKEMCiphertext   -- ^ ML-KEM ciphertext from Alice
             -> IO (Maybe ByteString)  -- ^ 32-byte shared secret, or Nothing on low-order point
pqxdhRespond bobIK spkSecret mOPKSecret pqDK aliceIKPub aliceEKPub pqCt = do
    xSecret <- toByteString (ikX25519Secret bobIK)
    -- Mirror the DH computations from Alice's perspective
    -- Abort if any DH yields all-zero (low-order point attack)
    mDh1 <- X25519FFI.x25519 spkSecret aliceIKPub
    mDh2 <- X25519FFI.x25519 xSecret   aliceEKPub
    mDh3 <- X25519FFI.x25519 spkSecret aliceEKPub
    mDh4 <- case mOPKSecret of
                 Nothing     -> pure (Just Nothing)
                 Just opkSec -> fmap (fmap Just) (X25519FFI.x25519 opkSec aliceEKPub)
    case (mDh1, mDh2, mDh3, mDh4) of
        (Just dh1, Just dh2, Just dh3, Just mDh4') -> do
            let -- ML-KEM decapsulation
                !pqSS = mlkemDecaps pqDK pqCt
            -- M23.3.2: validate ML-KEM decapsulation output
            if BS.length pqSS /= 32 || constantEq pqSS (BS.replicate 32 0)
                then pure Nothing
                else fmap Just (derivePQSecret dh1 dh2 dh3 mDh4' pqSS pqCt aliceIKPub (ikX25519Public bobIK))
        _ -> pure Nothing

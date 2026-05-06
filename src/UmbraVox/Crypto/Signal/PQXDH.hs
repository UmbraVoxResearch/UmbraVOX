-- | {-# REQ "SIGNAL-002" #-} PQXDH (Post-Quantum Extended Triple Diffie-Hellman)
--
-- Hybrid classical + post-quantum key agreement combining X3DH with ML-KEM-768.
-- Security holds if EITHER the classical CDH problem OR the ML-KEM (Module-LWE)
-- problem is hard. See: doc/03-cryptography.md
module UmbraVox.Crypto.Signal.PQXDH
    ( PQPreKeyBundle(..)
    , PQXDHResult(..)
    , pqxdhInitiate
    , pqxdhRespond
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import UmbraVox.Crypto.Curve25519 (x25519)
import UmbraVox.Crypto.Ed25519 (ed25519Verify)
import UmbraVox.Crypto.HKDF (hkdf)
import UmbraVox.Crypto.MLKEM (MLKEMEncapKey, MLKEMDecapKey, MLKEMCiphertext,
                               mlkemEncaps, mlkemDecaps)
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

------------------------------------------------------------------------
-- Shared secret derivation
------------------------------------------------------------------------

-- | Derive the PQXDH master secret from DH outputs and ML-KEM shared secret.
-- ikm = 0xFF*32 || dh1 || dh2 || dh3 || [dh4] || pq_ss
-- salt = 0x00*32
-- info = "UmbraVox_PQXDH_v1" || IK_A_pub || IK_B_pub (identity binding)
-- output = 32 bytes
derivePQSecret :: ByteString   -- ^ dh1
               -> ByteString   -- ^ dh2
               -> ByteString   -- ^ dh3
               -> Maybe ByteString  -- ^ dh4 (optional)
               -> ByteString   -- ^ pq_ss (ML-KEM shared secret)
               -> ByteString   -- ^ Initiator (Alice) X25519 identity public key
               -> ByteString   -- ^ Responder (Bob) X25519 identity public key
               -> ByteString   -- ^ 32-byte master secret
derivePQSecret !dh1 !dh2 !dh3 !mDh4 !pqSS !aliceIKPub !bobIKPub =
    let !pad  = BS.replicate 32 0xff
        !salt = BS.replicate 32 0x00
        !ikm  = BS.concat $ [pad, dh1, dh2, dh3]
                          ++ maybe [] (:[]) mDh4
                          ++ [pqSS]
        !info = pqxdhInfo <> aliceIKPub <> bobIKPub
    in hkdf salt ikm info 32

------------------------------------------------------------------------
-- PQXDH Initiation (Alice's side)
------------------------------------------------------------------------

-- | Alice initiates PQXDH to Bob.
--
-- Takes Alice's identity key, Bob's prekey bundle, a 32-byte ephemeral
-- secret for generating the ephemeral X25519 keypair, and a 32-byte
-- randomness value for ML-KEM encapsulation.
-- Returns Nothing if SPK signature verification fails.
pqxdhInitiate :: IdentityKey      -- ^ Alice's identity key
              -> PQPreKeyBundle   -- ^ Bob's prekey bundle
              -> ByteString       -- ^ 32-byte ephemeral secret
              -> ByteString       -- ^ 32-byte ML-KEM randomness
              -> Maybe PQXDHResult
pqxdhInitiate aliceIK bundle ekSecret mlkemRand =
    -- Step 1: Verify SPK signature
    if not (ed25519Verify (pqpkbIdentityEd25519 bundle)
                          (pqpkbSignedPreKey bundle)
                          (pqpkbSPKSignature bundle))
    then Nothing
    else Just (initSession aliceIK bundle ekSecret mlkemRand)

-- | Compute the PQXDH session after SPK verification succeeds.
initSession :: IdentityKey -> PQPreKeyBundle -> ByteString -> ByteString -> PQXDHResult
initSession aliceIK bundle ekSecret mlkemRand =
    let -- Generate ephemeral keypair
        !ek   = generateKeyPair ekSecret
        -- Compute DH values
        !dh1  = x25519 (ikX25519Secret aliceIK) (pqpkbSignedPreKey bundle)
        !dh2  = x25519 (kpSecret ek)             (pqpkbIdentityKey bundle)
        !dh3  = x25519 (kpSecret ek)             (pqpkbSignedPreKey bundle)
        !mDh4 = fmap (x25519 (kpSecret ek)) (pqpkbOneTimePreKey bundle)
        -- ML-KEM encapsulation
        !(pqCt, pqSS) = mlkemEncaps (pqpkbPQPreKey bundle) mlkemRand
        -- Derive master secret (with identity binding)
        !masterSecret = derivePQSecret dh1 dh2 dh3 mDh4 pqSS
                            (ikX25519Public aliceIK) (pqpkbIdentityKey bundle)
    in PQXDHResult
        { pqxdhSharedSecret = masterSecret
        , pqxdhEphemeralKey = kpPublic ek
        , pqxdhPQCiphertext = pqCt
        , pqxdhUsedOPK      = pqpkbOneTimePreKey bundle
        }

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
             -> ByteString        -- ^ 32-byte shared secret
pqxdhRespond bobIK spkSecret mOPKSecret pqDK aliceIKPub aliceEKPub pqCt =
    let -- Mirror the DH computations from Alice's perspective
        !dh1  = x25519 spkSecret              aliceIKPub
        !dh2  = x25519 (ikX25519Secret bobIK) aliceEKPub
        !dh3  = x25519 spkSecret              aliceEKPub
        !mDh4 = fmap (\opkSec -> x25519 opkSec aliceEKPub) mOPKSecret
        -- ML-KEM decapsulation
        !pqSS = mlkemDecaps pqDK pqCt
    in derivePQSecret dh1 dh2 dh3 mDh4 pqSS aliceIKPub (ikX25519Public bobIK)

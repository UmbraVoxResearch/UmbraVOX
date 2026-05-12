-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "SIGNAL-001" #-} X3DH (Extended Triple Diffie-Hellman)
--
-- Classical Signal X3DH key agreement protocol (Phase 1).
-- See: attic/doc-legacy-2026-04-28/03-cryptography.md
module UmbraVox.Crypto.Signal.X3DH
    ( KeyPair(..)
    , IdentityKey(..)
    , PreKeyBundle(..)
    , X3DHResult(..)
    , generateKeyPair
    , generateIdentityKey
    , signPreKey
    , x3dhInitiate
    , x3dhRespond
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.Ed25519 (ed25519Sign, ed25519Verify, ed25519PublicKey)
import UmbraVox.Crypto.HKDF (hkdf)

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

-- | An X25519 keypair (secret + public).
data KeyPair = KeyPair
    { kpSecret :: !ByteString  -- ^ 32-byte X25519 secret key
    , kpPublic :: !ByteString  -- ^ 32-byte X25519 public key
    }

-- | An identity key bundle: Ed25519 keypair for signing, X25519 keypair for DH.
--
-- SecureBytes migration (M15.3.3): both secret fields hold long-lived identity
-- key material and should eventually be wrapped in
-- 'UmbraVox.Crypto.SecureBytes.SecureBytes' so that their memory is zeroed
-- when the bundle is discarded:
--
--   * 'ikEd25519Secret' — Ed25519 secret scalar used in signing.
--   * 'ikX25519Secret'  — X25519 secret scalar used in DH (x3dhInitiate / x3dhRespond).
--
-- These fields are also persisted via 'UmbraVox.Crypto.KeyStore' (which already
-- wraps the derived wrapping key in SecureBytes as of M15.3.2) and are
-- referenced throughout the Signal protocol — coordinate the migration with
-- PQXDH, DoubleRatchet, and KeyStore before changing field types.
data IdentityKey = IdentityKey
    { ikEd25519Secret :: !ByteString  -- ^ 32-byte Ed25519 secret key
                                      -- TODO(M15.3): wrap in SecureBytes (see module note above)
    , ikEd25519Public :: !ByteString  -- ^ 32-byte Ed25519 public key
    , ikX25519Secret  :: !ByteString  -- ^ 32-byte X25519 secret key
                                      -- TODO(M15.3): wrap in SecureBytes (see module note above)
    , ikX25519Public  :: !ByteString  -- ^ 32-byte X25519 public key
    }

-- | A prekey bundle published by the responder (Bob).
data PreKeyBundle = PreKeyBundle
    { pkbIdentityKey    :: !ByteString        -- ^ Bob's X25519 identity public key
    , pkbSignedPreKey   :: !ByteString        -- ^ Bob's SPK public key
    , pkbSPKSignature   :: !ByteString        -- ^ Ed25519 signature over SPK
    , pkbIdentityEd25519 :: !ByteString       -- ^ Bob's Ed25519 identity public key
    , pkbOneTimePreKey  :: !(Maybe ByteString) -- ^ Bob's OPK public key (if available)
    }

-- | Result of the X3DH initiation (Alice's side).
data X3DHResult = X3DHResult
    { x3dhSharedSecret :: !ByteString        -- ^ 32-byte master secret
    , x3dhEphemeralKey :: !ByteString        -- ^ Alice's ephemeral public key
    , x3dhUsedOPK      :: !(Maybe ByteString) -- ^ Which OPK was consumed
    }

------------------------------------------------------------------------
-- Key generation
------------------------------------------------------------------------

-- | Generate an X25519 keypair from a 32-byte secret.
-- Basepoint multiplication cannot produce all-zero for any non-zero secret.
generateKeyPair :: ByteString -> KeyPair
generateKeyPair secret =
    case x25519 secret x25519Basepoint of
        Just pub -> KeyPair { kpSecret = secret, kpPublic = pub }
        Nothing  -> error "generateKeyPair: x25519 basepoint returned all-zero (impossible)"

-- | Generate an identity key from two 32-byte secrets:
-- the first for Ed25519, the second for X25519.
generateIdentityKey :: ByteString -> ByteString -> IdentityKey
generateIdentityKey edSecret xSecret =
    case x25519 xSecret x25519Basepoint of
        Just xPub ->
            IdentityKey
                { ikEd25519Secret = edSecret
                , ikEd25519Public = ed25519PublicKey edSecret
                , ikX25519Secret  = xSecret
                , ikX25519Public  = xPub
                }
        Nothing -> error "generateIdentityKey: x25519 basepoint returned all-zero (impossible)"

------------------------------------------------------------------------
-- SPK signing
------------------------------------------------------------------------

-- | Sign a signed prekey's public key with the identity Ed25519 key.
signPreKey :: IdentityKey -> ByteString -> ByteString
signPreKey ik spkPub = ed25519Sign (ikEd25519Secret ik) spkPub

------------------------------------------------------------------------
-- Protocol info string
------------------------------------------------------------------------

x3dhInfo :: ByteString
x3dhInfo = BS.pack (map (fromIntegral . fromEnum) "UmbraVox_X3DH_v1")

------------------------------------------------------------------------
-- Shared secret derivation (common to initiator and responder)
------------------------------------------------------------------------

-- | Derive the master secret from DH outputs.
-- ikm = 0xFF*32 || dh1 || dh2 || dh3 || [dh4]
-- salt = 0x00*32
-- info = "UmbraVox_X3DH_v1" || IK_A_pub || IK_B_pub (identity binding)
-- output = 32 bytes
deriveSecret :: ByteString  -- ^ dh1
             -> ByteString  -- ^ dh2
             -> ByteString  -- ^ dh3
             -> Maybe ByteString  -- ^ dh4 (optional)
             -> ByteString  -- ^ Initiator (Alice) X25519 identity public key
             -> ByteString  -- ^ Responder (Bob) X25519 identity public key
             -> ByteString
deriveSecret !dh1 !dh2 !dh3 !mDh4 !aliceIKPub !bobIKPub =
    let !pad = BS.replicate 32 0xff
        !salt = BS.replicate 32 0x00
        !ikm = BS.concat $ [pad, dh1, dh2, dh3] ++ maybe [] (:[]) mDh4
        !info = x3dhInfo <> aliceIKPub <> bobIKPub
    in hkdf salt ikm info 32

------------------------------------------------------------------------
-- X3DH Initiation (Alice's side)
------------------------------------------------------------------------

-- | Alice initiates X3DH to Bob.
--
-- Takes Alice's identity key, Bob's prekey bundle, and a 32-byte
-- ephemeral secret for generating the ephemeral keypair.
-- Returns the shared secret, ephemeral public key, and used OPK.
x3dhInitiate :: IdentityKey -> PreKeyBundle -> ByteString -> Maybe X3DHResult
x3dhInitiate aliceIK bundle ekSecret =
    -- Step 1: Verify SPK signature
    if not (ed25519Verify (pkbIdentityEd25519 bundle) (pkbSignedPreKey bundle) (pkbSPKSignature bundle))
    then Nothing
    else
        let -- Step 2: Generate ephemeral keypair
            !ek = generateKeyPair ekSecret
        -- Step 3: Compute DH values; abort if any yields all-zero (low-order point)
        in do
            !dh1  <- x25519 (ikX25519Secret aliceIK) (pkbSignedPreKey bundle)
            !dh2  <- x25519 (kpSecret ek)             (pkbIdentityKey bundle)
            !dh3  <- x25519 (kpSecret ek)             (pkbSignedPreKey bundle)
            !mDh4 <- case pkbOneTimePreKey bundle of
                         Nothing  -> Just Nothing
                         Just opk -> fmap Just (x25519 (kpSecret ek) opk)
            let -- Step 4: Derive master secret (with identity binding)
                !masterSecret = deriveSecret dh1 dh2 dh3 mDh4
                                    (ikX25519Public aliceIK) (pkbIdentityKey bundle)
            Just X3DHResult
                { x3dhSharedSecret = masterSecret
                , x3dhEphemeralKey = kpPublic ek
                , x3dhUsedOPK      = pkbOneTimePreKey bundle
                }

------------------------------------------------------------------------
-- X3DH Response (Bob's side)
------------------------------------------------------------------------

-- | Bob responds to Alice's X3DH initiation.
--
-- Takes Bob's identity key, Bob's SPK secret, maybe OPK secret,
-- Alice's X25519 identity public key, and Alice's ephemeral public key.
-- Returns the same shared secret Alice derived.
x3dhRespond :: IdentityKey       -- ^ Bob's identity key
            -> ByteString        -- ^ Bob's SPK secret key
            -> Maybe ByteString  -- ^ Bob's OPK secret key (if used)
            -> ByteString        -- ^ Alice's X25519 identity public key
            -> ByteString        -- ^ Alice's ephemeral public key
            -> Maybe ByteString  -- ^ Shared secret (32 bytes), or Nothing on low-order point
x3dhRespond bobIK spkSecret mOPKSecret aliceIKPub aliceEKPub = do
    -- Mirror the DH computations from Alice's perspective
    -- Abort if any DH yields all-zero (low-order point attack)
    !dh1  <- x25519 spkSecret              aliceIKPub
    !dh2  <- x25519 (ikX25519Secret bobIK) aliceEKPub
    !dh3  <- x25519 spkSecret              aliceEKPub
    !mDh4 <- case mOPKSecret of
                 Nothing     -> Just Nothing
                 Just opkSec -> fmap Just (x25519 opkSec aliceEKPub)
    Just (deriveSecret dh1 dh2 dh3 mDh4 aliceIKPub (ikX25519Public bobIK))

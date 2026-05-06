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
data IdentityKey = IdentityKey
    { ikEd25519Secret :: !ByteString  -- ^ 32-byte Ed25519 secret key
    , ikEd25519Public :: !ByteString  -- ^ 32-byte Ed25519 public key
    , ikX25519Secret  :: !ByteString  -- ^ 32-byte X25519 secret key
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
generateKeyPair :: ByteString -> KeyPair
generateKeyPair secret =
    KeyPair
        { kpSecret = secret
        , kpPublic = x25519 secret x25519Basepoint
        }

-- | Generate an identity key from two 32-byte secrets:
-- the first for Ed25519, the second for X25519.
generateIdentityKey :: ByteString -> ByteString -> IdentityKey
generateIdentityKey edSecret xSecret =
    IdentityKey
        { ikEd25519Secret = edSecret
        , ikEd25519Public = ed25519PublicKey edSecret
        , ikX25519Secret  = xSecret
        , ikX25519Public  = x25519 xSecret x25519Basepoint
        }

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
            -- Step 3: Compute DH values
            !dh1 = x25519 (ikX25519Secret aliceIK) (pkbSignedPreKey bundle)
            !dh2 = x25519 (kpSecret ek)             (pkbIdentityKey bundle)
            !dh3 = x25519 (kpSecret ek)             (pkbSignedPreKey bundle)
            !mDh4 = fmap (x25519 (kpSecret ek)) (pkbOneTimePreKey bundle)
            -- Step 4: Derive master secret (with identity binding)
            !masterSecret = deriveSecret dh1 dh2 dh3 mDh4
                                (ikX25519Public aliceIK) (pkbIdentityKey bundle)
        in Just X3DHResult
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
            -> ByteString        -- ^ Shared secret (32 bytes)
x3dhRespond bobIK spkSecret mOPKSecret aliceIKPub aliceEKPub =
    let -- Mirror the DH computations from Alice's perspective
        !dh1 = x25519 spkSecret              aliceIKPub
        !dh2 = x25519 (ikX25519Secret bobIK) aliceEKPub
        !dh3 = x25519 spkSecret              aliceEKPub
        !mDh4 = fmap (\opkSec -> x25519 opkSec aliceEKPub) mOPKSecret
    in deriveSecret dh1 dh2 dh3 mDh4 aliceIKPub (ikX25519Public bobIK)

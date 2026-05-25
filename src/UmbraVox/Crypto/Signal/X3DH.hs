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
    -- * M23.2.1: OPK depletion protection
    , OPKPoolStatus(..)
    , minOPKPoolSize
    , checkOPKDepletion
    -- * M23.2.2: Prekey bundle freshness
    , maxBundleAge
    , isBundleFresh
    , x3dhInitiateWithTimestamp
    , x3dhRespondWithTimestamp
    ) where

import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32, Word64)

import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.Ed25519 (ed25519Sign, ed25519Verify, ed25519PublicKey)
import UmbraVox.Crypto.HKDF (hkdf)
import UmbraVox.Crypto.SecureBytes (SecureBytes, fromByteString, toByteString)

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

-- | An X25519 keypair (secret + public).
data KeyPair = KeyPair
    { kpSecret :: !SecureBytes  -- ^ 32-byte X25519 secret key (pinned, zeroed on free)
    , kpPublic :: !ByteString   -- ^ 32-byte X25519 public key
    }

-- | An identity key bundle: Ed25519 keypair for signing, X25519 keypair for DH.
--
-- SecureBytes migration (M15.3.3): completed.  Both secret fields are now
-- wrapped in 'UmbraVox.Crypto.SecureBytes.SecureBytes' so that their memory
-- is pinned, mlock'd, and zeroed when the bundle is discarded.
--
-- Callers that need the raw bytes for crypto operations should use
-- 'toByteString' (IO, creates a temporary GC-heap copy) or 'withSecureKey'.
data IdentityKey = IdentityKey
    { ikEd25519Secret :: !SecureBytes  -- ^ 32-byte Ed25519 secret key (pinned, zeroed on free)
    , ikEd25519Public :: !ByteString   -- ^ 32-byte Ed25519 public key
    , ikX25519Secret  :: !SecureBytes  -- ^ 32-byte X25519 secret key (pinned, zeroed on free)
    , ikX25519Public  :: !ByteString   -- ^ 32-byte X25519 public key
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
-- The secret is copied into a 'SecureBytes' buffer; the original 'ByteString'
-- argument is not zeroed (caller should avoid retaining it).
generateKeyPair :: ByteString -> IO KeyPair
generateKeyPair secret =
    case x25519 secret x25519Basepoint of
        Just pub -> do
            sbSecret <- fromByteString secret
            pure KeyPair { kpSecret = sbSecret, kpPublic = pub }
        Nothing  -> error "generateKeyPair: x25519 basepoint returned all-zero (impossible)"

-- | Generate an identity key from two 32-byte secrets:
-- the first for Ed25519, the second for X25519.
-- Both secrets are copied into 'SecureBytes' buffers; the original
-- 'ByteString' arguments are not zeroed (caller should avoid retaining them).
generateIdentityKey :: ByteString -> ByteString -> IO IdentityKey
generateIdentityKey edSecret xSecret =
    case x25519 xSecret x25519Basepoint of
        Just xPub -> do
            sbEdSecret <- fromByteString edSecret
            sbXSecret  <- fromByteString xSecret
            pure IdentityKey
                { ikEd25519Secret = sbEdSecret
                , ikEd25519Public = ed25519PublicKey edSecret
                , ikX25519Secret  = sbXSecret
                , ikX25519Public  = xPub
                }
        Nothing -> error "generateIdentityKey: x25519 basepoint returned all-zero (impossible)"

------------------------------------------------------------------------
-- SPK signing
------------------------------------------------------------------------

-- | Sign a signed prekey's public key with the identity Ed25519 key.
--
-- The signed message is @ikEd25519Public || spkPub@, binding the identity
-- key into the signature to prevent cross-identity SPK reuse (M23.3.1).
--
-- Requires IO to extract the Ed25519 secret from its 'SecureBytes' wrapper.
signPreKey :: IdentityKey -> ByteString -> IO ByteString
signPreKey ik spkPub = do
    edSec <- toByteString (ikEd25519Secret ik)
    pure $ ed25519Sign edSec (ikEd25519Public ik <> spkPub)

------------------------------------------------------------------------
-- Protocol info string
------------------------------------------------------------------------

x3dhInfo :: ByteString
x3dhInfo = BS.pack (map (fromIntegral . fromEnum) "UmbraVox_X3DH_v1")

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
-- Shared secret derivation (common to initiator and responder)
------------------------------------------------------------------------

-- | Derive the master secret from DH outputs.
-- ikm = 0xFF*32 || dh1 || dh2 || dh3 || [dh4]
-- salt = 0x00*32
-- info = "UmbraVox_X3DH_v1" || len(IK_A_pub) || IK_A_pub || len(IK_B_pub) || IK_B_pub
-- output = 32 bytes
--
-- M27.6.10: Each key in the info string is now preceded by its 4-byte
-- big-endian length to prevent concatenation collisions under protocol
-- evolution (e.g. if key sizes change in future versions).
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
        -- M27.6.10: length-prefixed keys in info string
        !info = x3dhInfo <> lenPrefix aliceIKPub <> lenPrefix bobIKPub
    in hkdf salt ikm info 32

------------------------------------------------------------------------
-- X3DH Initiation (Alice's side)
------------------------------------------------------------------------

-- | Alice initiates X3DH to Bob.
--
-- Takes Alice's identity key, Bob's prekey bundle, and a 32-byte
-- ephemeral secret for generating the ephemeral keypair.
-- Returns the shared secret, ephemeral public key, and used OPK.
--
-- Requires IO to extract secrets from their 'SecureBytes' wrappers.
x3dhInitiate :: IdentityKey -> PreKeyBundle -> ByteString -> IO (Maybe X3DHResult)
x3dhInitiate aliceIK bundle ekSecret =
    -- Step 1: Verify SPK signature (M23.3.1: message = ikEd25519 || spkPub)
    if not (ed25519Verify (pkbIdentityEd25519 bundle)
                          (pkbIdentityEd25519 bundle <> pkbSignedPreKey bundle)
                          (pkbSPKSignature bundle))
    then pure Nothing
    else do
        -- Step 2: Generate ephemeral keypair
        ek <- generateKeyPair ekSecret
        -- Extract secrets from SecureBytes for DH operations
        xSecret  <- toByteString (ikX25519Secret aliceIK)
        ekSecret' <- toByteString (kpSecret ek)
        -- Step 3: Compute DH values; abort if any yields all-zero (low-order point)
        pure $ do
            !dh1  <- x25519 xSecret   (pkbSignedPreKey bundle)
            !dh2  <- x25519 ekSecret' (pkbIdentityKey bundle)
            !dh3  <- x25519 ekSecret' (pkbSignedPreKey bundle)
            !mDh4 <- case pkbOneTimePreKey bundle of
                         Nothing  -> Just Nothing
                         Just opk -> fmap Just (x25519 ekSecret' opk)
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
--
-- Requires IO to extract the X25519 secret from its 'SecureBytes' wrapper.
x3dhRespond :: IdentityKey       -- ^ Bob's identity key
            -> ByteString        -- ^ Bob's SPK secret key
            -> Maybe ByteString  -- ^ Bob's OPK secret key (if used)
            -> ByteString        -- ^ Alice's X25519 identity public key
            -> ByteString        -- ^ Alice's ephemeral public key
            -> IO (Maybe ByteString)  -- ^ Shared secret (32 bytes), or Nothing on low-order point
x3dhRespond bobIK spkSecret mOPKSecret aliceIKPub aliceEKPub = do
    xSecret <- toByteString (ikX25519Secret bobIK)
    -- Mirror the DH computations from Alice's perspective
    -- Abort if any DH yields all-zero (low-order point attack)
    pure $ do
        !dh1  <- x25519 spkSecret aliceIKPub
        !dh2  <- x25519 xSecret   aliceEKPub
        !dh3  <- x25519 spkSecret aliceEKPub
        !mDh4 <- case mOPKSecret of
                     Nothing     -> Just Nothing
                     Just opkSec -> fmap Just (x25519 opkSec aliceEKPub)
        Just (deriveSecret dh1 dh2 dh3 mDh4 aliceIKPub (ikX25519Public bobIK))

------------------------------------------------------------------------
-- M23.2.1: OPK depletion protection
------------------------------------------------------------------------

-- | Finding:     M23.2.1 -- OPK pool can be exhausted by an attacker
--                requesting many sessions in rapid succession.
-- Vulnerability: Without monitoring, a malicious peer can silently deplete
--                all one-time prekeys, forcing all subsequent sessions to
--                fall back to 3-DH (no OPK), which weakens forward secrecy
--                for the initial message.
-- Fix:           Added 'minOPKPoolSize' threshold and 'checkOPKDepletion'
--                to detect when the OPK pool is critically low after
--                consuming a key.  Callers (server-side OPK management)
--                should check the returned 'OPKPoolStatus' and replenish
--                the pool or rate-limit bundle requests when depleted.
-- Verified:      testOPKDepletionDetection (Test.Crypto.Signal.X3DH):
--                pool sizes at and below the threshold are flagged;
--                pool sizes above the threshold are not.

-- | Status of the OPK pool after consuming a one-time prekey.
data OPKPoolStatus
    = OPKPoolHealthy   -- ^ Pool size is above 'minOPKPoolSize'
    | OPKPoolDepleted  -- ^ Pool size is at or below 'minOPKPoolSize'; replenish urgently
    deriving (Eq, Show)

-- | Minimum acceptable OPK pool size.  When the pool drops to this level
-- or below after consuming an OPK, 'checkOPKDepletion' returns
-- 'OPKPoolDepleted' to signal that new OPKs should be generated and
-- uploaded to the server.
minOPKPoolSize :: Int
minOPKPoolSize = 5

-- | Check whether the OPK pool is critically low after consuming one key.
--
-- @remainingOPKs@ is the number of OPKs still available in the pool
-- /after/ the current OPK has been consumed.  Returns 'OPKPoolDepleted'
-- when the pool is at or below 'minOPKPoolSize'.
--
-- Usage (responder side):
--
-- @
-- let status = checkOPKDepletion (length remainingKeys)
-- case status of
--     OPKPoolDepleted -> replenishOPKs  -- upload fresh OPKs to server
--     OPKPoolHealthy  -> pure ()
-- @
checkOPKDepletion :: Int -> OPKPoolStatus
checkOPKDepletion remainingOPKs
    | remainingOPKs <= minOPKPoolSize = OPKPoolDepleted
    | otherwise                       = OPKPoolHealthy

------------------------------------------------------------------------
-- M23.2.2: Prekey bundle freshness check
------------------------------------------------------------------------

-- | Finding:     M23.2.2 -- Prekey bundles have no expiration.
-- Vulnerability: An attacker who captures an old prekey bundle can replay
--                it indefinitely.  Even if the underlying keys have been
--                rotated server-side, a stale bundle still produces a
--                valid X3DH handshake, and the responder may not notice
--                that a long-retired SPK was used.  Replaying old bundles
--                also undermines forward secrecy guarantees that depend on
--                regular SPK rotation.
-- Fix:           Added 'isBundleFresh' to reject bundles older than
--                'maxBundleAge' (30 days), and 'x3dhInitiateWithTimestamp' /
--                'x3dhRespondWithTimestamp' which bind the bundle timestamp
--                into the HKDF info string.  This ensures that even if an
--                old bundle's keys are somehow still valid, the derived
--                session keys differ from those of a replayed bundle,
--                preventing silent key reuse.
-- Verified:      testBundleFreshness, testBundleFreshnessBinding
--                (Test.Crypto.Signal.X3DH): fresh bundles are accepted,
--                stale bundles are rejected, and different timestamps
--                produce different shared secrets.

-- | Maximum age of a prekey bundle in seconds (30 days).
maxBundleAge :: Word64
maxBundleAge = 86400 * 30

-- | Check whether a prekey bundle is fresh enough for use.
--
-- Returns 'True' if the bundle timestamp is within 'maxBundleAge' seconds
-- of the current time.  Both arguments are POSIX timestamps (seconds since
-- epoch).  Handles clock skew gracefully: if @bundleTs > now@ (bundle
-- appears to be from the future), the bundle is accepted since it cannot
-- be stale.
-- | Maximum allowed future clock skew in seconds (1 hour).
--
-- Finding:     M27.6.23 — 'isBundleFresh' unconditionally accepted any
--              bundle with a timestamp in the future (@bundleTs > now@).
-- Vulnerability: An attacker can craft a bundle with a timestamp far in
--              the future (e.g. year 2099), causing it to be accepted
--              indefinitely — effectively bypassing the staleness check.
-- Fix:         Reject bundles whose timestamp is more than 1 hour in the
--              future.  A 1-hour tolerance accommodates reasonable clock
--              skew between peers.
-- Verified:    Bundles > 1 hour in the future return False.
maxFutureSkew :: Word64
maxFutureSkew = 3600

isBundleFresh :: Word64  -- ^ Current POSIX time (seconds)
              -> Word64  -- ^ Bundle creation timestamp (seconds)
              -> Bool
isBundleFresh now bundleTs
    | bundleTs > now + maxFutureSkew = False  -- too far in the future
    | bundleTs > now = True   -- future timestamp within skew tolerance
    | otherwise      = now - bundleTs < maxBundleAge

-- | Encode a 'Word64' timestamp as an 8-byte big-endian 'ByteString'.
encodeTimestamp :: Word64 -> ByteString
encodeTimestamp w = BS.pack
    [ fromIntegral (w `shiftR` 56)
    , fromIntegral (w `shiftR` 48)
    , fromIntegral (w `shiftR` 40)
    , fromIntegral (w `shiftR` 32)
    , fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR`  8)
    , fromIntegral  w
    ]

-- | Derive the master secret with a bundle timestamp bound into the HKDF
-- info string.  This is identical to 'deriveSecret' except the info becomes:
--
-- @"UmbraVox_X3DH_v1" || IK_A_pub || IK_B_pub || BE64(bundleTimestamp)@
--
-- Binding the timestamp ensures that replayed bundles (even with the same
-- keys) produce different session keys, preventing silent key reuse.
deriveSecretWithTimestamp :: ByteString      -- ^ dh1
                         -> ByteString      -- ^ dh2
                         -> ByteString      -- ^ dh3
                         -> Maybe ByteString -- ^ dh4 (optional)
                         -> ByteString      -- ^ Initiator (Alice) X25519 identity public key
                         -> ByteString      -- ^ Responder (Bob) X25519 identity public key
                         -> Word64          -- ^ Bundle creation timestamp
                         -> ByteString
deriveSecretWithTimestamp !dh1 !dh2 !dh3 !mDh4 !aliceIKPub !bobIKPub !bundleTs =
    let !pad = BS.replicate 32 0xff
        !salt = BS.replicate 32 0x00
        !ikm = BS.concat $ [pad, dh1, dh2, dh3] ++ maybe [] (:[]) mDh4
        !info = x3dhInfo <> lenPrefix aliceIKPub <> lenPrefix bobIKPub <> encodeTimestamp bundleTs
    in hkdf salt ikm info 32

-- | Alice initiates X3DH with bundle freshness validation (M23.2.2).
--
-- Like 'x3dhInitiate', but additionally:
--
-- 1. Rejects bundles older than 'maxBundleAge' (returns 'Nothing').
-- 2. Binds the bundle timestamp into the HKDF info string so replayed
--    bundles produce different keys.
--
-- Both sides must use the timestamp-aware variants for the shared secrets
-- to match.
x3dhInitiateWithTimestamp :: IdentityKey
                          -> PreKeyBundle
                          -> ByteString   -- ^ 32-byte ephemeral secret
                          -> Word64       -- ^ Current POSIX time (seconds)
                          -> Word64       -- ^ Bundle creation timestamp (seconds)
                          -> IO (Maybe X3DHResult)
x3dhInitiateWithTimestamp aliceIK bundle ekSecret now bundleTs
    -- Step 0: Reject stale bundles
    | not (isBundleFresh now bundleTs) = pure Nothing
    -- Step 1: Verify SPK signature (M23.3.1: message = ikEd25519 || spkPub)
    | not (ed25519Verify (pkbIdentityEd25519 bundle)
                         (pkbIdentityEd25519 bundle <> pkbSignedPreKey bundle)
                         (pkbSPKSignature bundle)) = pure Nothing
    | otherwise = do
        ek <- generateKeyPair ekSecret
        xSecret  <- toByteString (ikX25519Secret aliceIK)
        ekSecret' <- toByteString (kpSecret ek)
        pure $ do
            !dh1  <- x25519 xSecret   (pkbSignedPreKey bundle)
            !dh2  <- x25519 ekSecret' (pkbIdentityKey bundle)
            !dh3  <- x25519 ekSecret' (pkbSignedPreKey bundle)
            !mDh4 <- case pkbOneTimePreKey bundle of
                         Nothing  -> Just Nothing
                         Just opk -> fmap Just (x25519 ekSecret' opk)
            let !masterSecret = deriveSecretWithTimestamp dh1 dh2 dh3 mDh4
                                    (ikX25519Public aliceIK) (pkbIdentityKey bundle)
                                    bundleTs
            Just X3DHResult
                { x3dhSharedSecret = masterSecret
                , x3dhEphemeralKey = kpPublic ek
                , x3dhUsedOPK      = pkbOneTimePreKey bundle
                }

-- | Bob responds to Alice's X3DH initiation with timestamp binding (M23.2.2).
--
-- Like 'x3dhRespond', but binds the bundle timestamp into the HKDF info
-- string.  The @bundleTs@ must match the timestamp Alice used in
-- 'x3dhInitiateWithTimestamp' for the shared secrets to agree.
x3dhRespondWithTimestamp :: IdentityKey       -- ^ Bob's identity key
                         -> ByteString        -- ^ Bob's SPK secret key
                         -> Maybe ByteString  -- ^ Bob's OPK secret key (if used)
                         -> ByteString        -- ^ Alice's X25519 identity public key
                         -> ByteString        -- ^ Alice's ephemeral public key
                         -> Word64            -- ^ Bundle creation timestamp (seconds)
                         -> IO (Maybe ByteString)  -- ^ Shared secret (32 bytes), or Nothing
x3dhRespondWithTimestamp bobIK spkSecret mOPKSecret aliceIKPub aliceEKPub bundleTs = do
    xSecret <- toByteString (ikX25519Secret bobIK)
    pure $ do
        !dh1  <- x25519 spkSecret aliceIKPub
        !dh2  <- x25519 xSecret   aliceEKPub
        !dh3  <- x25519 spkSecret aliceEKPub
        !mDh4 <- case mOPKSecret of
                     Nothing     -> Just Nothing
                     Just opkSec -> fmap Just (x25519 opkSec aliceEKPub)
        Just (deriveSecretWithTimestamp dh1 dh2 dh3 mDh4 aliceIKPub (ikX25519Public bobIK) bundleTs)

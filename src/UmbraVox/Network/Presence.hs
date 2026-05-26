-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "M23.1.1g" #-} DHT presence integration for stealth addressing.
--
-- Presence records use stealth-derived keys so the DHT never stores or
-- exposes raw identity hashes.  A recipient scans the DHT for keys
-- matching their scan secret; non-recipients see only opaque 32-byte
-- tokens that rotate every epoch.
--
-- Key derivation:
--   presenceKey = HKDF-SHA-512(scanKey, SHA-256(identityPub),
--                               "UmbraVox_Presence_v1" || BE64(epoch))
--
-- Record layout (serialized):
--   [stealthPubKey:32][contactInfo:var][timestamp:8][signature:64][ttl:4]
--
-- See: doc/ENCRYPTED-ENVELOPE-DESIGN.md Section 7.7
--      doc/DHT-NETWORK-PLAN.md
module UmbraVox.Network.Presence
    ( PresenceRecord(..)
    , derivePresenceKey
    , createPresenceRecord
    , verifyPresenceRecord
    , serializePresence
    , deserializePresence
    ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32, Word64)

import UmbraVox.Crypto.Ed25519 (ed25519Sign, ed25519Verify, ed25519PublicKey)
import UmbraVox.Crypto.HKDF (hkdf)
import UmbraVox.Crypto.SHA256 (sha256)
import UmbraVox.Protocol.Encoding (putWord32BE, getWord32BE, putWord64BE)

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

-- | A presence record published to the DHT.
--
-- The DHT stores these keyed by 'prStealthPubKey', which is a
-- stealth-derived 32-byte token that rotates every epoch.  Only the
-- intended recipient can derive the same key from their scan secret
-- and thus locate the record.
data PresenceRecord = PresenceRecord
    { prStealthPubKey :: !ByteString  -- ^ 32 bytes: stealth-derived presence key
    , prContactInfo   :: !ByteString  -- ^ Encrypted: address + port + capabilities
    , prTimestamp     :: !Word64      -- ^ POSIX seconds when record was created
    , prSignature    :: !ByteString   -- ^ 64 bytes: Ed25519 signature over record
    , prTTL          :: !Word32       -- ^ Seconds until expiry (default 86400 = 24h)
    } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | Default TTL for presence records: 24 hours.
defaultTTL :: Word32
defaultTTL = 86400

-- | HKDF salt: 32 zero bytes (matches stealth address convention).
presenceSalt :: ByteString
presenceSalt = BS.replicate 32 0

-- | Info prefix for presence key derivation.
presenceInfoPrefix :: ByteString
presenceInfoPrefix = "UmbraVox_Presence_v1"

------------------------------------------------------------------------
-- Presence key derivation
------------------------------------------------------------------------

-- | Derive a stealth presence key from a scan key, identity hash, and epoch.
--
-- The epoch parameter allows key rotation: each epoch produces a
-- different presence key, preventing long-term linkability.  Callers
-- typically set epoch = @floor(posixTime / ttl)@.
--
-- @
-- presenceKey = HKDF-SHA-512(scanKey, SHA-256(identityPub),
--                             "UmbraVox_Presence_v1" || BE64(epoch))
-- @
derivePresenceKey :: ByteString  -- ^ Scan key (32 bytes, X25519 public or secret)
                  -> ByteString  -- ^ Identity public key (any length; SHA-256 hashed)
                  -> Word64      -- ^ Epoch number
                  -> ByteString  -- ^ 32-byte stealth presence key
derivePresenceKey scanKey identityPub epoch =
    let !identityHash = sha256 identityPub
        !info = presenceInfoPrefix <> putWord64BE epoch
    in hkdf presenceSalt (scanKey <> identityHash) info 32

------------------------------------------------------------------------
-- Record creation
------------------------------------------------------------------------

-- | Create a signed presence record.
--
-- The record is signed with the provided Ed25519 secret key so that
-- peers can verify authenticity without learning the signer's long-term
-- identity (they verify against 'prStealthPubKey', not the identity key).
--
-- Returns 'Left' with a description if the Ed25519 implementation produces
-- an unexpected output length (signature != 64 bytes or public key != 32 bytes).
-- Under normal operation this branch is unreachable; exposing it as 'Either'
-- allows callers to handle it without a process crash.
createPresenceRecord :: ByteString  -- ^ Ed25519 secret key (32-byte seed)
                     -> ByteString  -- ^ Scan key (32 bytes)
                     -> ByteString  -- ^ Identity public key
                     -> ByteString  -- ^ Contact info (pre-encrypted by caller)
                     -> IO (Either String PresenceRecord)
createPresenceRecord edSecret scanKey identityPub contactInfo = do
    now <- floor <$> getPOSIXTime :: IO Word64
    let !epoch = now `div` fromIntegral defaultTTL
        !stealthKey = derivePresenceKey scanKey identityPub epoch
        -- Build the signable payload: stealthKey || contactInfo || timestamp || ttl
        !payload = stealthKey
                <> contactInfo
                <> putWord64BE now
                <> putWord32BE defaultTTL
        !sig = ed25519Sign edSecret payload
        !pubKey = ed25519PublicKey edSecret
    -- Sanity: signature must be 64 bytes, public key must be 32 bytes
    if BS.length sig /= 64 || BS.length pubKey /= 32
        then return (Left "createPresenceRecord: Ed25519 produced invalid signature/key")
        else return (Right PresenceRecord
            { prStealthPubKey = stealthKey
            , prContactInfo   = contactInfo
            , prTimestamp     = now
            , prSignature    = sig
            , prTTL          = defaultTTL
            })

------------------------------------------------------------------------
-- Record verification
------------------------------------------------------------------------

-- | Verify a presence record's Ed25519 signature.
--
-- The verifier must supply the public key corresponding to the secret
-- that signed the record (obtained out-of-band or from a prior
-- handshake).  This prevents the DHT from being polluted with forged
-- presence records.
verifyPresenceRecord :: ByteString      -- ^ Ed25519 public key of the signer
                     -> PresenceRecord
                     -> Bool
verifyPresenceRecord pubKey pr =
    let !payload = prStealthPubKey pr
                <> prContactInfo pr
                <> putWord64BE (prTimestamp pr)
                <> putWord32BE (prTTL pr)
    in BS.length (prSignature pr) == 64
       && BS.length pubKey == 32
       && ed25519Verify pubKey payload (prSignature pr)

------------------------------------------------------------------------
-- Serialization
------------------------------------------------------------------------

-- | Serialize a presence record to bytes.
--
-- Format:
--   [stealthPubKey:32][contactInfoLen:4][contactInfo:N][timestamp:8][signature:64][ttl:4]
--
-- Total overhead: 32 + 4 + 8 + 64 + 4 = 112 bytes + contactInfo length.
serializePresence :: PresenceRecord -> ByteString
serializePresence pr =
    prStealthPubKey pr
    <> putWord32BE (fromIntegral (BS.length (prContactInfo pr)))
    <> prContactInfo pr
    <> putWord64BE (prTimestamp pr)
    <> prSignature pr
    <> putWord32BE (prTTL pr)

-- | Deserialize a presence record from bytes.
--
-- Returns 'Nothing' on malformed input (too short, truncated contact
-- info, wrong signature length).
deserializePresence :: ByteString -> Maybe PresenceRecord
deserializePresence bs
    | BS.length bs < 112 = Nothing  -- minimum: 32+4+0+8+64+4
    | otherwise =
        let !stealthKey = BS.take 32 bs
            !ciLenBS    = BS.take 4 (BS.drop 32 bs)
            !ciLen      = fromIntegral (getWord32BE ciLenBS) :: Int
            !remaining  = BS.length bs - 36
        in if ciLen < 0 || ciLen > remaining - 76  -- need 8+64+4=76 after contactInfo
            then Nothing
            else
                let !contactInfo = BS.take ciLen (BS.drop 36 bs)
                    !tsOffset   = 36 + ciLen
                    !timestamp  = getWord64BE (BS.drop tsOffset bs)
                    !sig        = BS.take 64 (BS.drop (tsOffset + 8) bs)
                    !ttl        = getWord32BE (BS.drop (tsOffset + 72) bs)
                in Just PresenceRecord
                    { prStealthPubKey = stealthKey
                    , prContactInfo   = contactInfo
                    , prTimestamp     = timestamp
                    , prSignature    = sig
                    , prTTL          = ttl
                    }

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- | Decode 8 big-endian bytes to a Word64.
-- Returns 0 if the input is shorter than 8 bytes.
getWord64BE :: ByteString -> Word64
getWord64BE !bs
    | BS.length bs < 8 = 0
    | otherwise =
        (fromIntegral (BS.index bs 0) `shiftL` 56) .|.
        (fromIntegral (BS.index bs 1) `shiftL` 48) .|.
        (fromIntegral (BS.index bs 2) `shiftL` 40) .|.
        (fromIntegral (BS.index bs 3) `shiftL` 32) .|.
        (fromIntegral (BS.index bs 4) `shiftL` 24) .|.
        (fromIntegral (BS.index bs 5) `shiftL` 16) .|.
        (fromIntegral (BS.index bs 6) `shiftL`  8) .|.
         fromIntegral (BS.index bs 7)

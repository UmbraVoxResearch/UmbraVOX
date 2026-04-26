-- | {-# REQ "HARDENING-03" #-} Dual-Key Stealth Address Protocol (DKSAP)
--
-- Implements stealth addresses per doc/hardening/03-stealth-addresses.md.
-- Each recipient publishes a scan key (X25519) and spend key (Ed25519).
-- Senders derive one-time addresses that only the recipient can identify
-- and spend from.
--
-- Pure Haskell reference implementation. NOT constant-time.
-- Production builds use FFI to constant-time C (see doc/03-cryptography.md).
module UmbraVox.Crypto.StealthAddress
    ( StealthKeys(..)
    , StealthAddress(..)
    , generateStealthKeys
    , deriveStealthAddress
    , scanForPayment
    , viewTag
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.Ed25519
    ( ExtPoint, basepoint, pointAdd, scalarMul
    , encodePoint, decodePoint, groupL
    , decodeLE, encodeLEn, clampScalar
    , ed25519PublicKey
    )
import UmbraVox.Crypto.HKDF (hkdf)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.SHA512 (sha512)

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

-- | Stealth meta-address: scan keypair (X25519) + spend keypair (Ed25519).
data StealthKeys = StealthKeys
    { skScanSecret  :: !ByteString  -- ^ 32 bytes, X25519 secret
    , skScanPublic  :: !ByteString  -- ^ 32 bytes, X25519 public
    , skSpendSecret :: !ByteString  -- ^ 32 bytes, Ed25519 secret (seed)
    , skSpendPublic :: !ByteString  -- ^ 32 bytes, Ed25519 public
    } deriving stock (Show)

-- | A derived one-time stealth address with ephemeral key and view tag.
data StealthAddress = StealthAddress
    { saAddress    :: !ByteString  -- ^ 32 bytes, one-time Ed25519 public key
    , saEphemeral  :: !ByteString  -- ^ 32 bytes, ephemeral X25519 public key R
    , saViewTag    :: !Word8       -- ^ First byte of shared secret (fast filter)
    } deriving stock (Show)

------------------------------------------------------------------------
-- HKDF domain separation constants (Section 13 of spec)
------------------------------------------------------------------------

-- | Salt: 32 zero bytes, per spec.
hkdfSalt :: ByteString
hkdfSalt = BS.replicate 32 0

-- | Info string for view tag derivation.
viewTagInfo :: ByteString
viewTagInfo = "UmbraVox_ViewTag_v2"

-- | Info string for stealth scalar derivation.
stealthKeyInfo :: ByteString
stealthKeyInfo = "UmbraVox_StealthKey_v1"

------------------------------------------------------------------------
-- Key generation
------------------------------------------------------------------------

-- | Generate a fresh stealth meta-address (scan + spend keypairs).
--
-- The scan key is an X25519 keypair for Diffie-Hellman scanning.
-- The spend key is an Ed25519 keypair for signing transactions.
generateStealthKeys :: IO StealthKeys
generateStealthKeys = do
    scanSecret  <- randomBytes 32
    spendSecret <- randomBytes 32
    let !scanPublic  = x25519 scanSecret x25519Basepoint
        !spendPublic = ed25519PublicKey spendSecret
    return StealthKeys
        { skScanSecret  = scanSecret
        , skScanPublic  = scanPublic
        , skSpendSecret = spendSecret
        , skSpendPublic = spendPublic
        }

------------------------------------------------------------------------
-- Stealth address derivation (sender side)
------------------------------------------------------------------------

-- | Derive a one-time stealth address for a recipient.
--
-- Given the recipient's scan public key (X25519) and spend public key
-- (Ed25519), generates an ephemeral keypair and computes the stealth
-- address per Section 3.1 of the spec.
deriveStealthAddress :: ByteString   -- ^ Recipient scan public key (32 bytes)
                     -> ByteString   -- ^ Recipient spend public key (32 bytes)
                     -> IO StealthAddress
deriveStealthAddress scanPub spendPub = do
    -- Step 1-2: Generate ephemeral keypair
    ephSecret <- randomBytes 32
    let !ephPublic = x25519 ephSecret x25519Basepoint
    -- Step 3: Shared secret via ECDH
    let !sharedSecret = x25519 ephSecret scanPub
    -- Steps 4-6: Derive view tag and stealth address
    let !sa = computeStealthAddress sharedSecret spendPub ephPublic
    return sa

-- | Compute stealth address from shared secret, spend pubkey, and ephemeral R.
-- Factored out to keep cyclomatic complexity low.
computeStealthAddress :: ByteString -> ByteString -> ByteString -> StealthAddress
computeStealthAddress sharedSecret spendPub ephPublic =
    let -- Step 4: View tag
        !vt = viewTag (deriveViewTagBytes sharedSecret)
        -- Step 5: Stealth scalar
        !stealthScalar = hkdf hkdfSalt sharedSecret stealthKeyInfo 32
        !s = decodeLE stealthScalar `mod` groupL
        -- Step 6: PK_stealth = s * G_ed + PK_spend
        !sG = scalarMul s basepoint
        !stealthPoint = addSpendKey sG spendPub
    in StealthAddress
        { saAddress   = stealthPoint
        , saEphemeral = ephPublic
        , saViewTag   = vt
        }

-- | Derive the view tag bytes from shared secret via HKDF.
deriveViewTagBytes :: ByteString -> ByteString
deriveViewTagBytes sharedSecret = hkdf hkdfSalt sharedSecret viewTagInfo 32

-- | Add the spend public key point to a computed point s*G.
-- Decodes the spend pubkey, performs Ed25519 point addition, and encodes.
addSpendKey :: ExtPoint -> ByteString -> ByteString
addSpendKey sG spendPubBS =
    case decodePoint spendPubBS of
        Nothing -> error "StealthAddress: invalid spend public key"
        Just !bPoint -> encodePoint (pointAdd sG bPoint)

------------------------------------------------------------------------
-- View tag extraction
------------------------------------------------------------------------

-- | Extract the view tag (first byte) from HKDF output.
-- Used for fast filtering: 255/256 of non-matching transactions
-- are rejected with a single byte comparison.
viewTag :: ByteString -> Word8
viewTag !bs = BS.index bs 0

------------------------------------------------------------------------
-- Recipient scanning
------------------------------------------------------------------------

-- | Scan a transaction output to check if it belongs to us.
--
-- Given our scan secret key, spend public key, the ephemeral R from the
-- transaction, and the candidate stealth address P, returns the spending
-- secret key if this payment matches, or Nothing otherwise.
--
-- The spending secret is: sk_stealth = s + sk_spend (mod L)
-- where s is the HKDF-derived stealth scalar and sk_spend is the
-- clamped Ed25519 secret scalar.
scanForPayment :: ByteString   -- ^ Scan secret key (32 bytes, X25519)
               -> ByteString   -- ^ Spend secret key (32 bytes, Ed25519 seed)
               -> ByteString   -- ^ Spend public key (32 bytes, Ed25519)
               -> ByteString   -- ^ Ephemeral public key R (32 bytes, X25519)
               -> ByteString   -- ^ Candidate stealth address P (32 bytes)
               -> Maybe ByteString
scanForPayment scanSecret spendSecret spendPub ephR candidateP =
    let -- Step 1: Recompute shared secret
        !sharedSecret = x25519 scanSecret ephR
        -- Step 2: Recompute stealth scalar
        !stealthScalar = hkdf hkdfSalt sharedSecret stealthKeyInfo 32
        !s = decodeLE stealthScalar `mod` groupL
        -- Step 3: Recompute expected stealth public key
        !sG = scalarMul s basepoint
        !expectedP = addSpendKey sG spendPub
    in if expectedP == candidateP
       then Just (computeSpendingSecret s spendSecret)
       else Nothing

-- | Compute the spending secret: sk_stealth = s + sk_spend (mod L).
-- The spend secret is an Ed25519 seed; we hash and clamp it to get
-- the actual scalar, then add the stealth scalar.
computeSpendingSecret :: Integer -> ByteString -> ByteString
computeSpendingSecret s spendSecret =
    let -- Ed25519 secret scalar from seed (hash + clamp, per RFC 8032)
        !h = sha512 spendSecret
        !a = clampScalar (BS.take 32 h)
        !skStealth = (s + a) `mod` groupL
    in encodeLEn 32 skStealth

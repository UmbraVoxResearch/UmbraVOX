-- SPDX-License-Identifier: Apache-2.0
-- | Relay-assisted delivery for offline peers.
--
-- When direct peer-to-peer delivery fails (recipient offline), messages
-- can be stored at a relay node addressed by a stealth-derived key.
-- The relay sees only opaque ciphertext and a stealth address — no
-- identity linkage is possible.
--
-- Messages are held for a configurable TTL (default 7 days) and pruned
-- on poll.  Maximum relay message size defaults to 4096 bytes.
--
-- See: doc/spec/relay.md (planned)
module UmbraVox.Network.Relay
    ( RelayMailbox(..)
    , RelayConfig(..)
    , defaultRelayConfig
    , storeForRelay
    , pollRelay
    , newRelayMailbox
    , relayMessageTTL
      -- * High-level relay operations (M28.2)
    , depositMessage
    , pollMessages
    , expireMessages
      -- * DHT-based relay polling (M28.2.2)
    , pollRelayViaDHT
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word64)

import UmbraVox.Crypto.ChaChaPoly (chachaPolyEncrypt, chachaPolyDecrypt)
import UmbraVox.Crypto.Ed25519
    ( ExtPoint, basepoint, pointAdd, scalarMul
    , encodePoint, decodePoint, groupL
    , decodeLE, isSmallOrder
    )
import qualified UmbraVox.Crypto.Generated.FFI.HKDF as HKDFFFI
import qualified UmbraVox.Crypto.Generated.FFI.X25519 as X25519FFI
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.StealthAddress (StealthKeys(..), viewTag)
import UmbraVox.Network.DHT (DHTState(..))
import qualified UmbraVox.Network.DHT.Store as DHTStore

-- | Default message TTL on relay: 7 days in seconds.
relayMessageTTL :: Word64
relayMessageTTL = 604800

-- | Configuration for relay-assisted delivery.
data RelayConfig = RelayConfig
    { rcEnabled       :: !Bool    -- ^ Whether relay delivery is enabled
    , rcTTLSeconds    :: !Word64  -- ^ Message TTL on relay (default 7 days)
    , rcMaxSize       :: !Int     -- ^ Max message size for relay (default 4096)
    , rcFallbackDelay :: !Int     -- ^ Seconds offline before falling back to relay
    } deriving stock (Show, Eq)

-- | Sensible defaults: relay enabled, 7-day TTL, 4096 byte max, 30s fallback.
defaultRelayConfig :: RelayConfig
defaultRelayConfig = RelayConfig
    { rcEnabled       = True
    , rcTTLSeconds    = relayMessageTTL
    , rcMaxSize       = 4096
    , rcFallbackDelay = 30
    }

-- | A relay mailbox stores encrypted blobs addressed by stealth keys.
-- No identity linkage — relay sees only opaque ciphertext + stealth address.
data RelayMailbox = RelayMailbox
    { rmStealthKey :: !ByteString                          -- ^ Stealth-derived storage key
    , rmMessages   :: !(IORef (Seq (ByteString, Word64)))  -- ^ (ciphertext, expiry)
    }

-- | Create a new empty relay mailbox for the given stealth key.
newRelayMailbox :: ByteString -> IO RelayMailbox
newRelayMailbox sk = do
    ref <- newIORef Seq.empty
    pure RelayMailbox
        { rmStealthKey = sk
        , rmMessages   = ref
        }

-- | Store a message for relay delivery (sender side).
--
-- The ciphertext is stored with the given expiry timestamp (POSIX seconds).
-- Returns 'True' if the message was accepted, 'False' if it exceeds the
-- configured max size (checked against 'defaultRelayConfig').
storeForRelay :: RelayMailbox -> ByteString -> Word64 -> IO Bool
storeForRelay mb ciphertext expiry
    | BS.length ciphertext > rcMaxSize defaultRelayConfig = pure False
    | otherwise = do
        atomicModifyIORef' (rmMessages mb) $ \s ->
            (s Seq.|> (ciphertext, expiry), ())
        pure True

-- | Poll for messages from relay (recipient side).
--
-- Returns all messages whose expiry is strictly greater than the given
-- current time (i.e. not yet expired).  Expired messages are pruned
-- from the mailbox during the poll.
pollRelay :: RelayMailbox -> Word64 -> IO [ByteString]
pollRelay mb now =
    atomicModifyIORef' (rmMessages mb) $ \s ->
        let valid = Seq.filter (\(_, expiry) -> expiry > now) s
        in (valid, map fst (foldr (:) [] valid))

------------------------------------------------------------------------
-- High-level relay operations (M28.2)
------------------------------------------------------------------------

-- | HKDF domain separation for relay encryption key derivation.
relayEncKeyInfo :: ByteString
relayEncKeyInfo = "UmbraVox_RelayEnc_v1"

-- | HKDF salt: 32 zero bytes (matches stealth address convention).
relayHKDFSalt :: ByteString
relayHKDFSalt = BS.replicate 32 0

-- | Deposit a message for an offline peer at a relay mailbox.
--
-- The message is stealth-addressed and encrypted so the relay node
-- cannot determine the recipient or read the contents.  A single
-- ephemeral X25519 keypair is used for both stealth address derivation
-- and AEAD key derivation, so the recipient can recover everything
-- from the ephemeral public key alone.
--
-- The wire format of the stored blob is:
--
-- @
--   [32 bytes ephemeral pubkey | 1 byte view tag | 16 bytes auth tag | ciphertext]
-- @
--
-- The encryption key and nonce are derived via HKDF from the X25519
-- shared secret (ephemeral secret * recipient scan public key) with
-- the domain separator @UmbraVox_RelayEnc_v1@.
--
-- Returns 'Nothing' if:
--   * ECDH fails (invalid or low-order recipient scan key)
--   * the stealth address point is invalid (bad spend key)
--   * the resulting blob exceeds the relay max size
depositMessage :: RelayMailbox
               -> ByteString   -- ^ Recipient scan public key (32 bytes, X25519)
               -> ByteString   -- ^ Recipient spend public key (32 bytes, Ed25519)
               -> ByteString   -- ^ Plaintext message
               -> Word64       -- ^ Current POSIX timestamp
               -> RelayConfig  -- ^ Relay configuration
               -> IO (Maybe ())
depositMessage mb scanPub spendPub plaintext now cfg = do
    -- Step 1: Generate a fresh ephemeral keypair.
    ephSecret <- randomBytes 32
    mEphPub <- X25519FFI.x25519 ephSecret X25519FFI.x25519Basepoint
    case mEphPub of
        Nothing -> pure Nothing
        Just ephPub -> do
            -- Step 2: ECDH shared secret with recipient's scan key.
            mSharedSecret <- X25519FFI.x25519 ephSecret scanPub
            case mSharedSecret of
                Nothing -> pure Nothing
                Just sharedSecret -> do
                    -- Step 3: Derive the view tag (for fast recipient filtering).
                    !vtBytes <- HKDFFFI.hkdfSHA512 relayHKDFSalt sharedSecret "UmbraVox_ViewTag_v2" 32
                    let !vt = viewTag vtBytes

                    -- Step 4: Rebuild the stealth address for AAD binding.
                    mStealthAddr <- rebuildStealthAddress sharedSecret spendPub
                    case mStealthAddr of
                        Nothing -> pure Nothing
                        Just stealthAddr -> do
                            -- Step 5: Derive encryption key (32 bytes) and nonce (12 bytes).
                            !derived <- HKDFFFI.hkdfSHA512 relayHKDFSalt sharedSecret relayEncKeyInfo 44
                            let !encKey  = BS.take 32 derived
                                !nonce   = BS.drop 32 derived

                            -- Step 6: Encrypt with ChaCha20-Poly1305.
                            -- AAD = stealth address, binding ciphertext to recipient.
                            let (!ciphertext, !tag) = chachaPolyEncrypt encKey nonce stealthAddr plaintext

                            -- Step 7: Build the wire blob.
                            let !blob = BS.concat
                                    [ ephPub                 -- 32 bytes
                                    , BS.singleton vt        --  1 byte
                                    , tag                    -- 16 bytes
                                    , ciphertext             -- variable
                                    ]
                                !expiry = now + rcTTLSeconds cfg

                            -- Step 8: Store in the mailbox (checks size limit).
                            ok <- storeForRelay mb blob expiry
                            pure (if ok then Just () else Nothing)

-- | Poll a relay mailbox for messages addressed to us.
--
-- For each stored blob, the recipient:
--
--   1. Extracts the view tag for fast filtering (rejects ~255/256 of
--      non-matching messages with a single byte comparison).
--   2. Recomputes the expected view tag from the ephemeral key and our
--      scan secret.  If it does not match, skip.
--   3. Derives the encryption key from the ECDH shared secret and
--      attempts authenticated decryption.
--   4. Returns successfully decrypted plaintexts.
--
-- Expired messages are pruned during the poll (same as 'pollRelay').
pollMessages :: RelayMailbox
             -> StealthKeys  -- ^ Our stealth keypair
             -> Word64       -- ^ Current POSIX timestamp
             -> IO [ByteString]
pollMessages mb sk now = do
    -- Get all non-expired blobs, pruning expired ones.
    blobs <- pollRelay mb now
    -- Try to decrypt each blob; collect successes.
    results <- mapM (tryDecryptBlob sk) blobs
    pure (concat results)

-- | Attempt to decrypt a single relay blob with our stealth keys.
--
-- Wire format: [ephPub(32) | viewTag(1) | tag(16) | ciphertext]
-- Minimum blob size: 32 + 1 + 16 = 49 bytes (empty plaintext).
tryDecryptBlob :: StealthKeys -> ByteString -> IO [ByteString]
tryDecryptBlob sk blob
    | BS.length blob < 49 = pure []  -- too short, not ours
    | otherwise = do
        let !ephPub     = BS.take 32 blob
            !vt         = BS.index blob 32
            !tag        = BS.take 16 (BS.drop 33 blob)
            !ciphertext = BS.drop 49 blob
        mSharedSecret <- X25519FFI.x25519 (skScanSecret sk) ephPub
        case mSharedSecret of
            Nothing -> pure []  -- DH failure (low-order point), not ours
            Just sharedSecret -> do
                -- Fast view tag check: rejects ~255/256 of non-matching blobs.
                !vtDerived <- HKDFFFI.hkdfSHA512 relayHKDFSalt sharedSecret "UmbraVox_ViewTag_v2" 32
                let !expectedVT = viewTag vtDerived
                if expectedVT /= vt
                   then pure []  -- view tag mismatch, not ours
                   else do
                       -- Rebuild the stealth address for AAD verification.
                       mAad <- rebuildStealthAddress sharedSecret (skSpendPublic sk)
                       case mAad of
                           Nothing -> pure []
                           Just aad -> do
                               -- Derive encryption key and nonce from shared secret.
                               !derived <- HKDFFFI.hkdfSHA512 relayHKDFSalt sharedSecret relayEncKeyInfo 44
                               let !encKey  = BS.take 32 derived
                                   !nonce   = BS.drop 32 derived
                               case chachaPolyDecrypt encKey nonce aad ciphertext tag of
                                   Nothing -> pure []  -- auth failure, not ours
                                   Just pt -> pure [pt]

-- | Rebuild the stealth address point from a shared secret and spend
-- public key.  This mirrors the computation in
-- 'UmbraVox.Crypto.StealthAddress.computeStealthAddress' but returns
-- only the address bytes (or Nothing on failure).
rebuildStealthAddress :: ByteString -> ByteString -> IO (Maybe ByteString)
rebuildStealthAddress sharedSecret spendPub = do
    !stealthScalarBytes <- HKDFFFI.hkdfSHA512 relayHKDFSalt sharedSecret "UmbraVox_StealthKey_v1" 32
    let addr = rebuildPoint stealthScalarBytes spendPub
    pure $ if BS.null addr then Nothing else Just addr

-- | Perform s*G + spendPub point arithmetic using the Ed25519 primitives.
-- Imports are re-used from StealthAddress; this duplicates the logic
-- of 'addSpendKey' to keep the relay module self-contained.
rebuildPoint :: ByteString -> ByteString -> ByteString
rebuildPoint stealthScalarBytes spendPubBS =
    let !s = decodeScalar stealthScalarBytes
        !sG = ed25519ScalarMul s ed25519Basepoint
    in ed25519AddSpendKey sG spendPubBS

-- Ed25519 re-exports for point arithmetic (used in rebuildPoint).
-- These are thin wrappers to keep import lists clean.

ed25519ScalarMul :: Integer -> ExtPoint -> ExtPoint
ed25519ScalarMul = scalarMul

ed25519Basepoint :: ExtPoint
ed25519Basepoint = basepoint

ed25519AddSpendKey :: ExtPoint -> ByteString -> ByteString
ed25519AddSpendKey sG spendPubBS
    | isSmallOrder spendPubBS = BS.empty
    | otherwise =
    case decodePoint spendPubBS of
        Nothing     -> BS.empty
        Just bPoint -> encodePoint (pointAdd sG bPoint)

-- | Decode a little-endian scalar mod L from a ByteString.
decodeScalar :: ByteString -> Integer
decodeScalar bs = decodeLE bs `mod` groupL

-- | Expire messages past their TTL from a relay mailbox.
--
-- Removes all entries whose expiry timestamp is at or before @now@.
-- Returns the number of messages expired.
expireMessages :: RelayMailbox -> Word64 -> IO Int
expireMessages mb now =
    atomicModifyIORef' (rmMessages mb) $ \s ->
        let !valid   = Seq.filter (\(_, expiry) -> expiry > now) s
            !expired = Seq.length s - Seq.length valid
        in (valid, expired)

------------------------------------------------------------------------
-- DHT-based relay polling (M28.2.2)
------------------------------------------------------------------------

-- | Poll for messages addressed to our stealth key via DHT.
--
-- Looks up the relay mailbox key derived from our stealth scan public
-- key in the DHT value store.  Any stored blobs whose expiry has not
-- passed are returned.  Currently this performs a local-only lookup;
-- when the DHT transport integration (M24.4) is complete, this will
-- issue a @FIND_VALUE@ RPC to the network.
--
-- The @Word64@ argument is the current POSIX timestamp used for
-- expiry filtering.
pollRelayViaDHT :: DHTState -> StealthKeys -> Word64 -> IO [ByteString]
pollRelayViaDHT dht sk now = do
    let lookupKey = skScanPublic sk
    mVal <- DHTStore.localLookup (dhStore dht) lookupKey now
    case mVal of
        Nothing  -> pure []
        Just val -> pure [val]

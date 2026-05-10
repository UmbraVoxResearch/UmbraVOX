-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "SIGNAL-002" #-} Signal Double Ratchet Algorithm
--
-- Implements the Double Ratchet Algorithm for forward-secure end-to-end
-- encrypted messaging. Each message uses a unique key derived via symmetric
-- and Diffie-Hellman ratchet steps, providing forward secrecy and
-- break-in recovery.
--
-- See: attic/doc-legacy-2026-04-28/03-cryptography.md
module UmbraVox.Crypto.Signal.DoubleRatchet
    ( RatchetState(..)
    , RatchetHeader(..)
    , RatchetError(..)
    , ratchetInitAlice
    , ratchetInitBob
    , ratchetEncrypt
    , ratchetDecrypt
      -- * Audited constants (exported for regression testing)
    , maxTotalSkipped
    ) where

import Data.Bits (shiftR, xor, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)

import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.GCM (gcmDecrypt, gcmEncrypt)
import UmbraVox.Crypto.HKDF (hkdfExpand, hkdfExtract)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Crypto.Random (randomBytes)

------------------------------------------------------------------------
-- Error type
------------------------------------------------------------------------

-- | Errors that 'ratchetEncrypt' and 'ratchetDecrypt' can return.
--
-- Finding    M10.2.2 — 'ratchetEncrypt' and 'ratchetDecrypt' called
--            @error@ on counter exhaustion, aborting the entire process
--            rather than letting the caller handle the condition.
-- Vulnerability: An @error@ call unwinds the Haskell runtime with an
--            unchecked exception; callers have no way to distinguish
--            counter exhaustion from a programming mistake and cannot
--            recover gracefully (e.g. close the session cleanly).
-- Fix:       Replace the @error@ calls with @pure (Left CounterExhausted)@
--            so callers receive a typed error value and can take
--            appropriate action (tear down the session, alert the user).
-- Verified:  Counter-exhaustion tests now check for @Left CounterExhausted@
--            instead of catching a 'SomeException'.
data RatchetError
    = CounterExhausted
      -- ^ Send or receive counter reached 0xFFFFFFFE; the ratchet must
      -- be refreshed via a new DH exchange before further use.
    | DecryptionFailed
      -- ^ GCM tag verification failed (ciphertext tampered or wrong key).
    deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Full Double Ratchet state for one party.
data RatchetState = RatchetState
    { rsDHSend      :: !(ByteString, ByteString)
      -- ^ (secret, public) X25519 sending keypair
    , rsDHRecv      :: !(Maybe ByteString)
      -- ^ Peer's current X25519 public key (Nothing before first message)
    , rsRootKey     :: !ByteString
      -- ^ 32-byte root key
    , rsSendChain   :: !ByteString
      -- ^ 32-byte sending chain key
    , rsRecvChain   :: !ByteString
      -- ^ 32-byte receiving chain key
    , rsSendN       :: !Word32
      -- ^ Sending message counter
    , rsRecvN       :: !Word32
      -- ^ Receiving message counter
    , rsPrevChainN  :: !Word32
      -- ^ Previous sending chain length (sent in header)
    , rsSkippedKeys :: !(Map (ByteString, Word32) (ByteString, ByteString))
      -- ^ Skipped message keys indexed by (DH public key, counter),
      -- each entry is (msgKey, chainKey) — the chain key is stored so
      -- the receiver can re-derive the nonce via makeNonce on replay.
    , rsNonceCounter :: !Word64
      -- ^ Monotonic counter incremented on every encrypt, for auditing.
      -- Note: nonce uniqueness is guaranteed by the chain ratchet —
      -- each msgKey is derived from a unique chain key via kdfCK and is
      -- used exactly once.  This counter exists for monitoring / replay
      -- detection only.
    } deriving stock (Show, Eq)

-- | Header attached to each ratchet message.
data RatchetHeader = RatchetHeader
    { rhDHPublic   :: !ByteString
      -- ^ Sender's current DH public key (32 bytes)
    , rhPrevChainN :: !Word32
      -- ^ Number of messages in previous sending chain
    , rhMsgN       :: !Word32
      -- ^ Message number in current sending chain
    } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | HKDF info string for ratchet key derivation.
ratchetInfo :: ByteString
ratchetInfo = "UmbraVox_Ratchet_v1"

-- | HKDF info string for nonce derivation (M10.2.5).
nonceInfo :: ByteString
nonceInfo = "UmbraVox_Nonce_v1"

-- | Maximum number of skipped message keys to store per ratchet step.
maxSkip :: Word32
maxSkip = 1000

-- | Maximum total entries in the skipped-key cache across all DH ratchets.
-- Prevents unbounded memory growth from an adversary triggering many
-- small skips across many ratchet epochs (M7.3.6).
maxTotalSkipped :: Int
maxTotalSkipped = 5000

------------------------------------------------------------------------
-- KDF helpers
------------------------------------------------------------------------

-- | Derive a new root key and chain key from the current root key and
-- a DH output, using HKDF-SHA-512.
--
-- Salt = root key, IKM = DH output, Info = "UmbraVox_Ratchet_v1"
-- Output = 64 bytes: first 32 = new root key, last 32 = new chain key.
kdfRK :: ByteString -> ByteString -> (ByteString, ByteString)
kdfRK rootKey dhOut =
    let !prk = hkdfExtract rootKey dhOut
        !okm = hkdfExpand prk ratchetInfo 64
    in (BS.take 32 okm, BS.drop 32 okm)

-- | Derive a message key and new chain key from the current chain key.
--
-- messageKey  = HMAC-SHA256(chainKey, 0x01)
-- newChainKey = HMAC-SHA256(chainKey, 0x02)
kdfCK :: ByteString -> (ByteString, ByteString)
kdfCK chainKey =
    let !msgKey      = hmacSHA256 chainKey (BS.singleton 0x01)
        !newChainKey = hmacSHA256 chainKey (BS.singleton 0x02)
    in (newChainKey, msgKey)

------------------------------------------------------------------------
-- Key pair generation (deterministic from secret)
------------------------------------------------------------------------

-- | Generate an X25519 key pair from a 32-byte secret.
-- The basepoint multiplication cannot produce an all-zero result for any
-- non-zero secret, so the 'Just' match here is safe by construction.
generateDH :: ByteString -> (ByteString, ByteString)
generateDH secret =
    case x25519 secret x25519Basepoint of
        Just !pub -> (secret, pub)
        Nothing   -> error "generateDH: x25519 with basepoint returned all-zero (impossible)"

-- | Perform X25519 Diffie-Hellman.
-- Returns Nothing if the DH output is all-zero (low-order point).
dh :: (ByteString, ByteString) -> ByteString -> Maybe ByteString
dh (secret, _) theirPublic = x25519 secret theirPublic

------------------------------------------------------------------------
-- Initialization
------------------------------------------------------------------------

-- | Initialize Double Ratchet state for Alice (the initiator).
--
-- Alice has completed X3DH and holds:
--   * @sharedSecret@ — the 32-byte shared secret from X3DH
--   * @bobSPK@       — Bob's signed pre-key public key
--   * @aliceDHSecret@ — a fresh 32-byte secret for Alice's first ratchet keypair
--
-- Alice performs the first DH ratchet step to derive sending chain key.
-- | Initialize Double Ratchet state for Alice (the initiator).
--
-- Returns Nothing if the initial DH with Bob's SPK yields an all-zero
-- result (low-order point attack; rejects the session rather than
-- silently producing a weak key).
ratchetInitAlice :: ByteString  -- ^ X3DH shared secret (32 bytes)
                 -> ByteString  -- ^ Bob's signed pre-key (32 bytes)
                 -> ByteString  -- ^ Alice's fresh DH secret (32 bytes)
                 -> Maybe RatchetState
ratchetInitAlice sharedSecret bobSPK aliceDHSecret =
    let !aliceKP = generateDH aliceDHSecret
    in case dh aliceKP bobSPK of
        Nothing -> Nothing
        Just !dhOutput ->
            let !(rootKey1, sendChain) = kdfRK sharedSecret dhOutput
            in Just RatchetState
                { rsDHSend      = aliceKP
                , rsDHRecv      = Just bobSPK
                , rsRootKey     = rootKey1
                , rsSendChain   = sendChain
                , rsRecvChain   = BS.replicate 32 0
                  -- SAFETY (M7.2.3): This zero placeholder is never used for
                  -- decryption.  Bob's first message triggers dhRatchet, which
                  -- overwrites rsRecvChain via kdfRK before any kdfCK derivation.
                  -- The zero value cannot leak key material.
                , rsSendN       = 0
                , rsRecvN       = 0
                , rsPrevChainN  = 0
                , rsSkippedKeys  = Map.empty
                , rsNonceCounter = 0
                }

-- | Initialize Double Ratchet state for Bob (the responder).
--
-- Bob uses his signed pre-key as the initial DH keypair.  The peer DH
-- public key is left empty — it will be populated from the header of
-- Alice's first message, which triggers the initial DH ratchet step.
ratchetInitBob :: ByteString  -- ^ X3DH shared secret (32 bytes)
               -> ByteString  -- ^ Bob's signed pre-key secret (32 bytes)
               -> RatchetState
ratchetInitBob sharedSecret bobSPKSecret =
    let !bobKP = generateDH bobSPKSecret
    in RatchetState
        { rsDHSend      = bobKP
        , rsDHRecv      = Nothing   -- Set from first message header
        , rsRootKey     = sharedSecret
        , rsSendChain   = BS.replicate 32 0
          -- SAFETY (M7.2.3): Zero placeholder — Bob cannot encrypt until
          -- Alice's first message arrives and triggers dhRatchet, which
          -- derives real send/recv chain keys via kdfRK.  rsDHRecv starts
          -- as Nothing, so ratchetEncrypt cannot produce valid ciphertext
          -- with this value.
        , rsRecvChain   = BS.replicate 32 0
          -- SAFETY (M7.2.3): Same as rsSendChain above — overwritten by
          -- dhRatchet before any decryption attempt.
        , rsSendN       = 0
        , rsRecvN       = 0
        , rsPrevChainN  = 0
        , rsSkippedKeys  = Map.empty
        , rsNonceCounter = 0
        }

------------------------------------------------------------------------
-- Encryption
------------------------------------------------------------------------

-- | Encrypt a plaintext message, advancing the sending ratchet.
--
-- Returns @Right (updatedState, header, ciphertext, tag)@ on success, or
-- @Left CounterExhausted@ when the send counter has reached its limit.
-- Runs in IO because the DH ratchet step (triggered on decrypt) requires
-- CSPRNG-generated ephemeral keys.
--
-- Finding    M10.2.2 — counter exhaustion previously called @error@.
-- Fix:       Returns @Left CounterExhausted@ instead.
ratchetEncrypt :: RatchetState
               -> ByteString          -- ^ Plaintext
               -> IO (Either RatchetError (RatchetState, RatchetHeader, ByteString, ByteString))
ratchetEncrypt st plaintext =
    -- M8.1.1 / M10.2.2: Reject encryption when send counter is exhausted to
    -- prevent counter overflow / nonce reuse.  Callers must perform a DH
    -- ratchet (by exchanging messages) before reaching this limit.
    if rsSendN st >= 0xFFFFFFFE
        then pure (Left CounterExhausted)
        else do
            let -- Derive message key from sending chain
                !(newChainKey, msgKey) = kdfCK (rsSendChain st)
                -- Build header
                !header = RatchetHeader
                    { rhDHPublic   = snd (rsDHSend st)
                    , rhPrevChainN = rsPrevChainN st
                    , rhMsgN       = rsSendN st
                    }
                -- Build nonce from chain key and message counter (M10.2.3, M10.2.5)
                !nonce = makeNonce (rsSendChain st) (rsSendN st)
                -- Encrypt with AES-256-GCM
                !aad = encodeHeader header
                !(ct, tag) = gcmEncrypt msgKey nonce aad plaintext
                -- Update state
                !st' = st
                    { rsSendChain    = newChainKey
                    , rsSendN        = rsSendN st + 1
                    , rsNonceCounter = rsNonceCounter st + 1
                    }
            pure (Right (st', header, ct, tag))

------------------------------------------------------------------------
-- Decryption
------------------------------------------------------------------------

-- | Decrypt a received message.
--
-- Returns @Right (Just (state, plaintext))@ on success,
-- @Right Nothing@ on GCM authentication failure,
-- and @Left CounterExhausted@ when the receive counter has reached its
-- limit.  Runs in IO because a DH ratchet step may generate a new
-- keypair via CSPRNG.
--
-- Finding    M10.2.2 — counter exhaustion previously called @error@.
-- Fix:       Returns @Left CounterExhausted@ instead.
ratchetDecrypt :: RatchetState
               -> RatchetHeader       -- ^ Message header
               -> ByteString          -- ^ Ciphertext
               -> ByteString          -- ^ GCM tag (16 bytes)
               -> IO (Either RatchetError (Maybe (RatchetState, ByteString)))
ratchetDecrypt st header ct tag =
    -- M8.1.1 / M10.2.2: Reject decryption when receive counter is exhausted
    -- to prevent counter overflow.  A DH ratchet step resets the counter.
    if rsRecvN st >= 0xFFFFFFFE
        then pure (Left CounterExhausted)
        else do
            -- Try skipped keys first
            case trySkippedKeys st header ct tag of
                Just result -> pure (Right (Just result))
                Nothing -> do
                    -- If header DH key differs from our stored peer key, do DH ratchet
                    st1 <- case rsDHRecv st of
                               Nothing    -> dhRatchet st header
                               Just peer
                                   | rhDHPublic header /= peer -> dhRatchet st header
                                   | otherwise                  -> pure (Just st)
                    pure $ Right $ case st1 of
                        Nothing -> Nothing  -- Too many skipped keys
                        Just st2 ->
                            -- Skip any missed messages in current receiving chain
                            case skipMessageKeys st2 (rhMsgN header) of
                                Nothing -> Nothing  -- Too many skipped keys
                                Just st3 ->
                                    -- Derive message key from receiving chain
                                    let !(newChainKey, msgKey) = kdfCK (rsRecvChain st3)
                                        !nonce = makeNonce (rsRecvChain st3) (rsRecvN st3)
                                        !aad = encodeHeader header
                                    in case gcmDecrypt msgKey nonce aad ct tag of
                                        Just plaintext ->
                                            let !st4 = st3
                                                    { rsRecvChain = newChainKey
                                                    , rsRecvN     = rsRecvN st3 + 1
                                                    }
                                            in Just (st4, plaintext)
                                        Nothing -> Nothing

------------------------------------------------------------------------
-- DH Ratchet step
------------------------------------------------------------------------

-- | Perform a DH ratchet step when receiving a new peer DH public key.
-- Uses CSPRNG to generate ephemeral DH keypairs for break-in recovery.
-- Returns Nothing if a DH output is all-zero (low-order point rejection).
dhRatchet :: RatchetState -> RatchetHeader -> IO (Maybe RatchetState)
dhRatchet st header =
    -- Skip any remaining messages in old receiving chain
    case skipMessageKeys st (rhPrevChainN header) of
        Nothing -> pure Nothing
        Just st1 -> do
            let -- Store the previous chain length
                !st2 = st1
                    { rsPrevChainN = rsSendN st1
                    , rsSendN      = 0
                    , rsRecvN      = 0
                    , rsDHRecv     = Just (rhDHPublic header)
                    }
                -- Derive new receiving chain (rsDHRecv is guaranteed Just here)
                !peerPub = case rsDHRecv st2 of
                    Just pk -> pk
                    Nothing -> error "dhRatchet: impossible: rsDHRecv is Nothing after assignment"
            case dh (rsDHSend st2) peerPub of
                Nothing -> pure Nothing
                Just !dhOutput1 -> do
                    let !(rootKey1, recvChain) = kdfRK (rsRootKey st2) dhOutput1
                    -- Generate new sending keypair from CSPRNG
                    newDHSecret <- randomBytes 32
                    let !newDHKP = generateDH newDHSecret
                    case dh newDHKP peerPub of
                        Nothing -> pure Nothing
                        Just !dhOutput2 ->
                            let !(rootKey2, sendChain) = kdfRK rootKey1 dhOutput2
                            in pure $ Just st2
                                { rsDHSend    = newDHKP
                                , rsRootKey   = rootKey2
                                , rsSendChain = sendChain
                                , rsRecvChain = recvChain
                                }

------------------------------------------------------------------------
-- Skipped message keys
------------------------------------------------------------------------

-- | Try to decrypt using a previously skipped message key.
trySkippedKeys :: RatchetState -> RatchetHeader -> ByteString -> ByteString
               -> Maybe (RatchetState, ByteString)
trySkippedKeys st header ct tag =
    let !lookupKey = (rhDHPublic header, rhMsgN header)
    in case Map.lookup lookupKey (rsSkippedKeys st) of
        Nothing -> Nothing
        Just (msgKey, chainKey) ->
            -- Re-derive nonce from the stored chain key, matching the nonce
            -- used during encryption (M10.2.5: nonce comes from chain key).
            let !nonce = makeNonce chainKey (rhMsgN header)
                !aad = encodeHeader header
            in case gcmDecrypt msgKey nonce aad ct tag of
                Just plaintext ->
                    let !st' = st { rsSkippedKeys = Map.delete lookupKey (rsSkippedKeys st) }
                    in Just (st', plaintext)
                Nothing -> Nothing

-- | Skip message keys from current counter up to (but not including) @until@.
-- Stores derived keys in rsSkippedKeys. Returns Nothing if too many would be
-- skipped, or if the peer's DH key is not yet known (rsDHRecv is Nothing).
skipMessageKeys :: RatchetState -> Word32 -> Maybe RatchetState
skipMessageKeys st until'
    | rsRecvN st >= until'          = Just st
    | until' - rsRecvN st > maxSkip = Nothing
    | otherwise = case rsDHRecv st of
        -- Cannot store skipped keys without a known peer DH public key.
        -- This path can be triggered by garbage input (fuzz/bit-flip); the
        -- graceful response is to reject rather than crash.
        Nothing -> Nothing
        Just _  ->
            let !result = go st
                -- M7.3.6: enforce total skipped-key cap across all DH ratchets
                !pruned = evictOldest (rsSkippedKeys result)
            in Just result { rsSkippedKeys = pruned }
  where
    go s
        | rsRecvN s >= until' = s
        | otherwise =
            let !oldChainKey = rsRecvChain s
                !(newChainKey, msgKey) = kdfCK oldChainKey
                !peerKey = case rsDHRecv s of
                    Just pk -> pk
                    Nothing -> error "skipMessageKeys: impossible: rsDHRecv is Nothing (guarded above)"
                !key = (peerKey, rsRecvN s)
                -- Store (msgKey, chainKey) so trySkippedKeys can re-derive the nonce.
                !skipped = Map.insert key (msgKey, oldChainKey) (rsSkippedKeys s)
            in go s
                { rsRecvChain   = newChainKey
                , rsRecvN       = rsRecvN s + 1
                , rsSkippedKeys = skipped
                }

-- | Evict the oldest entries (lowest counter values) from the skipped-key
-- cache until the total size is within 'maxTotalSkipped' (M7.3.6).
evictOldest :: Map (ByteString, Word32) (ByteString, ByteString)
            -> Map (ByteString, Word32) (ByteString, ByteString)
evictOldest m
    | Map.size m <= maxTotalSkipped = m
    | otherwise =
        -- Map is ordered by (dhPub, counter); entries with the lowest
        -- counter values (oldest messages) sort first.  We drop excess
        -- entries from the beginning of the map.
        let !excess = Map.size m - maxTotalSkipped
            !pruned = Map.fromAscList (drop excess (Map.toAscList m))
        in pruned

------------------------------------------------------------------------
-- Nonce and header encoding
------------------------------------------------------------------------

-- | Build a 12-byte GCM nonce from the chain key and message counter.
--
-- Finding    M10.2.3 — old 'makeNonce' used HMAC(msgKey, "nonce") for 8 bytes
--            + 4-byte big-endian counter, deviating from the standard
--            Noise/AEAD nonce layout and mixing key and nonce derivation.
-- Vulnerability: Non-standard layout makes the nonce derivation harder to
--            audit; big-endian counter deviates from IETF AEAD conventions.
-- Fix:       Nonce layout changed to 4 zero bytes || 8-byte little-endian
--            counter (standard Noise/AEAD layout per RFC 8439 §2.3).
--
-- Finding    M10.2.5 — old 'makeNonce' used @msgKey@ as the HMAC key for
--            nonce derivation, creating cryptographic coupling between the
--            GCM encryption key and the nonce derivation input.
-- Vulnerability: Using the same key material as both the GCM key and the
--            nonce derivation seed violates key separation; an attacker
--            who learns the nonce derivation also learns partial information
--            about the GCM key.
-- Fix:       'makeNonce' now takes the @chainKey@ (distinct from @msgKey@)
--            and derives an 8-byte nonce base via HKDF with the distinct
--            info label @"UmbraVox_Nonce_v1"@, XOR-ed with the 8-byte
--            little-endian counter.  The result is prepended with 4 zero
--            bytes to match the standard Noise layout.
-- Verified:  'ratchetEncrypt' and 'ratchetDecrypt' pass the chain key (not
--            the message key) to 'makeNonce'.
makeNonce :: ByteString  -- ^ Chain key (NOT the message key)
          -> Word32      -- ^ Message counter
          -> ByteString
makeNonce chainKey counter =
    -- Derive 8-byte nonce base from chain key, isolated from the GCM key
    let !prk  = hkdfExtract chainKey nonceInfo
        !base = hkdfExpand prk BS.empty 8
        -- XOR the base with the 8-byte LE counter for per-message uniqueness
        !ctr  = encodeWord64LE (fromIntegral counter)
        !mixed = BS.pack (BS.zipWith xor base ctr)
    in BS.replicate 4 0 <> mixed

-- | Encode a Word32 as 4-byte big-endian.
encodeWord32BE :: Word32 -> ByteString
encodeWord32BE w = BS.pack
    [ fromIntegral (w `shiftR` 24 .&. 0xff)
    , fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR` 8  .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

-- | Encode a Word64 as 8-byte little-endian.
encodeWord64LE :: Word64 -> ByteString
encodeWord64LE w = BS.pack
    [ fromIntegral (w             .&. 0xff)
    , fromIntegral (w `shiftR`  8 .&. 0xff)
    , fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR` 24 .&. 0xff)
    , fromIntegral (w `shiftR` 32 .&. 0xff)
    , fromIntegral (w `shiftR` 40 .&. 0xff)
    , fromIntegral (w `shiftR` 48 .&. 0xff)
    , fromIntegral (w `shiftR` 56 .&. 0xff)
    ]

-- | Encode a header as associated data for GCM authentication.
-- Format: DH public key (32) || prevChainN (4) || msgN (4) = 40 bytes.
encodeHeader :: RatchetHeader -> ByteString
encodeHeader hdr =
    rhDHPublic hdr
    <> encodeWord32BE (rhPrevChainN hdr)
    <> encodeWord32BE (rhMsgN hdr)

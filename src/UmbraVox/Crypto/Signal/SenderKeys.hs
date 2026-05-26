-- SPDX-License-Identifier: Apache-2.0
-- | Signal Sender Keys for group messaging.
--
-- Implements the Signal Sender Key distribution protocol used for
-- efficient group messaging.  Each group member maintains a sending
-- chain; messages are encrypted with a symmetric ratchet (HMAC-based
-- chain key derivation, same pattern as DoubleRatchet's kdfCK) and
-- AES-256-GCM.  Distribution messages carry the initial chain key
-- and group secret so new members can join.
--
-- == Deniability
--
-- Per-message signatures use an ephemeral Ed25519 key pair generated
-- per-group session — NOT the sender's long-term identity key.  The
-- ephemeral private key stays with the sender; only the public key is
-- distributed via the SKDM (which travels over deniable pairwise
-- channels).  This preserves plausible deniability while preventing
-- intra-group forgery (CVE-CANDIDATE-4).
--
-- == M15.3 SecureBytes migration
--
-- 'sksChainKey', 'sksSigningKey', 'skdChainKey', and 'skdSigningKey'
-- are stored as 'SecureBytes' (pinned, zeroed-on-free).  Functions
-- that operate on these fields now live in IO.
--
-- See: doc/spec/signal-protocol.md
module UmbraVox.Crypto.Signal.SenderKeys
    ( SenderKeyState(..)
    , SenderKeyMessage(..)
    , SenderKeyDistributionMessage(..)
    , SenderKeyError(..)
    , createSenderKeyDistribution
    , processSenderKeyDistribution
    , encryptSenderKey
    , decryptSenderKey
    ) where

import Data.Bits (shiftR, xor, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)

import UmbraVox.Crypto.Ed25519 (ed25519PublicKey, ed25519Sign, ed25519Verify)
import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import UmbraVox.Crypto.HKDF (hkdfExpand, hkdfExtract)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.SecureBytes (SecureBytes, fromByteString, toByteString, zeroAndFree)

------------------------------------------------------------------------
-- Error type
------------------------------------------------------------------------

-- | Errors returned by sender key operations.
data SenderKeyError
    = UnknownSender !ByteString
      -- ^ No sender key state for the given sender ID.
    | ChainExhausted
      -- ^ Chain counter reached maximum; sender must re-distribute.
    | DecryptionFailed
      -- ^ GCM tag verification failed.
    | SignatureVerificationFailed
      -- ^ Ed25519 signature on the sender key message did not verify.
    | NotSender
      -- ^ Attempted to encrypt with a receiver-side state (no ephemeral
      -- signing key).  Only the sender's own 'SenderKeyState' (returned
      -- by 'createSenderKeyDistribution') carries the private key.
    | ChainTooFarAhead
      -- ^ Message iteration exceeds max skip limit (resource exhaustion
      -- prevention).
    | InvalidDistribution !String
      -- ^ Distribution message failed validation.
    deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | HKDF info string for sender key nonce derivation.
senderKeyNonceInfo :: ByteString
senderKeyNonceInfo = "UmbraVox_SenderKey_Nonce_v1"

-- | Maximum number of chain steps to skip per message.
maxSenderKeySkip :: Word32
maxSenderKeySkip = 2000

-- | Maximum entries in the skipped-key cache.
maxSkippedSenderKeys :: Int
maxSkippedSenderKeys = 256

-- | Maximum age for skipped sender keys (48 hours in seconds).
skippedSenderKeyMaxAgeSecs :: Word64
skippedSenderKeyMaxAgeSecs = 172800

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | State for a single sender's key chain within a group.
--
-- Each group member maintains one of these per known sender (including
-- themselves for their own sending chain).
--
-- 'sksChainKey' and 'sksSigningKey' are stored as 'SecureBytes'
-- (pinned, zeroed-on-free) per M15.3.
--
-- == Deniability design
--
-- Per-message signatures use an /ephemeral/ Ed25519 key pair generated
-- per-group session, NOT the sender's long-term identity key.  The
-- ephemeral private key is stored in 'sksEphSignKey' (sender only);
-- recipients receive only the public key via the distribution message.
-- This prevents third parties from linking signatures to the sender's
-- real identity, preserving plausible deniability while still preventing
-- intra-group forgery (CVE-CANDIDATE-4).
data SenderKeyState = SenderKeyState
    { sksSenderId    :: !ByteString
      -- ^ Opaque sender identifier (e.g. UUID or device+identity hash).
    , sksChainKey    :: !SecureBytes
      -- ^ Current 32-byte symmetric chain key (M15.3: SecureBytes).
    , sksIteration   :: !Word32
      -- ^ Current chain iteration (message counter).
    , sksSigningKey  :: !SecureBytes
      -- ^ 32-byte shared group secret (M15.3: SecureBytes).  Used as
      -- AAD context for GCM encryption, not for digital signatures.
      -- Distributed to all group members via the SKDM.
    , sksSignPub     :: !ByteString
      -- ^ 32-byte ephemeral Ed25519 public key for verifying per-message
      -- signatures on received 'SenderKeyMessage' values.
    , sksEphSignKey  :: !(Maybe SecureBytes)
      -- ^ Ephemeral Ed25519 signing private key.  @Just@ for the
      -- sender's own state (returned by 'createSenderKeyDistribution'),
      -- @Nothing@ for receiver states (returned by
      -- 'processSenderKeyDistribution').  This key is never distributed;
      -- only the corresponding 'sksSignPub' is shared.
    , sksSkippedKeys :: !(Map (ByteString, Word32) (ByteString, ByteString, Word64))
      -- ^ Skipped message keys indexed by (senderId, iteration).
      -- Each entry is (msgKey, chainKey, wallTimestamp).
      -- Allows decryption of out-of-order messages.
    , sksSkipSeq     :: !Word64
      -- ^ Monotonic counter for insertion-order eviction.
    }

-- | An encrypted group message produced by the sender key chain.
data SenderKeyMessage = SenderKeyMessage
    { skmSenderId   :: !ByteString
      -- ^ Sender identifier (plaintext, for recipient key lookup).
    , skmIteration  :: !Word32
      -- ^ Chain iteration at which this message was encrypted.
    , skmCiphertext :: !ByteString
      -- ^ AES-256-GCM ciphertext.
    , skmTag        :: !ByteString
      -- ^ 16-byte GCM authentication tag.
    , skmSignature  :: !ByteString
      -- ^ 64-byte Ed25519 signature over senderId || iteration(4 BE) ||
      -- ciphertext || tag, preventing group member forgery.
    } deriving stock (Show, Eq)

-- | Initial key distribution message sent when a member joins a group
-- or rotates their sender key.
--
-- 'skdChainKey' and 'skdSigningKey' carry secret material received from
-- peers and are stored as 'SecureBytes' per M15.3.
data SenderKeyDistributionMessage = SenderKeyDistributionMessage
    { skdSenderId   :: !ByteString
      -- ^ Sender identifier.
    , skdChainKey   :: !SecureBytes
      -- ^ Initial 32-byte chain key (M15.3: SecureBytes).
    , skdIteration  :: !Word32
      -- ^ Starting iteration (usually 0).
    , skdSigningKey :: !SecureBytes
      -- ^ 32-byte signing/identity key (M15.3: SecureBytes).
    , skdSignPub    :: !ByteString
      -- ^ 32-byte ephemeral Ed25519 public key used to verify
      -- per-message signatures on 'SenderKeyMessage' values from this
      -- sender.  The corresponding private key is known only to the
      -- sender (stored in 'sksEphSignKey').
    }

------------------------------------------------------------------------
-- KDF helper (same pattern as DoubleRatchet.kdfCK)
------------------------------------------------------------------------

-- | Derive a message key and new chain key from the current chain key.
--
-- messageKey  = HMAC-SHA256(chainKey, 0x01)
-- newChainKey = HMAC-SHA256(chainKey, 0x02)
--
-- Finding:     M15.5 — 'senderKdfCK' returned a plain @(ByteString, ByteString)@
--              pair.  The new chain key, which is long-lived symmetric key
--              material, was left as a plain @ByteString@ until the caller
--              wrapped it in 'SecureBytes', leaving a window where it resided
--              in unpinned GC-managed heap.
-- Vulnerability: During the window between derivation and wrapping, the new
--              chain key could be copied by the GC, leaving residual copies in
--              heap that are never zeroed.
-- Fix:         'senderKdfCK' now returns @IO (SecureBytes, ByteString)@ —
--              the new chain key is immediately wrapped in 'SecureBytes' inside
--              the function.  The message key remains a plain @ByteString@
--              because it is short-lived (used immediately for one GCM
--              operation and then discarded).
-- Verified:    'encryptSenderKey' and 'advanceChain' receive the new chain
--              key as 'SecureBytes' and store or zero it directly.
senderKdfCK :: ByteString -> IO (SecureBytes, ByteString)
senderKdfCK chainKey = do
    let !msgKey      = hmacSHA256 chainKey (BS.singleton 0x01)
        !newChainKey = hmacSHA256 chainKey (BS.singleton 0x02)
    newChainKeySB <- fromByteString newChainKey
    pure (newChainKeySB, msgKey)

------------------------------------------------------------------------
-- Nonce derivation
------------------------------------------------------------------------

-- | Build a 12-byte GCM nonce from the chain key and iteration counter.
--
-- Layout: 4 zero bytes || 8-byte (HKDF-derived base XOR LE counter).
-- Same approach as DoubleRatchet.makeNonce.
makeSenderNonce :: ByteString -> Word32 -> ByteString
makeSenderNonce chainKey iteration =
    let !prk   = hkdfExtract (BS.replicate 32 0) chainKey
        !base  = hkdfExpand prk senderKeyNonceInfo 8
        !ctr   = encodeWord64LE (fromIntegral iteration)
        !mixed = BS.pack (BS.zipWith xor base ctr)
    in BS.replicate 4 0 <> mixed

------------------------------------------------------------------------
-- Distribution
------------------------------------------------------------------------

-- | Generate an initial sender key distribution message.
--
-- Creates a fresh chain key, shared group secret, and an /ephemeral/
-- Ed25519 signing key pair for this sender.  The returned
-- 'SenderKeyState' carries the ephemeral private key in
-- 'sksEphSignKey'; the 'SenderKeyDistributionMessage' carries only
-- the public half ('skdSignPub').
--
-- The distribution message should be sent (encrypted via pairwise
-- Double Ratchet sessions) to every group member.  Because the
-- pairwise channel is deniable and the ephemeral key is not bound
-- to the sender's long-term identity, per-message signatures
-- preserve plausible deniability while preventing intra-group
-- forgery.
createSenderKeyDistribution
    :: ByteString  -- ^ Sender identifier
    -> IO (SenderKeyState, SenderKeyDistributionMessage)
createSenderKeyDistribution senderId = do
    chainKeyRaw    <- randomBytes 32
    groupKeyRaw    <- randomBytes 32  -- shared group secret (AAD)
    ephSignKeyRaw  <- randomBytes 32  -- ephemeral signing private key
    chainKeySB     <- fromByteString chainKeyRaw
    groupKeySB     <- fromByteString groupKeyRaw
    ephSignKeySB   <- fromByteString ephSignKeyRaw
    -- Distribution message gets its own copies so the SenderKeyState
    -- and SenderKeyDistributionMessage do not alias.
    distChainKey   <- fromByteString chainKeyRaw
    distGroupKey   <- fromByteString groupKeyRaw
    let !ephPubKey = ed25519PublicKey ephSignKeyRaw
        st = SenderKeyState
            { sksSenderId    = senderId
            , sksChainKey    = chainKeySB
            , sksIteration   = 0
            , sksSigningKey  = groupKeySB
            , sksSignPub     = ephPubKey
            , sksEphSignKey  = Just ephSignKeySB
            , sksSkippedKeys = Map.empty
            , sksSkipSeq     = 0
            }
        dist = SenderKeyDistributionMessage
            { skdSenderId   = senderId
            , skdChainKey   = distChainKey
            , skdIteration  = 0
            , skdSigningKey = distGroupKey
            , skdSignPub    = ephPubKey
            }
    pure (st, dist)

-- | Process a received sender key distribution message.
--
-- Returns a 'SenderKeyState' that can be used to decrypt future
-- messages from this sender.  The caller is responsible for storing
-- this state indexed by sender ID.
--
-- Now in IO because it creates 'SecureBytes' copies of the key
-- material from the distribution message.
processSenderKeyDistribution
    :: SenderKeyDistributionMessage
    -> IO (Either SenderKeyError SenderKeyState)
processSenderKeyDistribution dist = do
    chainKeyBS   <- toByteString (skdChainKey dist)
    signingKeyBS <- toByteString (skdSigningKey dist)
    if BS.length chainKeyBS /= 32
    then pure $ Left (InvalidDistribution "chain key must be 32 bytes")
    else if BS.length signingKeyBS /= 32
    then pure $ Left (InvalidDistribution "signing key must be 32 bytes")
    else if BS.null (skdSenderId dist)
    then pure $ Left (InvalidDistribution "sender ID must not be empty")
    else if BS.length (skdSignPub dist) /= 32
    then pure $ Left (InvalidDistribution "Ed25519 public key must be 32 bytes")
    else do
        -- Create fresh SecureBytes copies for the new state
        ck <- fromByteString chainKeyBS
        sk <- fromByteString signingKeyBS
        pure $ Right SenderKeyState
            { sksSenderId    = skdSenderId dist
            , sksChainKey    = ck
            , sksIteration   = skdIteration dist
            , sksSigningKey  = sk
            , sksSignPub     = skdSignPub dist
            , sksEphSignKey  = Nothing  -- receivers cannot sign
            , sksSkippedKeys = Map.empty
            , sksSkipSeq     = 0
            }

------------------------------------------------------------------------
-- Encryption
------------------------------------------------------------------------

-- | Encrypt a plaintext message for the group using the sender's key chain.
--
-- Advances the chain by one step (kdfCK), encrypts with AES-256-GCM,
-- then signs @senderId || iteration(4 BE) || ciphertext || tag@ with the
-- sender's /ephemeral/ Ed25519 key (from 'sksEphSignKey') to prevent
-- forgery by other group members while preserving plausible deniability.
--
-- Returns 'Left' 'NotSender' if called on a receiver-side state (which
-- does not carry the ephemeral signing private key).
--
-- Now in IO because it reads from and writes to 'SecureBytes' fields.
encryptSenderKey
    :: SenderKeyState
    -> ByteString            -- ^ Plaintext
    -> IO (Either SenderKeyError (SenderKeyState, SenderKeyMessage))
encryptSenderKey st plaintext
    | sksIteration st >= 0xFFFFFFFE
    = pure $ Left ChainExhausted
    | otherwise = case sksEphSignKey st of
        Nothing -> pure $ Left NotSender
        Just ephKeySB -> do
            ephKeyBS     <- toByteString ephKeySB
            chainKeyBS   <- toByteString (sksChainKey st)
            signingKeyBS <- toByteString (sksSigningKey st)
            -- M15.5: senderKdfCK now returns IO (SecureBytes, ByteString).
            (newChainKeySB, msgKey) <- senderKdfCK chainKeyBS
            let !nonce = makeSenderNonce chainKeyBS (sksIteration st)
                -- AAD = senderId || signingKey || iteration (4 bytes BE)
                !aad = sksSenderId st
                    <> signingKeyBS
                    <> encodeWord32BE (sksIteration st)
                !(ct, tag) = gcmEncrypt msgKey nonce aad plaintext
                -- Sign: senderId || iteration(4 BE) || ciphertext || tag
                !sigPayload = sksSenderId st
                    <> encodeWord32BE (sksIteration st)
                    <> ct
                    <> tag
                !sig = ed25519Sign ephKeyBS sigPayload
            -- Zero old chain key before it becomes unreachable (M15.5).
            zeroAndFree (sksChainKey st)
            let !st' = st
                    { sksChainKey  = newChainKeySB
                    , sksIteration = sksIteration st + 1
                    }
                !msg = SenderKeyMessage
                    { skmSenderId   = sksSenderId st
                    , skmIteration  = sksIteration st
                    , skmCiphertext = ct
                    , skmTag        = tag
                    , skmSignature  = sig
                    }
            pure $ Right (st', msg)

------------------------------------------------------------------------
-- Decryption
------------------------------------------------------------------------

-- | Decrypt a received group message using the stored sender key state.
--
-- If the message iteration is ahead of our chain, we advance the chain
-- (up to 'maxSenderKeySkip' steps) to reach the correct message key,
-- storing intermediate keys in the skipped-key cache for out-of-order
-- delivery.  If the iteration is behind our chain, we check the
-- skipped-key cache.
--
-- @nowSecs@ is the current wall-clock time in POSIX seconds, used for
-- age-based eviction of cached skipped keys (48 hours).
--
-- Now in IO because it reads from and writes to 'SecureBytes' fields.
decryptSenderKey
    :: SenderKeyState
    -> SenderKeyMessage
    -> Word64               -- ^ Current POSIX time (seconds)
    -> IO (Either SenderKeyError (SenderKeyState, ByteString))
decryptSenderKey st msg nowSecs
    | skmSenderId msg /= sksSenderId st
    = pure $ Left (UnknownSender (skmSenderId msg))
    | skmIteration msg < sksIteration st
    = trySkippedSenderKeys st msg nowSecs
    | skmIteration msg - sksIteration st > maxSenderKeySkip
    = pure $ Left ChainTooFarAhead
    | otherwise = do
        chainKeyBS   <- toByteString (sksChainKey st)
        signingKeyBS <- toByteString (sksSigningKey st)
        -- Verify Ed25519 signature before touching the ciphertext.
        let !sigPayload = skmSenderId msg
                <> encodeWord32BE (skmIteration msg)
                <> skmCiphertext msg
                <> skmTag msg
        if not (ed25519Verify (sksSignPub st) sigPayload (skmSignature msg))
        then pure $ Left SignatureVerificationFailed
        else do
            -- Store intermediate keys in the skipped-key cache, then
            -- decrypt the target message.
            -- M15.5: advanceChain is now IO; returns SecureBytes for new chain key.
            (advancedChainKeySB, targetChainKeyBS, targetMsgKey, skipped) <-
                advanceChain chainKeyBS (sksIteration st) (skmIteration msg)
                             (sksSenderId st) (sksSkippedKeys st) (sksSkipSeq st) nowSecs
            let !nonce = makeSenderNonce targetChainKeyBS (skmIteration msg)
                !aad = sksSenderId st
                    <> signingKeyBS
                    <> encodeWord32BE (skmIteration msg)
            case gcmDecrypt targetMsgKey nonce aad (skmCiphertext msg) (skmTag msg) of
                Nothing -> do
                    -- Decrypt failed; zero the unused new chain key.
                    zeroAndFree advancedChainKeySB
                    pure $ Left DecryptionFailed
                Just plaintext -> do
                    -- Zero old chain key before it becomes unreachable (M15.5).
                    zeroAndFree (sksChainKey st)
                    let !evicted = evictSkippedSenderKeys nowSecs skipped
                        !st' = st
                            { sksChainKey    = advancedChainKeySB
                            , sksIteration   = skmIteration msg + 1
                            , sksSkippedKeys = evicted
                            , sksSkipSeq     = sksSkipSeq st + fromIntegral (skmIteration msg - sksIteration st)
                            }
                    pure $ Right (st', plaintext)

-- | Try to decrypt using a previously cached skipped message key.
-- Verifies the Ed25519 signature first, then evicts expired entries
-- (older than 48 hours) before lookup.
trySkippedSenderKeys :: SenderKeyState -> SenderKeyMessage -> Word64
                     -> IO (Either SenderKeyError (SenderKeyState, ByteString))
trySkippedSenderKeys st msg nowSecs = do
    signingKeyBS <- toByteString (sksSigningKey st)
    -- Verify Ed25519 signature before touching the ciphertext.
    let !sigPayload = skmSenderId msg
            <> encodeWord32BE (skmIteration msg)
            <> skmCiphertext msg
            <> skmTag msg
    if not (ed25519Verify (sksSignPub st) sigPayload (skmSignature msg))
    then pure $ Left SignatureVerificationFailed
    else do
        let !lookupKey = (sksSenderId st, skmIteration msg)
            -- Evict expired entries
            !pruned = Map.filter (\(_, _, ts) ->
                nowSecs <= ts || (nowSecs - ts) <= skippedSenderKeyMaxAgeSecs) (sksSkippedKeys st)
        case Map.lookup lookupKey pruned of
            Nothing -> pure $ Left DecryptionFailed
            Just (msgKey, chainKey, _insertTime) ->
                let !nonce = makeSenderNonce chainKey (skmIteration msg)
                    !aad = sksSenderId st
                        <> signingKeyBS
                        <> encodeWord32BE (skmIteration msg)
                in case gcmDecrypt msgKey nonce aad (skmCiphertext msg) (skmTag msg) of
                    Nothing -> pure $ Left DecryptionFailed
                    Just plaintext ->
                        let !st' = st { sksSkippedKeys = Map.delete lookupKey pruned }
                        in pure $ Right (st', plaintext)

-- | Advance the chain from @currentIter@ to @targetIter@, storing
-- intermediate message keys in the skipped-key cache.
-- Returns (newChainKey after target, chainKey at target, msgKey at target, updatedSkippedKeys).
-- M15.5: Now monadic (IO) because 'senderKdfCK' returns @IO (SecureBytes, ByteString)@.
-- The returned new chain key after the target is 'SecureBytes'.  Intermediate
-- chain keys stored in the skipped-key map are extracted back to 'ByteString'
-- (via 'toByteString' + 'zeroAndFree') because the map stores plain 'ByteString'
-- and those entries are short-lived (used once for nonce re-derivation).
advanceChain :: ByteString -> Word32 -> Word32
             -> ByteString
             -> Map (ByteString, Word32) (ByteString, ByteString, Word64)
             -> Word64 -> Word64
             -> IO (SecureBytes, ByteString, ByteString, Map (ByteString, Word32) (ByteString, ByteString, Word64))
advanceChain chainKey currentIter targetIter senderId skipped skipSeq nowSecs =
    go chainKey currentIter skipped skipSeq
  where
    go !ck !i !sk !sq
        | i == targetIter = do
            (newCKSB, msgKey) <- senderKdfCK ck
            pure (newCKSB, ck, msgKey, sk)
        | otherwise = do
            (newCKSB, msgKey) <- senderKdfCK ck
            -- Extract ByteString for skipped map; immediately zero the SecureBytes.
            newCKBS <- toByteString newCKSB
            zeroAndFree newCKSB
            let !key = (senderId, i)
                !sk' = Map.insert key (msgKey, ck, nowSecs) sk
            go newCKBS (i + 1) sk' (sq + 1)

-- | Evict skipped sender keys that are too old or exceed the cache cap.
evictSkippedSenderKeys :: Word64
                       -> Map (ByteString, Word32) (ByteString, ByteString, Word64)
                       -> Map (ByteString, Word32) (ByteString, ByteString, Word64)
evictSkippedSenderKeys nowSecs m =
    let -- Age-based eviction (48 hours)
        !aged = Map.filter (\(_, _, ts) ->
            nowSecs <= ts || (nowSecs - ts) <= skippedSenderKeyMaxAgeSecs) m
    in -- Size-based eviction: remove oldest entries by timestamp
       if Map.size aged <= maxSkippedSenderKeys
       then aged
       else evictOldestSenderKeys aged

-- | Remove the oldest entries (by wall-clock timestamp) until the cache
-- is within 'maxSkippedSenderKeys'.
evictOldestSenderKeys :: Map (ByteString, Word32) (ByteString, ByteString, Word64)
                      -> Map (ByteString, Word32) (ByteString, ByteString, Word64)
evictOldestSenderKeys m
    | Map.size m <= maxSkippedSenderKeys = m
    | otherwise =
        -- Find the entry with the smallest timestamp and remove it
        let !oldest = Map.foldlWithKey'
                (\acc k (_, _, ts) -> case acc of
                    Nothing -> Just (k, ts)
                    Just (_, accTs) -> if ts < accTs then Just (k, ts) else acc)
                Nothing m
        in case oldest of
            Nothing -> m
            Just (k, _) -> evictOldestSenderKeys (Map.delete k m)

------------------------------------------------------------------------
-- Encoding helpers
------------------------------------------------------------------------

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

-- SPDX-License-Identifier: Apache-2.0
-- | Signal Sender Keys for group messaging.
--
-- Implements the Signal Sender Key distribution protocol used for
-- efficient group messaging.  Each group member maintains a sending
-- chain; messages are encrypted with a symmetric ratchet (HMAC-based
-- chain key derivation, same pattern as DoubleRatchet's kdfCK) and
-- AES-256-GCM.  Distribution messages carry the initial chain key
-- and signing key so new members can join.
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

import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import UmbraVox.Crypto.HKDF (hkdfExpand, hkdfExtract)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Crypto.Random (randomBytes)

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
data SenderKeyState = SenderKeyState
    { sksSenderId    :: !ByteString
      -- ^ Opaque sender identifier (e.g. UUID or device+identity hash).
    , sksChainKey    :: !ByteString
      -- ^ Current 32-byte symmetric chain key.
    , sksIteration   :: !Word32
      -- ^ Current chain iteration (message counter).
    , sksSigningKey  :: !ByteString
      -- ^ 32-byte signing/identity key for this sender (used as AAD
      -- context, not for digital signatures in this layer).
    , sksSkippedKeys :: !(Map (ByteString, Word32) (ByteString, ByteString, Word64))
      -- ^ Skipped message keys indexed by (senderId, iteration).
      -- Each entry is (msgKey, chainKey, wallTimestamp).
      -- Allows decryption of out-of-order messages.
    , sksSkipSeq     :: !Word64
      -- ^ Monotonic counter for insertion-order eviction.
    } deriving stock (Show, Eq)

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
    } deriving stock (Show, Eq)

-- | Initial key distribution message sent when a member joins a group
-- or rotates their sender key.
data SenderKeyDistributionMessage = SenderKeyDistributionMessage
    { skdSenderId   :: !ByteString
      -- ^ Sender identifier.
    , skdChainKey   :: !ByteString
      -- ^ Initial 32-byte chain key.
    , skdIteration  :: !Word32
      -- ^ Starting iteration (usually 0).
    , skdSigningKey :: !ByteString
      -- ^ 32-byte signing/identity key.
    } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- KDF helper (same pattern as DoubleRatchet.kdfCK)
------------------------------------------------------------------------

-- | Derive a message key and new chain key from the current chain key.
--
-- messageKey  = HMAC-SHA256(chainKey, 0x01)
-- newChainKey = HMAC-SHA256(chainKey, 0x02)
senderKdfCK :: ByteString -> (ByteString, ByteString)
senderKdfCK chainKey =
    let !msgKey      = hmacSHA256 chainKey (BS.singleton 0x01)
        !newChainKey = hmacSHA256 chainKey (BS.singleton 0x02)
    in (newChainKey, msgKey)

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
-- Creates a fresh chain key and signing key for this sender.  The
-- returned 'SenderKeyState' is the sender's own state; the
-- 'SenderKeyDistributionMessage' should be sent (encrypted via
-- pairwise Double Ratchet sessions) to every group member.
createSenderKeyDistribution
    :: ByteString  -- ^ Sender identifier
    -> IO (SenderKeyState, SenderKeyDistributionMessage)
createSenderKeyDistribution senderId = do
    chainKey   <- randomBytes 32
    signingKey <- randomBytes 32
    let st = SenderKeyState
            { sksSenderId    = senderId
            , sksChainKey    = chainKey
            , sksIteration   = 0
            , sksSigningKey  = signingKey
            , sksSkippedKeys = Map.empty
            , sksSkipSeq     = 0
            }
        dist = SenderKeyDistributionMessage
            { skdSenderId   = senderId
            , skdChainKey   = chainKey
            , skdIteration  = 0
            , skdSigningKey = signingKey
            }
    pure (st, dist)

-- | Process a received sender key distribution message.
--
-- Returns a 'SenderKeyState' that can be used to decrypt future
-- messages from this sender.  The caller is responsible for storing
-- this state indexed by sender ID.
processSenderKeyDistribution
    :: SenderKeyDistributionMessage
    -> Either SenderKeyError SenderKeyState
processSenderKeyDistribution dist
    | BS.length (skdChainKey dist) /= 32
    = Left (InvalidDistribution "chain key must be 32 bytes")
    | BS.length (skdSigningKey dist) /= 32
    = Left (InvalidDistribution "signing key must be 32 bytes")
    | BS.null (skdSenderId dist)
    = Left (InvalidDistribution "sender ID must not be empty")
    | otherwise
    = Right SenderKeyState
        { sksSenderId    = skdSenderId dist
        , sksChainKey    = skdChainKey dist
        , sksIteration   = skdIteration dist
        , sksSigningKey  = skdSigningKey dist
        , sksSkippedKeys = Map.empty
        , sksSkipSeq     = 0
        }

------------------------------------------------------------------------
-- Encryption
------------------------------------------------------------------------

-- | Encrypt a plaintext message for the group using the sender's key chain.
--
-- Advances the chain by one step (kdfCK), encrypts with AES-256-GCM,
-- and returns the updated state and the sender key message.
encryptSenderKey
    :: SenderKeyState
    -> ByteString            -- ^ Plaintext
    -> Either SenderKeyError (SenderKeyState, SenderKeyMessage)
encryptSenderKey st plaintext
    | sksIteration st >= 0xFFFFFFFE
    = Left ChainExhausted
    | otherwise =
        let !(newChainKey, msgKey) = senderKdfCK (sksChainKey st)
            !nonce = makeSenderNonce (sksChainKey st) (sksIteration st)
            -- AAD = senderId || signingKey || iteration (4 bytes BE)
            !aad = sksSenderId st
                <> sksSigningKey st
                <> encodeWord32BE (sksIteration st)
            !(ct, tag) = gcmEncrypt msgKey nonce aad plaintext
            !st' = st
                { sksChainKey  = newChainKey
                , sksIteration = sksIteration st + 1
                }
            !msg = SenderKeyMessage
                { skmSenderId   = sksSenderId st
                , skmIteration  = sksIteration st
                , skmCiphertext = ct
                , skmTag        = tag
                }
        in Right (st', msg)

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
decryptSenderKey
    :: SenderKeyState
    -> SenderKeyMessage
    -> Word64               -- ^ Current POSIX time (seconds)
    -> Either SenderKeyError (SenderKeyState, ByteString)
decryptSenderKey st msg nowSecs
    | skmSenderId msg /= sksSenderId st
    = Left (UnknownSender (skmSenderId msg))
    | skmIteration msg < sksIteration st
    = trySkippedSenderKeys st msg nowSecs
    | skmIteration msg - sksIteration st > maxSenderKeySkip
    = Left ChainTooFarAhead
    | otherwise =
        -- Store intermediate keys in the skipped-key cache, then
        -- decrypt the target message.
        let !(advancedChainKey, targetChainKey, targetMsgKey, skipped) =
                advanceChain (sksChainKey st) (sksIteration st) (skmIteration msg)
                             (sksSenderId st) (sksSkippedKeys st) (sksSkipSeq st) nowSecs
            !nonce = makeSenderNonce targetChainKey (skmIteration msg)
            !aad = sksSenderId st
                <> sksSigningKey st
                <> encodeWord32BE (skmIteration msg)
        in case gcmDecrypt targetMsgKey nonce aad (skmCiphertext msg) (skmTag msg) of
            Nothing -> Left DecryptionFailed
            Just plaintext ->
                let !evicted = evictSkippedSenderKeys nowSecs skipped
                    !st' = st
                        { sksChainKey    = advancedChainKey
                        , sksIteration   = skmIteration msg + 1
                        , sksSkippedKeys = evicted
                        , sksSkipSeq     = sksSkipSeq st + fromIntegral (skmIteration msg - sksIteration st)
                        }
                in Right (st', plaintext)

-- | Try to decrypt using a previously cached skipped message key.
-- Evicts expired entries (older than 48 hours) before lookup.
trySkippedSenderKeys :: SenderKeyState -> SenderKeyMessage -> Word64
                     -> Either SenderKeyError (SenderKeyState, ByteString)
trySkippedSenderKeys st msg nowSecs =
    let !lookupKey = (sksSenderId st, skmIteration msg)
        -- Evict expired entries
        !pruned = Map.filter (\(_, _, ts) ->
            nowSecs <= ts || (nowSecs - ts) <= skippedSenderKeyMaxAgeSecs) (sksSkippedKeys st)
    in case Map.lookup lookupKey pruned of
        Nothing -> Left DecryptionFailed
        Just (msgKey, chainKey, _insertTime) ->
            let !nonce = makeSenderNonce chainKey (skmIteration msg)
                !aad = sksSenderId st
                    <> sksSigningKey st
                    <> encodeWord32BE (skmIteration msg)
            in case gcmDecrypt msgKey nonce aad (skmCiphertext msg) (skmTag msg) of
                Nothing -> Left DecryptionFailed
                Just plaintext ->
                    let !st' = st { sksSkippedKeys = Map.delete lookupKey pruned }
                    in Right (st', plaintext)

-- | Advance the chain from @currentIter@ to @targetIter@, storing
-- intermediate message keys in the skipped-key cache.
-- Returns (newChainKey after target, chainKey at target, msgKey at target, updatedSkippedKeys).
advanceChain :: ByteString -> Word32 -> Word32
             -> ByteString
             -> Map (ByteString, Word32) (ByteString, ByteString, Word64)
             -> Word64 -> Word64
             -> (ByteString, ByteString, ByteString, Map (ByteString, Word32) (ByteString, ByteString, Word64))
advanceChain chainKey currentIter targetIter senderId skipped skipSeq nowSecs =
    go chainKey currentIter skipped skipSeq
  where
    go !ck !i !sk !sq
        | i == targetIter =
            let !(newCK, msgKey) = senderKdfCK ck
            in (newCK, ck, msgKey, sk)
        | otherwise =
            let !(newCK, msgKey) = senderKdfCK ck
                !key = (senderId, i)
                !sk' = Map.insert key (msgKey, ck, nowSecs) sk
            in go newCK (i + 1) sk' (sq + 1)

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

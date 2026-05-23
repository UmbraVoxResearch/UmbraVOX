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
            { sksSenderId   = senderId
            , sksChainKey   = chainKey
            , sksIteration  = 0
            , sksSigningKey = signingKey
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
        { sksSenderId   = skdSenderId dist
        , sksChainKey   = skdChainKey dist
        , sksIteration  = skdIteration dist
        , sksSigningKey = skdSigningKey dist
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
-- (up to 'maxSenderKeySkip' steps) to reach the correct message key.
-- Returns the updated state and plaintext on success.
decryptSenderKey
    :: SenderKeyState
    -> SenderKeyMessage
    -> Either SenderKeyError (SenderKeyState, ByteString)
decryptSenderKey st msg
    | skmSenderId msg /= sksSenderId st
    = Left (UnknownSender (skmSenderId msg))
    | skmIteration msg < sksIteration st
    = Left DecryptionFailed  -- cannot go backwards in chain
    | skmIteration msg - sksIteration st > maxSenderKeySkip
    = Left ChainTooFarAhead
    | otherwise =
        -- Advance chain to the target iteration
        let !(advancedChainKey, targetChainKey, targetMsgKey) =
                advanceChain (sksChainKey st) (sksIteration st) (skmIteration msg)
            !nonce = makeSenderNonce targetChainKey (skmIteration msg)
            !aad = sksSenderId st
                <> sksSigningKey st
                <> encodeWord32BE (skmIteration msg)
        in case gcmDecrypt targetMsgKey nonce aad (skmCiphertext msg) (skmTag msg) of
            Nothing -> Left DecryptionFailed
            Just plaintext ->
                let !st' = st
                        { sksChainKey  = advancedChainKey
                        , sksIteration = skmIteration msg + 1
                        }
                in Right (st', plaintext)

-- | Advance the chain from @currentIter@ to @targetIter@, returning
-- (newChainKey after target, chainKey at target, msgKey at target).
advanceChain :: ByteString -> Word32 -> Word32 -> (ByteString, ByteString, ByteString)
advanceChain chainKey currentIter targetIter = go chainKey currentIter
  where
    go !ck !i
        | i == targetIter =
            let !(newCK, msgKey) = senderKdfCK ck
            in (newCK, ck, msgKey)
        | otherwise =
            let !(newCK, _msgKey) = senderKdfCK ck
            in go newCK (i + 1)

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

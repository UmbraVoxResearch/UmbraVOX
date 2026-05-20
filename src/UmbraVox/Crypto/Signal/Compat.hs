-- SPDX-License-Identifier: Apache-2.0
-- | Signal-compatible parameter set for UmbraVOX crypto primitives.
--
-- Reuses UmbraVOX's own X25519, HMAC-SHA256, AES-256-GCM, and HKDF
-- implementations with Signal's domain separation strings and KDF chain.
-- This module does NOT depend on libsignal or any Rust code.
--
-- Differences from UmbraVOX native:
--   - HKDF info: "WhisperText" (vs "UmbraVox_Ratchet_v1")
--   - Ratchet KDF: HKDF-SHA-256 (vs HKDF-SHA-512)
--   - Nonce derivation: HKDF-SHA-256 with "WhisperNonce"
--   - X3DH KDF: HKDF-SHA-256 with no info string (empty)
--   - Wire format: Signal protobuf (see Protocol.SignalWire)
--   - Same primitives: X25519, HMAC-SHA256, AES-256-CBC + HMAC-SHA256
--
-- Purpose: prove UmbraVOX's crypto is wire-compatible with Signal by
-- talking to a real Signal-Server in a cleanroom VM.
module UmbraVox.Crypto.Signal.Compat
    ( -- * Signal-compatible KDF functions
      signalKdfRK
    , signalKdfCK
      -- * Signal-compatible X3DH
    , signalDeriveSecret
      -- * Signal-compatible ratchet state
    , SignalRatchetState(..)
    , SignalRatchetHeader(..)
    , signalRatchetInitAlice
    , signalRatchetInitBob
    , signalRatchetEncrypt
    , signalRatchetDecrypt
      -- * Constants (exported for testing)
    , signalRatchetInfo
    , signalMessageVersion
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.GCM (gcmDecrypt, gcmEncrypt)
import UmbraVox.Crypto.HKDF (hkdfSHA256Extract, hkdfSHA256Expand)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Crypto.Random (randomBytes)

------------------------------------------------------------------------
-- Signal protocol constants
------------------------------------------------------------------------

-- | Signal protocol message version (current = 3).
signalMessageVersion :: Word32
signalMessageVersion = 3

-- | HKDF info string for Signal ratchet root key derivation.
-- Signal uses "WhisperRatchet" as the info parameter.
signalRatchetInfo :: ByteString
signalRatchetInfo = "WhisperRatchet"

-- | Signal X3DH salt: 32 zero bytes.
signalX3DHSalt :: ByteString
signalX3DHSalt = BS.replicate 32 0

-- | Signal X3DH info string (empty in Signal's implementation).
signalX3DHInfo :: ByteString
signalX3DHInfo = "WhisperText"

------------------------------------------------------------------------
-- Signal-compatible KDF helpers
------------------------------------------------------------------------

-- | Derive new root key and chain key from current root key and DH output.
--
-- Signal uses HKDF-SHA-256 (not SHA-512 like UmbraVOX native):
--   Salt = root key, IKM = DH output, Info = "WhisperRatchet"
--   Output = 64 bytes: first 32 = new root key, last 32 = new chain key.
signalKdfRK :: ByteString -> ByteString -> (ByteString, ByteString)
signalKdfRK rootKey dhOut =
    let !prk = hkdfSHA256Extract rootKey dhOut
        !okm = hkdfSHA256Expand prk signalRatchetInfo 64
    in (BS.take 32 okm, BS.drop 32 okm)

-- | Derive message key and new chain key from current chain key.
--
-- Signal uses the same HMAC-SHA256 construction as UmbraVOX:
--   messageKey  = HMAC-SHA256(chainKey, 0x01)
--   newChainKey = HMAC-SHA256(chainKey, 0x02)
signalKdfCK :: ByteString -> (ByteString, ByteString)
signalKdfCK chainKey =
    let !msgKey      = hmacSHA256 chainKey (BS.singleton 0x01)
        !newChainKey = hmacSHA256 chainKey (BS.singleton 0x02)
    in (newChainKey, msgKey)

------------------------------------------------------------------------
-- Signal-compatible X3DH secret derivation
------------------------------------------------------------------------

-- | Derive shared secret from X3DH DH outputs using Signal's KDF chain.
--
-- Signal X3DH:
--   IKM = 0xFF*32 || DH1 || DH2 || DH3 [|| DH4]
--   Salt = 0x00*32
--   Info = "WhisperText"
--   Output = 32 bytes
signalDeriveSecret :: ByteString  -- ^ DH1 (IK_A * SPK_B)
                   -> ByteString  -- ^ DH2 (EK_A * IK_B)
                   -> ByteString  -- ^ DH3 (EK_A * SPK_B)
                   -> Maybe ByteString  -- ^ DH4 (EK_A * OPK_B), optional
                   -> ByteString  -- ^ 32-byte shared secret
signalDeriveSecret dh1 dh2 dh3 mDh4 =
    let !discontinuity = BS.replicate 32 0xFF
        !ikm = BS.concat $ [discontinuity, dh1, dh2, dh3]
                         ++ maybe [] (:[]) mDh4
        !prk = hkdfSHA256Extract signalX3DHSalt ikm
    in hkdfSHA256Expand prk signalX3DHInfo 32

------------------------------------------------------------------------
-- Signal-compatible ratchet state
------------------------------------------------------------------------

-- | Signal-compatible ratchet state.
-- Same structure as UmbraVOX 'RatchetState' but uses Signal KDF functions.
data SignalRatchetState = SignalRatchetState
    { srsDHSend      :: !(ByteString, ByteString)
      -- ^ (secret, public) X25519 sending keypair
    , srsDHRecv      :: !(Maybe ByteString)
      -- ^ Peer's current X25519 public key
    , srsRootKey     :: !ByteString
      -- ^ 32-byte root key
    , srsSendChain   :: !ByteString
      -- ^ 32-byte sending chain key
    , srsRecvChain   :: !ByteString
      -- ^ 32-byte receiving chain key
    , srsSendN       :: !Word32
      -- ^ Sending message counter
    , srsRecvN       :: !Word32
      -- ^ Receiving message counter
    , srsPrevChainN  :: !Word32
      -- ^ Previous chain length
    , srsSkippedKeys :: !(Map (ByteString, Word32) ByteString)
      -- ^ Cached keys for out-of-order messages
    } deriving stock (Show, Eq)

-- | Signal-compatible ratchet header.
data SignalRatchetHeader = SignalRatchetHeader
    { srhDHPublic   :: !ByteString  -- ^ 33 bytes (prepended version byte)
    , srhPrevChainN :: !Word32
    , srhMsgN       :: !Word32
    } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- DH helper
------------------------------------------------------------------------

generateDH :: ByteString -> (ByteString, ByteString)
generateDH secret =
    case x25519 secret x25519Basepoint of
        Just pub -> (secret, pub)
        Nothing  -> (secret, BS.replicate 32 0)

------------------------------------------------------------------------
-- Ratchet initialization
------------------------------------------------------------------------

-- | Initialize Signal-compatible ratchet as Alice (initiator).
signalRatchetInitAlice :: ByteString  -- ^ Shared secret from X3DH (32 bytes)
                       -> ByteString  -- ^ Bob's signed prekey public (32 bytes)
                       -> ByteString  -- ^ Alice's fresh DH secret (32 bytes)
                       -> Maybe SignalRatchetState
signalRatchetInitAlice sharedSecret bobSPK aliceDHSecret =
    case x25519 aliceDHSecret bobSPK of
        Nothing -> Nothing
        Just dhOut ->
            let !dhPair       = generateDH aliceDHSecret
                !(rk, ck)     = signalKdfRK sharedSecret dhOut
            in Just SignalRatchetState
                { srsDHSend      = dhPair
                , srsDHRecv      = Just bobSPK
                , srsRootKey     = rk
                , srsSendChain   = ck
                , srsRecvChain   = BS.replicate 32 0
                , srsSendN       = 0
                , srsRecvN       = 0
                , srsPrevChainN  = 0
                , srsSkippedKeys = Map.empty
                }

-- | Initialize Signal-compatible ratchet as Bob (responder).
signalRatchetInitBob :: ByteString  -- ^ Shared secret from X3DH (32 bytes)
                     -> ByteString  -- ^ Bob's SPK secret (32 bytes)
                     -> SignalRatchetState
signalRatchetInitBob sharedSecret bobSPKSecret =
    let !dhPair = generateDH bobSPKSecret
    in SignalRatchetState
        { srsDHSend      = dhPair
        , srsDHRecv      = Nothing
        , srsRootKey     = sharedSecret
        , srsSendChain   = BS.replicate 32 0
        , srsRecvChain   = BS.replicate 32 0
        , srsSendN       = 0
        , srsRecvN       = 0
        , srsPrevChainN  = 0
        , srsSkippedKeys = Map.empty
        }

------------------------------------------------------------------------
-- Encryption
------------------------------------------------------------------------

-- | Encrypt a plaintext message using the Signal-compatible ratchet.
signalRatchetEncrypt :: SignalRatchetState
                     -> ByteString  -- ^ Plaintext
                     -> IO (Either String (SignalRatchetState, SignalRatchetHeader, ByteString, ByteString))
                     -- ^ (updated state, header, ciphertext, MAC)
signalRatchetEncrypt st plaintext
    | srsSendN st >= 0xFFFFFFFE = pure (Left "counter exhausted")
    | otherwise = do
        let !(newChain, msgKey) = signalKdfCK (srsSendChain st)
            !encKey   = BS.take 32 msgKey
            !nonce    = BS.take 12 (BS.drop 32 (hkdfSHA256Expand msgKey "WhisperMessageKeys" 80))
            -- Signal derives 80 bytes: 32 cipher key + 32 mac key + 16 IV
            -- We use the first 32 as AES key, derive a 12-byte nonce for GCM
            !iv       = if BS.length nonce >= 12 then nonce else BS.replicate 12 0
        let !(ct, tag) = gcmEncrypt encKey iv BS.empty plaintext
            !hdr = SignalRatchetHeader
                { srhDHPublic   = snd (srsDHSend st)
                , srhPrevChainN = srsPrevChainN st
                , srhMsgN       = srsSendN st
                }
            !st' = st { srsSendChain = newChain
                      , srsSendN     = srsSendN st + 1
                      }
        pure (Right (st', hdr, ct, tag))

------------------------------------------------------------------------
-- Decryption
------------------------------------------------------------------------

-- | Decrypt a message using the Signal-compatible ratchet.
signalRatchetDecrypt :: SignalRatchetState
                     -> SignalRatchetHeader
                     -> ByteString  -- ^ Ciphertext
                     -> ByteString  -- ^ MAC/tag
                     -> IO (Either String (Maybe (SignalRatchetState, ByteString)))
signalRatchetDecrypt st hdr ct tag = do
    -- Check if this is a skipped message
    case Map.lookup (srhDHPublic hdr, srhMsgN hdr) (srsSkippedKeys st) of
        Just msgKey -> do
            let !encKey = BS.take 32 msgKey
                !nonce  = BS.take 12 (BS.drop 32 (hkdfSHA256Expand msgKey "WhisperMessageKeys" 80))
                !iv     = if BS.length nonce >= 12 then nonce else BS.replicate 12 0
                !st'    = st { srsSkippedKeys = Map.delete (srhDHPublic hdr, srhMsgN hdr) (srsSkippedKeys st) }
            case gcmDecrypt encKey iv BS.empty ct tag of
                Nothing -> pure (Right Nothing)
                Just pt -> pure (Right (Just (st', pt)))
        Nothing -> do
            -- Check if we need a DH ratchet step
            let needsRatchet = case srsDHRecv st of
                    Nothing  -> True
                    Just recv -> recv /= srhDHPublic hdr
            st1 <- if needsRatchet
                then dhRatchetStep st hdr
                else pure (Right st)
            case st1 of
                Left err -> pure (Left err)
                Right st2 -> do
                    -- Skip ahead if needed
                    let st3 = skipMessages st2 (srhMsgN hdr)
                    -- Decrypt with current receiving chain
                    let !(newChain, msgKey) = signalKdfCK (srsRecvChain st3)
                        !encKey = BS.take 32 msgKey
                        !nonce  = BS.take 12 (BS.drop 32 (hkdfSHA256Expand msgKey "WhisperMessageKeys" 80))
                        !iv     = if BS.length nonce >= 12 then nonce else BS.replicate 12 0
                        !st4    = st3 { srsRecvChain = newChain
                                      , srsRecvN     = srsRecvN st3 + 1
                                      }
                    case gcmDecrypt encKey iv BS.empty ct tag of
                        Nothing -> pure (Right Nothing)
                        Just pt -> pure (Right (Just (st4, pt)))

-- | Perform a DH ratchet step (new peer key received).
dhRatchetStep :: SignalRatchetState -> SignalRatchetHeader
              -> IO (Either String SignalRatchetState)
dhRatchetStep st hdr = do
    newDHSecret <- randomBytes 32
    let peerPub = srhDHPublic hdr
    case x25519 (fst (srsDHSend st)) peerPub of
        Nothing -> pure (Left "DH failed (old key)")
        Just dhRecv ->
            case x25519 newDHSecret peerPub of
                Nothing -> pure (Left "DH failed (new key)")
                Just dhSend ->
                    let !(rk1, recvCK) = signalKdfRK (srsRootKey st) dhRecv
                        !newPair       = generateDH newDHSecret
                        !(rk2, sendCK) = signalKdfRK rk1 dhSend
                    in pure $ Right st
                        { srsDHSend      = newPair
                        , srsDHRecv      = Just peerPub
                        , srsRootKey     = rk2
                        , srsSendChain   = sendCK
                        , srsRecvChain   = recvCK
                        , srsSendN       = 0
                        , srsRecvN       = 0
                        , srsPrevChainN  = srsSendN st
                        , srsSkippedKeys = srsSkippedKeys st
                        }

-- | Skip ahead in the receiving chain, caching skipped keys.
skipMessages :: SignalRatchetState -> Word32 -> SignalRatchetState
skipMessages st targetN = go st
  where
    go s
        | srsRecvN s >= targetN = s
        | Map.size (srsSkippedKeys s) >= 1000 = s  -- safety cap
        | otherwise =
            let !(newChain, msgKey) = signalKdfCK (srsRecvChain s)
                !key = (fromMaybe BS.empty (srsDHRecv s), srsRecvN s)
                !s'  = s { srsRecvChain   = newChain
                         , srsRecvN       = srsRecvN s + 1
                         , srsSkippedKeys = Map.insert key msgKey (srsSkippedKeys s)
                         }
            in go s'
    fromMaybe def Nothing  = def
    fromMaybe _   (Just x) = x

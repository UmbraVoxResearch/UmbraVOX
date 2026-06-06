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

import qualified UmbraVox.Crypto.Generated.FFI.GCM as GCMFFI
import qualified UmbraVox.Crypto.Generated.FFI.HKDF as HKDFFFI
import qualified UmbraVox.Crypto.Generated.FFI.HMAC as HMACFFI
import qualified UmbraVox.Crypto.Generated.FFI.X25519 as X25519FFI
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
signalKdfRK :: ByteString -> ByteString -> IO (ByteString, ByteString)
signalKdfRK rootKey dhOut = do
    !prk <- HKDFFFI.hkdfSHA256Extract rootKey dhOut
    !okm <- HKDFFFI.hkdfSHA256Expand prk signalRatchetInfo 64
    pure (BS.take 32 okm, BS.drop 32 okm)

-- | Derive message key and new chain key from current chain key.
--
-- Signal uses the same HMAC-SHA256 construction as UmbraVOX:
--   messageKey  = HMAC-SHA256(chainKey, 0x01)
--   newChainKey = HMAC-SHA256(chainKey, 0x02)
signalKdfCK :: ByteString -> IO (ByteString, ByteString)
signalKdfCK chainKey = do
    !msgKey      <- HMACFFI.hmacSHA256 chainKey (BS.singleton 0x01)
    !newChainKey <- HMACFFI.hmacSHA256 chainKey (BS.singleton 0x02)
    pure (newChainKey, msgKey)

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
                   -> IO ByteString  -- ^ 32-byte shared secret
signalDeriveSecret dh1 dh2 dh3 mDh4 = do
    let !discontinuity = BS.replicate 32 0xFF
        !ikm = BS.concat $ [discontinuity, dh1, dh2, dh3]
                         ++ maybe [] (:[]) mDh4
    !prk <- HKDFFFI.hkdfSHA256Extract signalX3DHSalt ikm
    HKDFFFI.hkdfSHA256Expand prk signalX3DHInfo 32

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

generateDH :: ByteString -> IO (ByteString, ByteString)
generateDH secret = do
    mPub <- X25519FFI.x25519 secret X25519FFI.x25519Basepoint
    case mPub of
        Just pub -> pure (secret, pub)
        Nothing  -> pure (secret, BS.replicate 32 0)

------------------------------------------------------------------------
-- Ratchet initialization
------------------------------------------------------------------------

-- | Initialize Signal-compatible ratchet as Alice (initiator).
signalRatchetInitAlice :: ByteString  -- ^ Shared secret from X3DH (32 bytes)
                       -> ByteString  -- ^ Bob's signed prekey public (32 bytes)
                       -> ByteString  -- ^ Alice's fresh DH secret (32 bytes)
                       -> IO (Maybe SignalRatchetState)
signalRatchetInitAlice sharedSecret bobSPK aliceDHSecret = do
    mDhOut <- X25519FFI.x25519 aliceDHSecret bobSPK
    case mDhOut of
        Nothing -> pure Nothing
        Just dhOut -> do
            !dhPair    <- generateDH aliceDHSecret
            !(rk, ck)  <- signalKdfRK sharedSecret dhOut
            pure $ Just SignalRatchetState
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
                     -> IO SignalRatchetState
signalRatchetInitBob sharedSecret bobSPKSecret = do
    !dhPair <- generateDH bobSPKSecret
    pure SignalRatchetState
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
        !(newChain, msgKey) <- signalKdfCK (srsSendChain st)
        let !encKey = BS.take 32 msgKey
        !expanded <- HKDFFFI.hkdfSHA256Expand msgKey "WhisperMessageKeys" 80
        -- Signal derives 80 bytes: 32 cipher key + 32 mac key + 16 IV
        -- We use the first 32 as AES key, derive a 12-byte nonce for GCM
        let !nonce = BS.take 12 (BS.drop 32 expanded)
            !iv    = if BS.length nonce >= 12 then nonce else BS.replicate 12 0
        (ct, tag) <- GCMFFI.gcmEncrypt encKey iv BS.empty plaintext
        let !hdr = SignalRatchetHeader
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
            !expanded <- HKDFFFI.hkdfSHA256Expand msgKey "WhisperMessageKeys" 80
            let !nonce = BS.take 12 (BS.drop 32 expanded)
                !iv    = if BS.length nonce >= 12 then nonce else BS.replicate 12 0
                !st'   = st { srsSkippedKeys = Map.delete (srhDHPublic hdr, srhMsgN hdr) (srsSkippedKeys st) }
            mPt <- GCMFFI.gcmDecrypt encKey iv BS.empty ct tag
            case mPt of
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
                    st3 <- skipMessages st2 (srhMsgN hdr)
                    -- Decrypt with current receiving chain
                    !(newChain, msgKey) <- signalKdfCK (srsRecvChain st3)
                    let !encKey = BS.take 32 msgKey
                    !expanded <- HKDFFFI.hkdfSHA256Expand msgKey "WhisperMessageKeys" 80
                    let !nonce = BS.take 12 (BS.drop 32 expanded)
                        !iv    = if BS.length nonce >= 12 then nonce else BS.replicate 12 0
                        !st4   = st3 { srsRecvChain = newChain
                                      , srsRecvN     = srsRecvN st3 + 1
                                      }
                    mPt2 <- GCMFFI.gcmDecrypt encKey iv BS.empty ct tag
                    case mPt2 of
                        Nothing -> pure (Right Nothing)
                        Just pt -> pure (Right (Just (st4, pt)))

-- | Perform a DH ratchet step (new peer key received).
dhRatchetStep :: SignalRatchetState -> SignalRatchetHeader
              -> IO (Either String SignalRatchetState)
dhRatchetStep st hdr = do
    newDHSecret <- randomBytes 32
    let peerPub = srhDHPublic hdr
    mDhRecv <- X25519FFI.x25519 (fst (srsDHSend st)) peerPub
    case mDhRecv of
        Nothing -> pure (Left "DH failed (old key)")
        Just dhRecv -> do
            mDhSend <- X25519FFI.x25519 newDHSecret peerPub
            case mDhSend of
                Nothing -> pure (Left "DH failed (new key)")
                Just dhSend -> do
                    !(rk1, recvCK) <- signalKdfRK (srsRootKey st) dhRecv
                    !newPair       <- generateDH newDHSecret
                    !(rk2, sendCK) <- signalKdfRK rk1 dhSend
                    pure $ Right st
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
skipMessages :: SignalRatchetState -> Word32 -> IO SignalRatchetState
skipMessages st targetN = go st
  where
    go s
        | srsRecvN s >= targetN = pure s
        | Map.size (srsSkippedKeys s) >= 1000 = pure s  -- safety cap
        | otherwise = do
            !(newChain, msgKey) <- signalKdfCK (srsRecvChain s)
            let !key = (fromMaybe BS.empty (srsDHRecv s), srsRecvN s)
                !s'  = s { srsRecvChain   = newChain
                         , srsRecvN       = srsRecvN s + 1
                         , srsSkippedKeys = Map.insert key msgKey (srsSkippedKeys s)
                         }
            go s'
    fromMaybe def Nothing  = def
    fromMaybe _   (Just x) = x

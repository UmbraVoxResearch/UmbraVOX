-- | {-# REQ "SIGNAL-002" #-} Signal Double Ratchet Algorithm
--
-- Implements the Double Ratchet Algorithm for forward-secure end-to-end
-- encrypted messaging. Each message uses a unique key derived via symmetric
-- and Diffie-Hellman ratchet steps, providing forward secrecy and
-- break-in recovery.
--
-- See: doc/03-cryptography.md
module UmbraVox.Crypto.Signal.DoubleRatchet
    ( RatchetState(..)
    , RatchetHeader(..)
    , ratchetInitAlice
    , ratchetInitBob
    , ratchetEncrypt
    , ratchetDecrypt
    ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import UmbraVox.Crypto.HKDF (hkdfExtract, hkdfExpand)
import UmbraVox.Crypto.HMAC (hmacSHA256)

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
    , rsSkippedKeys :: !(Map (ByteString, Word32) ByteString)
      -- ^ Skipped message keys indexed by (DH public key, counter)
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

-- | Maximum number of skipped message keys to store.
maxSkip :: Word32
maxSkip = 1000

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
generateDH :: ByteString -> (ByteString, ByteString)
generateDH secret =
    let !pub = x25519 secret x25519Basepoint
    in (secret, pub)

-- | Perform X25519 Diffie-Hellman.
dh :: (ByteString, ByteString) -> ByteString -> ByteString
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
ratchetInitAlice :: ByteString  -- ^ X3DH shared secret (32 bytes)
                 -> ByteString  -- ^ Bob's signed pre-key (32 bytes)
                 -> ByteString  -- ^ Alice's fresh DH secret (32 bytes)
                 -> RatchetState
ratchetInitAlice sharedSecret bobSPK aliceDHSecret =
    let !aliceKP = generateDH aliceDHSecret
        -- First DH ratchet: derive recv chain key (not used until Bob sends)
        -- then derive send chain key with our new keypair
        !dhOutput = dh aliceKP bobSPK
        !(rootKey1, sendChain) = kdfRK sharedSecret dhOutput
    in RatchetState
        { rsDHSend      = aliceKP
        , rsDHRecv      = Just bobSPK
        , rsRootKey     = rootKey1
        , rsSendChain   = sendChain
        , rsRecvChain   = BS.replicate 32 0  -- Will be set on first DH ratchet from Bob
        , rsSendN       = 0
        , rsRecvN       = 0
        , rsPrevChainN  = 0
        , rsSkippedKeys = Map.empty
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
        , rsSendChain   = BS.replicate 32 0  -- Will be set on first DH ratchet
        , rsRecvChain   = BS.replicate 32 0  -- Will be set on first DH ratchet
        , rsSendN       = 0
        , rsRecvN       = 0
        , rsPrevChainN  = 0
        , rsSkippedKeys = Map.empty
        }

------------------------------------------------------------------------
-- Encryption
------------------------------------------------------------------------

-- | Encrypt a plaintext message, advancing the sending ratchet.
--
-- Returns the updated state, header, ciphertext, and 16-byte GCM tag.
ratchetEncrypt :: RatchetState
               -> ByteString          -- ^ Plaintext
               -> (RatchetState, RatchetHeader, ByteString, ByteString)
ratchetEncrypt st plaintext =
    let -- Derive message key from sending chain
        !(newChainKey, msgKey) = kdfCK (rsSendChain st)
        -- Build header
        !header = RatchetHeader
            { rhDHPublic   = snd (rsDHSend st)
            , rhPrevChainN = rsPrevChainN st
            , rhMsgN       = rsSendN st
            }
        -- Build nonce: 4 bytes from HMAC + 4 bytes counter + 4 zero bytes
        !nonce = makeNonce msgKey (rsSendN st)
        -- Encrypt with AES-256-GCM
        !aad = encodeHeader header
        !(ct, tag) = gcmEncrypt msgKey nonce aad plaintext
        -- Update state
        !st' = st
            { rsSendChain = newChainKey
            , rsSendN     = rsSendN st + 1
            }
    in (st', header, ct, tag)

------------------------------------------------------------------------
-- Decryption
------------------------------------------------------------------------

-- | Decrypt a received message. Returns Nothing if authentication fails.
ratchetDecrypt :: RatchetState
               -> RatchetHeader       -- ^ Message header
               -> ByteString          -- ^ Ciphertext
               -> ByteString          -- ^ GCM tag (16 bytes)
               -> Maybe (RatchetState, ByteString)
ratchetDecrypt st header ct tag =
    -- Try skipped keys first
    case trySkippedKeys st header ct tag of
        Just result -> Just result
        Nothing ->
            -- If header DH key differs from our stored peer key, do DH ratchet
            let !st1 = case rsDHRecv st of
                           Nothing    -> dhRatchet st header
                           Just peer
                               | rhDHPublic header /= peer -> dhRatchet st header
                               | otherwise                 -> Just st
            in case st1 of
                Nothing -> Nothing  -- Too many skipped keys
                Just st2 ->
                    -- Skip any missed messages in current receiving chain
                    case skipMessageKeys st2 (rhMsgN header) of
                        Nothing -> Nothing  -- Too many skipped keys
                        Just st3 ->
                            -- Derive message key from receiving chain
                            let !(newChainKey, msgKey) = kdfCK (rsRecvChain st3)
                                !nonce = makeNonce msgKey (rsRecvN st3)
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
dhRatchet :: RatchetState -> RatchetHeader -> Maybe RatchetState
dhRatchet st header =
    -- Skip any remaining messages in old receiving chain
    case skipMessageKeys st (rhPrevChainN header) of
        Nothing -> Nothing
        Just st1 ->
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
                !dhOutput1 = dh (rsDHSend st2) peerPub
                !(rootKey1, recvChain) = kdfRK (rsRootKey st2) dhOutput1
                -- Generate new sending keypair
                -- We derive a new DH secret deterministically from current state
                -- In production, this would use a CSPRNG. Here we derive from
                -- the root key for determinism.
                !newDHSecret = hmacSHA256 rootKey1 (snd (rsDHSend st2))
                !newDHKP = generateDH newDHSecret
                -- Derive new sending chain
                !dhOutput2 = dh newDHKP peerPub
                !(rootKey2, sendChain) = kdfRK rootKey1 dhOutput2
            in Just st2
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
        Just msgKey ->
            let !nonce = makeNonce msgKey (rhMsgN header)
                !aad = encodeHeader header
            in case gcmDecrypt msgKey nonce aad ct tag of
                Just plaintext ->
                    let !st' = st { rsSkippedKeys = Map.delete lookupKey (rsSkippedKeys st) }
                    in Just (st', plaintext)
                Nothing -> Nothing

-- | Skip message keys from current counter up to (but not including) @until@.
-- Stores derived keys in rsSkippedKeys. Returns Nothing if too many would be skipped.
skipMessageKeys :: RatchetState -> Word32 -> Maybe RatchetState
skipMessageKeys st until'
    | rsRecvN st >= until' = Just st
    | until' - rsRecvN st > maxSkip = Nothing
    | otherwise = Just (go st)
  where
    go s
        | rsRecvN s >= until' = s
        | otherwise =
            let !(newChainKey, msgKey) = kdfCK (rsRecvChain s)
                !peerKey = case rsDHRecv s of
                    Just pk -> pk
                    Nothing -> error "skipMessageKeys: rsDHRecv is Nothing"
                !key = (peerKey, rsRecvN s)
                !skipped = Map.insert key msgKey (rsSkippedKeys s)
                -- Evict keys older than 500 ratchet steps from current counter
                !currentN = rsRecvN s + 1
                !skipped' = Map.filterWithKey
                    (\(_, msgN) _ -> currentN <= msgN + 500)
                    skipped
            in go s
                { rsRecvChain   = newChainKey
                , rsRecvN       = rsRecvN s + 1
                , rsSkippedKeys = skipped'
                }

------------------------------------------------------------------------
-- Nonce and header encoding
------------------------------------------------------------------------

-- | Build a 12-byte GCM nonce from message key and counter.
--
-- Nonce = first 4 bytes of HMAC-SHA256(msgKey, "nonce")
--       + 4-byte big-endian message counter
--       + 4 zero bytes
makeNonce :: ByteString -> Word32 -> ByteString
makeNonce msgKey counter =
    let !h = hmacSHA256 msgKey "nonce"
        !prefix = BS.take 4 h
        !ctrBytes = encodeWord32BE counter
    in prefix <> ctrBytes <> BS.replicate 4 0

-- | Encode a Word32 as 4-byte big-endian.
encodeWord32BE :: Word32 -> ByteString
encodeWord32BE w = BS.pack
    [ fromIntegral (w `shiftR` 24 .&. 0xff)
    , fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR` 8  .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

-- | Encode a header as associated data for GCM authentication.
-- Format: DH public key (32) || prevChainN (4) || msgN (4) = 40 bytes.
encodeHeader :: RatchetHeader -> ByteString
encodeHeader hdr =
    rhDHPublic hdr
    <> encodeWord32BE (rhPrevChainN hdr)
    <> encodeWord32BE (rhMsgN hdr)

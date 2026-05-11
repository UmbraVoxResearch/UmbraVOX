-- SPDX-License-Identifier: Apache-2.0
-- | Noise_IK handshake protocol implementation.
--
-- Pattern:
--   -> e, es, s, ss
--   <- e, ee, se
--
-- After handshake, both sides share symmetric send/recv keys
-- derived via HKDF from the chained DH outputs.
--
-- See: doc/spec/network.md
module UmbraVox.Network.Noise.Handshake
  ( noiseHandshakeInitiator
  , noiseHandshakeResponder
  -- * Helpers (exported for testing)
  , hkdfCK
  , splitKeys
  , encryptWithKey
  , decryptWithKey
  , initHash
  , initCK
  , mixHash
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)

import UmbraVox.App.Defaults (maxFrameSize)
import UmbraVox.Crypto.ConstantTime (constantEq)
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.HKDF (hkdfSHA256Extract, hkdfSHA256Expand)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Crypto.Random (chacha20Encrypt, randomBytes)
import UmbraVox.Crypto.SHA256 (sha256)
import UmbraVox.Network.Noise.State
    ( NoiseState(..)
    , prologue
    , protocolName
    , packASCII
    )
import UmbraVox.Network.TransportClass (AnyTransport, anySend, anyRecv)
import UmbraVox.Protocol.Encoding (putWord32BE, getWord32BE)

------------------------------------------------------------------------
-- Handshake: Initiator
------------------------------------------------------------------------

-- | Perform the Noise_IK handshake as initiator.
--
-- The initiator already knows the responder's static public key.
-- Sends message 1, receives message 2, derives session keys.
--
-- The @trustCheck@ callback is invoked with the responder's static public
-- key after the handshake DH computations succeed, allowing the caller to
-- verify the responder's identity matches expectations (e.g. pinned key,
-- TOFU database). Return 'True' to accept, 'False' to abort.
noiseHandshakeInitiator
    :: ByteString   -- ^ Our static secret key (32 bytes)
    -> ByteString   -- ^ Our static public key (32 bytes)
    -> ByteString      -- ^ Responder's static public key (32 bytes)
    -> (ByteString -> IO Bool) -- ^ Trust check: verify responder's static key
    -> AnyTransport    -- ^ Network transport
    -> IO (Maybe NoiseState)
noiseHandshakeInitiator iStaticSec iStaticPub rStaticPub trustCheck transport = do
    -- M7.1.3 Identity binding: Both parties' static keys are bound into the
    -- handshake hash early, per the Noise IK pattern:
    --   - Responder's static key: mixed into h as a pre-message (h2 below)
    --   - Initiator's static key: encrypted under es key and mixed in msg 1
    -- This ensures the handshake transcript commits to both identities before
    -- the ephemeral key exchange completes, preventing identity misbinding.

    -- Initialize handshake hash: h = SHA256(protocolName padded to 32)
    let !h0 = initHash
    -- Mix in prologue
    let !h1 = mixHash h0 prologue
    -- Mix in responder's static public key (pre-message: <- s)
    let !h2 = mixHash h1 rStaticPub
    -- Initialize chaining key
    let !ck0 = initCK

    -- Generate ephemeral keypair; x25519 returns Nothing on all-zero (low-order point)
    eSec <- randomBytes 32
    case x25519 eSec x25519Basepoint of
      Nothing   -> pure Nothing
      Just !ePub -> do
        -- -> e: send ephemeral public, mix into hash
        let !h3 = mixHash h2 ePub

        -- -> es: DH(e, rs); reject all-zero (low-order point)
        case x25519 eSec rStaticPub of
          Nothing    -> pure Nothing
          Just !dhES -> do
            let (!ck1, !k1) = hkdfCK ck0 dhES

            -- -> s: encrypt and send initiator's static public key
            let !encStaticPub = encryptWithKey k1 h3 iStaticPub
            let !h4 = mixHash h3 encStaticPub

            -- -> ss: DH(s, rs); reject all-zero (low-order point)
            case x25519 iStaticSec rStaticPub of
              Nothing    -> pure Nothing
              Just !dhSS -> do
                let (!ck2, !_k2) = hkdfCK ck1 dhSS

                -- Build and send message 1: ePub || encStaticPub
                let !msg1 = ePub <> encStaticPub
                sendFrame transport msg1

                -- Receive message 2: rEPub
                mMsg2 <- recvFrame transport
                case mMsg2 of
                  Nothing -> pure Nothing
                  Just msg2
                    -- Validate message length: msg2 must be exactly 32 bytes (responder ephemeral key).
                    -- Reject short messages and trailing bytes that could indicate injection.
                    | BS.length msg2 /= 32 -> pure Nothing
                    | otherwise -> do
                        let !rEPub = BS.take 32 msg2

                        -- <- e: mix responder ephemeral into hash
                        -- M10.1.6: h5 is the final handshake hash; carried into
                        -- transport phase as channel-binding associated data.
                        let !h5 = mixHash h4 rEPub

                        -- <- ee: DH(e_i, e_r); reject all-zero
                        case x25519 eSec rEPub of
                          Nothing    -> pure Nothing
                          Just !dhEE -> do
                            let (!ck3, !_k3) = hkdfCK ck2 dhEE

                            -- Finding: Noise IK `se` leg used the wrong keys, computing
                            --   DH(e_i, s_r) instead of the spec-required DH(s_i, e_r).
                            --   This produced the same DH output as the `es` leg (modulo
                            --   key-role symmetry), collapsing two independent DH
                            --   contributions into one, weakening the handshake's
                            --   forward-secrecy and identity-binding guarantees.
                            -- Vulnerability: Because ck4 was derived from a duplicate DH
                            --   value rather than a fresh independent one, an adversary who
                            --   learns the initiator's static secret can recover ck4 without
                            --   knowledge of the initiator's ephemeral secret.  This breaks
                            --   the mutual-auth property of Noise IK: the session keys no
                            --   longer cryptographically bind both the initiator's static and
                            --   ephemeral keys to the transcript.
                            -- Fix: Compute se = DH(s_i, e_r) = x25519 iStaticSec rEPub,
                            --   where iStaticSec is the initiator's static secret (available
                            --   throughout the initiator path) and rEPub is the responder's
                            --   ephemeral public key extracted from msg2 above.  This matches
                            --   the Noise IK spec: the `se` token always means "initiator
                            --   static x responder ephemeral" from the initiator's perspective.
                            -- Verified: matches noiseHandshakeResponder which correctly
                            --   computes x25519 rStaticSec iEPub (= DH(s_r, e_i)), the same
                            --   value from the responder's side.
                            -- Low-order point: also reject all-zero DH (x25519 returns Maybe).
                            case x25519 iStaticSec rEPub of
                              Nothing    -> pure Nothing
                              Just !dhSE -> do
                                let (!ck4, !_k4) = hkdfCK ck3 dhSE

                                -- Verify responder identity before committing to session keys.
                                -- In IK pattern the responder's static key is pre-known, but the
                                -- trust callback lets the caller enforce pinning / TOFU policy.
                                trusted <- trustCheck rStaticPub
                                if not trusted
                                  then pure Nothing
                                  else do
                                    -- Derive final send/recv keys (one per direction)
                                    let (!sendEncKey, !recvEncKey) = splitKeys ck4
                                    pure (Just NoiseState
                                        { nsSendEncKey    = sendEncKey
                                        , nsRecvEncKey    = recvEncKey
                                        , nsSendN         = 0
                                        , nsRecvN         = 0
                                        , nsHandshakeHash = h5
                                        })

------------------------------------------------------------------------
-- Handshake: Responder
------------------------------------------------------------------------

-- | Perform the Noise_IK handshake as responder.
--
-- Receives message 1, sends message 2, derives session keys.
noiseHandshakeResponder
    :: ByteString   -- ^ Our static secret key (32 bytes)
    -> ByteString      -- ^ Our static public key (32 bytes)
    -> AnyTransport    -- ^ Network transport
    -> IO (Maybe (NoiseState, ByteString))
noiseHandshakeResponder rStaticSec rStaticPub transport = do
    -- Initialize handshake hash and chaining key (same as initiator)
    let !h0 = initHash
    let !h1 = mixHash h0 prologue
    -- Mix in our (responder's) static public key (pre-message: <- s)
    let !h2 = mixHash h1 rStaticPub
    let !ck0 = initCK

    -- Receive message 1: iEPub || encStaticPub
    mMsg1 <- recvFrame transport
    case mMsg1 of
      Nothing -> pure Nothing
      Just msg1
        -- Validate message length: need at least 32 (ephemeral) + 32 (ct) + 32 (HMAC)
        | BS.length msg1 < (32 + 32 + hsHmacLen) -> pure Nothing
        | otherwise -> do
            let !iEPub        = BS.take 32 msg1
                !encStaticPub = BS.drop 32 msg1

            -- -> e: mix initiator ephemeral into hash
            let !h3 = mixHash h2 iEPub

            -- -> es: DH(e_i, s_r) — responder computes DH(s_r, e_i); reject all-zero
            case x25519 rStaticSec iEPub of
              Nothing    -> pure Nothing
              Just !dhES -> do
                let (!ck1, !k1) = hkdfCK ck0 dhES

                -- -> s: decrypt initiator's static public key
                case decryptWithKey k1 h3 encStaticPub of
                  Nothing         -> pure Nothing
                  Just iStaticPub -> do
                    let !h4 = mixHash h3 encStaticPub

                    -- -> ss: DH(s_r, s_i) — responder computes DH(s_r, s_i); reject all-zero
                    case x25519 rStaticSec iStaticPub of
                      Nothing    -> pure Nothing
                      Just !dhSS -> do
                        let (!ck2, !_k2) = hkdfCK ck1 dhSS

                        -- Generate responder ephemeral keypair
                        eSec <- randomBytes 32
                        case x25519 eSec x25519Basepoint of
                          Nothing    -> pure Nothing
                          Just !ePub -> do
                            -- <- e: send ephemeral public, mix into hash
                            -- M10.1.6: h5 is the final handshake hash; carried into
                            -- transport phase as channel-binding associated data.
                            let !h5 = mixHash h4 ePub

                            -- <- ee: DH(e_r, e_i); reject all-zero
                            case x25519 eSec iEPub of
                              Nothing    -> pure Nothing
                              Just !dhEE -> do
                                let (!ck3, !_k3) = hkdfCK ck2 dhEE

                                -- <- se: DH(e_r, s_i) — responder's ephemeral, initiator's static.
                                -- The `se` token means sender-ephemeral x recipient-static from
                                -- the responder's side: x25519(e_r_sec, s_i_pub).
                                -- Mirrors the initiator's DH(s_i, e_r) = x25519(iStaticSec, rEPub).
                                case x25519 eSec iStaticPub of
                                  Nothing    -> pure Nothing
                                  Just !dhSE -> do
                                    let (!ck4, !_k4) = hkdfCK ck3 dhSE

                                    -- Send message 2: ePub
                                    sendFrame transport ePub

                                    -- Derive final send/recv keys (one per direction).
                                    -- Responder's send = initiator's recv and vice versa.
                                    let (!iSendEncKey, !iRecvEncKey) = splitKeys ck4
                                    let !noiseState = NoiseState
                                            { nsSendEncKey    = iRecvEncKey
                                            , nsRecvEncKey    = iSendEncKey
                                            , nsSendN         = 0
                                            , nsRecvN         = 0
                                            , nsHandshakeHash = h5
                                            }
                                    pure (Just (noiseState, iStaticPub))

------------------------------------------------------------------------
-- Handshake helpers
------------------------------------------------------------------------

-- | Initial handshake hash from protocol name.
-- If protocolName <= 32 bytes, pad to 32; otherwise hash it.
initHash :: ByteString
initHash
    | BS.length protocolName <= 32 =
        protocolName <> BS.replicate (32 - BS.length protocolName) 0
    | otherwise = sha256 protocolName

-- | Initial chaining key (same as initHash per Noise spec).
initCK :: ByteString
initCK = initHash

-- | Mix data into the handshake hash: h = SHA256(h || data).
mixHash :: ByteString -> ByteString -> ByteString
mixHash !h !dat = sha256 (h <> dat)

-- | Derive new chaining key and encryption key from a DH result.
-- ck', k = HKDF(ck, dh_result)
hkdfCK :: ByteString -> ByteString -> (ByteString, ByteString)
hkdfCK !ck !ikm =
    let !prk  = hkdfSHA256Extract ck ikm
        !out  = hkdfSHA256Expand prk BS.empty 64
        !ck'  = BS.take 32 out
        !k    = BS.drop 32 (BS.take 64 out)
    in (ck', k)

-- | Derive final send and recv keys from the final chaining key.
-- Returns (sendEncKey, recvEncKey) from initiator's perspective.
-- With ChaCha20-Poly1305 AEAD a single key per direction suffices; the
-- Poly1305 one-time key is derived internally from the encryption key.
splitKeys :: ByteString -> (ByteString, ByteString)
splitKeys !ck =
    let !prk        = hkdfSHA256Extract ck BS.empty
        !sendEncKey = hkdfSHA256Expand prk (packASCII "enc-send") 32
        !recvEncKey = hkdfSHA256Expand prk (packASCII "enc-recv") 32
    in (sendEncKey, recvEncKey)

-- | HMAC-SHA256 tag length used in the Noise IK handshake (not the transport).
-- The transport uses 16-byte Poly1305 tags (RFC 8439); the handshake uses
-- 32-byte HMAC-SHA256 tags for encrypting the initiator's static public key.
hsHmacLen :: Int
hsHmacLen = 32

-- | Encrypt data with a handshake key (ChaCha20 + HMAC for authentication).
-- Uses a zero nonce since each handshake key is used only once.
encryptWithKey :: ByteString -> ByteString -> ByteString -> ByteString
encryptWithKey !k !h !plaintext =
    let !nonce = BS.replicate 12 0
        !ct    = chacha20Encrypt k nonce 0 plaintext
        !mac   = hmacSHA256 k (h <> ct)
    in ct <> mac

-- | Decrypt data with a handshake key, verifying the HMAC tag.
-- Returns 'Nothing' if the MAC does not match.
decryptWithKey :: ByteString -> ByteString -> ByteString -> Maybe ByteString
decryptWithKey !k !h !cipherMac
    | BS.length cipherMac < hsHmacLen = Nothing
    | otherwise =
        let !ctLen = BS.length cipherMac - hsHmacLen
            !ct    = BS.take ctLen cipherMac
            !mac   = BS.drop ctLen cipherMac
            !expected = hmacSHA256 k (h <> ct)
            !nonce = BS.replicate 12 0
        in if constantEq mac expected
           then Just (chacha20Encrypt k nonce 0 ct)
           else Nothing

------------------------------------------------------------------------
-- Framing: length-prefixed messages over transport
------------------------------------------------------------------------

-- | Send a length-prefixed frame (4-byte big-endian length + payload).
sendFrame :: AnyTransport -> ByteString -> IO ()
sendFrame t payload = do
    let !len = fromIntegral (BS.length payload) :: Word32
    anySend t (putWord32BE len)
    anySend t payload

-- | Receive a length-prefixed frame. Returns 'Nothing' if the length
-- prefix is too short or the frame exceeds 64 KiB (prevents DoS via
-- large allocations without crashing the process).
recvFrame :: AnyTransport -> IO (Maybe ByteString)
recvFrame t = do
    lenBS <- anyRecv t 4
    if BS.length lenBS < 4
        then pure Nothing
        else do
            let !len = getWord32BE lenBS
            if len >= maxFrameSize
                then pure Nothing
                else do
                    payload <- anyRecv t (fromIntegral len)
                    if BS.length payload < fromIntegral len
                        then pure Nothing
                        else pure (Just payload)

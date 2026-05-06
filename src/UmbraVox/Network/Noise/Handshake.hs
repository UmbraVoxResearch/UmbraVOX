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
    , macLen
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
noiseHandshakeInitiator
    :: ByteString   -- ^ Our static secret key (32 bytes)
    -> ByteString   -- ^ Our static public key (32 bytes)
    -> ByteString      -- ^ Responder's static public key (32 bytes)
    -> AnyTransport    -- ^ Network transport
    -> IO (Maybe NoiseState)
noiseHandshakeInitiator iStaticSec iStaticPub rStaticPub transport = do
    -- Initialize handshake hash: h = SHA256(protocolName padded to 32)
    let !h0 = initHash
    -- Mix in prologue
    let !h1 = mixHash h0 prologue
    -- Mix in responder's static public key (pre-message: <- s)
    let !h2 = mixHash h1 rStaticPub
    -- Initialize chaining key
    let !ck0 = initCK

    -- Generate ephemeral keypair
    eSec <- randomBytes 32
    let !ePub = x25519 eSec x25519Basepoint

    -- -> e: send ephemeral public, mix into hash
    let !h3 = mixHash h2 ePub

    -- -> es: DH(e, rs)
    let !dhES = x25519 eSec rStaticPub
    let (!ck1, !k1) = hkdfCK ck0 dhES

    -- -> s: encrypt and send initiator's static public key
    let !encStaticPub = encryptWithKey k1 h3 iStaticPub
    let !h4 = mixHash h3 encStaticPub

    -- -> ss: DH(s, rs)
    let !dhSS = x25519 iStaticSec rStaticPub
    let (!ck2, !_k2) = hkdfCK ck1 dhSS

    -- Build and send message 1: ePub || encStaticPub
    let !msg1 = ePub <> encStaticPub
    sendFrame transport msg1

    -- Receive message 2: rEPub
    msg2 <- recvFrame transport
    -- Validate message length: need at least 32 bytes for responder ephemeral key
    if BS.length msg2 < 32
      then pure Nothing
      else do
        let !rEPub = BS.take 32 msg2

        -- <- e: mix responder ephemeral into hash
        let !_h5 = mixHash h4 rEPub

        -- <- ee: DH(e_i, e_r)
        let !dhEE = x25519 eSec rEPub
        let (!ck3, !_k3) = hkdfCK ck2 dhEE

        -- <- se: responder's static x initiator's ephemeral
        -- Initiator computes: x25519(eSec, rStaticPub)
        let !dhSE = x25519 eSec rStaticPub
        let (!ck4, !_k4) = hkdfCK ck3 dhSE

        -- Derive final send/recv keys with key separation
        let (!sendEncKey, !sendMacKey, !recvEncKey, !recvMacKey) = splitKeys ck4

        pure (Just NoiseState
            { nsSendEncKey = sendEncKey
            , nsSendMacKey = sendMacKey
            , nsRecvEncKey = recvEncKey
            , nsRecvMacKey = recvMacKey
            , nsSendN      = 0
            , nsRecvN      = 0
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
    msg1 <- recvFrame transport
    -- Validate message length: need at least 32 (ephemeral) + 32 (ct) + 32 (mac)
    if BS.length msg1 < (32 + 32 + macLen)
      then pure Nothing
      else do
        let !iEPub        = BS.take 32 msg1
            !encStaticPub = BS.drop 32 msg1

        -- -> e: mix initiator ephemeral into hash
        let !h3 = mixHash h2 iEPub

        -- -> es: DH(e_i, s_r) — responder computes DH(s_r, e_i)
        let !dhES = x25519 rStaticSec iEPub
        let (!ck1, !k1) = hkdfCK ck0 dhES

        -- -> s: decrypt initiator's static public key
        case decryptWithKey k1 h3 encStaticPub of
            Nothing -> pure Nothing
            Just iStaticPub -> do
                let !h4 = mixHash h3 encStaticPub

                -- -> ss: DH(s_r, s_i) — responder computes DH(s_r, s_i)
                let !dhSS = x25519 rStaticSec iStaticPub
                let (!ck2, !_k2) = hkdfCK ck1 dhSS

                -- Generate responder ephemeral keypair
                eSec <- randomBytes 32
                let !ePub = x25519 eSec x25519Basepoint

                -- <- e: send ephemeral public, mix into hash
                let !_h5 = mixHash h4 ePub

                -- <- ee: DH(e_r, e_i)
                let !dhEE = x25519 eSec iEPub
                let (!ck3, !_k3) = hkdfCK ck2 dhEE

                -- <- se: DH(s_r, e_i) — responder's static, initiator's ephemeral
                let !dhSE = x25519 rStaticSec iEPub
                let (!ck4, !_k4) = hkdfCK ck3 dhSE

                -- Send message 2: ePub
                let !msg2 = ePub
                sendFrame transport msg2

                -- Derive final send/recv keys with key separation
                -- Responder's send = initiator's recv and vice versa
                let (!iSendEncKey, !iSendMacKey, !iRecvEncKey, !iRecvMacKey) = splitKeys ck4

                let !noiseState = NoiseState
                        { nsSendEncKey = iRecvEncKey
                        , nsSendMacKey = iRecvMacKey
                        , nsRecvEncKey = iSendEncKey
                        , nsRecvMacKey = iSendMacKey
                        , nsSendN      = 0
                        , nsRecvN      = 0
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
-- Returns (sendEncKey, sendMacKey, recvEncKey, recvMacKey) from initiator's
-- perspective. Key separation: encryption and MAC use independent keys.
splitKeys :: ByteString -> (ByteString, ByteString, ByteString, ByteString)
splitKeys !ck =
    let !prk = hkdfSHA256Extract ck BS.empty
        !sendEncKey = hkdfSHA256Expand prk (packASCII "enc-send") 32
        !sendMacKey = hkdfSHA256Expand prk (packASCII "mac-send") 32
        !recvEncKey = hkdfSHA256Expand prk (packASCII "enc-recv") 32
        !recvMacKey = hkdfSHA256Expand prk (packASCII "mac-recv") 32
    in (sendEncKey, sendMacKey, recvEncKey, recvMacKey)

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
    | BS.length cipherMac < macLen = Nothing
    | otherwise =
        let !ctLen = BS.length cipherMac - macLen
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

-- | Maximum frame size (64 KiB) to prevent DoS via large allocations.
maxFrameSize :: Word32
maxFrameSize = 65536

-- | Receive a length-prefixed frame. Rejects frames larger than 64 KiB.
recvFrame :: AnyTransport -> IO ByteString
recvFrame t = do
    lenBS <- anyRecv t 4
    let !len = getWord32BE lenBS
    if len > maxFrameSize
        then error ("recvFrame: frame too large (" ++ show len ++ " > " ++ show maxFrameSize ++ ")")
        else anyRecv t (fromIntegral len)

-- | Noise_IK handshake (simplified MVP)
--
-- Pattern:
--   -> e, es, s, ss
--   <- e, ee, se
--
-- After handshake, both sides share symmetric send/recv keys
-- derived via HKDF from the chained DH outputs.
--
-- See: doc/spec/network.md
module UmbraVox.Network.Noise
  ( NoiseState(..)
  , noiseEncrypt
  , noiseDecrypt
  , noiseHandshakeInitiator
  , noiseHandshakeResponder
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32, Word64)

import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.HKDF (hkdfSHA256Extract, hkdfSHA256Expand)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Crypto.Random (chacha20Encrypt, randomBytes)
import UmbraVox.Crypto.SHA256 (sha256)
import UmbraVox.Network.Transport (Transport, send, recv)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Post-handshake session state for encrypting/decrypting messages.
data NoiseState = NoiseState
    { nsSendKey :: !ByteString   -- ^ 32-byte ChaCha20 key for sending
    , nsRecvKey :: !ByteString   -- ^ 32-byte ChaCha20 key for receiving
    , nsSendN   :: !Word64       -- ^ Send nonce counter
    , nsRecvN   :: !Word64       -- ^ Recv nonce counter
    } deriving (Show)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | Protocol prologue, mixed into the handshake hash.
prologue :: ByteString
prologue = packASCII "UmbraVox_v1"

-- | Protocol name for the initial hash.
protocolName :: ByteString
protocolName = packASCII "Noise_IK_25519_ChaChaPoly_SHA256"

-- | HMAC tag length appended to each encrypted message.
macLen :: Int
macLen = 32

------------------------------------------------------------------------
-- Post-handshake encrypt / decrypt
------------------------------------------------------------------------

-- | Encrypt a plaintext message, appending an HMAC-SHA256 tag.
-- Returns updated state (incremented nonce) and ciphertext || mac.
noiseEncrypt :: NoiseState -> ByteString -> (NoiseState, ByteString)
noiseEncrypt st plaintext =
    let !nonce  = makeNonce (nsSendN st)
        !ct     = chacha20Encrypt (nsSendKey st) nonce 1 plaintext
        !mac    = hmacSHA256 (nsSendKey st) (nonce <> ct)
        !st'    = st { nsSendN = nsSendN st + 1 }
    in (st', ct <> mac)

-- | Decrypt a ciphertext || mac message, verifying the HMAC-SHA256 tag.
-- Returns Nothing if the MAC does not match.
noiseDecrypt :: NoiseState -> ByteString -> Maybe (NoiseState, ByteString)
noiseDecrypt st msg
    | BS.length msg < macLen = Nothing
    | otherwise =
        let !ctLen  = BS.length msg - macLen
            !ct     = BS.take ctLen msg
            !mac    = BS.drop ctLen msg
            !nonce  = makeNonce (nsRecvN st)
            !expected = hmacSHA256 (nsRecvKey st) (nonce <> ct)
        in if constantEq mac expected
           then let !pt  = chacha20Encrypt (nsRecvKey st) nonce 1 ct
                    !st' = st { nsRecvN = nsRecvN st + 1 }
                in Just (st', pt)
           else Nothing

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
    -> ByteString   -- ^ Responder's static public key (32 bytes)
    -> Transport    -- ^ Network transport
    -> IO NoiseState
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

    -- Derive final send/recv keys (initiator sends with k_i, recvs with k_r)
    let (!sendKey, !recvKey) = splitKeys ck4

    pure NoiseState
        { nsSendKey = sendKey
        , nsRecvKey = recvKey
        , nsSendN   = 0
        , nsRecvN   = 0
        }

------------------------------------------------------------------------
-- Handshake: Responder
------------------------------------------------------------------------

-- | Perform the Noise_IK handshake as responder.
--
-- Receives message 1, sends message 2, derives session keys.
noiseHandshakeResponder
    :: ByteString   -- ^ Our static secret key (32 bytes)
    -> ByteString   -- ^ Our static public key (32 bytes)
    -> Transport    -- ^ Network transport
    -> IO NoiseState
noiseHandshakeResponder rStaticSec rStaticPub transport = do
    -- Initialize handshake hash and chaining key (same as initiator)
    let !h0 = initHash
    let !h1 = mixHash h0 prologue
    -- Mix in our (responder's) static public key (pre-message: <- s)
    let !h2 = mixHash h1 rStaticPub
    let !ck0 = initCK

    -- Receive message 1: iEPub || encStaticPub
    msg1 <- recvFrame transport
    let !iEPub        = BS.take 32 msg1
        !encStaticPub = BS.drop 32 msg1

    -- -> e: mix initiator ephemeral into hash
    let !h3 = mixHash h2 iEPub

    -- -> es: DH(e_i, s_r) — responder computes DH(s_r, e_i)
    let !dhES = x25519 rStaticSec iEPub
    let (!ck1, !k1) = hkdfCK ck0 dhES

    -- -> s: decrypt initiator's static public key
    let !iStaticPub = decryptWithKey k1 h3 encStaticPub
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

    -- Derive final send/recv keys (responder's send = initiator's recv)
    let (!iSendKey, !rSendKey) = splitKeys ck4

    pure NoiseState
        { nsSendKey = rSendKey
        , nsRecvKey = iSendKey
        , nsSendN   = 0
        , nsRecvN   = 0
        }

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
-- Returns (initiator_send_key, responder_send_key).
splitKeys :: ByteString -> (ByteString, ByteString)
splitKeys !ck =
    let !out = hkdfSHA256Expand (hkdfSHA256Extract ck BS.empty)
                                (packASCII "UmbraVox_session") 64
        !k1  = BS.take 32 out
        !k2  = BS.drop 32 (BS.take 64 out)
    in (k1, k2)

-- | Encrypt data with a handshake key (ChaCha20 + HMAC for authentication).
-- Uses a zero nonce since each handshake key is used only once.
encryptWithKey :: ByteString -> ByteString -> ByteString -> ByteString
encryptWithKey !k !h !plaintext =
    let !nonce = BS.replicate 12 0
        !ct    = chacha20Encrypt k nonce 0 plaintext
        !mac   = hmacSHA256 k (h <> ct)
    in ct <> mac

-- | Decrypt data with a handshake key, verifying the HMAC tag.
decryptWithKey :: ByteString -> ByteString -> ByteString -> ByteString
decryptWithKey !k !h !cipherMac =
    let !ctLen = BS.length cipherMac - macLen
        !ct    = BS.take ctLen cipherMac
        !mac   = BS.drop ctLen cipherMac
        !expected = hmacSHA256 k (h <> ct)
        !nonce = BS.replicate 12 0
    in if constantEq mac expected
       then chacha20Encrypt k nonce 0 ct
       else error "Noise: handshake decryption failed (bad MAC)"

------------------------------------------------------------------------
-- Framing: length-prefixed messages over transport
------------------------------------------------------------------------

-- | Send a length-prefixed frame (4-byte big-endian length + payload).
sendFrame :: Transport -> ByteString -> IO ()
sendFrame t payload = do
    let !len = fromIntegral (BS.length payload) :: Word32
    send t (putWord32BE len)
    send t payload

-- | Receive a length-prefixed frame.
recvFrame :: Transport -> IO ByteString
recvFrame t = do
    lenBS <- recv t 4
    let !len = getWord32BE lenBS
    recv t (fromIntegral len)

------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------

-- | Build a 12-byte nonce: 4 zero bytes followed by 8-byte LE counter.
makeNonce :: Word64 -> ByteString
makeNonce !n = BS.pack
    [ 0, 0, 0, 0
    , w8 n 0, w8 n 1, w8 n 2, w8 n 3
    , w8 n 4, w8 n 5, w8 n 6, w8 n 7
    ]
  where
    w8 :: Word64 -> Int -> Word8
    w8 !v !i = fromIntegral (shiftR v (8 * i) .&. 0xff)

-- | Constant-time byte string comparison to prevent timing attacks.
constantEq :: ByteString -> ByteString -> Bool
constantEq !a !b
    | BS.length a /= BS.length b = False
    | otherwise = BS.foldl' (\acc w -> acc .|. w) (0 :: Word8)
                    (BS.pack (BS.zipWith xor a b)) == 0

-- | Encode a Word32 as 4 big-endian bytes.
putWord32BE :: Word32 -> ByteString
putWord32BE !w = BS.pack
    [ fromIntegral (shiftR w 24 .&. 0xff)
    , fromIntegral (shiftR w 16 .&. 0xff)
    , fromIntegral (shiftR w  8 .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

-- | Decode 4 big-endian bytes to a Word32.
getWord32BE :: ByteString -> Word32
getWord32BE !bs =
    (fromIntegral (BS.index bs 0) `shiftL` 24) .|.
    (fromIntegral (BS.index bs 1) `shiftL` 16) .|.
    (fromIntegral (BS.index bs 2) `shiftL`  8) .|.
     fromIntegral (BS.index bs 3)

-- | Convert an ASCII string literal to ByteString.
packASCII :: String -> ByteString
packASCII = BS.pack . map (fromIntegral . fromEnum)

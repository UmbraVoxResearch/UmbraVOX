-- SPDX-License-Identifier: Apache-2.0
-- | Noise protocol types, constants, and nonce management.
module UmbraVox.Network.Noise.State
  ( NoiseState(..)
  , prologue
  , protocolName
  , macLen
  , makeNonce
  , packASCII
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word64)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Post-handshake session state for encrypting/decrypting messages.
-- Uses separate keys for encryption and MAC (key separation).
--
-- Finding    M10.1.6 — The handshake hash (h5, the final Noise transcript
--            hash) was computed but then discarded.  The Noise spec requires
--            the handshake hash to be carried into the transport phase and
--            mixed into the transport-phase AEAD as associated data, providing
--            channel binding (tying the ciphertext to the completed handshake
--            transcript).
-- Vulnerability: Without carrying h forward, an active adversary who can
--            forge or swap transport-layer ciphertexts is not cryptographically
--            bound to the negotiated handshake transcript.  The transport AEAD
--            would accept the same ciphertext under any NoiseState with
--            matching keys, regardless of which handshake produced those keys.
-- Fix:       Added 'nsHandshakeHash' field to NoiseState.  Both
--            noiseHandshakeInitiator and noiseHandshakeResponder now compute
--            h5 = mixHash h4 rEPub / mixHash h4 ePub and store it in this
--            field.  noiseEncrypt passes nsHandshakeHash as a prefix in the
--            HMAC input; noiseDecrypt does likewise for verification.
-- Verified:  noiseEncrypt and noiseDecrypt both thread nsHandshakeHash into
--            the HMAC computation.  A state with a different handshakeHash
--            will produce a different MAC, causing noiseDecrypt to return
--            Nothing (rejected by constantEq).
data NoiseState = NoiseState
    { nsSendEncKey     :: !ByteString   -- ^ 32-byte ChaCha20 key for sending
    , nsSendMacKey     :: !ByteString   -- ^ 32-byte HMAC key for send authentication
    , nsRecvEncKey     :: !ByteString   -- ^ 32-byte ChaCha20 key for receiving
    , nsRecvMacKey     :: !ByteString   -- ^ 32-byte HMAC key for recv authentication
    , nsSendN          :: !Word64       -- ^ Send nonce counter
    , nsRecvN          :: !Word64       -- ^ Recv nonce counter
    , nsHandshakeHash  :: !ByteString
      -- ^ 32-byte final handshake hash (h5).  Mixed into every transport-phase
      -- AEAD invocation as associated data, binding the ciphertext to the
      -- completed handshake transcript (channel binding per Noise spec).
    } deriving (Show)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | Protocol prologue, mixed into the handshake hash.
prologue :: ByteString
prologue = packASCII "UmbraVox_v1"

-- | Protocol name for the initial hash.
--
-- Finding    M10.1.7 — The protocol name string claimed "ChaChaPoly" but the
--            actual AEAD used throughout the project is AES-256-GCM.
-- Vulnerability: A mismatched protocol name string causes the handshake hash
--            h0 (initialised from this string) to diverge from what a correct
--            Noise_IK_AESGCM implementation would compute, breaking
--            interoperability and audit clarity.
-- Fix:       Changed "Noise_IK_25519_ChaChaPoly_SHA256" to
--            "Noise_IK_25519_AESGCM_SHA256" to match the actual AEAD in use.
--            The actual AEAD algorithm (AES-256-GCM) is unchanged.
-- Verified:  Only the name string is changed; gcmEncrypt/gcmDecrypt calls
--            are unmodified.  initHash recomputes h0 deterministically from
--            the updated string; both sides use the same string so agreement
--            is preserved.
protocolName :: ByteString
protocolName = packASCII "Noise_IK_25519_AESGCM_SHA256"

-- | HMAC tag length appended to each encrypted message.
macLen :: Int
macLen = 32

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

-- | Convert an ASCII string literal to ByteString.
packASCII :: String -> ByteString
packASCII = BS.pack . map (fromIntegral . fromEnum)

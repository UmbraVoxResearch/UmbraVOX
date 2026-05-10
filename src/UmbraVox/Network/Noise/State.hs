-- SPDX-License-Identifier: Apache-2.0
-- | Noise protocol types, constants, and nonce management.
--
-- == Nonce invariant (M10.1.11)
--
-- __AEAD nonce construction__
--
-- Every transport-phase AEAD invocation uses a 12-byte nonce formed as:
--
-- @
--   nonce = 0x00 0x00 0x00 0x00  ||  LE64(counter)
-- @
--
-- The leading 4 zero bytes match the ChaCha20-Poly1305 convention from the
-- Noise specification (§11.1) and RFC 8439 §2.6.
-- The counter occupies bytes 4–11 in 64-bit little-endian order.
-- See 'makeNonce' for the reference implementation.
--
-- __Counter semantics__
--
-- * The send counter ('nsSendN') increments by one after each call to
--   @noiseEncrypt@; the receive counter ('nsRecvN') increments by one after
--   each successful call to @noiseDecrypt@.
-- * Both counters start at 0 and are never reset within a session.
--   If a session requires rekeying (counter exhaustion or explicit rekey
--   token), a new @NoiseState@ is created from fresh key material; the
--   counters are not reused across sessions.
--
-- __Nonce uniqueness guarantee__
--
-- Within a single session key lifetime, the counter is strictly monotonic:
-- every (key, nonce) pair is used at most once.  Because ChaCha20-Poly1305
-- is a nonce-based AEAD, reusing a (key, nonce) pair would break
-- confidentiality and authenticity.  The monotonic counter prevents this
-- without requiring a random nonce (which would need 192+ bits to avoid
-- birthday-bound collisions at high message volumes).
--
-- __Channel binding via handshake hash__
--
-- The 32-byte final handshake hash @nsHandshakeHash@ (field @h5@ computed
-- by 'noiseHandshakeInitiator' and 'noiseHandshakeResponder') is passed as
-- associated data (AAD) to every AEAD invocation in the transport phase.
-- This ties each transport ciphertext to the specific handshake transcript
-- that established the session keys, preventing an active adversary from
-- replaying a transport message from one session under different session
-- keys derived from a different (e.g. forged) handshake.
-- See also: M10.1.6 finding in 'NoiseState'.
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
-- Uses RFC 8439 ChaCha20-Poly1305 AEAD; one key per direction.
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
--            field.  noiseEncrypt passes nsHandshakeHash as AAD to
--            chachaPolyEncrypt; noiseDecrypt does likewise for verification.
-- Verified:  noiseEncrypt and noiseDecrypt both pass nsHandshakeHash as
--            AAD to the AEAD.  A state with a different handshakeHash
--            produces a different Poly1305 tag, causing noiseDecrypt to
--            return Nothing (rejected by constantEq inside chachaPolyDecrypt).
--
-- Finding    M10.1.7 — Previously the record carried separate nsSendMacKey /
--            nsRecvMacKey fields used with HMAC-SHA256, diverging from the
--            Noise spec's expectation of a standard AEAD cipher.
-- Vulnerability: Non-standard separate-key HMAC construction is harder to
--            audit and incompatible with reference Noise implementations.
-- Fix:       Removed nsSendMacKey and nsRecvMacKey.  Authentication is now
--            provided by RFC 8439 ChaCha20-Poly1305 AEAD, which derives the
--            Poly1305 one-time key internally from (nsSendEncKey, nonce).
-- Verified:  noiseEncrypt calls chachaPolyEncrypt(nsSendEncKey, nonce, aad,
--            plaintext) and noiseDecrypt calls chachaPolyDecrypt; no separate
--            MAC key fields are needed.
data NoiseState = NoiseState
    { nsSendEncKey     :: !ByteString   -- ^ 32-byte ChaCha20-Poly1305 key for sending
    , nsRecvEncKey     :: !ByteString   -- ^ 32-byte ChaCha20-Poly1305 key for receiving
    , nsSendN          :: !Word64       -- ^ Send nonce counter
    , nsRecvN          :: !Word64       -- ^ Recv nonce counter
    , nsHandshakeHash  :: !ByteString
      -- ^ 32-byte final handshake hash (h5).  Passed as AAD to every
      -- transport-phase AEAD invocation, binding the ciphertext to the
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
-- Finding    M10.1.7 — The transport AEAD has been replaced with RFC 8439
--            ChaCha20-Poly1305.  The protocol name string is updated to
--            "Noise_IK_25519_ChaChaPoly_SHA256" to accurately reflect the
--            AEAD in use, matching the Noise specification naming convention.
-- Vulnerability: A mismatched protocol name string causes the handshake hash
--            h0 (initialised from this string) to diverge from what a correct
--            Noise_IK_ChaChaPoly implementation would compute, breaking
--            interoperability and audit clarity.
-- Fix:       Set protocolName to "Noise_IK_25519_ChaChaPoly_SHA256" to match
--            the actual RFC 8439 ChaCha20-Poly1305 AEAD now used in transport.
-- Verified:  Both initiator and responder derive h0 from the same string;
--            all transport messages are now authenticated by chachaPolyEncrypt /
--            chachaPolyDecrypt.
protocolName :: ByteString
protocolName = packASCII "Noise_IK_25519_ChaChaPoly_SHA256"

-- | Poly1305 tag length appended to each encrypted message (RFC 8439).
macLen :: Int
macLen = 16

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

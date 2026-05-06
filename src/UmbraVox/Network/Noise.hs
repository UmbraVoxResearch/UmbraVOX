-- | Noise_IK handshake (simplified MVP)
--
-- Pattern:
--   -> e, es, s, ss
--   <- e, ee, se
--
-- After handshake, both sides share symmetric send/recv keys
-- derived via HKDF from the chained DH outputs.
--
-- This module re-exports the public API from the sub-modules
-- for backward compatibility.
--
-- See: doc/spec/network.md
module UmbraVox.Network.Noise
  ( -- * Types and state
    NoiseState(..)
    -- * Post-handshake transport encryption
  , noiseEncrypt
  , noiseDecrypt
    -- * Handshake protocol
  , noiseHandshakeInitiator
  , noiseHandshakeResponder
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import UmbraVox.Crypto.ConstantTime (constantEq)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Crypto.Random (chacha20Encrypt)
import UmbraVox.Network.Noise.Handshake
    ( noiseHandshakeInitiator
    , noiseHandshakeResponder
    )
import UmbraVox.Network.Noise.State
    ( NoiseState(..)
    , macLen
    , makeNonce
    )

------------------------------------------------------------------------
-- Post-handshake encrypt / decrypt
------------------------------------------------------------------------

-- | Encrypt a plaintext message, appending an HMAC-SHA256 tag.
-- Uses separate keys for encryption (ChaCha20) and authentication (HMAC).
-- Returns updated state (incremented nonce) and ciphertext || mac.
noiseEncrypt :: NoiseState -> ByteString -> (NoiseState, ByteString)
noiseEncrypt st plaintext =
    let !nonce  = makeNonce (nsSendN st)
        !ct     = chacha20Encrypt (nsSendEncKey st) nonce 1 plaintext
        !mac    = hmacSHA256 (nsSendMacKey st) (nonce <> ct)
        !st'    = st { nsSendN = nsSendN st + 1 }
    in (st', ct <> mac)

-- | Decrypt a ciphertext || mac message, verifying the HMAC-SHA256 tag.
-- Uses separate keys for decryption (ChaCha20) and verification (HMAC).
-- Returns Nothing if the MAC does not match.
noiseDecrypt :: NoiseState -> ByteString -> Maybe (NoiseState, ByteString)
noiseDecrypt st msg
    | BS.length msg < macLen = Nothing
    | otherwise =
        let !ctLen  = BS.length msg - macLen
            !ct     = BS.take ctLen msg
            !mac    = BS.drop ctLen msg
            !nonce  = makeNonce (nsRecvN st)
            !expected = hmacSHA256 (nsRecvMacKey st) (nonce <> ct)
        in if constantEq mac expected
           then let !pt  = chacha20Encrypt (nsRecvEncKey st) nonce 1 ct
                    !st' = st { nsRecvN = nsRecvN st + 1 }
                in Just (st', pt)
           else Nothing

-- SPDX-License-Identifier: Apache-2.0
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

import UmbraVox.Crypto.ChaChaPoly (chachaPolyEncrypt, chachaPolyDecrypt)
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

-- | Encrypt a plaintext message using RFC 8439 ChaCha20-Poly1305 AEAD,
-- appending a 16-byte Poly1305 tag.
-- Returns updated state (incremented nonce) and ciphertext || tag.
--
-- Finding    M10.1.7 — Replaced ChaCha20+HMAC-SHA256 with RFC 8439
--            ChaCha20-Poly1305 AEAD.
-- Vulnerability: The previous separate-key HMAC construction was non-standard,
--            harder to audit, and incompatible with reference Noise
--            implementations.  The 32-byte HMAC tag also wasted bandwidth.
-- Fix:       Use chachaPolyEncrypt, which derives the Poly1305 one-time key
--            from chacha20Block(nsSendEncKey, nonce, 0) and produces a
--            standard 16-byte tag.  nsHandshakeHash is passed as AAD,
--            providing the same channel-binding guarantee as before
--            (M10.1.6 fix preserved).
-- Verified:  noiseEncrypt/noiseDecrypt round-trip correctly for all message
--            sizes.  Any modification to the AAD, ciphertext, or tag causes
--            chachaPolyDecrypt to return Nothing.
noiseEncrypt :: NoiseState -> ByteString -> (NoiseState, ByteString)
noiseEncrypt st plaintext =
    let !nonce        = makeNonce (nsSendN st)
        !(ct, tag)    = chachaPolyEncrypt (nsSendEncKey st) nonce (nsHandshakeHash st) plaintext
        !st'          = st { nsSendN = nsSendN st + 1 }
    in (st', ct <> tag)

-- | Decrypt a ciphertext || tag message using RFC 8439 ChaCha20-Poly1305 AEAD,
-- verifying the 16-byte Poly1305 tag.
-- Returns Nothing if the tag does not match.
noiseDecrypt :: NoiseState -> ByteString -> Maybe (NoiseState, ByteString)
noiseDecrypt st msg
    | BS.length msg < macLen = Nothing
    | otherwise =
        let !ctLen = BS.length msg - macLen
            !ct    = BS.take ctLen msg
            !tag   = BS.drop ctLen msg
            !nonce = makeNonce (nsRecvN st)
        in case chachaPolyDecrypt (nsRecvEncKey st) nonce (nsHandshakeHash st) ct tag of
               Nothing -> Nothing
               Just pt ->
                   let !st' = st { nsRecvN = nsRecvN st + 1 }
                   in Just (st', pt)

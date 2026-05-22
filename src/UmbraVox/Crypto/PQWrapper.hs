-- SPDX-License-Identifier: Apache-2.0
-- | Post-quantum outer encryption layer (M21.1.4)
--
-- Composes ML-KEM-768 key encapsulation with AES-256-GCM symmetric
-- encryption via HKDF-SHA-256 key derivation.
--
-- Wire format: kemCiphertext (1088 bytes) || gcmCiphertext || gcmTag (16 bytes)
--
-- See: doc/spec/crypto.md
module UmbraVox.Crypto.PQWrapper
  ( pqEncrypt
  , pqDecrypt
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.IO.Unsafe (unsafePerformIO)

import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import UmbraVox.Crypto.HKDF (hkdfSHA256)
import UmbraVox.Crypto.MLKEM
    ( MLKEMEncapKey(..)
    , MLKEMDecapKey(..)
    , MLKEMCiphertext(..)
    , mlkemEncaps
    , mlkemDecaps
    )
import UmbraVox.Crypto.Random (randomBytes)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | ML-KEM-768 ciphertext size (bytes).
kemCtLen :: Int
kemCtLen = 1088

-- | AES-256-GCM authentication tag size (bytes).
gcmTagLen :: Int
gcmTagLen = 16

-- | HKDF info string for the encryption key.
keyInfo :: ByteString
keyInfo = "UmbraVox_PQ_v1"

-- | HKDF info string for the GCM nonce.
nonceInfo :: ByteString
nonceInfo = "UmbraVox_PQ_v1_nonce"

------------------------------------------------------------------------
-- Key derivation
------------------------------------------------------------------------

-- | Derive a 32-byte AES-256 key and 12-byte GCM nonce from the KEM
-- shared secret using HKDF-SHA-256.
deriveKeyNonce :: ByteString -> (ByteString, ByteString)
deriveKeyNonce sharedSecret =
    let !key   = hkdfSHA256 sharedSecret sharedSecret keyInfo   32
        !nonce = hkdfSHA256 sharedSecret sharedSecret nonceInfo 12
    in (key, nonce)

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Apply the post-quantum outer encryption layer.
--
-- Input: recipient ML-KEM-768 encapsulation key (1184 bytes) and plaintext.
-- Output: kemCiphertext (1088) || gcmCiphertext || gcmTag (16).
pqEncrypt :: ByteString -> ByteString -> ByteString
pqEncrypt encapKeyBS plaintext =
    -- unsafePerformIO is safe here: randomBytes is the only effect and
    -- each call produces an independent, unobservable random draw.
    unsafePerformIO $ do
        rand <- randomBytes 32
        let ek = MLKEMEncapKey encapKeyBS
            (MLKEMCiphertext kemCt, sharedSecret) = mlkemEncaps ek rand
            (key, nonce) = deriveKeyNonce sharedSecret
            (gcmCt, tag) = gcmEncrypt key nonce BS.empty plaintext
        pure $! kemCt <> gcmCt <> tag

-- | Remove the post-quantum outer encryption layer.
--
-- Input: recipient ML-KEM-768 decapsulation key (2400 bytes) and combined
-- ciphertext.  Returns 'Nothing' on authentication failure or malformed input.
pqDecrypt :: ByteString -> ByteString -> Maybe ByteString
pqDecrypt decapKeyBS combined
    | BS.length combined < kemCtLen + gcmTagLen = Nothing
    | otherwise =
        let kemCt      = BS.take kemCtLen combined
            gcmPayload = BS.drop kemCtLen combined
            gcmCt      = BS.take (BS.length gcmPayload - gcmTagLen) gcmPayload
            tag        = BS.drop (BS.length gcmPayload - gcmTagLen) gcmPayload
            dk           = MLKEMDecapKey decapKeyBS
            sharedSecret = mlkemDecaps dk (MLKEMCiphertext kemCt)
            (key, nonce) = deriveKeyNonce sharedSecret
        in gcmDecrypt key nonce BS.empty gcmCt tag

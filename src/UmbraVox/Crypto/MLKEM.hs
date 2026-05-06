-- | ML-KEM-768 (FIPS 203)
--
-- See: doc/spec/crypto.md
module UmbraVox.Crypto.MLKEM
  ( keygen
  , encaps
  , decaps
  ) where

import Data.ByteString (ByteString)

-- | ML-KEM key generation. Returns (encapsulation key, decapsulation key).
keygen :: IO (ByteString, ByteString)
keygen = error "not implemented"

-- | ML-KEM encapsulation. Returns (ciphertext, shared secret).
encaps :: ByteString -> IO (ByteString, ByteString)
encaps = error "not implemented"

-- | ML-KEM decapsulation. Returns the shared secret.
decaps :: ByteString -> ByteString -> ByteString
decaps = error "not implemented"

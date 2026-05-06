-- | X25519 ECDH (RFC 7748)
--
-- See: doc/spec/crypto.md
module UmbraVox.Crypto.Curve25519
  ( x25519
  ) where

import Data.ByteString (ByteString)

-- | X25519 Diffie-Hellman key agreement.
x25519 :: ByteString -> ByteString -> ByteString
x25519 = error "not implemented"

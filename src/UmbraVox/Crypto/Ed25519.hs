-- | Ed25519 (RFC 8032)
--
-- See: doc/spec/crypto.md
module UmbraVox.Crypto.Ed25519
  ( sign
  , verify
  ) where

import Data.ByteString (ByteString)

-- | Ed25519 signature generation.
sign :: ByteString -> ByteString -> ByteString
sign = error "not implemented"

-- | Ed25519 signature verification.
verify :: ByteString -> ByteString -> ByteString -> Bool
verify = error "not implemented"

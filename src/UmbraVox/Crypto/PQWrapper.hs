-- | Post-quantum outer encryption layer
--
-- See: doc/spec/crypto.md
module UmbraVox.Crypto.PQWrapper
  ( pqEncrypt
  , pqDecrypt
  ) where

import Data.ByteString (ByteString)

-- | Apply the post-quantum outer encryption layer.
pqEncrypt :: ByteString -> ByteString -> ByteString
pqEncrypt = error "not implemented"

-- | Remove the post-quantum outer encryption layer.
pqDecrypt :: ByteString -> ByteString -> Maybe ByteString
pqDecrypt = error "not implemented"

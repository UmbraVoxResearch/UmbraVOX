-- | CSPRNG (ChaCha20, RFC 8439)
--
-- See: doc/spec/crypto.md
module UmbraVox.Crypto.Random
  ( randomBytes
  ) where

import Data.ByteString (ByteString)

-- | Generate the specified number of cryptographically secure random bytes.
randomBytes :: Int -> IO ByteString
randomBytes = error "not implemented"

-- | Encrypted-at-rest key management
--
-- See: doc/spec/crypto.md
module UmbraVox.Crypto.KeyStore
  ( KeyStore
  , openKeyStore
  ) where

import Data.ByteString (ByteString)

-- | Handle to an encrypted-at-rest key store.
data KeyStore = KeyStore
  deriving (Show)

-- | Open or create a key store at the given path.
openKeyStore :: FilePath -> ByteString -> IO KeyStore
openKeyStore = error "not implemented"

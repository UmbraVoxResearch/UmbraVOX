-- | New user token bootstrap
--
-- See: doc/spec/economics.md
module UmbraVox.Economics.Onboarding
  ( bootstrapUser
  ) where

import Data.ByteString (ByteString)

-- | Bootstrap a new user with their initial token allocation.
bootstrapUser :: ByteString -> IO ()
bootstrapUser = error "not implemented"

-- | Extended Triple DH
--
-- See: doc/spec/signal-protocol.md
module UmbraVox.Crypto.Signal.X3DH
  ( x3dh
  ) where

import Data.ByteString (ByteString)

-- | Perform the X3DH key agreement protocol.
x3dh :: ByteString -> ByteString -> ByteString
x3dh = error "not implemented"

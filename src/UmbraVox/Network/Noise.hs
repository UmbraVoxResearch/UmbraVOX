-- | Noise_IK handshake
--
-- See: doc/spec/network.md
module UmbraVox.Network.Noise
  ( handshake
  ) where

import Data.ByteString (ByteString)

-- | Perform a Noise_IK handshake, returning the session keys.
handshake :: ByteString -> ByteString -> IO (ByteString, ByteString)
handshake = error "not implemented"

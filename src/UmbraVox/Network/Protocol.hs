-- | Wire protocol encoding/decoding
--
-- See: doc/spec/network.md
module UmbraVox.Network.Protocol
  ( encode
  , decode
  ) where

import Data.ByteString (ByteString)

-- | Encode a message for wire transmission.
encode :: ByteString -> ByteString
encode = error "not implemented"

-- | Decode a message from the wire format.
decode :: ByteString -> Either String ByteString
decode = error "not implemented"

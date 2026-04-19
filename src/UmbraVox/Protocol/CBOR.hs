-- | CBOR serialization (hand-implemented)
--
-- See: doc/spec/protocol.md
module UmbraVox.Protocol.CBOR
  ( encodeCBOR
  , decodeCBOR
  ) where

import Data.ByteString (ByteString)

-- | Encode a value to CBOR bytes.
encodeCBOR :: ByteString -> ByteString
encodeCBOR = error "not implemented"

-- | Decode a value from CBOR bytes.
decodeCBOR :: ByteString -> Either String ByteString
decodeCBOR = error "not implemented"

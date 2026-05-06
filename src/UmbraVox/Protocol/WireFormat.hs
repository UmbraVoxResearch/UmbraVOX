-- | Network message envelope
--
-- See: doc/spec/protocol.md
module UmbraVox.Protocol.WireFormat
  ( Envelope
  , wrapEnvelope
  , unwrapEnvelope
  ) where

import Data.ByteString (ByteString)

-- | A network message envelope with routing metadata.
data Envelope = Envelope
  deriving (Show)

-- | Wrap a payload in a network envelope.
wrapEnvelope :: ByteString -> Envelope
wrapEnvelope = error "not implemented"

-- | Unwrap a network envelope to extract the payload.
unwrapEnvelope :: Envelope -> ByteString
unwrapEnvelope = error "not implemented"

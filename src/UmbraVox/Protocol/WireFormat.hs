-- SPDX-License-Identifier: Apache-2.0
-- | Network message envelope
--
-- See: doc/spec/protocol.md
module UmbraVox.Protocol.WireFormat
  ( Envelope
  , wrapEnvelope
  , unwrapEnvelope
  ) where

import Data.ByteString (ByteString)

{-# WARNING Envelope "UmbraVox.Protocol.WireFormat is a stub -- not implemented" #-}
{-# WARNING wrapEnvelope "UmbraVox.Protocol.WireFormat is a stub -- not implemented" #-}
{-# WARNING unwrapEnvelope "UmbraVox.Protocol.WireFormat is a stub -- not implemented" #-}

-- | A network message envelope with routing metadata.
data Envelope = Envelope
  deriving (Show)

-- | Wrap a payload in a network envelope.
wrapEnvelope :: ByteString -> Envelope
wrapEnvelope = error "not implemented"

-- | Unwrap a network envelope to extract the payload.
unwrapEnvelope :: Envelope -> ByteString
unwrapEnvelope = error "not implemented"

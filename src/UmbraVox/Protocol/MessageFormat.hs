-- | 1024-byte block layout
--
-- See: doc/spec/protocol.md
module UmbraVox.Protocol.MessageFormat
  ( MessageBlock
  , packBlock
  ) where

import Data.ByteString (ByteString)

-- | A fixed-size 1024-byte message block.
data MessageBlock = MessageBlock
  deriving (Show)

-- | Pack payload into a 1024-byte block with header and padding.
packBlock :: ByteString -> MessageBlock
packBlock = error "not implemented"

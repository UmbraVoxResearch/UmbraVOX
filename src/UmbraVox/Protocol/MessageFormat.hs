-- SPDX-License-Identifier: Apache-2.0
-- | 1024-byte block layout
--
-- See: doc/spec/protocol.md
module UmbraVox.Protocol.MessageFormat
  ( MessageBlock
  , packBlock
  ) where

import Data.ByteString (ByteString)

{-# WARNING MessageBlock "UmbraVox.Protocol.MessageFormat is a stub -- not implemented" #-}
{-# WARNING packBlock "UmbraVox.Protocol.MessageFormat is a stub -- not implemented" #-}

-- | A fixed-size 1024-byte message block.
data MessageBlock = MessageBlock
  deriving (Show)

-- | Pack payload into a 1024-byte block with header and padding.
packBlock :: ByteString -> MessageBlock
packBlock = error "not implemented"

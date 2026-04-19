-- | Message types, 1K block chunking
--
-- See: doc/spec/chat.md
module UmbraVox.Chat.Message
  ( ChatMessage
  , chunkMessage
  ) where

import Data.ByteString (ByteString)

-- | A chat message.
data ChatMessage = ChatMessage
  deriving (Show)

-- | Chunk a message into 1024-byte blocks.
chunkMessage :: ByteString -> [ByteString]
chunkMessage = error "not implemented"

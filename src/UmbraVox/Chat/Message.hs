-- | Message chunking and reassembly
--
-- For MVP: single-chunk messages (< 64 KB).
-- See: doc/spec/chat.md
module UmbraVox.Chat.Message
  ( chunkMessage
  , reassemble
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Chunk a message into a list of fragments.
-- For MVP, messages are sent as a single chunk (must be < 64 KB).
chunkMessage :: ByteString -> [ByteString]
chunkMessage msg = [msg]

-- | Reassemble chunks back into a single message.
reassemble :: [ByteString] -> ByteString
reassemble = BS.concat

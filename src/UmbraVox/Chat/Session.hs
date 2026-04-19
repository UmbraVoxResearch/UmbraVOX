-- | Chat conversation state
--
-- See: doc/spec/chat.md
module UmbraVox.Chat.Session
  ( ChatSession
  , newSession
  ) where

import Data.ByteString (ByteString)

-- | An active chat session.
data ChatSession = ChatSession
  deriving (Show)

-- | Create a new chat session with the given peer identity.
newSession :: ByteString -> IO ChatSession
newSession = error "not implemented"

-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
-- | Chat message -> blockchain transaction
--
-- See: doc/spec/chat.md
module UmbraVox.Chat.Transaction
  ( messageToTx
  ) where

import Data.ByteString (ByteString)

-- | Convert a chat message into a blockchain transaction.
messageToTx :: ByteString -> ByteString
messageToTx = error "not implemented"

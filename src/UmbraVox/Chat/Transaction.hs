-- SPDX-License-Identifier: Apache-2.0
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

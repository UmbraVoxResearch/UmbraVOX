-- | Group messaging sender keys
--
-- See: doc/spec/signal-protocol.md
module UmbraVox.Crypto.Signal.SenderKeys
  ( distributeSenderKey
  ) where

import Data.ByteString (ByteString)

-- | Distribute a sender key for group messaging.
distributeSenderKey :: ByteString -> ByteString
distributeSenderKey = error "not implemented"

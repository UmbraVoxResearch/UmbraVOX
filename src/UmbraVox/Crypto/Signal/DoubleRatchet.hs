-- | Double Ratchet Algorithm
--
-- See: doc/spec/signal-protocol.md
module UmbraVox.Crypto.Signal.DoubleRatchet
  ( ratchetStep
  ) where

import Data.ByteString (ByteString)

-- | Perform a single step of the double ratchet.
ratchetStep :: ByteString -> ByteString -> ByteString
ratchetStep = error "not implemented"

-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
-- | Block + transaction gossip protocol
--
-- See: doc/spec/network.md
module UmbraVox.Network.Gossip
  ( gossipBlock
  ) where

import Data.ByteString (ByteString)

-- | Gossip a block to connected peers.
gossipBlock :: ByteString -> IO ()
gossipBlock = error "not implemented"

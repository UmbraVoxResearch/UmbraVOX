-- SPDX-License-Identifier: Apache-2.0
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

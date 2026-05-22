-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
-- | Peer discovery, scoring, banning
--
-- See: doc/spec/network.md
module UmbraVox.Network.PeerManager
  ( PeerManager
  , newPeerManager
  ) where

-- | Manages peer connections, scoring, and banning.
data PeerManager = PeerManager
  deriving (Show)

-- | Create a new peer manager.
newPeerManager :: IO PeerManager
newPeerManager = error "not implemented"

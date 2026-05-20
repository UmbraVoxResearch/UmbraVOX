-- SPDX-License-Identifier: Apache-2.0
-- | Peer discovery, scoring, banning
--
-- See: doc/spec/network.md
module UmbraVox.Network.PeerManager
  ( PeerManager
  , newPeerManager
  ) where

{-# WARNING PeerManager "UmbraVox.Network.PeerManager is a stub -- not implemented" #-}
{-# WARNING newPeerManager "UmbraVox.Network.PeerManager is a stub -- not implemented" #-}

-- | Manages peer connections, scoring, and banning.
data PeerManager = PeerManager
  deriving (Show)

-- | Create a new peer manager.
newPeerManager :: IO PeerManager
newPeerManager = error "not implemented"

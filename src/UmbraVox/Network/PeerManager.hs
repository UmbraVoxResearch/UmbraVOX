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

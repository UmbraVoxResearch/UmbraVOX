-- SPDX-License-Identifier: Apache-2.0
-- | Peer discovery, scoring, and banning.
--
-- See: doc/spec/network.md
module UmbraVox.Network.PeerManager
  ( PeerManager
  , PeerInfo(..)
  , PeerSource(..)
  , newPeerManager
  , addPeer
  , removePeer
  , banPeer
  , unbanPeer
  , getPeers
  , getActivePeers
  , updateScore
  , evictStale
  ) where

import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Word (Word64)

import qualified Data.Map.Strict as Map

-- | Information about a known peer.
data PeerInfo = PeerInfo
  { piAddress   :: !String              -- ^ host:port
  , piPublicKey :: !(Maybe ByteString)  -- ^ identity key if known
  , piScore     :: !Int                 -- ^ reputation score (0-100)
  , piLastSeen  :: !Word64              -- ^ unix timestamp
  , piBanned    :: !Bool                -- ^ currently banned?
  , piBanExpiry :: !Word64              -- ^ ban expiry timestamp (0 = not banned)
  , piSource    :: !PeerSource          -- ^ how we discovered this peer
  } deriving stock (Show, Eq)

-- | How a peer was discovered.
data PeerSource
  = SourceMDNS
  | SourcePEX
  | SourceBootstrap
  | SourceManual
  deriving stock (Show, Eq)

-- | Manages peer connections, scoring, and banning.
data PeerManager = PeerManager
  { pmPeers      :: !(IORef (Map String PeerInfo))
  , pmMaxPeers   :: !Int  -- ^ max tracked peers (default 1000)
  , pmBanDefault :: !Int  -- ^ default ban duration in seconds (3600)
  }

-- | Create a new peer manager with default settings.
newPeerManager :: IO PeerManager
newPeerManager = do
  ref <- newIORef Map.empty
  pure PeerManager
    { pmPeers      = ref
    , pmMaxPeers   = 1000
    , pmBanDefault = 3600
    }

-- | Add a peer. If the peer map is at capacity and the peer is new,
-- the peer is silently dropped. If the peer already exists, its
-- last-seen timestamp is refreshed (set to 0 here; callers should
-- follow up with an appropriate timestamp update).
addPeer :: PeerManager -> String -> PeerSource -> IO ()
addPeer pm addr src = atomicModifyIORef' (pmPeers pm) $ \m ->
  case Map.lookup addr m of
    Just existing ->
      -- Peer already known: refresh last-seen, keep everything else.
      let updated = existing { piLastSeen = 0 }
      in  (Map.insert addr updated m, ())
    Nothing
      | Map.size m >= pmMaxPeers pm -> (m, ())  -- at capacity
      | otherwise ->
          let info = PeerInfo
                { piAddress   = addr
                , piPublicKey = Nothing
                , piScore     = 50  -- neutral starting score
                , piLastSeen  = 0
                , piBanned    = False
                , piBanExpiry = 0
                , piSource    = src
                }
          in  (Map.insert addr info m, ())

-- | Remove a peer entirely.
removePeer :: PeerManager -> String -> IO ()
removePeer pm addr = atomicModifyIORef' (pmPeers pm) $ \m ->
  (Map.delete addr m, ())

-- | Ban a peer for the given number of seconds. The ban expiry is
-- stored as an absolute timestamp; callers must pass the *current*
-- unix time plus the duration, or simply pass the duration and let
-- callers translate. Here we store the raw duration as the expiry
-- field for simplicity -- real wall-clock conversion belongs in the
-- caller.
banPeer :: PeerManager -> String -> Int -> IO ()
banPeer pm addr duration = atomicModifyIORef' (pmPeers pm) $ \m ->
  let expiry = fromIntegral duration :: Word64
      update info = info { piBanned = True, piBanExpiry = expiry, piScore = 0 }
  in  case Map.lookup addr m of
        Just info -> (Map.insert addr (update info) m, ())
        Nothing   ->
          -- Ban an unknown peer: create a record so we remember.
          let info = PeerInfo
                { piAddress   = addr
                , piPublicKey = Nothing
                , piScore     = 0
                , piLastSeen  = 0
                , piBanned    = True
                , piBanExpiry = expiry
                , piSource    = SourceManual
                }
          in  (Map.insert addr info m, ())

-- | Remove a ban from a peer.
unbanPeer :: PeerManager -> String -> IO ()
unbanPeer pm addr = atomicModifyIORef' (pmPeers pm) $ \m ->
  let update info = info { piBanned = False, piBanExpiry = 0 }
  in  (Map.adjust update addr m, ())

-- | Return all known peers.
getPeers :: PeerManager -> IO [PeerInfo]
getPeers pm = Map.elems <$> readIORef (pmPeers pm)

-- | Return active peers: not banned and score > 0.
getActivePeers :: PeerManager -> IO [PeerInfo]
getActivePeers pm = filter isActive . Map.elems <$> readIORef (pmPeers pm)
  where
    isActive info = not (piBanned info) && piScore info > 0

-- | Adjust a peer's score by a signed delta, clamping to [0, 100].
updateScore :: PeerManager -> String -> Int -> IO ()
updateScore pm addr delta = atomicModifyIORef' (pmPeers pm) $ \m ->
  let clamp x = max 0 (min 100 x)
      update info = info { piScore = clamp (piScore info + delta) }
  in  (Map.adjust update addr m, ())

-- | Evict peers whose last-seen timestamp is older than the given
-- threshold (absolute unix timestamp). Returns the number of peers
-- evicted. Banned peers are never evicted by staleness.
evictStale :: PeerManager -> Word64 -> IO Int
evictStale pm threshold = atomicModifyIORef' (pmPeers pm) $ \m ->
  let isStale info = not (piBanned info) && piLastSeen info < threshold
      staleKeys    = Map.keys (Map.filter isStale m)
      m'           = foldr Map.delete m staleKeys
  in  (m', length staleKeys)

-- SPDX-License-Identifier: Apache-2.0
-- | Dandelion++ stem/fluff privacy routing (M21.3.3)
--
-- Privacy routing that prevents linking messages to their origin IP:
--
-- 1. __Stem phase__: message forwarded along a single relay chain (1-3 hops)
-- 2. __Fluff phase__: after stem, broadcast to all peers (standard gossip)
-- 3. __Transition__: each relay independently decides to fluff with
--    probability /p/ (default 0.1 per hop)
--
-- Epoch rotation selects a new stem peer every 'dsEpochLen' seconds,
-- preventing long-lived stem paths from becoming traffic-analysis targets.
--
-- See: doc/spec/network.md
module UmbraVox.Network.Dandelion
  ( DandelionState(..)
  , RouteMode(..)
  , RouteDecision(..)
  , newDandelionState
  , routeMessage
  , rotateStemPeer
  , checkEpoch
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word8, Word64)

import UmbraVox.Crypto.Random (randomBytes)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Current routing mode for the local node.
data RouteMode = Stem | Fluff
    deriving stock (Show, Eq)

-- | Routing decision returned by 'routeMessage'.
data RouteDecision
    = StemForward String ByteString    -- ^ Forward to specific stem peer
    | FluffBroadcast ByteString        -- ^ Broadcast to all peers
    | DropMessage                      -- ^ No stem peer available
    deriving stock (Show, Eq)

-- | Mutable state for the Dandelion++ router.
data DandelionState = DandelionState
    { dsMode       :: !(IORef RouteMode)     -- ^ Current routing mode
    , dsStemPeer   :: !(IORef (Maybe String)) -- ^ Designated stem relay
    , dsEpochStart :: !(IORef Word64)        -- ^ Current epoch start (POSIX seconds)
    , dsEpochLen   :: !Int                   -- ^ Epoch length in seconds (default 600)
    , dsFluffProb  :: !Double                -- ^ Probability of stem->fluff per hop (default 0.1)
    }

------------------------------------------------------------------------
-- Initialization
------------------------------------------------------------------------

-- | Initialize Dandelion routing state with default parameters.
--
-- Epoch length: 600 seconds (10 minutes).
-- Fluff probability: 0.1 per hop.
-- Starts in Stem mode with no stem peer selected.
newDandelionState :: IO DandelionState
newDandelionState = do
    modeRef  <- newIORef Stem
    peerRef  <- newIORef Nothing
    epochRef <- newIORef 0
    return DandelionState
        { dsMode       = modeRef
        , dsStemPeer   = peerRef
        , dsEpochStart = epochRef
        , dsEpochLen   = 600
        , dsFluffProb  = 0.1
        }

------------------------------------------------------------------------
-- Routing
------------------------------------------------------------------------

-- | Route a message through Dandelion++.
--
-- In stem mode: probabilistically decide whether to continue stemming
-- (forward to the designated stem peer) or transition to fluff
-- (broadcast to all peers).  The transition probability is 'dsFluffProb'.
--
-- In fluff mode: always broadcast to all peers.
--
-- Returns 'DropMessage' if in stem mode but no stem peer is selected
-- (caller should call 'rotateStemPeer' first).
routeMessage :: DandelionState -> ByteString -> IO RouteDecision
routeMessage ds msg
    | BS.null msg = return DropMessage
    | otherwise = do
        mode <- readIORef (dsMode ds)
        case mode of
            Fluff -> return (FluffBroadcast msg)
            Stem  -> do
                shouldFluff <- coinFlip (dsFluffProb ds)
                if shouldFluff
                    then do
                        writeIORef (dsMode ds) Fluff
                        return (FluffBroadcast msg)
                    else do
                        mPeer <- readIORef (dsStemPeer ds)
                        case mPeer of
                            Nothing   -> return DropMessage
                            Just peer -> return (StemForward peer msg)

------------------------------------------------------------------------
-- Epoch management
------------------------------------------------------------------------

-- | Select a new stem peer for the current epoch from the given peer list.
--
-- Uses cryptographic randomness to pick a uniform random peer.
-- Resets routing mode to Stem.  If the peer list is empty, clears the
-- stem peer (subsequent 'routeMessage' calls will return 'DropMessage').
rotateStemPeer :: DandelionState -> [String] -> IO ()
rotateStemPeer ds [] = do
    writeIORef (dsStemPeer ds) Nothing
    writeIORef (dsMode ds) Stem
rotateStemPeer ds peers = do
    let n = length peers
    idx <- uniformIndex n
    writeIORef (dsStemPeer ds) (Just (peers !! idx))
    writeIORef (dsMode ds) Stem

-- | Check if the current epoch has expired and rotate if needed.
--
-- Compares the current POSIX time against 'dsEpochStart' + 'dsEpochLen'.
-- If expired, updates the epoch start to now and resets mode to Stem.
-- The caller is responsible for calling 'rotateStemPeer' with a fresh
-- peer list after 'checkEpoch' signals a rotation (by resetting to Stem
-- with no peer).
--
-- Note: This uses a coarse wall-clock check.  The caller should invoke
-- this periodically (e.g. before each 'routeMessage').
checkEpoch :: DandelionState -> Word64 -> IO Bool
checkEpoch ds now = do
    start <- readIORef (dsEpochStart ds)
    let expired = start == 0 || now >= start + fromIntegral (dsEpochLen ds)
    if expired
        then do
            writeIORef (dsEpochStart ds) now
            writeIORef (dsMode ds) Stem
            writeIORef (dsStemPeer ds) Nothing
            return True
        else return False

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- | Probabilistic coin flip using CSPRNG.
--
-- Returns True with probability /p/ (clamped to [0,1]).
-- Draws one byte from 'randomBytes' and compares against the threshold.
coinFlip :: Double -> IO Bool
coinFlip p
    | p <= 0.0  = return False
    | p >= 1.0  = return True
    | otherwise = do
        b <- randomBytes 1
        let !val = fromIntegral (BS.index b 0) :: Double
            -- Threshold: p * 256, so P(val < threshold) ~ p
            !threshold = p * 256.0
        return (val < threshold)

-- | Pick a uniform random index in [0, n-1] using CSPRNG.
--
-- For small n (< 256), draws one byte and uses modular reduction.
-- For larger n, draws two bytes.  Bias is negligible for practical
-- peer list sizes (< 1000).
uniformIndex :: Int -> IO Int
uniformIndex n
    | n <= 0    = return 0
    | n == 1    = return 0
    | n <= 256  = do
        b <- randomBytes 1
        let !val = fromIntegral (BS.index b 0) :: Int
        return (val `mod` n)
    | otherwise = do
        bs <- randomBytes 2
        let !b0 = fromIntegral (BS.index bs 0 :: Word8) :: Int
            !b1 = fromIntegral (BS.index bs 1 :: Word8) :: Int
            !val = b0 + b1 * 256
        return (val `mod` n)

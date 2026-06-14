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
  , routeMessageRateLimited
  , rotateStemPeer
  , checkEpoch
  , effectiveFluffProb
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word64)
import Control.Concurrent (threadDelay)

import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Network.RateLimit (RateLimiter, checkRate)

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
    { dsMode         :: !(IORef RouteMode)     -- ^ Current routing mode
    , dsStemPeer     :: !(IORef (Maybe String)) -- ^ Designated stem relay
    , dsPrevStemPeer :: !(IORef (Maybe String)) -- ^ Previous epoch's stem peer (M23.2.6)
    , dsEpochStart   :: !(IORef Word64)        -- ^ Current epoch start (POSIX seconds)
    , dsEpochLen     :: !Int                   -- ^ Epoch length in seconds (default 600)
    , dsFluffProb    :: !Double                -- ^ Probability of stem->fluff per hop (default 0.1)
    , dsPeerCount    :: !(IORef Int)           -- ^ Current known peer count (M23.2.5)
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
    modeRef     <- newIORef Stem
    peerRef     <- newIORef Nothing
    prevPeerRef <- newIORef Nothing
    epochRef    <- newIORef 0
    peerCntRef  <- newIORef 0
    return DandelionState
        { dsMode         = modeRef
        , dsStemPeer     = peerRef
        , dsPrevStemPeer = prevPeerRef
        , dsEpochStart   = epochRef
        , dsEpochLen     = 600
        , dsFluffProb    = 0.1
        , dsPeerCount    = peerCntRef
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
-- Falls back to 'FluffBroadcast' if in stem mode but no stem peer is
-- selected (M23.2.7), ensuring messages are never silently dropped.
-- Returns 'DropMessage' only for empty messages.
routeMessage :: DandelionState -> ByteString -> IO RouteDecision
routeMessage ds msg
    | BS.null msg = return DropMessage
    | otherwise = do
        mode <- readIORef (dsMode ds)
        case mode of
            Fluff -> return (FluffBroadcast msg)
            Stem  -> do
                prob <- effectiveFluffProb ds
                shouldFluff <- coinFlip prob
                if shouldFluff
                    then do
                        writeIORef (dsMode ds) Fluff
                        return (FluffBroadcast msg)
                    else do
                        mPeer <- readIORef (dsStemPeer ds)
                        case mPeer of
                            -- M23.2.7: fall back to fluff instead of dropping
                            Nothing   -> return (FluffBroadcast msg)
                            Just peer -> do
                                -- M27.6.3: Add timing jitter before stem forwarding
                                -- to frustrate timing-based traffic analysis.
                                --
                                -- Finding:     M27.6.3 — Stem-phase messages were
                                --              forwarded immediately, allowing a
                                --              timing-correlated adversary to link
                                --              sender and recipient.
                                -- Vulnerability: Without jitter, an adversary
                                --              observing message arrival and departure
                                --              times at relay nodes can correlate
                                --              stem-phase messages to their origin.
                                -- Fix:         Introduce a random delay of 50-500 ms
                                --              (drawn from CSPRNG) before forwarding
                                --              stem-phase messages.
                                -- Verified:    Stem forwarding now incurs a random
                                --              delay; timing correlation is disrupted.
                                jitterUs <- stemJitter
                                threadDelay jitterUs
                                return (StemForward peer msg)

-- | Route a message through Dandelion++ with per-session relay rate
-- limiting (M23.1.1h DoS mitigation).
--
-- @routeMessageRateLimited ds limiter now msg@ checks the relay rate
-- limiter before routing.  If the source session has exceeded
-- 'defaultRelayCap' forwards in the current window, the message is
-- dropped.  Otherwise it delegates to 'routeMessage'.
routeMessageRateLimited :: DandelionState -> RateLimiter -> Word64 -> ByteString -> IO RouteDecision
routeMessageRateLimited ds limiter now msg = do
    allowed <- checkRate limiter now
    if allowed
        then routeMessage ds msg
        else return DropMessage

------------------------------------------------------------------------
-- Epoch management
------------------------------------------------------------------------

-- | Select a new stem peer for the current epoch from the given peer list.
--
-- Uses cryptographic randomness to pick a uniform random peer.
-- Resets routing mode to Stem.  If the peer list is empty, clears the
-- stem peer (subsequent 'routeMessage' calls will return 'DropMessage').
rotateStemPeer :: DandelionState -> [String] -> IO ()
rotateStemPeer ds peers = do
    writeIORef (dsPeerCount ds) (length peers)
    -- M23.2.6: remember current stem peer as previous before replacing.
    current <- readIORef (dsStemPeer ds)
    writeIORef (dsPrevStemPeer ds) current
    -- Filter out the previous stem peer for one epoch.
    prev <- readIORef (dsPrevStemPeer ds)
    let candidates = case prev of
            Nothing -> peers
            Just p  -> let filtered = filter (/= p) peers
                       in if null filtered then peers else filtered
    case candidates of
        [] -> do
            writeIORef (dsStemPeer ds) Nothing
            writeIORef (dsMode ds) Stem
        _  -> do
            let n = length candidates
            idx <- uniformIndex n
            writeIORef (dsStemPeer ds) (Just (candidates !! idx))
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

-- | Compute effective fluff probability scaled by peer count (M23.2.5).
--
-- With fewer than 5 peers, stem routing provides negligible privacy
-- (too few hops to decorrelate origin), so we force fluff (p=1.0).
-- Otherwise, use the configured 'dsFluffProb'.
effectiveFluffProb :: DandelionState -> IO Double
effectiveFluffProb ds = do
    n <- readIORef (dsPeerCount ds)
    if n < 5
        then return 1.0
        else return (dsFluffProb ds)

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

-- | Pick a uniform random index in [0, n-1] using CSPRNG with rejection
-- sampling to eliminate modular bias (M23.3.6).
--
-- Draws the smallest number of bytes whose value range (256^k) covers /n/,
-- then rejects any draw at or above the largest multiple of /n/ within that
-- range before reducing modulo /n/. Supports arbitrary positive /n/.
--
-- SECURITY (M40.42): the previous implementation only handled n <= 65536
-- (a fixed 2-byte sampler). For n > 65536 the rejection limit computed to
-- @65536 - (65536 `mod` n) == 0@, so every draw was rejected and the sampler
-- recursed forever. 'stemJitter' calls @uniformIndex 450001@, so every
-- Dandelion stem-forward busy-looped indefinitely (a denial of service). The
-- byte width is now derived from /n/ so the rejection limit is always > 0.
uniformIndex :: Int -> IO Int
uniformIndex n
    | n <= 1    = return 0
    | otherwise = go
  where
    !nbytes = bytesNeeded n                  -- smallest k with 256^k >= n
    !range  = 256 ^ nbytes :: Integer        -- size of the draw space
    !nI     = fromIntegral n :: Integer
    !limit  = range - (range `mod` nI)       -- largest multiple of n <= range (> 0)
    go = do
        bs <- randomBytes nbytes
        let !val = BS.foldl' (\acc w -> acc * 256 + fromIntegral w) 0 bs :: Integer
        if val >= limit
            then go  -- reject biased tail and redraw
            else return (fromIntegral (val `mod` nI))

-- | Smallest number of bytes @k@ such that @256^k >= n@ (n >= 1).
bytesNeeded :: Int -> Int
bytesNeeded n = go 1 256
  where
    nI = fromIntegral n :: Integer
    go !k !range
        | range >= nI = k
        | otherwise   = go (k + 1) (range * 256)

-- | Generate a random stem-forwarding jitter delay in microseconds.
-- Range: 50,000 - 500,000 us (50 - 500 ms).
-- Uses CSPRNG via 'uniformIndex' for uniform distribution.
stemJitter :: IO Int
stemJitter = do
    -- 450001 possible values: [50000, 50001, ..., 500000]
    -- We sample uniformly in [0, 450000] and add 50000.
    offset <- uniformIndex 450001
    return (50000 + offset)

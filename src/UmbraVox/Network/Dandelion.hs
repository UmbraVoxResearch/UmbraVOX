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
  , effectiveFluffProb
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
-- For n <= 256 we draw one byte; for larger n we draw two bytes.
-- Values at or above the largest multiple of n that fits in the byte
-- range are rejected and redrawn.
uniformIndex :: Int -> IO Int
uniformIndex n
    | n <= 0    = return 0
    | n == 1    = return 0
    | n <= 256  = rejectionSample1 n
    | otherwise = rejectionSample2 n

-- | Rejection sampling with 1 byte (range 0-255).
rejectionSample1 :: Int -> IO Int
rejectionSample1 n = do
    b <- randomBytes 1
    let !val  = fromIntegral (BS.index b 0) :: Int
        !limit = 256 - (256 `mod` n)  -- largest multiple of n <= 256
    if val >= limit
        then rejectionSample1 n  -- reject and retry
        else return (val `mod` n)

-- | Rejection sampling with 2 bytes (range 0-65535).
rejectionSample2 :: Int -> IO Int
rejectionSample2 n = do
    bs <- randomBytes 2
    let !b0  = fromIntegral (BS.index bs 0 :: Word8) :: Int
        !b1  = fromIntegral (BS.index bs 1 :: Word8) :: Int
        !val = b0 + b1 * 256
        !limit = 65536 - (65536 `mod` n)  -- largest multiple of n <= 65536
    if val >= limit
        then rejectionSample2 n  -- reject and retry
        else return (val `mod` n)

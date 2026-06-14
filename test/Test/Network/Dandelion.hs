-- SPDX-License-Identifier: Apache-2.0
-- | Dandelion++ routing test suite (M21.3.3).
--
-- Tests stem/fluff mode transitions, epoch management, and edge cases.
module Test.Network.Dandelion (runTests) where

import Data.IORef
import qualified Data.ByteString as BS

import Test.Util (assertEq)
import UmbraVox.Network.Dandelion
    ( DandelionState(..)
    , RouteMode(..)
    , RouteDecision(..)
    , newDandelionState
    , routeMessage
    , rotateStemPeer
    , checkEpoch
    )

runTests :: IO Bool
runTests = do
    putStrLn "[Dandelion] Running Dandelion++ routing tests..."
    results <- sequence
        [ testNewStateIsStem
        , testStemNoPeerDrops
        , testRotateThenStemForward
        , testEmptyMessageDropped
        , testEventualFluffTransition
        , testCheckEpochFreshNoRotation
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Dandelion] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Test 1: New state starts in Stem mode.
testNewStateIsStem :: IO Bool
testNewStateIsStem = do
    ds <- newDandelionState
    mode <- readIORef (dsMode ds)
    r1 <- assertEq "new state is Stem" Stem mode
    peer <- readIORef (dsStemPeer ds)
    r2 <- assertEq "new state has no stem peer" Nothing peer
    pure (r1 && r2)

-- | Test 2: routeMessage in Stem with no peer falls back to FluffBroadcast
-- (M23.2.7 — no silent message drops).
testStemNoPeerDrops :: IO Bool
testStemNoPeerDrops = do
    ds <- newDandelionState
    -- Force stem mode, ensure no fluff transition by setting probability to 0
    writeIORef (dsMode ds) Stem
    writeIORef (dsStemPeer ds) Nothing
    -- We need dsFluffProb = 0 so coinFlip always returns False (stay in stem).
    -- DandelionState uses immutable dsFluffProb, so we create a state with 0 prob.
    ds0 <- newDandelionStateWith 0.0
    let msg = BS.pack [1, 2, 3]
    result <- routeMessage ds0 msg
    assertEq "stem with no peer -> FluffBroadcast" (FluffBroadcast msg) result

-- | Test 3: After rotateStemPeer, routeMessage in Stem returns StemForward.
testRotateThenStemForward :: IO Bool
testRotateThenStemForward = do
    ds <- newDandelionStateWith 0.0  -- fluff prob 0 = always stay in stem
    -- M23.2.5: effectiveFluffProb forces fluff (p=1.0) when peer count < 5
    -- (too few hops to decorrelate origin). Supply >= 5 peers so the
    -- configured fluff prob (0.0) applies and stem forwarding can occur.
    let peers = ["peer-A", "peer-B", "peer-C", "peer-D", "peer-E"]
    rotateStemPeer ds peers
    let msg = BS.pack [10, 20, 30]
    result <- routeMessage ds msg
    case result of
        StemForward peer payload -> do
            r1 <- assertEq "StemForward payload matches" msg payload
            -- Peer should be one of the provided peers
            let valid = peer `elem` peers
            r2 <- assertEq "StemForward peer is valid" True valid
            pure (r1 && r2)
        other -> do
            putStrLn $ "  FAIL: expected StemForward, got " ++ show other
            pure False

-- | Test 4: Empty message returns DropMessage regardless of mode.
testEmptyMessageDropped :: IO Bool
testEmptyMessageDropped = do
    ds <- newDandelionState
    rotateStemPeer ds ["peer-X"]
    result <- routeMessage ds BS.empty
    assertEq "empty message -> DropMessage" DropMessage result

-- | Test 5: The stem/fluff decision is probabilistic — over many trials it
-- produces some Fluff transitions but not all (i.e. it is neither stuck in
-- stem nor stuck in fluff).
--
-- Uses fluffProb = 0.5 (not the production default 0.1) and a modest trial
-- count deliberately, for two reasons:
--   * M27.6.3: every StemForward incurs a real 50-500 ms 'threadDelay'
--     (timing jitter). With p=0.1 ~90% of trials take the stem path, so 1000
--     trials would sleep for minutes. At p=0.5 only ~half the trials sleep
--     and the count is small, keeping the test fast.
--   * Statistical safety: at p=0.5, P(zero fluffs) = P(all fluffs) = 0.5^64,
--     which is ~5e-20 — far below any meaningful flake threshold, so the
--     "some but not all" assertions are effectively deterministic.
-- M23.2.5: >= 5 peers so effectiveFluffProb uses the configured 0.5 rather
-- than the forced 1.0 applied when peer count < 5.
testEventualFluffTransition :: IO Bool
testEventualFluffTransition = do
    let trials = 64 :: Int
    fluffCount <- go trials (0 :: Int)
    let atLeastSome = fluffCount > 0
    r1 <- assertEq "at least some Fluff transitions over trials" True atLeastSome
    let notAll = fluffCount < trials
    r2 <- assertEq "not all trials are Fluff" True notAll
    putStrLn $ "    (fluff count: " ++ show fluffCount ++ "/" ++ show trials ++ ")"
    pure (r1 && r2)
  where
    go :: Int -> Int -> IO Int
    go 0 !acc = pure acc
    go !n !acc = do
        ds <- newDandelionStateWith 0.5  -- fresh state each trial, p=0.5
        rotateStemPeer ds ["relay-A", "relay-B", "relay-C", "relay-D", "relay-E"]
        result <- routeMessage ds (BS.pack [42])
        case result of
            FluffBroadcast _ -> go (n - 1) (acc + 1)
            _                -> go (n - 1) acc

-- | Test 6: checkEpoch with fresh state (epoch start = 0) triggers rotation.
-- Then immediately calling again with the same time does NOT trigger.
testCheckEpochFreshNoRotation :: IO Bool
testCheckEpochFreshNoRotation = do
    ds <- newDandelionState
    -- Fresh state has epochStart = 0, so checkEpoch should trigger
    rotated1 <- checkEpoch ds 1000
    r1 <- assertEq "checkEpoch on fresh state triggers rotation" True rotated1
    -- Now epochStart = 1000, epochLen = 600, so at t=1000 it should NOT rotate
    rotated2 <- checkEpoch ds 1000
    r2 <- assertEq "checkEpoch immediately after does not rotate" False rotated2
    -- At t=1599 still within epoch
    rotated3 <- checkEpoch ds 1599
    r3 <- assertEq "checkEpoch within epoch does not rotate" False rotated3
    -- At t=1600 (1000+600) it should rotate
    rotated4 <- checkEpoch ds 1600
    r4 <- assertEq "checkEpoch at epoch boundary triggers rotation" True rotated4
    pure (r1 && r2 && r3 && r4)

------------------------------------------------------------------------
-- Helper: create DandelionState with custom fluff probability
------------------------------------------------------------------------

-- | Create a DandelionState with a specific fluff probability.
-- This works around the immutable dsFluffProb field.
newDandelionStateWith :: Double -> IO DandelionState
newDandelionStateWith prob = do
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
        , dsFluffProb    = prob
        , dsPeerCount    = peerCntRef
        }

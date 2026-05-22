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
    rotateStemPeer ds ["peer-A", "peer-B", "peer-C"]
    let msg = BS.pack [10, 20, 30]
    result <- routeMessage ds msg
    case result of
        StemForward peer payload -> do
            r1 <- assertEq "StemForward payload matches" msg payload
            -- Peer should be one of the provided peers
            let valid = peer `elem` ["peer-A", "peer-B", "peer-C"]
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

-- | Test 5: With default fluff probability (0.1), running 1000 times
-- should produce at least some Fluff transitions.
testEventualFluffTransition :: IO Bool
testEventualFluffTransition = do
    let trials = 1000 :: Int
    fluffCount <- go trials (0 :: Int)
    let atLeastSome = fluffCount > 0
    r1 <- assertEq "at least some Fluff transitions in 1000 trials" True atLeastSome
    -- With p=0.1, expected ~100 fluffs. Check we don't get all fluffs either.
    let notAll = fluffCount < trials
    r2 <- assertEq "not all trials are Fluff" True notAll
    putStrLn $ "    (fluff count: " ++ show fluffCount ++ "/" ++ show trials ++ ")"
    pure (r1 && r2)
  where
    go :: Int -> Int -> IO Int
    go 0 !acc = pure acc
    go !n !acc = do
        ds <- newDandelionState  -- fresh state each time (default fluffProb=0.1)
        rotateStemPeer ds ["stem-relay"]
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
    modeRef  <- newIORef Stem
    peerRef  <- newIORef Nothing
    epochRef <- newIORef 0
    return DandelionState
        { dsMode       = modeRef
        , dsStemPeer   = peerRef
        , dsEpochStart = epochRef
        , dsEpochLen   = 600
        , dsFluffProb  = prob
        }

-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Economics.Penalty
module Test.Economics.Penalty (runTests) where

import Test.Util (assertEq, checkProperty, nextWord32)
import UmbraVox.Economics.Penalty
    ( computePenalty
    , applyTier1
    , applyTier2
    , applyTier3
    , recoverTier1
    , recoverTier2
    , slashStake
    )

runTests :: IO Bool
runTests = do
    putStrLn "Economics.Penalty"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testComputePenaltyTier1
        , testComputePenaltyTier2
        , testComputePenaltyTier3
        , testComputePenaltyInvalidTier
        , testComputePenaltyZeroStake
        , testApplyTier1
        , testApplyTier2
        , testApplyTier3
        , testRecoverTier1FromPoint9
        , testRecoverTier1Convergence
        , testRecoverTier2FromPoint5
        , testRecoverTier2Convergence
        , testSlashStake
        , testSlashStakeZero
        , testSlashStakeNegative
        , testTier1RecoveryTimeline
        , propSlashStakeQuarter
        , propTier3AlwaysZero
        , propRecoverTier1Monotonic
        ]
    pure (and results)

-- computePenalty
testComputePenaltyTier1 :: IO Bool
testComputePenaltyTier1 =
    assertEq "computePenalty 1: no slash" 0 (computePenalty 1 100000)

testComputePenaltyTier2 :: IO Bool
testComputePenaltyTier2 =
    assertEq "computePenalty 2: no slash" 0 (computePenalty 2 100000)

testComputePenaltyTier3 :: IO Bool
testComputePenaltyTier3 =
    -- 25% of 100000 = 25000
    assertEq "computePenalty 3: 25% slash" 25000 (computePenalty 3 100000)

testComputePenaltyInvalidTier :: IO Bool
testComputePenaltyInvalidTier =
    assertEq "computePenalty 0: no penalty" 0 (computePenalty 0 100000)

testComputePenaltyZeroStake :: IO Bool
testComputePenaltyZeroStake =
    assertEq "computePenalty tier3 zero stake" 0 (computePenalty 3 0)

-- applyTier*
testApplyTier1 :: IO Bool
testApplyTier1 =
    -- P=1.0 * 0.9 = 0.9
    assertEq "applyTier1 1.0 == 0.9" 0.9 (applyTier1 1.0)

testApplyTier2 :: IO Bool
testApplyTier2 =
    -- P=1.0 * 0.5 = 0.5
    assertEq "applyTier2 1.0 == 0.5" 0.5 (applyTier2 1.0)

testApplyTier3 :: IO Bool
testApplyTier3 =
    assertEq "applyTier3 anything == 0.0" 0.0 (applyTier3 0.75)

-- recovery
testRecoverTier1FromPoint9 :: IO Bool
testRecoverTier1FromPoint9 =
    -- 0.3 + 0.7 * 0.9 = 0.3 + 0.63 ~= 0.93 (floating-point)
    assertEq "recoverTier1 0.9 ~= 0.93" True (abs (recoverTier1 0.9 - 0.93) < 1e-10)

testRecoverTier1Convergence :: IO Bool
testRecoverTier1Convergence = do
    -- After several clean cycles from P=0.9, should approach 1.0
    let p0 = 0.9
        p1 = recoverTier1 p0  -- 0.93
        p2 = recoverTier1 p1  -- 0.951
        p3 = recoverTier1 p2  -- ~0.9657
    a <- assertEq "tier1 recovery p1 > p0" True (p1 > p0)
    b <- assertEq "tier1 recovery p2 > p1" True (p2 > p1)
    c <- assertEq "tier1 recovery p3 > p2" True (p3 > p2)
    d <- assertEq "tier1 recovery p2 > 0.95" True (p2 > 0.95)
    pure (a && b && c && d)

testRecoverTier2FromPoint5 :: IO Bool
testRecoverTier2FromPoint5 =
    -- 0.1 + 0.9 * 0.5 = 0.1 + 0.45 = 0.55
    assertEq "recoverTier2 0.5 == 0.55" 0.55 (recoverTier2 0.5)

testRecoverTier2Convergence :: IO Bool
testRecoverTier2Convergence = do
    -- From P=0.5, recovery is slow (242 days to >0.95)
    let p0 = 0.5
        p1 = recoverTier2 p0  -- 0.55
        p5 = iterate recoverTier2 p0 !! 5
        p10 = iterate recoverTier2 p0 !! 10
    a <- assertEq "tier2 recovery p1 > p0" True (p1 > p0)
    b <- assertEq "tier2 recovery p5 < 0.95" True (p5 < 0.95)
    c <- assertEq "tier2 recovery p10 monotonic" True (p10 > p5)
    pure (a && b && c)

-- slashStake
testSlashStake :: IO Bool
testSlashStake =
    assertEq "slashStake 100000 == 25000" 25000 (slashStake 100000)

testSlashStakeZero :: IO Bool
testSlashStakeZero =
    assertEq "slashStake 0 == 0" 0 (slashStake 0)

testSlashStakeNegative :: IO Bool
testSlashStakeNegative =
    assertEq "slashStake (-100) == 0" 0 (slashStake (-100))

-- Tier 1 recovery timeline per spec: P=0.9 -> 0.93 -> 0.951 -> 0.966
testTier1RecoveryTimeline :: IO Bool
testTier1RecoveryTimeline = do
    let p0 = applyTier1 1.0          -- 0.9
        p1 = recoverTier1 p0         -- 0.93
        p2 = recoverTier1 p1         -- 0.951
        p3 = recoverTier1 p2         -- ~0.9657
    a <- assertEq "tier1 timeline: start" 0.9 p0
    b <- assertEq "tier1 timeline: cycle 1 ~= 0.93" True (abs (p1 - 0.93) < 1e-10)
    c <- assertEq "tier1 timeline: cycle 2 > 0.95" True (p2 > 0.95)
    d <- assertEq "tier1 timeline: cycle 3 > 0.96" True (p3 > 0.96)
    pure (a && b && c && d)

-- Properties
propSlashStakeQuarter :: IO Bool
propSlashStakeQuarter =
    checkProperty "slashStake == stake * 25 / 100" 200 $ \g ->
        let (w, _) = nextWord32 g
            stake = fromIntegral w :: Integer
        in slashStake stake == (stake * 25) `div` 100

propTier3AlwaysZero :: IO Bool
propTier3AlwaysZero =
    checkProperty "applyTier3 always returns 0.0" 200 $ \g ->
        let (w, _) = nextWord32 g
            p = fromIntegral w / 1000000.0 :: Double
        in applyTier3 p == 0.0

propRecoverTier1Monotonic :: IO Bool
propRecoverTier1Monotonic =
    checkProperty "recoverTier1 increases toward 1.0" 200 $ \g ->
        let (w, _) = nextWord32 g
            p = fromIntegral (w `mod` 1000) / 1000.0 :: Double
        in recoverTier1 p > p

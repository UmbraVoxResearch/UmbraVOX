-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Economics.Rewards
module Test.Economics.Rewards (runTests) where

import Data.Word (Word32)
import Test.Util (assertEq, checkProperty, nextWord32)
import UmbraVox.Economics.Rewards
    ( computeReward
    , stakeMultiplier
    , poolAllocation
    , minRewardPerValidator
    , standbyReward
    )

runTests :: IO Bool
runTests = do
    putStrLn "Economics.Rewards"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testMinRewardPerValidator
        , testStandbyReward
        , testPoolAllocation
        , testPoolAllocationZero
        , testPoolAllocationLarge
        , testStakeMultiplierZeroUptime
        , testStakeMultiplierFullUptime
        , testStakeMultiplierHalfUptime
        , testStakeMultiplierClampNegative
        , testStakeMultiplierClampAboveOne
        , testComputeRewardZeroStake
        , testComputeRewardNegativeStake
        , testComputeRewardBaseline
        , testComputeRewardFullUptime
        , propStakeMultiplierRange
        , propComputeRewardNonNegative
        , propPoolAllocationLessOrEqual
        ]
    pure (and results)

-- Constants
testMinRewardPerValidator :: IO Bool
testMinRewardPerValidator =
    assertEq "minRewardPerValidator == 5000" 5000 minRewardPerValidator

testStandbyReward :: IO Bool
testStandbyReward =
    assertEq "standbyReward == 1000" 1000 standbyReward

-- poolAllocation
testPoolAllocation :: IO Bool
testPoolAllocation =
    -- 85% of 9,350,000,000 = 7,947,500,000
    assertEq "poolAllocation 9350000000" 7947500000 (poolAllocation 9350000000)

testPoolAllocationZero :: IO Bool
testPoolAllocationZero =
    assertEq "poolAllocation 0 == 0" 0 (poolAllocation 0)

testPoolAllocationLarge :: IO Bool
testPoolAllocationLarge =
    -- 85% of 1000 = 850
    assertEq "poolAllocation 1000 == 850" 850 (poolAllocation 1000)

-- stakeMultiplier
testStakeMultiplierZeroUptime :: IO Bool
testStakeMultiplierZeroUptime =
    assertEq "stakeMultiplier 0.0 == 0.5" 0.5 (stakeMultiplier 0.0)

testStakeMultiplierFullUptime :: IO Bool
testStakeMultiplierFullUptime =
    assertEq "stakeMultiplier 1.0 == 0.8" 0.8 (stakeMultiplier 1.0)

testStakeMultiplierHalfUptime :: IO Bool
testStakeMultiplierHalfUptime =
    assertEq "stakeMultiplier 0.5 == 0.65" 0.65 (stakeMultiplier 0.5)

testStakeMultiplierClampNegative :: IO Bool
testStakeMultiplierClampNegative =
    assertEq "stakeMultiplier (-0.5) clamped to 0.5" 0.5 (stakeMultiplier (-0.5))

testStakeMultiplierClampAboveOne :: IO Bool
testStakeMultiplierClampAboveOne =
    assertEq "stakeMultiplier 1.5 clamped to 0.8" 0.8 (stakeMultiplier 1.5)

-- computeReward
testComputeRewardZeroStake :: IO Bool
testComputeRewardZeroStake =
    assertEq "computeReward 0 1.0 == 0" 0 (computeReward 0 1.0)

testComputeRewardNegativeStake :: IO Bool
testComputeRewardNegativeStake =
    assertEq "computeReward (-100) 1.0 == 0" 0 (computeReward (-100) 1.0)

testComputeRewardBaseline :: IO Bool
testComputeRewardBaseline =
    -- stake=100000, uptime=0.0 -> 100000 * 0.5 = 50000
    assertEq "computeReward 100000 0.0" 50000 (computeReward 100000 0.0)

testComputeRewardFullUptime :: IO Bool
testComputeRewardFullUptime =
    -- stake=100000, uptime=1.0 -> 100000 * 0.8 = 80000
    assertEq "computeReward 100000 1.0" 80000 (computeReward 100000 1.0)

-- Properties
propStakeMultiplierRange :: IO Bool
propStakeMultiplierRange =
    checkProperty "stakeMultiplier in [0.5, 0.8]" 200 $ \g ->
        let (w, _) = nextWord32 g
            uptime = fromIntegral w / fromIntegral (maxBound :: Word32) :: Double
            sm = stakeMultiplier uptime
        in sm >= 0.5 && sm <= 0.8

propComputeRewardNonNegative :: IO Bool
propComputeRewardNonNegative =
    checkProperty "computeReward >= 0 for positive stake" 200 $ \g ->
        let (w, _) = nextWord32 g
            stake = fromIntegral w :: Integer
            uptime = fromIntegral w / fromIntegral (maxBound :: Word32) :: Double
        in computeReward stake uptime >= 0

propPoolAllocationLessOrEqual :: IO Bool
propPoolAllocationLessOrEqual =
    checkProperty "poolAllocation <= pool" 200 $ \g ->
        let (w, _) = nextWord32 g
            pool = fromIntegral w :: Integer
        in poolAllocation pool <= pool

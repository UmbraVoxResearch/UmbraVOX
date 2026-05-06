-- SPDX-License-Identifier: Apache-2.0
-- | Uptime-weighted stake multiplier rewards
--
-- See: attic/doc-legacy-2026-04-28/06-economics.md "Reward Formula"
module UmbraVox.Economics.Rewards
  ( computeReward
  , stakeMultiplier
  , poolAllocation
  , minRewardPerValidator
  , standbyReward
  ) where

-- | Minimum reward per active validator per cycle (5,000 MTK).
minRewardPerValidator :: Integer
minRewardPerValidator = 5000

-- | Standby reward for queued validators (1,000 MTK).
standbyReward :: Integer
standbyReward = 1000

-- | Compute pool allocation: 85% of the pool is distributed as rewards.
poolAllocation :: Integer -> Integer
poolAllocation pool = (pool * 85) `div` 100

-- | Compute stake multiplier S_i = 0.5 + 0.3 * uptimeRatio.
-- uptimeRatio is clamped to [0.0, 1.0].
-- Result is in range [0.5, 0.8].
stakeMultiplier :: Double -> Double
stakeMultiplier uptimeRatio = 0.5 + 0.3 * clamp01 uptimeRatio
  where
    clamp01 x
        | x < 0.0   = 0.0
        | x > 1.0   = 1.0
        | otherwise  = x

-- | Compute the reward for a single validator given their stake (used as
-- proportional weight) and uptime ratio.
--
-- This computes the raw reward weight: stake * stakeMultiplier(uptime).
-- The actual reward is (rawWeight / totalRawWeight) * poolAllocation.
-- This function returns the raw weight as an Integer for composability.
computeReward :: Integer -> Double -> Integer
computeReward stake uptimeRatio
    | stake <= 0 = 0
    | otherwise  = round (fromIntegral stake * stakeMultiplier uptimeRatio)

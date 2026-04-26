-- | Uptime-weighted stake multiplier rewards
--
-- See: doc/spec/economics.md
module UmbraVox.Economics.Rewards
  ( computeReward
  ) where

-- | Compute the reward for a validator given stake and uptime.
computeReward :: Integer -> Double -> Integer
computeReward = error "not implemented"

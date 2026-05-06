-- | Tiered punitive multipliers
--
-- See: doc/06-economics.md "Punitive Multipliers (Tiered)"
module UmbraVox.Economics.Penalty
  ( computePenalty
  , applyTier1
  , applyTier2
  , applyTier3
  , recoverTier1
  , recoverTier2
  , slashStake
  ) where

-- | Compute the penalty for a validator given infraction tier (1-3)
-- and their current stake.
--
-- Returns the amount of stake to slash (0 for tiers 1-2, 25% for tier 3).
-- Tiers 1 and 2 affect reward multipliers, not stake directly.
computePenalty :: Int -> Integer -> Integer
computePenalty tier stake
    | tier == 3 = slashStake stake
    | tier == 1 = 0  -- Tier 1: reward reduction only
    | tier == 2 = 0  -- Tier 2: reward reduction only
    | otherwise = 0  -- Invalid tier: no penalty

-- | Apply Tier 1 penalty: P_carryover = P_carryover * 0.9
-- Returns the new P_carryover value.
applyTier1 :: Double -> Double
applyTier1 pCarryover = pCarryover * 0.9

-- | Apply Tier 2 penalty: P_carryover = P_carryover * 0.5
-- Returns the new P_carryover value.
applyTier2 :: Double -> Double
applyTier2 pCarryover = pCarryover * 0.5

-- | Apply Tier 3 penalty: P_carryover = 0.0 (permanent).
-- Returns 0.0.
applyTier3 :: Double -> Double
applyTier3 _ = 0.0

-- | Tier 1 recovery: P_carryover = 0.3 + 0.7 * P_carryover per clean cycle.
recoverTier1 :: Double -> Double
recoverTier1 pCarryover = 0.3 + 0.7 * pCarryover

-- | Tier 2 recovery: P_carryover = 0.1 + 0.9 * P_carryover per clean cycle.
recoverTier2 :: Double -> Double
recoverTier2 pCarryover = 0.1 + 0.9 * pCarryover

-- | Slash 25% of stake (Tier 3). Returns the amount slashed.
slashStake :: Integer -> Integer
slashStake stake
    | stake <= 0 = 0
    | otherwise  = (stake * 25) `div` 100

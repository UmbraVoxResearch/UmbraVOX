-- | Tiered punitive multipliers
--
-- See: doc/spec/economics.md
module UmbraVox.Economics.Penalty
  ( computePenalty
  ) where

-- | Compute the penalty for a validator given infraction tier and stake.
computePenalty :: Int -> Integer -> Integer
computePenalty = error "not implemented"

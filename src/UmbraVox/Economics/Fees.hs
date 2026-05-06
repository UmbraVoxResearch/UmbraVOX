-- SPDX-License-Identifier: Apache-2.0
-- | Message cost: C_base * ceil(size/1024)
--
-- See: attic/doc-legacy-2026-04-28/06-economics.md "Message Cost (Dynamic, EMA-Adjusted)"
module UmbraVox.Economics.Fees
  ( messageFee
  , feeFloorDefault
  , feeCeilingDefault
  , clampFee
  ) where

-- | Default fee floor (10 MTK).
feeFloorDefault :: Integer
feeFloorDefault = 10

-- | Default fee ceiling (10,000 MTK).
feeCeilingDefault :: Integer
feeCeilingDefault = 10000

-- | Clamp a base fee to be within [floor, ceiling].
clampFee :: Integer -> Integer -> Integer -> Integer
clampFee fFloor fCeiling baseFee
    | baseFee < fFloor   = fFloor
    | baseFee > fCeiling = fCeiling
    | otherwise          = baseFee

-- | Compute the fee for a message of the given size in bytes.
-- Uses the formula: base_fee * ceil(size_bytes / 1024)
-- where base_fee defaults to fee_floor (10 MTK).
-- Returns 0 for non-positive sizes.
messageFee :: Int -> Integer
messageFee sizeBytes
    | sizeBytes <= 0 = 0
    | otherwise      = feeFloorDefault * fromIntegral (ceilDiv sizeBytes 1024)
  where
    ceilDiv a b = (a + b - 1) `div` b

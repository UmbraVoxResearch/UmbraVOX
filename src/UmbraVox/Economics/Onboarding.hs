-- | New user token bootstrap
--
-- See: doc/06-economics.md "New User Onboarding (Recycling Faucet)"
module UmbraVox.Economics.Onboarding
  ( bootstrapUser
  , faucetGrant
  , quadraticBondingStake
  , baseStake
  , inactiveReclaimThreshold
  ) where

import Data.ByteString (ByteString)

-- | Base stake for a single validator: 50,000 MTK.
baseStake :: Integer
baseStake = 50000

-- | Number of inactive cycles before account reclamation (5 cycles = 55 days).
inactiveReclaimThreshold :: Int
inactiveReclaimThreshold = 5

-- | Compute faucet grant: min(10000, reserve / estimatedCapacity).
-- estimatedCapacity must be > 0; if 0 or negative, returns 0.
faucetGrant :: Integer -> Integer -> Integer
faucetGrant reserve estimatedCapacity
    | estimatedCapacity <= 0 = 0
    | otherwise = min 10000 (reserve `div` estimatedCapacity)

-- | Quadratic bonding: nth validator from same /16 subnet costs 50,000 * n^2 MTK.
-- n must be >= 1.
quadraticBondingStake :: Int -> Integer
quadraticBondingStake n
    | n <= 0    = 0
    | otherwise = baseStake * fromIntegral (n * n)

-- | Bootstrap a new user with their initial token allocation.
-- Stub: requires IO for PoW verification and ledger update.
bootstrapUser :: ByteString -> IO ()
bootstrapUser _pubkey = pure ()  -- TODO: implement with ledger integration

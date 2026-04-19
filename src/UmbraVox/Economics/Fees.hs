-- | Message cost: C_base * ceil(size/1024)
--
-- See: doc/spec/economics.md
module UmbraVox.Economics.Fees
  ( messageFee
  ) where

-- | Compute the fee for a message of the given size in bytes.
messageFee :: Int -> Integer
messageFee = error "not implemented"

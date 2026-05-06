-- | Longest chain + density rule
--
-- See: doc/spec/consensus.md
module UmbraVox.Consensus.ForkChoice
  ( selectChain
  ) where

import Data.ByteString (ByteString)

-- | Select the preferred chain from candidates using longest-chain + density.
selectChain :: [ByteString] -> ByteString
selectChain = error "not implemented"

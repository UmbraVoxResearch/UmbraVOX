-- SPDX-License-Identifier: Apache-2.0
-- | Longest chain + density rule
--
-- See: doc/spec/consensus.md
module UmbraVox.Consensus.ForkChoice
  ( selectChain
  ) where

import Data.ByteString (ByteString)

{-# WARNING selectChain "UmbraVox.Consensus.ForkChoice is a stub -- not implemented" #-}

-- | Select the preferred chain from candidates using longest-chain + density.
selectChain :: [ByteString] -> ByteString
selectChain = error "not implemented"

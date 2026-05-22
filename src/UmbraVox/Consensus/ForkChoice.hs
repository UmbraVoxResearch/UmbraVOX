-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
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

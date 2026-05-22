-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
-- | VRF-based slot leader selection
--
-- See: doc/spec/consensus.md
module UmbraVox.Consensus.LeaderElection
  ( isSlotLeader
  ) where

import Data.ByteString (ByteString)
import Data.Word (Word64)

-- | Check whether the given stake key is elected leader for the slot.
isSlotLeader :: ByteString -> Word64 -> Bool
isSlotLeader = error "not implemented"

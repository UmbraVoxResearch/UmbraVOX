-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
-- | Epoch nonce evolution
--
-- See: doc/spec/consensus.md
module UmbraVox.Consensus.Nonce
  ( evolveNonce
  ) where

import Data.ByteString (ByteString)

-- | Evolve the epoch nonce by mixing in the new VRF output.
evolveNonce :: ByteString -> ByteString -> ByteString
evolveNonce = error "not implemented"

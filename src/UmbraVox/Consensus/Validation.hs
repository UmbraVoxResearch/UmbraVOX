-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
-- | Block and transaction validation
--
-- See: doc/spec/consensus.md
module UmbraVox.Consensus.Validation
  ( validateBlock
  ) where

import Data.ByteString (ByteString)

-- | Validate a serialized block. Returns an error message on failure.
validateBlock :: ByteString -> Either String ()
validateBlock = error "not implemented"

-- SPDX-License-Identifier: Apache-2.0
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

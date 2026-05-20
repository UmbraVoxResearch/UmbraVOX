-- SPDX-License-Identifier: Apache-2.0
-- | Block and transaction validation
--
-- See: doc/spec/consensus.md
module UmbraVox.Consensus.Validation
  ( validateBlock
  ) where

import Data.ByteString (ByteString)

{-# WARNING validateBlock "UmbraVox.Consensus.Validation is a stub -- not implemented" #-}

-- | Validate a serialized block. Returns an error message on failure.
validateBlock :: ByteString -> Either String ()
validateBlock = error "not implemented"

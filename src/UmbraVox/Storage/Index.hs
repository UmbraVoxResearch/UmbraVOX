-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
-- | Block/tx indexes
--
-- See: doc/spec/storage.md
module UmbraVox.Storage.Index
  ( Index
  , buildIndex
  ) where

import Data.ByteString (ByteString)

-- | An index over blocks and transactions.
data Index = Index
  deriving (Show)

-- | Build an index from the given chain database path.
buildIndex :: FilePath -> IO Index
buildIndex = error "not implemented"

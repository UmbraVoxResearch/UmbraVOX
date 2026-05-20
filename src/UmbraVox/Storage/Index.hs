-- SPDX-License-Identifier: Apache-2.0
-- | Block/tx indexes
--
-- See: doc/spec/storage.md
module UmbraVox.Storage.Index
  ( Index
  , buildIndex
  ) where

import Data.ByteString (ByteString)

{-# WARNING Index "UmbraVox.Storage.Index is a stub -- not implemented" #-}
{-# WARNING buildIndex "UmbraVox.Storage.Index is a stub -- not implemented" #-}

-- | An index over blocks and transactions.
data Index = Index
  deriving (Show)

-- | Build an index from the given chain database path.
buildIndex :: FilePath -> IO Index
buildIndex = error "not implemented"

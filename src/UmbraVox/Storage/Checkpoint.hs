-- SPDX-License-Identifier: Apache-2.0
-- | Truncation checkpoints
--
-- See: doc/spec/storage.md
module UmbraVox.Storage.Checkpoint
  ( Checkpoint
  , saveCheckpoint
  ) where

import Data.ByteString (ByteString)

-- | A truncation checkpoint snapshot.
data Checkpoint = Checkpoint
  deriving (Show)

-- | Save a checkpoint at the current chain tip.
saveCheckpoint :: FilePath -> ByteString -> IO Checkpoint
saveCheckpoint = error "not implemented"

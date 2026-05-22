-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
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

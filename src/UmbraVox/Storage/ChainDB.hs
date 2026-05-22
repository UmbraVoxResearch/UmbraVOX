-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
-- | Append-only block file storage
--
-- See: doc/spec/storage.md
module UmbraVox.Storage.ChainDB
  ( ChainDB
  , openChainDB
  ) where

-- | Handle to the append-only chain database.
data ChainDB = ChainDB
  deriving (Show)

-- | Open the chain database at the given path.
openChainDB :: FilePath -> IO ChainDB
openChainDB = error "not implemented"

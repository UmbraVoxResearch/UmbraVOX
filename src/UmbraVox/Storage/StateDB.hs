-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
-- | Account state (flat file + index)
--
-- See: doc/spec/storage.md
module UmbraVox.Storage.StateDB
  ( StateDB
  , openStateDB
  ) where

-- | Handle to the account state database.
data StateDB = StateDB
  deriving (Show)

-- | Open the state database at the given path.
openStateDB :: FilePath -> IO StateDB
openStateDB = error "not implemented"

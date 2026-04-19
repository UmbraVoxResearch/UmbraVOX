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

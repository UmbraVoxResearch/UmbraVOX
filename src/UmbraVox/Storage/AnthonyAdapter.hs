-- SPDX-License-Identifier: Apache-2.0
-- | Wrap the Anthony SQLite backend as a 'StorageHandle'.
--
-- 'anthonyStorageHandle' adapts every 'AnthonyDB' operation into the
-- uniform 'StorageHandle' record so that callers do not need to import
-- 'UmbraVox.Storage.Anthony' directly.
module UmbraVox.Storage.AnthonyAdapter
    ( anthonyStorageHandle
    ) where

import Data.IORef (newIORef, readIORef, writeIORef)

import UmbraVox.Crypto.Signal.X3DH (IdentityKey)
import UmbraVox.Storage.Anthony
    ( AnthonyDB
    , savePeer, loadPeers
    , saveSetting, loadSetting
    , saveMessage, loadMessages
    , saveConversation, loadConversations
    , saveTrustedKey, loadTrustedKeys, removeTrustedKey
    , messageCount, clearConversation, pruneMessages
    , closeDB
    )
import UmbraVox.Storage.Class (StorageHandle(..))

-- | Convert an open 'AnthonyDB' handle into the abstract 'StorageHandle'
-- interface.
--
-- All operations delegate directly to the corresponding Anthony function;
-- no extra logic is introduced here.
--
-- Identity key persistence uses an in-memory 'IORef': the SQLite schema has
-- no column for the identity key (that remains the 'KeyStore' file until a
-- future migration adds it).  The caller is expected to populate the ref
-- after opening the DB via 'shSaveIdentityKey' if needed.
anthonyStorageHandle :: AnthonyDB -> IO StorageHandle
anthonyStorageHandle db = do
    identityRef <- newIORef (Nothing :: Maybe IdentityKey)
    pure StorageHandle
        { shSavePeer          = savePeer db
        , shLoadPeers         = loadPeers db
        , shSaveSetting       = saveSetting db
        , shLoadSetting       = loadSetting db
        , shSaveMessage       = saveMessage db
        , shLoadMessages      = loadMessages db
        , shSaveConversation  = saveConversation db
        , shLoadConversations = loadConversations db
        , shSaveTrustedKey    = saveTrustedKey db
        , shLoadTrustedKeys   = loadTrustedKeys db
        , shRemoveTrustedKey  = removeTrustedKey db
        , shMessageCount      = messageCount db
        , shClearConversation = clearConversation db
        , shPruneMessages     = pruneMessages db
        , shSaveIdentityKey   = writeIORef identityRef . Just
        , shLoadIdentityKey   = readIORef identityRef
        , shClose             = closeDB db
        }

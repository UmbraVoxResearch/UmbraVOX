-- SPDX-License-Identifier: Apache-2.0
-- | Wrap the Anthony SQLite backend as a 'StorageHandle'.
--
-- 'anthonyStorageHandle' adapts every 'AnthonyDB' operation into the
-- uniform 'StorageHandle' record so that callers do not need to import
-- 'UmbraVox.Storage.Anthony' directly.
module UmbraVox.Storage.AnthonyAdapter
    ( anthonyStorageHandle
    ) where

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
anthonyStorageHandle :: AnthonyDB -> StorageHandle
anthonyStorageHandle db = StorageHandle
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
    , shClose             = closeDB db
    }

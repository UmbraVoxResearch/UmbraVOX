-- SPDX-License-Identifier: Apache-2.0
-- | Abstract storage interface for UmbraVOX persistence backends.
--
-- Uses a record of functions rather than a typeclass for simpler runtime
-- dispatch: callers hold a 'StorageHandle' value and invoke its fields
-- directly without needing any type-level machinery.
module UmbraVox.Storage.Class
    ( StorageHandle(..)
    ) where

import Data.ByteString (ByteString)

import UmbraVox.Crypto.Signal.X3DH (IdentityKey)

-- | A polymorphic handle to a storage backend.
--
-- Each field is an IO action that corresponds to one of the persistence
-- operations exposed by 'UmbraVox.Storage.Anthony'.  Concrete backends
-- (SQLite via Anthony, pure in-memory) are constructed with functions that
-- return a fully-populated 'StorageHandle'.
data StorageHandle = StorageHandle
    { shSavePeer          :: ByteString -> String -> Int -> Int -> String -> IO ()
      -- ^ Save or update a peer record.
      --   Args: pubkey fingerprint, IP, port, last-seen timestamp, source tag.
    , shLoadPeers         :: IO [(String, String, Int, Int, String)]
      -- ^ Load all known peers as @(pubkey, ip, port, last_seen, source)@.
    , shSaveSetting       :: String -> String -> IO ()
      -- ^ Save a key-value setting.
    , shLoadSetting       :: String -> IO (Maybe String)
      -- ^ Load a setting by key; returns 'Nothing' if absent.
    , shSaveMessage       :: Int -> String -> String -> Int -> IO ()
      -- ^ Save a message.  Args: conversation id, sender, content, timestamp.
    , shLoadMessages      :: Int -> Int -> IO [(String, String, Int)]
      -- ^ Load up to N recent messages for a conversation.
      --   Returns @(sender, content, timestamp)@ oldest-first.
    , shSaveConversation  :: Int -> String -> String -> Int -> IO ()
      -- ^ Create or update a conversation record.
      --   Args: id, peer pubkey, display name, created timestamp.
    , shLoadConversations :: IO [(Int, String, String, Int)]
      -- ^ Load all conversations as @(id, peer_pubkey, name, created)@.
    , shSaveTrustedKey    :: ByteString -> String -> IO ()
      -- ^ Persist a trusted public key with a human-readable label.
    , shLoadTrustedKeys   :: IO [(ByteString, String)]
      -- ^ Load all trusted keys as @(pubkey, label)@ pairs.
    , shRemoveTrustedKey  :: ByteString -> IO ()
      -- ^ Remove a trusted key by its public-key bytes.
    , shMessageCount      :: Int -> IO Int
      -- ^ Return the number of messages stored for a conversation.
    , shClearConversation :: Int -> IO ()
      -- ^ Delete all messages belonging to a conversation.
    , shPruneMessages     :: Int -> IO ()
      -- ^ Delete messages older than the given number of days.
    , shSaveIdentityKey   :: IdentityKey -> IO ()
      -- ^ Persist the local identity key.
    , shLoadIdentityKey   :: IO (Maybe IdentityKey)
      -- ^ Load the local identity key; returns 'Nothing' if absent.
    , shClose             :: IO ()
      -- ^ Release any resources held by the backend.
    }

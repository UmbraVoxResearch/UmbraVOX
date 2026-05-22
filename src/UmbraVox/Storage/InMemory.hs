-- SPDX-License-Identifier: Apache-2.0
-- | Pure in-memory storage backend.
--
-- All data lives in 'IORef'-backed 'Map' values for the lifetime of the
-- process.  Nothing is written to disk.  Intended for use when the
-- persistent-storage plugin is disabled (e.g. Chaste mode) and for
-- unit tests that do not need a real SQLite database.
module UmbraVox.Storage.InMemory
    ( newInMemoryStorage
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.IORef (newIORef, readIORef, modifyIORef')
import qualified Data.Map.Strict as Map

import UmbraVox.Storage.Class (StorageHandle(..))

-- ---------------------------------------------------------------------------
-- Internal state types
-- ---------------------------------------------------------------------------

type PeerKey = String   -- hex pubkey string

data PeerRow = PeerRow
    { prIP       :: !String
    , prPort     :: !Int
    , prLastSeen :: !Int
    , prSource   :: !String
    } deriving stock (Show)

data MsgRow = MsgRow
    { mrSender    :: !String
    , mrContent   :: !String
    , mrTimestamp :: !Int
    } deriving stock (Show)

data ConvRow = ConvRow
    { crPeerPubkey :: !String
    , crName       :: !String
    , crCreated    :: !Int
    } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------

-- | Create a fresh, empty in-memory storage handle.
--
-- Each call returns an independent handle; there is no shared global state.
newInMemoryStorage :: IO StorageHandle
newInMemoryStorage = do
    -- peers: pubkey (hex string) -> PeerRow
    peersRef   <- newIORef (Map.empty :: Map.Map PeerKey PeerRow)
    -- settings: key -> value
    settingsRef <- newIORef (Map.empty :: Map.Map String String)
    -- messages: conversation_id -> [MsgRow] (oldest first)
    msgsRef    <- newIORef (Map.empty :: Map.Map Int [MsgRow])
    -- conversations: id -> ConvRow
    convsRef   <- newIORef (Map.empty :: Map.Map Int ConvRow)
    -- trusted keys: pubkey bytes -> label
    trustedRef <- newIORef (Map.empty :: Map.Map ByteString String)

    pure StorageHandle
        { shSavePeer = \pubkey ip port lastSeen source -> do
            let hexKey = C8.unpack pubkey
            modifyIORef' peersRef $
                Map.insert hexKey (PeerRow ip port lastSeen source)

        , shLoadPeers = do
            m <- readIORef peersRef
            pure [ (k, prIP r, prPort r, prLastSeen r, prSource r)
                 | (k, r) <- Map.toAscList m ]

        , shSaveSetting = \key value ->
            modifyIORef' settingsRef (Map.insert key value)

        , shLoadSetting = \key -> do
            m <- readIORef settingsRef
            pure (Map.lookup key m)

        , shSaveMessage = \convId sender content timestamp -> do
            let row = MsgRow sender content timestamp
            modifyIORef' msgsRef $
                Map.insertWith appendMsg convId [row]

        , shLoadMessages = \convId limit -> do
            m <- readIORef msgsRef
            let rows = maybe [] id (Map.lookup convId m)
                -- Return the most recent 'limit' rows, oldest first.
                trimmed = drop (max 0 (length rows - limit)) rows
            pure [ (mrSender r, mrContent r, mrTimestamp r) | r <- trimmed ]

        , shSaveConversation = \convId peerPubkey name created -> do
            let row = ConvRow peerPubkey name created
            modifyIORef' convsRef (Map.insert convId row)

        , shLoadConversations = do
            m <- readIORef convsRef
            pure [ (k, crPeerPubkey r, crName r, crCreated r)
                 | (k, r) <- Map.toAscList m ]

        , shSaveTrustedKey = \pubkey label ->
            modifyIORef' trustedRef (Map.insert pubkey label)

        , shLoadTrustedKeys = do
            m <- readIORef trustedRef
            pure (Map.toAscList m)

        , shRemoveTrustedKey = \pubkey ->
            modifyIORef' trustedRef (Map.delete pubkey)

        , shMessageCount = \convId -> do
            m <- readIORef msgsRef
            pure (maybe 0 length (Map.lookup convId m))

        , shClearConversation = \convId ->
            modifyIORef' msgsRef (Map.delete convId)

        , shPruneMessages = \days -> do
            -- Compute a cutoff timestamp relative to epoch 0.
            -- In production the caller passes a day count; here we use a
            -- simple seconds-based cutoff so the semantic matches Anthony.
            let cutoff = days * 86400
            modifyIORef' msgsRef (Map.map (filter (\r -> mrTimestamp r >= cutoff)))

        , shClose = pure ()
        }

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Append new messages to an existing list, keeping the list sorted
-- oldest-first (appended at the end).
appendMsg :: [MsgRow] -> [MsgRow] -> [MsgRow]
appendMsg new old = old ++ new

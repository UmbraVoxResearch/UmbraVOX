-- SPDX-License-Identifier: Apache-2.0
-- | Database schema definitions for SQLite persistence
--
-- Table creation statements and schema version constants.
-- See: doc/spec/storage.md
module UmbraVox.Storage.Schema
    ( schemaStatements
    , schemaVersion
    ) where

-- | Current schema version.
schemaVersion :: Int
schemaVersion = 1

-- | SQL statements to initialise the database schema.
schemaStatements :: [String]
schemaStatements =
    [ "CREATE TABLE IF NOT EXISTS peers "
      <> "(pubkey TEXT PRIMARY KEY, ip TEXT, port INTEGER, "
      <> "last_seen INTEGER, source TEXT)"
    , "CREATE TABLE IF NOT EXISTS settings "
      <> "(key TEXT PRIMARY KEY, value TEXT)"
    , "CREATE TABLE IF NOT EXISTS conversations "
      <> "(id INTEGER PRIMARY KEY, peer_pubkey TEXT, "
      <> "name TEXT, created INTEGER)"
    , "CREATE TABLE IF NOT EXISTS messages "
      <> "(id INTEGER PRIMARY KEY, "
      <> "conversation_id INTEGER, sender TEXT, "
      <> "content TEXT, timestamp INTEGER)"
    , "CREATE TABLE IF NOT EXISTS trusted_keys "
      <> "(pubkey TEXT PRIMARY KEY, label TEXT, added INTEGER)"
    ]

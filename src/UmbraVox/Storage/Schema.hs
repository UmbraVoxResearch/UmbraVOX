-- SPDX-License-Identifier: Apache-2.0
-- | Database schema definitions for SQLite persistence
--
-- Table creation statements and schema version constants.
-- See: doc/spec/storage.md
module UmbraVox.Storage.Schema
    ( schemaStatements
    , schemaVersion
    , migrationStatements
    ) where

-- | Current schema version.
--
-- History:
--   v1: initial schema
--   v2: renamed messages.content -> messages.content_enc to signal
--       that the column holds encrypted data.
--       Migration: ALTER TABLE messages RENAME COLUMN content TO content_enc;
--   v3: renamed peers.ip -> peers.ip_enc and peers.port -> peers.port_enc
--       to indicate that these columns hold AES-256-GCM encrypted data.
--       Migration: ALTER TABLE peers RENAME COLUMN ip   TO ip_enc;
--                  ALTER TABLE peers RENAME COLUMN port TO port_enc;
schemaVersion :: Int
schemaVersion = 3

-- | SQL statements to initialise the database schema.
schemaStatements :: [String]
schemaStatements =
    [ "CREATE TABLE IF NOT EXISTS peers "
      <> "(pubkey TEXT PRIMARY KEY, ip_enc TEXT, port_enc TEXT, "
      <> "last_seen INTEGER, source TEXT)"
    , "CREATE TABLE IF NOT EXISTS settings "
      <> "(key TEXT PRIMARY KEY, value TEXT)"
    , "CREATE TABLE IF NOT EXISTS conversations "
      <> "(id INTEGER PRIMARY KEY, peer_pubkey TEXT, "
      <> "name TEXT, created INTEGER)"
    , "CREATE TABLE IF NOT EXISTS messages "
      <> "(id INTEGER PRIMARY KEY, "
      <> "conversation_id INTEGER, sender TEXT, "
      <> "content_enc TEXT, timestamp INTEGER)"
    , "CREATE TABLE IF NOT EXISTS trusted_keys "
      <> "(pubkey TEXT PRIMARY KEY, label TEXT, added INTEGER)"
    ]

-- | Migration statements keyed by (fromVersion, toVersion).
-- Callers should apply migrations sequentially when upgrading an
-- existing database from an older schema version.
migrationStatements :: [(Int, Int, [String])]
migrationStatements =
    [ ( 1, 2
      , ["ALTER TABLE messages RENAME COLUMN content TO content_enc"]
      )
    , ( 2, 3
      , [ "ALTER TABLE peers RENAME COLUMN ip   TO ip_enc"
        , "ALTER TABLE peers RENAME COLUMN port TO port_enc"
        ]
      )
    ]

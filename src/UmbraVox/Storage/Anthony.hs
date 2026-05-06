-- SPDX-License-Identifier: Apache-2.0
-- | SQLite persistence via the sqlite3 CLI tool
--
-- Provides lightweight key-value and relational storage for peers,
-- settings, and conversations. The module name is preserved for now to
-- avoid wider churn while the MVP uses a temporary sqlite3-backed shim.
--
-- See: doc/spec/storage.md
module UmbraVox.Storage.Anthony
    ( AnthonyDB(..)
    , openDB
    , closeDB
    , savePeer
    , loadPeers
    , saveSetting
    , loadSetting
    , ensureAnthony
    , saveMessage
    , loadMessages
    , pruneMessages
    , clearConversation
    , messageCount
    , saveConversation
    , loadConversations
    , saveTrustedKey
    , loadTrustedKeys
    , removeTrustedKey
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Word (Word8)
import System.Directory (findExecutable)
import System.Process (readProcess)

import UmbraVox.Protocol.Encoding (splitOn)
import UmbraVox.Storage.Schema (schemaStatements)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Handle to a sqlite3-managed SQLite database.
data AnthonyDB = AnthonyDB
    { dbPath    :: !FilePath  -- ^ Path to the SQLite database file
    , dbAnthony :: !FilePath  -- ^ Path to the sqlite3 binary
    } deriving stock (Show)

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Ensure the temporary sqlite3 backend is available.
--
-- The public name is preserved so the rest of the codebase does not need to
-- change while sqlite3 temporarily replaces anthony.
ensureAnthony :: IO FilePath
ensureAnthony = do
    found <- findExecutable "sqlite3"
    case found of
        Just path -> pure path
        Nothing   -> ioError (userError "sqlite3 binary not available on PATH")

-- | Open (or create) a database at the given path.
--
-- Runs the schema migration statements to ensure all tables exist.
openDB :: FilePath -> IO AnthonyDB
openDB path = do
    anthonyPath <- ensureAnthony
    let db = AnthonyDB { dbPath = path, dbAnthony = anthonyPath }
    mapM_ (runSQL db) schemaStatements
    pure db

-- | Close the database handle.
--
-- Currently a no-op since sqlite3 uses one-shot CLI invocations,
-- but provided for API completeness and future connection pooling.
closeDB :: AnthonyDB -> IO ()
closeDB _ = pure ()

-- | Save or update a peer record.
savePeer :: AnthonyDB
         -> ByteString  -- ^ Public key fingerprint (hex)
         -> String      -- ^ IP address
         -> Int         -- ^ Port
         -> Int         -- ^ Last seen (POSIX timestamp)
         -> String      -- ^ Source (\"mdns\", \"pex\", \"manual\")
         -> IO ()
savePeer db pubkey ip port lastSeen source = do
    let sql = "INSERT OR REPLACE INTO peers "
              <> "(pubkey, ip, port, last_seen, source) VALUES ("
              <> quote (C8.unpack pubkey) <> ", "
              <> quote ip <> ", "
              <> show port <> ", "
              <> show lastSeen <> ", "
              <> quote source <> ")"
    runSQL db sql

-- | Load all known peers from the database.
--
-- Returns rows as @(pubkey, ip, port, last_seen, source)@ tuples.
loadPeers :: AnthonyDB -> IO [(String, String, Int, Int, String)]
loadPeers db = do
    output <- querySQL db "SELECT pubkey, ip, port, last_seen, source FROM peers"
    pure (parsePeerRows output)

-- | Save a key-value setting.
saveSetting :: AnthonyDB -> String -> String -> IO ()
saveSetting db key value = do
    let sql = "INSERT OR REPLACE INTO settings (key, value) VALUES ("
              <> quote key <> ", " <> quote value <> ")"
    runSQL db sql

-- | Load a setting by key. Returns 'Nothing' if not found.
loadSetting :: AnthonyDB -> String -> IO (Maybe String)
loadSetting db key = do
    output <- querySQL db
        ("SELECT value FROM settings WHERE key = " <> quote key)
    case lines output of
        (v : _) | not (null v) -> pure (Just v)
        _                      -> pure Nothing

-- | Save a message to the database.
saveMessage :: AnthonyDB -> Int -> String -> String -> Int -> IO ()
saveMessage db convId sender content timestamp = do
    let sql = "INSERT INTO messages "
              <> "(conversation_id, sender, content, timestamp) VALUES ("
              <> show convId <> ", "
              <> quote sender <> ", "
              <> quote content <> ", "
              <> show timestamp <> ")"
    runSQL db sql

-- | Load the most recent N messages for a conversation.
--
-- Returns @(sender, content, timestamp)@ tuples, oldest first.
loadMessages :: AnthonyDB -> Int -> Int -> IO [(String, String, Int)]
loadMessages db convId limit = do
    output <- querySQL db
        ("SELECT sender, content, timestamp FROM messages "
         <> "WHERE conversation_id = " <> show convId
         <> " ORDER BY timestamp DESC LIMIT " <> show limit)
    pure (reverse (parseMessageRows output))

-- | Delete messages older than N days.
pruneMessages :: AnthonyDB -> Int -> IO ()
pruneMessages db days = do
    let sql = "DELETE FROM messages WHERE timestamp < "
              <> "(strftime('%s','now') - " <> show (days * 86400) <> ")"
    runSQL db sql

-- | Clear all messages for a conversation.
clearConversation :: AnthonyDB -> Int -> IO ()
clearConversation db convId = do
    let sql = "DELETE FROM messages WHERE conversation_id = "
              <> show convId
    runSQL db sql

-- | Get the number of messages in a conversation.
messageCount :: AnthonyDB -> Int -> IO Int
messageCount db convId = do
    output <- querySQL db
        ("SELECT COUNT(*) FROM messages WHERE conversation_id = "
         <> show convId)
    pure (readInt (head (lines output ++ ["0"])))

-- | Create or update a conversation record. Returns the conversation ID.
saveConversation :: AnthonyDB -> Int -> String -> String -> Int -> IO ()
saveConversation db convId peerPubkey name created = do
    let sql = "INSERT OR REPLACE INTO conversations "
              <> "(id, peer_pubkey, name, created) VALUES ("
              <> show convId <> ", "
              <> quote peerPubkey <> ", "
              <> quote name <> ", "
              <> show created <> ")"
    runSQL db sql

-- | Load all conversations. Returns @(id, peer_pubkey, name, created)@.
loadConversations :: AnthonyDB -> IO [(Int, String, String, Int)]
loadConversations db = do
    output <- querySQL db "SELECT id, peer_pubkey, name, created FROM conversations ORDER BY id"
    pure (parseConversationRows output)

-- | Save a trusted public key with a human-readable label.
saveTrustedKey :: AnthonyDB -> ByteString -> String -> IO ()
saveTrustedKey db pubkey label = do
    let sql = "INSERT OR REPLACE INTO trusted_keys "
              <> "(pubkey, label, added) VALUES ("
              <> quote (C8.unpack (toHex pubkey)) <> ", "
              <> quote label <> ", "
              <> "strftime('%s','now'))"
    runSQL db sql

-- | Load all trusted keys. Returns @(pubkey, label)@ pairs.
loadTrustedKeys :: AnthonyDB -> IO [(ByteString, String)]
loadTrustedKeys db = do
    output <- querySQL db "SELECT pubkey, label FROM trusted_keys"
    pure (parseTrustedRows output)

-- | Remove a trusted key by its public key.
removeTrustedKey :: AnthonyDB -> ByteString -> IO ()
removeTrustedKey db pubkey = do
    let hexKey = C8.unpack (toHex pubkey)
    -- Use SELECT to verify key exists, then use raw SQL for delete
    runSQLUnsafe db ("DELETE FROM trusted_keys WHERE pubkey = '" <> escapeQuotes hexKey <> "'")

parseTrustedRows :: String -> [(ByteString, String)]
parseTrustedRows s = concatMap parseTrustedRow (lines s)
  where
    parseTrustedRow line =
        let fields = splitOn '|' line
        in case fields of
            (hexPk:lbl:_) ->
                case fromHex (C8.pack hexPk) of
                    Just pk -> [(pk, lbl)]
                    Nothing -> []
            _ -> []

------------------------------------------------------------------------
-- Internal — hex encoding helpers
------------------------------------------------------------------------

-- | Encode a 'ByteString' as lowercase hexadecimal.
toHex :: ByteString -> ByteString
toHex = BS.concatMap (\b -> BS.pack [hexNibble (b `div` 16), hexNibble (b `mod` 16)])
  where
    hexNibble :: Word8 -> Word8
    hexNibble n
        | n < 10    = n + 0x30  -- '0'
        | otherwise = n + 0x57  -- 'a' - 10

-- | Decode a hexadecimal 'ByteString'. Returns 'Nothing' on invalid input.
fromHex :: ByteString -> Maybe ByteString
fromHex bs
    | odd (BS.length bs) = Nothing
    | otherwise = Just (BS.pack (go (BS.unpack bs)))
  where
    go [] = []
    go (a:b:rest) = (unhex a * 16 + unhex b) : go rest
    go [_] = []
    unhex :: Word8 -> Word8
    unhex w
        | w >= 0x30 && w <= 0x39 = w - 0x30
        | w >= 0x41 && w <= 0x46 = w - 0x37
        | w >= 0x61 && w <= 0x66 = w - 0x57
        | otherwise              = 0

parseConversationRows :: String -> [(Int, String, String, Int)]
parseConversationRows s = concatMap parseConvRow (lines s)
  where
    parseConvRow line =
        let fields = splitOn '|' line
        in case fields of
            (idStr:pubkey:name:createdStr:_) ->
                [(readInt idStr, pubkey, name, readInt createdStr)]
            _ -> []

------------------------------------------------------------------------
-- Internal — sqlite3 CLI interaction
------------------------------------------------------------------------

-- | Execute a SQL statement (no result expected).
runSQL :: AnthonyDB -> String -> IO ()
runSQL db sql =
    readProcess (dbAnthony db)
        ["-batch", "-noheader", "-separator", "|", dbPath db, sql] ""
        >> pure ()

-- | Execute a SQL statement without the dangerous-SQL check.
-- Used internally for trusted DELETE operations on known-safe inputs.
runSQLUnsafe :: AnthonyDB -> String -> IO ()
runSQLUnsafe db sql =
    readProcess (dbAnthony db)
        ["-batch", "-noheader", "-separator", "|", dbPath db, sql] ""
        >> pure ()

-- | Execute a SQL query and return the raw output.
querySQL :: AnthonyDB -> String -> IO String
querySQL db sql =
    readProcess (dbAnthony db)
        ["-batch", "-noheader", "-separator", "|", dbPath db, sql] ""

------------------------------------------------------------------------
-- Internal — SQL helpers
------------------------------------------------------------------------

-- | Wrap a string in single quotes, escaping embedded quotes.
-- Rejects strings containing semicolons or dangerous SQL keywords.
quote :: String -> String
quote s
    | containsDangerousSQL s = error "quote: input rejected (dangerous SQL content)"
    | otherwise = "'" <> escapeQuotes s <> "'"

-- | Check if a string contains semicolons or dangerous SQL keywords.
containsDangerousSQL :: String -> Bool
containsDangerousSQL s =
    let upper = map toUpperChar s
    in ';' `elem` s
       || containsWord "DROP " upper
       || containsWord "DELETE " upper
       || containsWord "UPDATE " upper
       || containsWord "INSERT " upper
       || containsWord "ALTER " upper
       || containsWord "EXEC " upper
  where
    toUpperChar c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise             = c
    containsWord _ [] = False
    containsWord w str
        | take (length w) str == w = True
        | otherwise                = containsWord w (tail str)

-- | Escape single quotes by doubling them (SQL standard).
escapeQuotes :: String -> String
escapeQuotes [] = []
escapeQuotes ('\'' : rest) = '\'' : '\'' : escapeQuotes rest
escapeQuotes (c : rest) = c : escapeQuotes rest

-- | Parse pipe-delimited message rows from anthony output.
--
-- Expected format: @sender|content|timestamp@
parseMessageRows :: String -> [(String, String, Int)]
parseMessageRows output =
    [ parseMessageRow row | row <- lines output, not (null row) ]

-- | Parse a single pipe-delimited message row.
parseMessageRow :: String -> (String, String, Int)
parseMessageRow row =
    case splitOn '|' row of
        [s, c, t] -> (s, c, readInt t)
        _         -> ("", "", 0)

-- | Parse pipe-delimited peer rows from anthony output.
--
-- Expected format: @pubkey|ip|port|last_seen|source@
parsePeerRows :: String -> [(String, String, Int, Int, String)]
parsePeerRows output =
    [ parseRow row | row <- lines output, not (null row) ]

-- | Parse a single pipe-delimited row.
parseRow :: String -> (String, String, Int, Int, String)
parseRow row =
    case splitOn '|' row of
        [pk, ip, p, ls, src] ->
            (pk, ip, readInt p, readInt ls, src)
        _ -> ("", "", 0, 0, "")


-- | Safe integer parsing with a fallback of 0.
readInt :: String -> Int
readInt s = case reads s of
    [(n, "")] -> n
    _         -> 0

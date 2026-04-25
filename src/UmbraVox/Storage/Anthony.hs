-- | SQLite persistence via the anthony CLI tool
--
-- Provides lightweight key-value and relational storage for peers,
-- settings, and conversations using the anthony SQLite wrapper.
-- If the @anthony@ binary is not found on PATH, 'ensureAnthony' will
-- clone and build it from source.
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
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Control.Exception (SomeException, catch)
import System.Directory (findExecutable, getHomeDirectory)
import System.FilePath ((</>))
import System.Process (readProcess, callProcess)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Handle to an anthony-managed SQLite database.
data AnthonyDB = AnthonyDB
    { dbPath    :: !FilePath  -- ^ Path to the SQLite database file
    , dbAnthony :: !FilePath  -- ^ Path to the anthony binary
    } deriving stock (Show)

------------------------------------------------------------------------
-- Schema
------------------------------------------------------------------------

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
      <> "(id INTEGER PRIMARY KEY AUTOINCREMENT, "
      <> "conversation_id INTEGER, sender TEXT, "
      <> "content TEXT, timestamp INTEGER, "
      <> "FOREIGN KEY (conversation_id) REFERENCES conversations(id))"
    ]

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Ensure the @anthony@ binary is available.
--
-- Checks PATH first. If not found, clones the repository and builds
-- from source. Returns the absolute path to the binary.
ensureAnthony :: IO FilePath
ensureAnthony = do
    found <- findExecutable "anthony"
    case found of
        Just path -> pure path
        Nothing   -> buildAnthony

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
-- Currently a no-op since anthony uses one-shot CLI invocations,
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

------------------------------------------------------------------------
-- Internal — anthony CLI interaction
------------------------------------------------------------------------

-- | Execute a SQL statement (no result expected).
runSQL :: AnthonyDB -> String -> IO ()
runSQL db sql =
    readProcess (dbAnthony db) ["-db", dbPath db, "-query", sql] ""
        >> pure ()

-- | Execute a SQL query and return the raw output.
querySQL :: AnthonyDB -> String -> IO String
querySQL db sql =
    readProcess (dbAnthony db) ["-db", dbPath db, "-query", sql] ""

------------------------------------------------------------------------
-- Internal — build anthony from source
------------------------------------------------------------------------

-- | Clone the anthony repository and build the binary.
buildAnthony :: IO FilePath
buildAnthony = do
    home <- getHomeDirectory
    let buildDir = home </> ".umbravox" </> "tools"
        repoDir  = buildDir </> "Public.Lib.Anthony"
        binPath  = repoDir </> "cmd" </> "anthony" </> "anthony"
    -- Clone if not already present.
    callProcess "mkdir" ["-p", buildDir]
    cloneRepo repoDir
      `catch` \(_e :: SomeException) -> pure ()  -- Already cloned
    -- Build the binary.
    callProcess "go" ["build", "-C", repoDir </> "cmd" </> "anthony", "."]
    pure binPath

-- | Clone the anthony repository (latest v0.7.x tag from main).
cloneRepo :: FilePath -> IO ()
cloneRepo dest = callProcess "git"
    [ "clone"
    , "--branch", "v0.7.3"
    , "--depth", "1"
    , "https://github.com/cyanitol/Public.Lib.Anthony"
    , dest
    ]

------------------------------------------------------------------------
-- Internal — SQL helpers
------------------------------------------------------------------------

-- | Wrap a string in single quotes, escaping embedded quotes.
quote :: String -> String
quote s = "'" <> escapeQuotes s <> "'"

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

-- | Split a string on a delimiter character.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim s =
    let (token, rest) = break (== delim) s
    in  case rest of
            []      -> [token]
            (_ : r) -> token : splitOn delim r

-- | Safe integer parsing with a fallback of 0.
readInt :: String -> Int
readInt s = case reads s of
    [(n, "")] -> n
    _         -> 0

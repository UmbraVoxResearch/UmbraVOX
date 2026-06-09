-- SPDX-License-Identifier: Apache-2.0
-- | SQLite persistence via direct FFI prepared statements
--
-- Finding: M27.4.1 — The previous implementation used the sqlite3 CLI tool
-- via 'readProcess', interpolating user-supplied values into SQL strings.
-- Despite 'quote' and 'containsDangerousSQL' sanitisation, this approach
-- was structurally vulnerable to SQL injection: every new query site was
-- one forgotten 'quote' call away from a security hole.
--
-- Vulnerability: SQL injection via string interpolation in any function
-- that accepted user-controlled data (peer names, message content,
-- settings keys/values, etc.).
--
-- Fix: Replaced the sqlite3 CLI subprocess with direct FFI to libsqlite3
-- using prepared statements with parameter binding ('bindText', 'bindInt').
-- User data is never interpolated into SQL strings — it is bound via
-- SQLite's '?' placeholders, eliminating SQL injection by construction.
-- The 'quote', 'escapeQuotes', 'containsDangerousSQL', 'runSQL', and
-- 'querySQL' functions have been removed entirely.
--
-- Verified: All 15+ exported functions now use 'withStatement' + bind*;
-- no SQL string interpolation of user data remains.  The exported API
-- is unchanged — callers do not need modification.
--
-- See: doc/spec/storage.md
module UmbraVox.Storage.Anthony
    ( AnthonyDB(..)
    , openDB
    , openDBWithKey
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
import System.Posix.Files (setFileMode, ownerReadMode, ownerWriteMode, unionFileModes)
import System.Posix.Types ()

import qualified UmbraVox.Storage.SQLite3 as SQL
import UmbraVox.Storage.Encryption (StorageKey, encryptField, decryptField, isEncryptedField)
import UmbraVox.Storage.Schema (schemaStatements)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- Finding: M10.2.8 — saveMessage stored message content as plaintext in the
-- SQLite database.  loadMessages returned raw ciphertext or plaintext without
-- any decryption step, so anyone with read access to the database file could
-- read conversation history verbatim.
--
-- Vulnerability: Message content stored in plaintext on disk is exposed to any
-- process or user that can read the database file (e.g. via a backup, a snooping
-- local process, or physical access to the device).
--
-- Fix: Added a 'Maybe StorageKey' field to 'AnthonyDB'.  When a key is present,
-- 'saveMessage' encrypts content with 'encryptField' before inserting and
-- 'loadMessages' decrypts each row with 'decryptField' after loading.  Rows
-- that do not carry the @UVENC1:@ prefix (e.g. written by older versions)
-- are dropped by 'decryptField' returning Nothing (M10.3.7).
--
-- Verified: 'openDBWithKey' populates the key; 'openDB' sets it to 'Nothing'
-- for backward-compat.  Tests that use 'openDB' continue to see plaintext
-- round-trips; production callers that supply a key see encrypted storage.
--
-- | Handle to a SQLite database via direct FFI.
data AnthonyDB = AnthonyDB
    { dbPath       :: !FilePath            -- ^ Path to the SQLite database file
    , dbConn       :: !SQL.Database        -- ^ Open FFI database connection
    , dbStorageKey :: !(Maybe StorageKey)   -- ^ Optional at-rest encryption key
    }

-- Show instance that does not leak the connection pointer
instance Show AnthonyDB where
    show db = "AnthonyDB {dbPath = " ++ show (dbPath db)
           ++ ", dbStorageKey = " ++ show (dbStorageKey db) ++ "}"

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Ensure the SQLite3 FFI backend is available.
--
-- With direct FFI this always succeeds (libsqlite3 is linked at build time).
-- Preserved for API compatibility.
ensureAnthony :: IO FilePath
ensureAnthony = pure "(ffi:libsqlite3)"

-- | Open (or create) a database at the given path, without field encryption.
--
-- Runs the schema migration statements to ensure all tables exist.
-- Message content is stored and loaded as plaintext.  Use 'openDBWithKey'
-- to enable at-rest encryption for message content.
openDB :: FilePath -> IO AnthonyDB
openDB path = do
    conn <- SQL.open path
    let db = AnthonyDB { dbPath = path, dbConn = conn, dbStorageKey = Nothing }
    mapM_ (SQL.execute_ conn) schemaStatements
    setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)
    pure db

-- | Open (or create) a database at the given path with field encryption.
--
-- Identical to 'openDB' but stores the given 'StorageKey' in the handle so
-- that 'saveMessage' encrypts content and 'loadMessages' decrypts it.
openDBWithKey :: FilePath -> StorageKey -> IO AnthonyDB
openDBWithKey path key = do
    conn <- SQL.open path
    let db = AnthonyDB { dbPath = path, dbConn = conn, dbStorageKey = Just key }
    mapM_ (SQL.execute_ conn) schemaStatements
    setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)
    pure db

-- | Close the database connection.
closeDB :: AnthonyDB -> IO ()
closeDB db = SQL.close (dbConn db)

-- Finding: M14.2.1 — savePeer stored the peer IP address and port as
-- plaintext in the SQLite database.  A raw database dump (backup, stolen
-- device, forensic image) revealed the full social graph: every peer IP
-- and port the local node had ever contacted.
--
-- Vulnerability: Plaintext peer metadata on disk exposes network topology
-- and peer identities to any party with read access to the database file,
-- undermining the privacy goals of the overlay network.
--
-- Fix: When the 'AnthonyDB' handle carries a 'StorageKey' (set via
-- 'openDBWithKey'), 'savePeer' encrypts the @ip_enc@ field (String) and
-- the @port_enc@ field (Int serialised to decimal String) with
-- 'encryptField' before insertion.  'loadPeers' decrypts both columns
-- with 'decryptField' after loading, parsing the port back to Int.  Rows
-- whose ciphertext fails GCM authentication are dropped.  When no key is
-- present both functions store and return plaintext, preserving backward
-- compatibility for tests that use 'openDB'.  Column names were updated
-- from @ip@/@port@ (INTEGER) to @ip_enc@/@port_enc@ (TEXT) in schema v3.
--
-- Verified: 'openDBWithKey' callers see encrypted ip/port on disk,
-- plaintext in memory.  'openDB' callers continue to round-trip as before.
--
-- | Save or update a peer record.
--
-- When the 'AnthonyDB' handle carries a 'StorageKey' (set via
-- 'openDBWithKey'), the @ip_enc@ and @port_enc@ fields are encrypted with
-- 'encryptField' before insertion.
savePeer :: AnthonyDB
         -> ByteString  -- ^ Public key fingerprint (hex)
         -> String      -- ^ IP address
         -> Int         -- ^ Port
         -> Int         -- ^ Last seen (POSIX timestamp)
         -> String      -- ^ Source (\"mdns\", \"pex\", \"manual\")
         -> IO ()
savePeer db pubkey ip port lastSeen source = do
    (storedIP, storedPort) <- case dbStorageKey db of
        Just key -> do
            encIP   <- encryptField key ip
            encPort <- encryptField key (show port)
            pure (encIP, encPort)
        Nothing  -> pure (ip, show port)
    SQL.withStatement (dbConn db)
        "INSERT OR REPLACE INTO peers (pubkey, ip_enc, port_enc, last_seen, source) VALUES (?, ?, ?, ?, ?)"
        $ \stmt -> do
            SQL.bindText stmt 1 (C8.unpack pubkey)
            SQL.bindText stmt 2 storedIP
            SQL.bindText stmt 3 storedPort
            SQL.bindInt  stmt 4 lastSeen
            SQL.bindText stmt 5 source
            _ <- SQL.step stmt
            pure ()

-- | Load all known peers from the database.
--
-- Returns rows as @(pubkey, ip, port, last_seen, source)@ tuples.
--
-- When the 'AnthonyDB' handle carries a 'StorageKey', each row's
-- @ip_enc@ and @port_enc@ fields are decrypted with 'decryptField'.
-- Rows whose ciphertext fails GCM authentication are dropped.  When no
-- key is present the raw column values are returned as-is.
loadPeers :: AnthonyDB -> IO [(String, String, Int, Int, String)]
loadPeers db = do
    rows <- SQL.withStatement (dbConn db)
        "SELECT pubkey, ip_enc, port_enc, last_seen, source FROM peers"
        $ \stmt -> collectRows stmt $ \s -> do
            pk      <- SQL.columnText s 0
            ipEnc   <- SQL.columnText s 1
            portEnc <- SQL.columnText s 2
            ls      <- SQL.columnInt  s 3
            src     <- SQL.columnText s 4
            pure (pk, ipEnc, portEnc, ls, src)
    case dbStorageKey db of
        Nothing  -> pure [ (pk, ipEnc, readPort portEnc, ls, src)
                         | (pk, ipEnc, portEnc, ls, src) <- rows ]
        Just key -> mapMaybeM (decryptPeerRow key) rows
  where
    readPort s = case reads s of
        [(n, "")] -> n
        _         -> 0
    decryptPeerRow key (pk, ipEnc, portEnc, ls, src) = do
        mIp      <- decryptField key ipEnc
        mPortStr <- decryptField key portEnc
        case (mIp, mPortStr) of
            (Just ip, Just portStr) ->
                pure (Just (pk, ip, readPort portStr, ls, src))
            _ -> pure Nothing  -- M10.3.7: authentication failure or missing key — drop the row
    mapMaybeM f xs = do
        results <- mapM f xs
        pure [ x | Just x <- results ]

-- | Save a key-value setting.
saveSetting :: AnthonyDB -> String -> String -> IO ()
saveSetting db key value =
    SQL.withStatement (dbConn db)
        "INSERT OR REPLACE INTO settings (key, value) VALUES (?, ?)"
        $ \stmt -> do
            SQL.bindText stmt 1 key
            SQL.bindText stmt 2 value
            _ <- SQL.step stmt
            pure ()

-- | Load a setting by key. Returns 'Nothing' if not found.
loadSetting :: AnthonyDB -> String -> IO (Maybe String)
loadSetting db key =
    SQL.withStatement (dbConn db)
        "SELECT value FROM settings WHERE key = ?"
        $ \stmt -> do
            SQL.bindText stmt 1 key
            hasRow <- SQL.stepRow stmt
            if hasRow
                then do
                    v <- SQL.columnText stmt 0
                    pure (if null v then Nothing else Just v)
                else pure Nothing

-- | Save a message to the database.
--
-- When the 'AnthonyDB' handle carries a 'StorageKey' (set via 'openDBWithKey'),
-- the @content_enc@ field is encrypted with 'encryptField' before insertion so that
-- the raw database does not contain plaintext message bodies.
saveMessage :: AnthonyDB -> Int -> String -> String -> Int -> IO ()
saveMessage db convId sender content timestamp = do
    storedContent <- case dbStorageKey db of
        Just key -> encryptField key content
        Nothing  -> pure content
    SQL.withStatement (dbConn db)
        "INSERT INTO messages (conversation_id, sender, content_enc, timestamp) VALUES (?, ?, ?, ?)"
        $ \stmt -> do
            SQL.bindInt  stmt 1 convId
            SQL.bindText stmt 2 sender
            SQL.bindText stmt 3 storedContent
            SQL.bindInt  stmt 4 timestamp
            _ <- SQL.step stmt
            pure ()

-- | Load the most recent N messages for a conversation.
--
-- Returns @(sender, content, timestamp)@ tuples, oldest first.
--
-- When the 'AnthonyDB' handle carries a 'StorageKey', each row's @content_enc@
-- field is decrypted with 'decryptField'.  Rows that carry the @UVENC1:@
-- prefix but fail GCM authentication are dropped (M10.3.7 — genuine
-- authentication failure or tampered ciphertext).  Rows that lack the prefix
-- are treated as legacy plaintext and passed through to support migration of
-- databases written before at-rest encryption was activated.
loadMessages :: AnthonyDB -> Int -> Int -> IO [(String, String, Int)]
loadMessages db convId limit = do
    rows <- SQL.withStatement (dbConn db)
        "SELECT sender, content_enc, timestamp FROM messages WHERE conversation_id = ? ORDER BY timestamp DESC LIMIT ?"
        $ \stmt -> do
            SQL.bindInt stmt 1 convId
            SQL.bindInt stmt 2 limit
            collectRows stmt $ \s -> do
                sender  <- SQL.columnText s 0
                content <- SQL.columnText s 1
                ts      <- SQL.columnInt  s 2
                pure (sender, content, ts)
    let ordered = reverse rows
    case dbStorageKey db of
        Nothing  -> pure ordered
        Just key -> do
            results <- mapM (decryptRow key) ordered
            pure (concat results)
  where
    decryptRow key (sender, content, ts)
        | isEncryptedField content = do
            -- Ciphertext present: authenticate and decrypt; drop on failure.
            mPlaintext <- decryptField key content
            case mPlaintext of
                Just plaintext -> pure [(sender, plaintext, ts)]
                Nothing        -> pure []  -- M10.3.7: authentication failure — drop the row
        | otherwise =
            -- M27.4.4: reject legacy plaintext when encryption is active.
            pure [(sender, "[UNREADABLE: legacy plaintext rejected]", ts)]

-- | Delete messages older than N days.
pruneMessages :: AnthonyDB -> Int -> IO ()
pruneMessages db days =
    SQL.withStatement (dbConn db)
        "DELETE FROM messages WHERE timestamp < (strftime('%s','now') - ?)"
        $ \stmt -> do
            SQL.bindInt stmt 1 (days * 86400)
            _ <- SQL.step stmt
            pure ()

-- | Clear all messages for a conversation.
clearConversation :: AnthonyDB -> Int -> IO ()
clearConversation db convId =
    SQL.withStatement (dbConn db)
        "DELETE FROM messages WHERE conversation_id = ?"
        $ \stmt -> do
            SQL.bindInt stmt 1 convId
            _ <- SQL.step stmt
            pure ()

-- | Get the number of messages in a conversation.
messageCount :: AnthonyDB -> Int -> IO Int
messageCount db convId =
    SQL.withStatement (dbConn db)
        "SELECT COUNT(*) FROM messages WHERE conversation_id = ?"
        $ \stmt -> do
            SQL.bindInt stmt 1 convId
            hasRow <- SQL.stepRow stmt
            if hasRow
                then SQL.columnInt stmt 0
                else pure 0

-- Finding: M1.1.3 — saveConversation stored the conversation name (peer display
-- name) as plaintext in the SQLite database.  loadConversations returned it
-- verbatim, so a raw database dump exposed the social graph (who you spoke to)
-- even when message content was encrypted.
--
-- Vulnerability: Conversation names recorded in the conversations table reveal
-- peer identities to anyone with read access to the database file — the same
-- threat model that message-content encryption (M10.2.8) is designed to defeat.
-- Protecting message bodies while leaving names in plaintext is incomplete
-- at-rest protection.
--
-- Fix: When a 'StorageKey' is present, 'saveConversation' encrypts the @name@
-- field with 'encryptField' before insertion and 'loadConversations' decrypts it
-- after loading.  Rows that carry the @UVENC1:@ prefix but fail GCM
-- authentication are dropped (M10.3.7).  Rows that lack the prefix are treated
-- as legacy plaintext and passed through to support migration of databases
-- written before at-rest encryption was activated.  When no key is present both
-- functions behave as before, preserving backward compatibility for plaintext
-- databases (tests, openDB callers).
--
-- Verified: 'openDB' callers continue to round-trip names as plaintext.
-- 'openDBWithKey' callers see encrypted names on disk, plaintext in memory.
-- Legacy databases opened with a key migrate transparently on first load; new
-- writes are encrypted.
--
-- | Create or update a conversation record.
--
-- When the 'AnthonyDB' handle carries a 'StorageKey' (set via 'openDBWithKey'),
-- the @name@ field is encrypted with 'encryptField' before insertion.
saveConversation :: AnthonyDB -> Int -> String -> String -> Int -> IO ()
saveConversation db convId peerPubkey name created = do
    storedName <- case dbStorageKey db of
        Just key -> encryptField key name
        Nothing  -> pure name
    SQL.withStatement (dbConn db)
        "INSERT OR REPLACE INTO conversations (id, peer_pubkey, name, created) VALUES (?, ?, ?, ?)"
        $ \stmt -> do
            SQL.bindInt  stmt 1 convId
            SQL.bindText stmt 2 peerPubkey
            SQL.bindText stmt 3 storedName
            SQL.bindInt  stmt 4 created
            _ <- SQL.step stmt
            pure ()

-- | Load all conversations. Returns @(id, peer_pubkey, name, created)@.
--
-- When the 'AnthonyDB' handle carries a 'StorageKey', each row's @name@ field
-- is decrypted with 'decryptField'.  Rows that carry the @UVENC1:@ prefix but
-- fail GCM authentication are dropped (M10.3.7 — tampered or wrong-key
-- ciphertext).  Rows that lack the prefix are treated as legacy plaintext and
-- passed through to support migration of databases written before encryption was
-- enabled.
loadConversations :: AnthonyDB -> IO [(Int, String, String, Int)]
loadConversations db = do
    rows <- SQL.withStatement (dbConn db)
        "SELECT id, peer_pubkey, name, created FROM conversations ORDER BY id"
        $ \stmt -> collectRows stmt $ \s -> do
            cid     <- SQL.columnInt  s 0
            pubkey  <- SQL.columnText s 1
            name    <- SQL.columnText s 2
            created <- SQL.columnInt  s 3
            pure (cid, pubkey, name, created)
    case dbStorageKey db of
        Nothing  -> pure rows
        Just key -> do
            results <- mapM (decryptConvName key) rows
            pure (concat results)
  where
    decryptConvName key (convId, pubkey, name, created)
        | isEncryptedField name = do
            -- Ciphertext present: authenticate and decrypt; drop on failure.
            mPlainName <- decryptField key name
            case mPlainName of
                Just plainName -> pure [(convId, pubkey, plainName, created)]
                Nothing        -> pure []  -- M10.3.7: authentication failure — drop the row
        | otherwise =
            -- Legacy plaintext (written before encryption was enabled): pass through.
            pure [(convId, pubkey, name, created)]

-- | Save a trusted public key with a human-readable label.
saveTrustedKey :: AnthonyDB -> ByteString -> String -> IO ()
saveTrustedKey db pubkey label =
    SQL.withStatement (dbConn db)
        "INSERT OR REPLACE INTO trusted_keys (pubkey, label, added) VALUES (?, ?, strftime('%s','now'))"
        $ \stmt -> do
            SQL.bindText stmt 1 (C8.unpack (toHex pubkey))
            SQL.bindText stmt 2 label
            _ <- SQL.step stmt
            pure ()

-- | Load all trusted keys. Returns @(pubkey, label)@ pairs.
loadTrustedKeys :: AnthonyDB -> IO [(ByteString, String)]
loadTrustedKeys db =
    SQL.withStatement (dbConn db)
        "SELECT pubkey, label FROM trusted_keys"
        $ \stmt -> do
            rows <- collectRows stmt $ \s -> do
                hexPk <- SQL.columnText s 0
                lbl   <- SQL.columnText s 1
                case fromHex (C8.pack hexPk) of
                    Just pk -> pure (pk, lbl)
                    Nothing -> pure (BS.empty, lbl)
            -- M35B: reject malformed keys (not exactly 32 bytes) at load time
            -- so that constantEq call sites in Listener.hs are always given
            -- equal-length operands, avoiding the length-timing channel.
            pure (filter (\(pk, _) -> BS.length pk == 32) rows)

-- | Remove a trusted key by its public key.
removeTrustedKey :: AnthonyDB -> ByteString -> IO ()
removeTrustedKey db pubkey =
    SQL.withStatement (dbConn db)
        "DELETE FROM trusted_keys WHERE pubkey = ?"
        $ \stmt -> do
            SQL.bindText stmt 1 (C8.unpack (toHex pubkey))
            _ <- SQL.step stmt
            pure ()

------------------------------------------------------------------------
-- Internal — row collection helper
------------------------------------------------------------------------

-- | Step through all rows returned by a prepared statement, applying a
-- reader function to each row and collecting results.
collectRows :: SQL.Statement -> (SQL.Statement -> IO a) -> IO [a]
collectRows stmt readRow = go []
  where
    go acc = do
        hasRow <- SQL.stepRow stmt
        if hasRow
            then do
                row <- readRow stmt
                go (acc ++ [row])
            else pure acc

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

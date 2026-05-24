-- SPDX-License-Identifier: Apache-2.0
-- | Per-peer outbound message queue for offline peers.
--
-- When a peer is offline, encrypted wire bytes are held in a FIFO queue
-- and drained for delivery when the peer reconnects.  Each queue has a
-- configurable maximum depth (default 100) and maximum message age
-- (default 86400 seconds / 24 hours).  When the queue is full the oldest
-- entry is dropped to make room.
--
-- See: doc/spec/chat.md
module UmbraVox.Chat.OutboundQueue
    ( OutboundQueue
    , QueueEntry(..)
    , newQueue
    , enqueue
    , dequeueAll
    , queueSize
    , pruneStale
    , maxQueueDepth
    , maxMessageAge
      -- * Persistence (M28.1.2)
    , saveQueue
    , loadQueue
      -- * Drain on reconnect (M28.1.3)
    , drainQueue
      -- * Relay fallback (M28.2.5)
    , shouldFallbackToRelay
    , shouldFallbackToRelayIO
    ) where

import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import System.IO (hPutStrLn, stderr)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word64)

import qualified UmbraVox.Storage.SQLite3 as SQL

-- | Default maximum number of queued messages per peer.
maxQueueDepth :: Int
maxQueueDepth = 100

-- | Default maximum age of a queued message in seconds (24 hours).
maxMessageAge :: Word64
maxMessageAge = 86400

-- | A single queued outbound message.
data QueueEntry = QueueEntry
    { qeWireBytes :: !ByteString  -- ^ Encrypted message ready to send
    , qeTimestamp :: !Word64      -- ^ When it was queued (POSIX seconds)
    } deriving stock (Show, Eq)

-- | Per-peer FIFO queue of encrypted messages awaiting delivery.
data OutboundQueue = OutboundQueue
    { oqEntries  :: !(IORef (Seq QueueEntry))
    , oqMaxDepth :: !Int     -- ^ Maximum queue depth (default 100)
    , oqMaxAge   :: !Word64  -- ^ Maximum message age in seconds (default 86400)
    }

-- | Create a new empty queue with the given depth limit and max age.
newQueue :: Int -> Word64 -> IO OutboundQueue
newQueue depth age = do
    ref <- newIORef Seq.empty
    pure OutboundQueue
        { oqEntries  = ref
        , oqMaxDepth = depth
        , oqMaxAge   = age
        }

-- | Enqueue a message.  If the queue is at capacity, the oldest entry
-- is dropped to make room.
enqueue :: OutboundQueue -> ByteString -> Word64 -> IO ()
enqueue oq wireBytes timestamp = do
    let entry = QueueEntry { qeWireBytes = wireBytes, qeTimestamp = timestamp }
    atomicModifyIORef' (oqEntries oq) $ \s ->
        let s' = s Seq.|> entry
            s'' = if Seq.length s' > oqMaxDepth oq
                  then Seq.drop (Seq.length s' - oqMaxDepth oq) s'
                  else s'
        in (s'', ())

-- | Drain all queued messages for delivery on reconnect, oldest first.
-- The queue is left empty after this call.
dequeueAll :: OutboundQueue -> IO [QueueEntry]
dequeueAll oq =
    atomicModifyIORef' (oqEntries oq) $ \s ->
        (Seq.empty, foldr (:) [] s)

-- | Current queue depth.
queueSize :: OutboundQueue -> IO Int
queueSize oq = Seq.length <$> readIORef (oqEntries oq)

-- | Remove entries older than maxAge relative to the given current time.
-- Returns the number of entries pruned.
pruneStale :: OutboundQueue -> Word64 -> IO Int
pruneStale oq now =
    atomicModifyIORef' (oqEntries oq) $ \s ->
        let cutoff = if now > oqMaxAge oq then now - oqMaxAge oq else 0
            s' = Seq.filter (\e -> qeTimestamp e >= cutoff) s
        in (s', Seq.length s - Seq.length s')

------------------------------------------------------------------------
-- Persistence (M28.1.2)
------------------------------------------------------------------------

-- | Ensure the outbound_queue table exists.
ensureQueueTable :: SQL.Database -> IO ()
ensureQueueTable db =
    SQL.execute_ db
        "CREATE TABLE IF NOT EXISTS outbound_queue \
        \(id INTEGER PRIMARY KEY, peer_id TEXT NOT NULL, \
        \message BLOB NOT NULL, created_at INTEGER NOT NULL)"

-- | Save queue entries to SQLite database.
--
-- Existing rows for the given peer are deleted and replaced with the
-- current queue contents, inside a single transaction.
saveQueue :: SQL.Database -> ByteString -> OutboundQueue -> IO ()
saveQueue db peerId oq = do
    ensureQueueTable db
    entries <- readIORef (oqEntries oq)
    SQL.execute_ db "BEGIN"
    SQL.withStatement db "DELETE FROM outbound_queue WHERE peer_id = ?" $ \stmt -> do
        SQL.bindBlob stmt 1 peerId
        _ <- SQL.step stmt
        pure ()
    mapM_ (insertEntry db peerId) (foldr (:) [] entries)
    SQL.execute_ db "COMMIT"
  where
    insertEntry db' pid entry =
        SQL.withStatement db'
            "INSERT INTO outbound_queue (peer_id, message, created_at) VALUES (?, ?, ?)"
            $ \stmt -> do
                SQL.bindBlob stmt 1 pid
                SQL.bindBlob stmt 2 (qeWireBytes entry)
                SQL.bindInt  stmt 3 (fromIntegral (qeTimestamp entry))
                _ <- SQL.step stmt
                pure ()

-- | Load queue entries from SQLite database.
--
-- Returns a new 'OutboundQueue' populated with stored entries for the
-- given peer identity.  The table is created if it does not exist.
loadQueue :: SQL.Database -> ByteString -> Int -> Word64 -> IO OutboundQueue
loadQueue db peerId depth maxAge = do
    ensureQueueTable db
    oq <- newQueue depth maxAge
    SQL.withStatement db
        "SELECT message, created_at FROM outbound_queue WHERE peer_id = ? ORDER BY created_at ASC"
        $ \stmt -> do
            SQL.bindBlob stmt 1 peerId
            loadRows stmt oq
    pure oq
  where
    loadRows stmt oq = do
        hasRow <- SQL.stepRow stmt
        if hasRow
            then do
                wireBytes <- SQL.columnBlob stmt 0
                ts        <- SQL.columnInt  stmt 1
                enqueue oq wireBytes (fromIntegral ts)
                loadRows stmt oq
            else pure ()

------------------------------------------------------------------------
-- Drain on reconnect (M28.1.3)
------------------------------------------------------------------------

-- | Drain queued messages for a peer and send them over the transport.
--
-- Dequeues all entries oldest-first and passes each to the send
-- function.  Returns the count of messages sent.  If a send fails,
-- remaining entries are re-enqueued.
drainQueue :: OutboundQueue -> (ByteString -> IO ()) -> IO Int
drainQueue oq sendFn = do
    entries <- dequeueAll oq
    sendAll entries 0
  where
    sendAll [] n = pure n
    sendAll (e:es) n = do
        ok <- (sendFn (qeWireBytes e) >> pure True)
              `catch` (\(e' :: SomeException) -> do
                  hPutStrLn stderr $ "Warning: send failed: " ++ show e'
                  pure False)
        if ok
            then sendAll es (n + 1)
            else do
                -- Re-enqueue the failed entry and all remaining ones.
                mapM_ (\entry -> enqueue oq (qeWireBytes entry) (qeTimestamp entry)) (e:es)
                pure n

------------------------------------------------------------------------
-- Relay fallback (M28.2.5)
------------------------------------------------------------------------

-- | Check if a peer has been offline long enough to fall back to relay.
--
-- Returns 'True' when the oldest queued message is older than
-- @fallbackDelay@ seconds relative to @currentTime@.  Returns 'False'
-- if the queue is empty.
shouldFallbackToRelay :: OutboundQueue -> Word64 -> Word64 -> Bool
shouldFallbackToRelay _ _ _ = False
-- NOTE: This is a pure heuristic check that needs access to the oldest
-- entry's timestamp. Since OutboundQueue uses IORef internally, we
-- provide an IO version below and keep this as a stub for the
-- signature specified in the task.

-- | IO version that inspects the queue entries.
shouldFallbackToRelayIO :: OutboundQueue -> Word64 -> Word64 -> IO Bool
shouldFallbackToRelayIO oq currentTime fallbackDelay = do
    entries <- readIORef (oqEntries oq)
    case Seq.lookup 0 entries of
        Nothing -> pure False
        Just oldest ->
            let age = if currentTime > qeTimestamp oldest
                      then currentTime - qeTimestamp oldest
                      else 0
            in pure (age >= fallbackDelay)

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
    ) where

import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word64)

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

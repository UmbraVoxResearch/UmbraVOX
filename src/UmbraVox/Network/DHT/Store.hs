-- SPDX-License-Identifier: Apache-2.0
-- | Local DHT value store with TTL-based expiration (M24.3.5).
--
-- Stores key-value pairs locally for the distributed hash table.
-- Values are capped at a configurable maximum size (default 1024 bytes)
-- and expire after a configurable TTL (default 24 hours).  Expired
-- entries are pruned both on lookup and via periodic sweep.
module UmbraVox.Network.DHT.Store
    ( ValueStore(..)
    , newValueStore
    , localStore
    , localLookup
    , expireEntries
    ) where

import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Word (Word64)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

------------------------------------------------------------------------
-- Value store
------------------------------------------------------------------------

-- | Local DHT value store with TTL.
--
-- Each entry maps a key to a value and an expiry timestamp (POSIX
-- seconds).  Values exceeding 'vsMaxValueSize' are rejected at
-- insertion time.
data ValueStore = ValueStore
    { vsEntries      :: !(IORef (Map ByteString (ByteString, Word64)))
      -- ^ key -> (value, expiry timestamp POSIX seconds)
    , vsMaxValueSize :: !Int
      -- ^ maximum value size in bytes (default 1024)
    }

-- | Create a new empty value store with the given maximum value size.
newValueStore :: Int -> IO ValueStore
newValueStore maxSize = do
    ref <- newIORef Map.empty
    return ValueStore
        { vsEntries      = ref
        , vsMaxValueSize = maxSize
        }

------------------------------------------------------------------------
-- Store / lookup
------------------------------------------------------------------------

-- | Store a value locally with a TTL.
--
-- Returns 'False' if the value exceeds 'vsMaxValueSize'; the entry is
-- not inserted in that case.  Returns 'True' on success.  If the key
-- already exists the entry is overwritten with the new value and expiry.
localStore :: ValueStore -> ByteString -> ByteString -> Word64 -> IO Bool
localStore vs key value expiry
    | BS.length value > vsMaxValueSize vs = return False
    | otherwise = do
        modifyIORef' (vsEntries vs) (Map.insert key (value, expiry))
        return True

-- | Retrieve a value locally.
--
-- Returns 'Nothing' if the key is not found or has expired.  Expired
-- entries are removed eagerly on lookup.  The caller supplies the
-- current POSIX timestamp for testability.
localLookup :: ValueStore -> ByteString -> Word64 -> IO (Maybe ByteString)
localLookup vs key now = do
    entries <- readIORef (vsEntries vs)
    case Map.lookup key entries of
        Nothing -> return Nothing
        Just (value, expiry)
            | now >= expiry -> do
                -- Evict expired entry eagerly
                modifyIORef' (vsEntries vs) (Map.delete key)
                return Nothing
            | otherwise -> return (Just value)

------------------------------------------------------------------------
-- Expiration sweep
------------------------------------------------------------------------

-- | Evict all expired entries from the store.
--
-- Returns the number of entries evicted.  The caller supplies the
-- current POSIX timestamp for testability.
expireEntries :: ValueStore -> Word64 -> IO Int
expireEntries vs now = do
    entries <- readIORef (vsEntries vs)
    let (expired, live) = Map.partition (\(_, expiry) -> now >= expiry) entries
        count = Map.size expired
    writeIORef (vsEntries vs) live
    return count

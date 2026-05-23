-- SPDX-License-Identifier: Apache-2.0
-- | Relay-assisted delivery for offline peers.
--
-- When direct peer-to-peer delivery fails (recipient offline), messages
-- can be stored at a relay node addressed by a stealth-derived key.
-- The relay sees only opaque ciphertext and a stealth address — no
-- identity linkage is possible.
--
-- Messages are held for a configurable TTL (default 7 days) and pruned
-- on poll.  Maximum relay message size defaults to 4096 bytes.
--
-- See: doc/spec/relay.md (planned)
module UmbraVox.Network.Relay
    ( RelayMailbox(..)
    , RelayConfig(..)
    , defaultRelayConfig
    , storeForRelay
    , pollRelay
    , newRelayMailbox
    , relayMessageTTL
      -- * DHT-based relay polling (M28.2.2)
    , pollRelayViaDHT
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word64)

import UmbraVox.Crypto.StealthAddress (StealthKeys(..))
import UmbraVox.Network.DHT (DHTState(..))
import qualified UmbraVox.Network.DHT.Store as DHTStore

-- | Default message TTL on relay: 7 days in seconds.
relayMessageTTL :: Word64
relayMessageTTL = 604800

-- | Configuration for relay-assisted delivery.
data RelayConfig = RelayConfig
    { rcEnabled       :: !Bool    -- ^ Whether relay delivery is enabled
    , rcTTLSeconds    :: !Word64  -- ^ Message TTL on relay (default 7 days)
    , rcMaxSize       :: !Int     -- ^ Max message size for relay (default 4096)
    , rcFallbackDelay :: !Int     -- ^ Seconds offline before falling back to relay
    } deriving stock (Show, Eq)

-- | Sensible defaults: relay enabled, 7-day TTL, 4096 byte max, 30s fallback.
defaultRelayConfig :: RelayConfig
defaultRelayConfig = RelayConfig
    { rcEnabled       = True
    , rcTTLSeconds    = relayMessageTTL
    , rcMaxSize       = 4096
    , rcFallbackDelay = 30
    }

-- | A relay mailbox stores encrypted blobs addressed by stealth keys.
-- No identity linkage — relay sees only opaque ciphertext + stealth address.
data RelayMailbox = RelayMailbox
    { rmStealthKey :: !ByteString                          -- ^ Stealth-derived storage key
    , rmMessages   :: !(IORef (Seq (ByteString, Word64)))  -- ^ (ciphertext, expiry)
    }

-- | Create a new empty relay mailbox for the given stealth key.
newRelayMailbox :: ByteString -> IO RelayMailbox
newRelayMailbox sk = do
    ref <- newIORef Seq.empty
    pure RelayMailbox
        { rmStealthKey = sk
        , rmMessages   = ref
        }

-- | Store a message for relay delivery (sender side).
--
-- The ciphertext is stored with the given expiry timestamp (POSIX seconds).
-- Returns 'True' if the message was accepted, 'False' if it exceeds the
-- configured max size (checked against 'defaultRelayConfig').
storeForRelay :: RelayMailbox -> ByteString -> Word64 -> IO Bool
storeForRelay mb ciphertext expiry
    | BS.length ciphertext > rcMaxSize defaultRelayConfig = pure False
    | otherwise = do
        atomicModifyIORef' (rmMessages mb) $ \s ->
            (s Seq.|> (ciphertext, expiry), ())
        pure True

-- | Poll for messages from relay (recipient side).
--
-- Returns all messages whose expiry is strictly greater than the given
-- current time (i.e. not yet expired).  Expired messages are pruned
-- from the mailbox during the poll.
pollRelay :: RelayMailbox -> Word64 -> IO [ByteString]
pollRelay mb now =
    atomicModifyIORef' (rmMessages mb) $ \s ->
        let valid = Seq.filter (\(_, expiry) -> expiry > now) s
        in (valid, map fst (foldr (:) [] valid))

------------------------------------------------------------------------
-- DHT-based relay polling (M28.2.2)
------------------------------------------------------------------------

-- | Poll for messages addressed to our stealth key via DHT.
--
-- Looks up the relay mailbox key derived from our stealth scan public
-- key in the DHT value store.  Any stored blobs whose expiry has not
-- passed are returned.  Currently this performs a local-only lookup;
-- when the DHT transport integration (M24.4) is complete, this will
-- issue a @FIND_VALUE@ RPC to the network.
--
-- The @Word64@ argument is the current POSIX timestamp used for
-- expiry filtering.
pollRelayViaDHT :: DHTState -> StealthKeys -> Word64 -> IO [ByteString]
pollRelayViaDHT dht sk now = do
    let lookupKey = skScanPublic sk
    mVal <- DHTStore.localLookup (dhStore dht) lookupKey now
    case mVal of
        Nothing  -> pure []
        Just val -> pure [val]

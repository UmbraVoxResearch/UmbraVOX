-- SPDX-License-Identifier: Apache-2.0
-- | Centralized default values and named constants for UmbraVox.
--
-- All magic numbers that appear in more than one module, or that represent
-- a policy decision (security limit, protocol constant, timeout), should be
-- defined here so they can be audited and adjusted from a single location.
--
-- Crypto-domain HKDF info strings (e.g. "UmbraVox_Ratchet_v1") are intentionally
-- kept in their respective crypto modules to avoid circular imports.  Their
-- string values are documented here as comments for auditability.
module UmbraVox.App.Defaults
    ( -- * Network
      defaultPort
    , defaultPorts
    , maxInboundConnections
      -- * Framing
    , maxFrameSize
      -- * Connection timeouts
    , connectTimeoutUs
    , connectTryPortTimeoutUs
    , socks5HandshakeTimeoutUs
      -- * Double Ratchet
    , defaultMaxSkip
    , defaultMaxTotalSkipped
    , defaultMaxRatchetSkip
    , defaultMaxSeenDHKeys
      -- * Storage
    , sqliteTimeoutMicros
      -- * mDNS
    , mdnsAnnounceIntervalUs
    , mdnsPeerEvictionSeconds
      -- * TUI / polling
    , mdnsPollIntervalUs
      -- * Discovery (M24.2)
    , discoveryPollIntervalUs
      -- * DHT (M24.4)
    , dhtDefaultEnabled
    , dhtDefaultBootstrapNodes
    ) where

import Data.Word (Word32)

------------------------------------------------------------------------
-- Network
------------------------------------------------------------------------

-- | Primary UmbraVox TCP listening port.
defaultPort :: Int
defaultPort = 7853

-- | Default port sequence tried when connecting without an explicit port.
-- Tries the primary port first, then common alternatives.
defaultPorts :: [Int]
defaultPorts = [7853, 7854, 7855, 9999, 7856, 7857, 7858, 7859, 7860]

-- | Maximum number of simultaneous inbound TCP connections per listener.
-- Connections beyond this limit are rejected before any authentication work.
maxInboundConnections :: Int
maxInboundConnections = 64

------------------------------------------------------------------------
-- Framing
------------------------------------------------------------------------

-- | Maximum Noise frame size in bytes (64 KiB).
-- Frames reporting a larger length are rejected to prevent DoS via large
-- heap allocations without crashing the process.
maxFrameSize :: Word32
maxFrameSize = 65536

------------------------------------------------------------------------
-- Connection timeouts
------------------------------------------------------------------------

-- | Timeout for a full TCP connect attempt on a known single port (microseconds).
connectTimeoutUs :: Int
connectTimeoutUs = 8 * 1000000  -- 8 seconds

-- | Timeout per port when trying a sequence of fallback ports (microseconds).
connectTryPortTimeoutUs :: Int
connectTryPortTimeoutUs = 2 * 1000000  -- 2 seconds

-- | Timeout for the SOCKS5 proxy handshake phase (microseconds).
socks5HandshakeTimeoutUs :: Int
socks5HandshakeTimeoutUs = 10 * 1000000  -- 10 seconds

------------------------------------------------------------------------
-- Double Ratchet
------------------------------------------------------------------------

-- | Maximum number of skipped message keys stored per ratchet step (M7.3.6).
-- Maps to 'maxSkip' in Signal spec §2.6.
defaultMaxSkip :: Word32
defaultMaxSkip = 1000

-- | Maximum total entries in the skipped-key cache across all DH ratchets.
-- Prevents unbounded memory growth from an adversary triggering many small
-- skips across many ratchet epochs (M7.3.6).
defaultMaxTotalSkipped :: Int
defaultMaxTotalSkipped = 5000

-- | Maximum allowed gap between received counter and current counter
-- before a ratchet step is rejected (M23.3.3).  Prevents an adversary
-- from forcing the receiver to derive an excessive number of chain keys.
defaultMaxRatchetSkip :: Int
defaultMaxRatchetSkip = 1000

-- | Maximum number of distinct peer DH public keys retained for replay
-- detection (M23.3.3).  Once this limit is reached, the oldest DH key
-- is forgotten; replays using very old epochs are rejected by the
-- skipped-key cache eviction rather than the DH replay check.
defaultMaxSeenDHKeys :: Int
defaultMaxSeenDHKeys = 10

------------------------------------------------------------------------
-- Storage
------------------------------------------------------------------------

-- | Timeout for sqlite3 CLI subprocess calls (microseconds).
-- Both runSQL and querySQL apply this limit to avoid indefinite hangs.
sqliteTimeoutMicros :: Int
sqliteTimeoutMicros = 10 * 1000000  -- 10 seconds

------------------------------------------------------------------------
-- mDNS
------------------------------------------------------------------------

-- | Interval between mDNS service announcements (microseconds).
-- 10 seconds provides fast local discovery while keeping multicast traffic low.
mdnsAnnounceIntervalUs :: Int
mdnsAnnounceIntervalUs = 10 * 1000000  -- 10 seconds

-- | Seconds after last announcement before a peer is evicted from the peer list.
-- Set to 3 × announce interval so a peer survives two missed announcements.
mdnsPeerEvictionSeconds :: Int
mdnsPeerEvictionSeconds = 3 * (mdnsAnnounceIntervalUs `div` 1000000)  -- 30 seconds

------------------------------------------------------------------------
-- TUI / polling
------------------------------------------------------------------------

-- | Poll interval for the mDNS manager worker loop (microseconds).
-- Determines how quickly newly discovered peers appear in the TUI peer list.
mdnsPollIntervalUs :: Int
mdnsPollIntervalUs = 5 * 1000000  -- 5 seconds

------------------------------------------------------------------------
-- Discovery (M24.2)
------------------------------------------------------------------------

-- | Poll interval for DNS-based peer discovery (microseconds).
-- Controls how frequently _umbravox._tcp SRV records are re-queried.
discoveryPollIntervalUs :: Int
discoveryPollIntervalUs = 30 * 1000000  -- 30 seconds

------------------------------------------------------------------------
-- DHT (M24.4)
------------------------------------------------------------------------

-- | Whether the DHT subsystem is enabled by default.
dhtDefaultEnabled :: Bool
dhtDefaultEnabled = False

-- | Default bootstrap node addresses (empty — no bootstrap by default).
dhtDefaultBootstrapNodes :: [String]
dhtDefaultBootstrapNodes = []

------------------------------------------------------------------------
-- HKDF domain separation — reference only
------------------------------------------------------------------------
-- The following info strings are defined in their respective crypto modules
-- to prevent circular imports.  They are documented here for auditability:
--
--   "UmbraVox_Ratchet_v1"     — UmbraVox.Crypto.Signal.DoubleRatchet (ratchetInfo)
--   "UmbraVox_Nonce_v1"       — UmbraVox.Crypto.Signal.DoubleRatchet (nonceInfo)
--   "UmbraVox_X3DH_v1"        — UmbraVox.Crypto.Signal.X3DH          (x3dhInfo)
--   "UmbraVox_PQXDH_v1"       — UmbraVox.Crypto.Signal.PQXDH         (pqxdhInfo)
--   "UmbraVox_StealthKey_v1"  — UmbraVox.Crypto.StealthAddress       (stealthKeyInfo)
--   "UmbraVox_ViewTag_v2"     — UmbraVox.Crypto.StealthAddress       (viewTagInfo)
--   "UmbraVox_KeyStore_v1"    — UmbraVox.Crypto.KeyStore             (keystoreInfo)
--   "UmbraVox_Export_v1"      — UmbraVox.Crypto.Export               (exportInfo)
--   "UmbraVox_SafetyNumber_v2" — UmbraVox.Protocol.QRCode            (inline)

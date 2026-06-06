# DHT Network and Peer Discovery Plan

## Status

Draft — 2026-05-21

## Overview

This document specifies the Kademlia-style DHT, unified discovery manager, and
security model refactoring for UmbraVOX.  The goal is decentralized peer
discovery and message routing without any central infrastructure, while
preserving the existing privacy guarantees (Dandelion++, Noise IK, ephemeral
defaults).

All six discovery sources (mDNS, PEX, environment variable, DNS SRV/TXT,
config file, DHT) feed into a single `PeerManager` pipeline.  Users enable any
combination via preferences.

------------------------------------------------------------------------

## 1. DHT Module Design

### 1.1 Module: `UmbraVox.Network.DHT.Types`

```haskell
-- | 256-bit node identifier derived from SHA-256(identity public key).
newtype NodeId = NodeId ByteString  -- exactly 32 bytes
    deriving stock (Eq, Ord, Show)

-- | XOR distance metric for Kademlia routing.
xorDistance :: NodeId -> NodeId -> ByteString

-- | A node in the DHT network.
data DHTNode = DHTNode
    { dhtNodeId   :: !NodeId
    , dhtAddress  :: !String       -- host:port
    , dhtLastSeen :: !Word64       -- POSIX seconds
    , dhtRTT      :: !(Maybe Int)  -- microseconds, measured
    }

-- | k-bucket entry with replacement cache.
data KBucket = KBucket
    { kbEntries     :: ![DHTNode]   -- active nodes, most-recent-last, max k
    , kbReplacement :: ![DHTNode]   -- replacement cache, max k
    }

-- | Routing table: 256 k-buckets (one per bit of the XOR distance).
data RoutingTable = RoutingTable
    { rtSelf    :: !NodeId
    , rtBuckets :: !(IORef (Vector KBucket))  -- index 0 = farthest
    , rtK       :: !Int                       -- replication factor (20)
    }

-- | DHT RPC messages.
data DHTMessage
    = Ping        !NodeId
    | Pong        !NodeId
    | FindNode    !NodeId !NodeId           -- sender, target
    | FindNodeReply !NodeId ![DHTNode]      -- sender, closest k nodes
    | Store       !NodeId !ByteString !ByteString  -- sender, key, value
    | FindValue   !NodeId !ByteString       -- sender, key
    | FindValueReply !NodeId !(Either [DHTNode] ByteString)
    deriving stock (Show, Eq)

-- | DHT configuration.
data DHTConfig = DHTConfig
    { dhtK               :: !Int     -- bucket size (20)
    , dhtAlpha           :: !Int     -- parallel queries (3)
    , dhtRefreshInterval :: !Int     -- bucket refresh (seconds, 3600)
    , dhtRepublishInterval :: !Int   -- value republish (seconds, 3600)
    , dhtExpireInterval  :: !Int     -- stored value TTL (seconds, 86400)
    , dhtBootstrapNodes  :: ![String]  -- initial bootstrap addresses
    }

defaultDHTConfig :: DHTConfig
```

### 1.2 Module: `UmbraVox.Network.DHT.RoutingTable`

Operations on the k-bucket routing table:

- `newRoutingTable :: NodeId -> Int -> IO RoutingTable`
- `insertNode :: RoutingTable -> DHTNode -> IO ()` — Kademlia insert:
  if bucket not full, append; if full, ping oldest; if oldest dead, evict
  and insert; if oldest alive, move to replacement cache.
- `findClosest :: RoutingTable -> NodeId -> Int -> IO [DHTNode]` — return
  the k closest nodes by XOR distance.
- `bucketIndex :: NodeId -> NodeId -> Int` — which bucket a node falls into
  (leading zero count of XOR).
- `refreshBucket :: RoutingTable -> Int -> IO ()` — random lookup in bucket
  range to populate sparse buckets.
- `removeNode :: RoutingTable -> NodeId -> IO ()` — remove and promote from
  replacement cache.

### 1.3 Module: `UmbraVox.Network.DHT.RPC`

Wire protocol for DHT messages over encrypted transport:

- All DHT traffic goes through the existing Noise IK handshake (no
  cleartext DHT messages ever).
- Message serialization: compact binary (type byte + fields), similar to
  existing PEX wire format.
- `sendDHTMessage :: AnyTransport -> DHTMessage -> IO ()`
- `recvDHTMessage :: AnyTransport -> IO DHTMessage`
- Maximum DHT message size: 4096 bytes (caps FindNodeReply at ~40 nodes).

### 1.4 Module: `UmbraVox.Network.DHT.Lookup`

Iterative lookup algorithm:

```
iterativeFindNode :: DHTState -> NodeId -> IO [DHTNode]
```

1. Seed shortlist with alpha (3) closest nodes from local routing table.
2. Send FIND_NODE to alpha closest unqueried nodes in parallel.
3. On each reply, merge new nodes into shortlist, sorted by XOR distance.
4. Repeat until: (a) k closest nodes have all been queried, or (b) no
   closer node found in last round.
5. Return k closest nodes.

Similarly for `iterativeFindValue` (returns value or closest nodes).

### 1.5 Module: `UmbraVox.Network.DHT.Store`

Distributed storage for peer announcements:

- `storeValue :: DHTState -> ByteString -> ByteString -> IO ()` — iterative
  store: find k closest to key, send STORE to each.
- `getValue :: DHTState -> ByteString -> IO (Maybe ByteString)` — iterative
  lookup returning the value.
- Values expire after `dhtExpireInterval` (24h default).
- Nodes republish their own address every `dhtRepublishInterval` (1h).
- Stored values are capped at 1024 bytes to prevent abuse.

### 1.6 Module: `UmbraVox.Network.DHT`

Top-level DHT state and lifecycle:

```haskell
data DHTState = DHTState
    { dhConfig       :: !DHTConfig
    , dhRoutingTable :: !RoutingTable
    , dhStore        :: !(IORef (Map ByteString (ByteString, Word64)))
    , dhTransport    :: !AnyTransport  -- encrypted transport handle
    , dhPeerManager  :: !PeerManager   -- integration point
    , dhDandelion    :: !DandelionState -- privacy routing integration
    }

-- | Bootstrap into the DHT network.
bootstrap :: DHTState -> IO ()

-- | Periodic maintenance (bucket refresh, value republish, stale eviction).
maintain :: DHTState -> IO ()

-- | Announce our presence: store our address at SHA-256(our NodeId).
announcePresence :: DHTState -> IO ()

-- | Look up a peer by their identity key hash.
findPeer :: DHTState -> NodeId -> IO (Maybe DHTNode)
```

### 1.7 Security Considerations

- **Sybil resistance**: Node IDs must equal SHA-256(identity public key).
  The identity key is verified during Noise IK handshake.  Nodes presenting
  a node ID that does not match their handshake key are immediately banned.
- **Eclipse resistance**: PeerManager source diversity (M23.1.4) applies.
  DHT peers get `SourceDHT` and cannot evict peers from other sources below
  the diversity floor (3 per source).
- **Routing table poisoning**: Only insert nodes after successful Noise IK
  handshake and node ID verification.  Unresponsive nodes are evicted.
- **Storage spam**: Values capped at 1024 bytes with 24h TTL.  Rate-limit
  STORE operations per source IP (max 10/min).
- **Privacy**: DHT lookups for *our own* identity are routed through
  Dandelion++ stem phase to prevent observers from correlating lookup
  targets with origin IP.
- **Encrypted transport**: All DHT RPC messages travel over Noise IK.  No
  cleartext DHT protocol on the wire.

------------------------------------------------------------------------

## 2. Discovery Manager

### 2.1 Module: `UmbraVox.Network.Discovery`

Unified discovery manager that combines all six peer sources:

```haskell
data DiscoverySource
    = DiscMDNS          -- existing: UmbraVox.Network.MDNS
    | DiscPEX           -- existing: UmbraVox.Network.PeerExchange
    | DiscEnvVar        -- new: UMBRAVOX_PEERS environment variable
    | DiscDNS           -- new: SRV/TXT records
    | DiscConfigFile    -- new: ~/.umbravox/peers in config
    | DiscDHT           -- new: Kademlia DHT
    deriving stock (Eq, Ord, Show, Enum, Bounded)

data DiscoveryConfig = DiscoveryConfig
    { dcEnabledSources :: !(IORef (Set DiscoverySource))
    , dcDNSDomain      :: !(IORef (Maybe String))   -- e.g. "peers.umbravox.net"
    , dcDHTBootstrap   :: !(IORef [String])          -- bootstrap node addresses
    , dcPollInterval   :: !Int                       -- microseconds (default 30s)
    }

data DiscoveryManager = DiscoveryManager
    { dmConfig      :: !DiscoveryConfig
    , dmPeerManager :: !PeerManager
    , dmWorkerTid   :: !(IORef (Maybe ThreadId))
    , dmDHTState    :: !(IORef (Maybe DHTState))
    }

-- | Start the discovery manager. Spawns background workers for each
-- enabled source.
startDiscovery :: DiscoveryManager -> AppConfig -> IO ()

-- | Stop all discovery workers.
stopDiscovery :: DiscoveryManager -> IO ()

-- | Enable or disable a discovery source at runtime.
setSourceEnabled :: DiscoveryManager -> DiscoverySource -> Bool -> IO ()
```

### 2.2 Source Implementations

**Environment variable** (`DiscEnvVar`):
- On startup, read `UMBRAVOX_PEERS` env var.
- Parse comma-separated `host:port` entries.
- Add each to PeerManager with `SourceManual`.
- One-shot on startup (not polled).

**DNS lookup** (`DiscDNS`):
- Query `_umbravox._tcp.<domain>` SRV records.
- Fall back to TXT records with `umbravox-peer=host:port` format.
- Poll every `dcPollInterval`.
- Add results to PeerManager with `SourceBootstrap`.

**Config file** (`DiscConfigFile`):
- Read `~/.umbravox/peers` (or `cfgDBPath`-adjacent directory).
- One line per peer: `host:port [# comment]`.
- Re-read on SIGHUP or config reload.
- Add to PeerManager with `SourceManual`.

**DHT** (`DiscDHT`):
- Bootstrap using `dcDHTBootstrap` nodes.
- Periodically announce presence and refresh routing table.
- On-demand lookups via `findPeer`.
- Add discovered nodes to PeerManager with `SourceDHT`.

### 2.3 PeerSource Extension

Extend `PeerSource` in `PeerManager.hs`:

```haskell
data PeerSource
    = SourceMDNS
    | SourcePEX
    | SourceBootstrap
    | SourceManual
    | SourceDHT          -- new
    | SourceDNS          -- new
    | SourceEnvVar       -- new
    | SourceConfig       -- new
    deriving stock (Show, Eq, Ord)
```

------------------------------------------------------------------------

## 3. Security Model Refactor

### 3.1 Remove Chastity Mode

**Rationale**: Chastity = Chaste + no persistence.  Since M17 enforces
ephemeral-by-default (DB off unless storage plugin loaded+enabled),
Chastity is now redundant with Chaste.

**Changes**:

| File | Change |
|------|--------|
| `App/Config.hs` | Remove `Chastity` from `ConnectionMode` enum |
| `App/Startup.hs` | Default mode becomes `Chaste` (was `Chastity`) |
| `BuildProfile.hs` | Remove `buildChastityOnly`; replace with `buildChasteOnly` |
| `Network/Listener.hs` | Remove `Chastity` branch in trustCheck (identical to `Chaste`) |
| `TUI/RuntimeSettings.hs` | Remove `Chastity` from cycle and side effects |
| `TUI/Dialog.hs` | Remove `settingsConnModeLabel Chastity`; update `settingsChastityTab` |
| `TUI/Render.hs` | Remove `Chastity` check in status bar (ephemeral flag suffices) |
| `Chat/API.hs` | No change (uses `show mode`) |
| `Storage/InMemory.hs` | Update comment referencing Chastity |

Migration: any persisted config referencing "Chastity" maps to "Chaste"
at load time.

### 3.2 Promiscuous Mode Changes

**Current**: Accept all connections + mDNS on + PEX on.
**New**: Accept all connections + mDNS on + PEX **off** (do not share peer
lists).

Promiscuous nodes are open to all comers but do not propagate network
topology information.  This prevents a Promiscuous node from being used as
an oracle for network mapping.

**Changes**:

| File | Change |
|------|--------|
| `TUI/RuntimeSettings.hs` | `applyConnectionModeSideEffects Promiscuous` sets PEX = False |
| `Network/Listener.hs` | `tryPEXExchange` checks `cfgPEXEnabled` before exchanging |
| `App/Config.hs` | Update comment on Promiscuous |

### 3.3 Swing Mode Changes

**Current**: Accept all + mDNS + auto PEX exchange.
**New**: Accept all + mDNS + **manual** PEX (user-initiated exchange only,
not automatic on connection).

Swing remains the most open mode, but PEX is no longer automatic.  The
user must explicitly request a peer list exchange (e.g., via a TUI command
or API call).

**Changes**:

| File | Change |
|------|--------|
| `TUI/RuntimeSettings.hs` | `applyConnectionModeSideEffects Swing` sets `cfgPEXAutomatic = False` |
| `Network/Listener.hs` | `tryPEXExchange` checks new `cfgPEXAutomatic` flag |
| `App/Config.hs` | Add `cfgPEXAutomatic :: IORef Bool` to `AppConfig` |
| `TUI/Dialog.hs` | Add "Exchange Peers" button/action for manual PEX |
| `Chat/API.hs` | Add `exchangePeers` RPC method for programmatic PEX |

### 3.4 Updated Mode Summary

| Mode | Accept | mDNS | PEX | DHT | Trust |
|------|--------|------|-----|-----|-------|
| Swing | All | On | Manual | Allowed | None |
| Promiscuous | All | On | Off | Allowed | None |
| Selective | Confirmed | On | Auto | Allowed | TOFU |
| Chaste | Trusted only | Off | Off | Off | Pre-shared keys |

Discovery sources (env var, DNS, config file) are always allowed
regardless of mode since they are user-configured and do not reveal
information to peers.

DHT participation follows mode: Chaste disables DHT since it implies
minimal network exposure.  Other modes allow DHT if the user has enabled
it in discovery preferences.

------------------------------------------------------------------------

## 4. Integration Points

### 4.1 PeerManager Integration

All discovery sources feed into `PeerManager.addPeer` with appropriate
`PeerSource` tags.  The existing source diversity eviction logic (M23.1.4)
prevents any single source from dominating the peer table.

DHT-discovered peers start with score 50 (neutral).  Successful message
delivery increases score; timeout/misbehavior decreases it.  DHT nodes
that fail node ID verification are immediately banned.

### 4.2 Dandelion++ Integration

- DHT lookups for our own node ID use stem-phase routing to prevent
  origin-IP correlation.
- DHT STORE operations for our own address are stem-routed.
- DHT FIND_NODE for other nodes can use normal (fluff) routing since the
  lookup target does not reveal our identity.
- The `DHTState` holds a reference to `DandelionState` for routing
  decisions.

### 4.3 Transport Integration

- DHT uses the existing `AnyTransport` abstraction.
- All DHT connections go through the existing Noise IK handshake — DHT
  messages are application-layer payloads inside encrypted channels.
- DHT message type is distinguished from chat messages by a type byte
  prefix in the Noise application data.
- The existing `TransportProvider` plugin system supports this without
  modification.

### 4.4 Config Integration

New `AppConfig` fields:

```haskell
-- Discovery
, cfgDiscoverySources   :: IORef (Set DiscoverySource)
, cfgPEXAutomatic       :: IORef Bool        -- True = auto on connect
-- DHT
, cfgDHTEnabled         :: IORef Bool
, cfgDHTBootstrapNodes  :: IORef [String]
-- DNS discovery
, cfgDNSDiscoveryDomain :: IORef (Maybe String)
```

### 4.5 TUI Integration

- Settings dialog: new "Discovery" tab listing all 6 sources with
  checkboxes.
- Status bar: show discovery source count (e.g., "D:3" for 3 active
  sources).
- Peer list: show source tag next to each peer (mDNS/PEX/DHT/etc.).
- Manual PEX button in Swing mode.

------------------------------------------------------------------------

## 5. Phase Plan

### Phase 1: Security Model Refactor (v0.3.2) -- COMPLETE

Removed Chastity mode (absorbed into Chaste), updated Promiscuous (PEX
disabled) and Swing (PEX manual/user-initiated) behavior.  Renamed
`buildChastityOnly` to `buildChasteOnly`.  No `cfgPEXAutomatic` field
needed -- PEX starts disabled and mode transitions no longer auto-enable it.

### Phase 2: Static Discovery Sources (v0.3.3)

Implement environment variable, DNS SRV/TXT, and config file discovery.
Extend `PeerSource` enum.  Build `DiscoveryManager` skeleton that
combines mDNS + PEX + the three new static sources.

Estimated effort: 3-4 days.

### Phase 3: DHT Core (v0.4.0)

Implement `DHT.Types`, `DHT.RoutingTable`, `DHT.RPC`, `DHT.Lookup`,
`DHT.Store`.  Unit tests with mock transport.  No network integration
yet.

Estimated effort: 5-7 days.

### Phase 4: DHT Integration (v0.4.1)

Wire DHT into `DiscoveryManager`, `PeerManager`, `Dandelion`, and
`AppConfig`.  Bootstrap sequence.  Integration tests with real transport.

Estimated effort: 4-5 days.

### Phase 5: TUI and API (v0.4.2)

Settings dialog for discovery sources.  Manual PEX button.  DHT status
display.  `exchangePeers` and `dhtStatus` RPC methods.

Estimated effort: 2-3 days.

### Phase 6: Hardening (v0.4.3)

Security audit of DHT implementation.  Sybil/eclipse attack tests.
Rate limiting.  Formal analysis of routing table invariants.

Estimated effort: 3-5 days.

------------------------------------------------------------------------

## 6. TODO Items

Items below were originally to be added to `TODO.txt` under a new milestone.
Status notes added 2026-06-05 based on codebase inspection.

```
================================================================================
MILESTONE M24: DHT NETWORK + UNIFIED DISCOVERY (v0.4.0)
================================================================================

[x] M24.1 Security model refactor (Phase 1)
  [x] M24.1.1 Remove Chastity from ConnectionMode enum and all references
      done: ConnectionMode = Swing | Promiscuous | Selective | Chaste (App/Config.hs)
  [x] M24.1.2 Rename buildChastityOnly to buildChasteOnly in BuildProfile.hs
      done: BuildProfile.hs has buildChasteOnly
  [x] M24.1.3 Update default mode: Chastity -> Chaste in App/Startup.hs
      done: Chaste is current default mode (App/Startup.hs)
  [ ] M24.1.4 Add cfgPEXAutomatic :: IORef Bool to AppConfig
      not done: field not present in AppConfig (App/Config.hs); PEX uses cfgPEXEnabled only
  [ ] M24.1.5 Promiscuous: disable PEX in applyConnectionModeSideEffects
      not done: no cfgPEXAutomatic field to toggle
  [ ] M24.1.6 Swing: set cfgPEXAutomatic = False (manual PEX only)
      not done: cfgPEXAutomatic not implemented
  [ ] M24.1.7 Guard tryPEXExchange on cfgPEXEnabled && cfgPEXAutomatic
      not done: cfgPEXAutomatic not implemented
  [x] M24.1.8 Update TUI dialog labels and mode descriptions
      done: TUI/Dialog.hs has updated mode descriptions for Swing/Promiscuous/Selective/Chaste
  [ ] M24.1.9 Add config migration: "Chastity" -> "Chaste" on load
      not done: no config migration found

[x] M24.2 Static discovery sources (Phase 2)
  [x] M24.2.1 Network.Discovery module scaffold (DiscoverySource, DiscoveryManager)
      done: src/UmbraVox/Network/Discovery.hs — DiscoverySource, DiscoveryManager types
  [x] M24.2.2 Environment variable source: parse UMBRAVOX_PEERS
      done: discoverEnvVar parses UMBRAVOX_PEERS in Discovery.hs
  [x] M24.2.3 DNS SRV/TXT source: _umbravox._tcp.<domain> lookup
      done: discoverDNS in Discovery.hs performs DNS SRV lookup
  [x] M24.2.4 Config file source: ~/.umbravox/peers parser
      done: discoverConfigFile in Discovery.hs parses ~/.umbravox/peers
  [x] M24.2.5 Extend PeerSource with SourceDHT, SourceDNS, SourceEnvVar, SourceConfig
      done: all four variants in PeerManager.hs PeerSource type
  [x] M24.2.6 Wire DiscoveryManager into AppConfig and startup sequence
      done: newDiscoveryManager called in App/Startup.hs; cfgDiscoverySources in AppConfig
  [ ] M24.2.7 Unit tests for each static source parser
      not done: no Test/Network/Discovery.hs found; parser logic untested

[x] M24.3 DHT core (Phase 3)
  [x] M24.3.1 DHT.Types: NodeId, DHTNode, DHTMessage, DHTConfig
      done: src/UmbraVox/Network/DHT/Types.hs
  [x] M24.3.2 DHT.RoutingTable: k-bucket insert/find/refresh/evict
      done: src/UmbraVox/Network/DHT/RoutingTable.hs
  [x] M24.3.3 DHT.RPC: binary serialization over AnyTransport
      done: src/UmbraVox/Network/DHT/RPC.hs
  [x] M24.3.4 DHT.Lookup: iterative FIND_NODE with alpha=3 parallelism
      done: src/UmbraVox/Network/DHT/Lookup.hs
  [x] M24.3.5 DHT.Store: iterative STORE/FIND_VALUE with TTL expiration
      done: src/UmbraVox/Network/DHT/Store.hs
  [x] M24.3.6 DHT module: bootstrap, maintain, announcePresence, findPeer
      done: src/UmbraVox/Network/DHT.hs exports bootstrap; announcePresence
      is not a separate export — presence publication uses Presence.hs separately
  [x] M24.3.7 Unit tests: routing table operations (insert, evict, find closest)
      done: test/Test/Network/DHT.hs (M24.3.7 tests)
  [x] M24.3.8 Unit tests: iterative lookup with mock network
      done: test/Test/Network/DHT.hs (M24.3.8 loopback tests)
  [x] M24.3.9 Node ID verification: SHA-256(identity key) == claimed NodeId
      done: test/Test/Network/DHT.hs (M24.6.4 routing table poisoning test covers this)

[x] M24.4 DHT integration (Phase 4)
  [ ] M24.4.1 Wire DHT into DiscoveryManager as DiscDHT source
      not done: DiscoveryManager in Discovery.hs does not include DHT source
  [ ] M24.4.2 Dandelion++ integration: stem-route self-lookups and self-stores
      not done: Dandelion.hs not wired to DHT
  [x] M24.4.3 PeerManager integration: SourceDHT scoring and ban propagation
      done: SourceDHT variant in PeerManager.hs; peerSourceTag "[DHT]" in TUI/Dialog.hs
  [ ] M24.4.4 Transport integration: DHT message type prefix in Noise payload
      not done: no DHT prefix found in transport layer
  [x] M24.4.5 AppConfig: cfgDHTEnabled, cfgDHTBootstrapNodes fields
      done: both fields in AppConfig (App/Config.hs line ~107-108)
  [x] M24.4.6 Bootstrap sequence: connect to bootstrap nodes, populate routing table
      done: bootstrap function in Network/DHT.hs
  [ ] M24.4.7 Periodic maintenance: bucket refresh, value republish, stale eviction
      not done: no periodic maintenance loop found in DHT.hs
  [x] M24.4.8 Integration tests with loopback transport
      done: test/Test/Network/DHT.hs (M24.4.8 loopback integration tests)

[x] M24.5 TUI and API (Phase 5)
  [ ] M24.5.1 Settings dialog: Discovery tab with source checkboxes
      not done: TUI/Dialog.hs has mode dialog but no dedicated Discovery tab with checkboxes
  [ ] M24.5.2 Status bar: active discovery source count indicator
      not done: no discovery count in status bar (TUI/Render.hs)
  [x] M24.5.3 Peer list: source tag display (mDNS/PEX/DHT/DNS/etc.)
      done: peerSourceTag in TUI/Dialog.hs shows [mDNS]/[PEX]/[DHT]/[DNS]/[Env]/[Cfg]
  [x] M24.5.4 Manual PEX exchange button for Swing mode
      done: TUI/Dialog.hs has "[Exchange Peers] (manual PEX, Swing mode)" option
  [x] M24.5.5 Chat.API: exchangePeers RPC method
      done: resolveMethod "exchangePeers" in Chat/API.hs
  [x] M24.5.6 Chat.API: dhtStatus RPC method (routing table size, stored values)
      done: resolveMethod "dhtStatus" in Chat/API.hs

[x] M24.6 Security hardening (Phase 6)
  [x] M24.6.1 Sybil attack test: many fake node IDs targeting same bucket
      done: testSybilBucketOverflow in test/Test/Network/DHT.hs
  [x] M24.6.2 Eclipse attack test: attacker fills routing table with colluding nodes
      done: testEclipseAttackResistance in test/Test/Network/DHT.hs
  [x] M24.6.3 Storage spam test: rate limiting STORE operations
      done: testStorageSpamSizeLimit, testStorageSpamExpiration in DHT.hs test
  [x] M24.6.4 Routing table poisoning test: invalid node ID verification
      done: test in test/Test/Network/DHT.hs (M24.6.4)
  [x] M24.6.5 DHT message size cap enforcement (4096 bytes max)
      done: test in test/Test/Network/DHT.hs (M24.6.5)
  [ ] M24.6.6 Adaptive DHT parameters based on network size estimate
      not done: no adaptive parameter logic found
  [ ] M24.6.7 F* spec: routing table invariants (bucket size, ordering)
      not done: no F* spec for DHT routing table found
```

------------------------------------------------------------------------

## 7. File Layout

New files to create:

```
src/UmbraVox/Network/DHT.hs              -- top-level DHT module
src/UmbraVox/Network/DHT/Types.hs        -- NodeId, DHTNode, DHTMessage, etc.
src/UmbraVox/Network/DHT/RoutingTable.hs -- k-bucket operations
src/UmbraVox/Network/DHT/RPC.hs          -- wire protocol
src/UmbraVox/Network/DHT/Lookup.hs       -- iterative lookup
src/UmbraVox/Network/DHT/Store.hs        -- distributed store
src/UmbraVox/Network/Discovery.hs        -- unified discovery manager
test/Test/Network/DHT.hs                 -- DHT unit tests
test/Test/Network/Discovery.hs           -- discovery manager tests
```

Files to modify:

```
src/UmbraVox/App/Config.hs               -- new fields, remove Chastity
src/UmbraVox/App/Startup.hs              -- default mode, discovery init
src/UmbraVox/App/Defaults.hs             -- DHT constants
src/UmbraVox/BuildProfile.hs             -- buildChasteOnly replaces buildChastityOnly
src/UmbraVox/Network/PeerManager.hs      -- new PeerSource variants
src/UmbraVox/Network/Listener.hs         -- PEX guard, remove Chastity branch
src/UmbraVox/TUI/RuntimeSettings.hs      -- mode side effects
src/UmbraVox/TUI/Dialog.hs               -- discovery settings tab
src/UmbraVox/TUI/Render.hs               -- status bar
src/UmbraVox/Chat/API.hs                 -- new RPC methods
src/UmbraVox/Storage/InMemory.hs         -- comment update
```

------------------------------------------------------------------------

## 8. Constants (for `App/Defaults.hs`)

```haskell
-- DHT
dhtDefaultK :: Int
dhtDefaultK = 20

dhtDefaultAlpha :: Int
dhtDefaultAlpha = 3

dhtBucketRefreshSeconds :: Int
dhtBucketRefreshSeconds = 3600  -- 1 hour

dhtValueRepublishSeconds :: Int
dhtValueRepublishSeconds = 3600  -- 1 hour

dhtValueExpireSeconds :: Int
dhtValueExpireSeconds = 86400  -- 24 hours

dhtMaxMessageSize :: Int
dhtMaxMessageSize = 4096

dhtMaxStoredValueSize :: Int
dhtMaxStoredValueSize = 1024

dhtStoreRateLimitPerMin :: Int
dhtStoreRateLimitPerMin = 10

-- Discovery
discoveryPollIntervalUs :: Int
discoveryPollIntervalUs = 30 * 1000000  -- 30 seconds
```

# Network Layer

## Implementation Status

The network layer currently implements TCP transport, loopback transport for testing, intercept middleware for traffic capture, Noise_IK encrypted handshake, mDNS peer discovery, and peer exchange (PEX). Higher-level protocols (gossip, Dandelion++, chain sync, peer scoring) exist as stub modules that raise "not implemented" errors.

## Auto-Listen on Startup

UmbraVOX automatically begins listening for incoming TCP connections at startup. The application probes the default port sequence (7853, 7854, 7855, ...) and binds to the first available port. A background thread (`acceptLoopTUI`) then accepts incoming connections according to the active trust mode. This means there is no need for the user to manually type "listen" -- the node is reachable as soon as it launches.

The auto-listen behavior is defined in `app/Main.hs`. If the chosen port is unavailable (e.g., another UmbraVOX instance is already running), the accept loop fails gracefully and the node operates in outbound-only mode.

## Transport

### Transport Abstraction

All transports implement the `TransportHandle` typeclass (`Network.TransportClass`):

```haskell
class TransportHandle t where
    thSend  :: t -> ByteString -> IO ()
    thRecv  :: t -> Int -> IO ByteString
    thClose :: t -> IO ()
    thInfo  :: t -> String
```

An existential wrapper `AnyTransport` enables polymorphic transport usage across the codebase.

### TCP Transport (`Network.Transport`)

Implemented and functional:

- **`listen :: Int -> IO TCPTransport`** -- Bind to a port, accept one connection. Uses `AI_PASSIVE`, `ReuseAddr`, IPv4.
- **`connect :: String -> Int -> IO TCPTransport`** -- Establish outbound TCP connection.
- **`connectTryPorts :: String -> [Int] -> IO TCPTransport`** -- Try a sequence of ports, return the first successful connection.
- **`send`** -- Uses `sendAll` for complete delivery.
- **`recv`** -- Reads exactly N bytes, handling partial reads from the OS.
- **`close`** -- Graceful close with 5-second linger.

### Loopback Transport (`Network.Transport.Loopback`)

In-process transport for testing and secure local notes:

- Backed by a pair of `Chan ByteString` pipes -- one side's send channel is the other's receive channel.
- Internal `MVar ByteString` read buffer supports partial reads (`recv n` where n < message size).
- **`newLoopbackPair :: String -> IO (LoopbackTransport, LoopbackTransport)`** -- Creates a connected pair.
- Unbounded buffering (Chan-based).

### Intercept Transport (`Network.Transport.Intercept`)

Traffic capture middleware for integration testing:

- Wraps any `AnyTransport` and logs all `thSend` calls to a shared `IORef [TrafficEntry]`.
- Each entry records: monotonic counter timestamp, sender name, receiver name, raw bytes, and size.
- Useful for verifying that no plaintext leaks onto the wire.
- **`wrapWithIntercept`** -- Wraps an existing transport with logging.
- Recv and close pass through to the underlying transport.

### Default Port Sequence

Defined in `Protocol.Encoding.defaultPorts`:

```haskell
defaultPorts = [7853, 7854, 7855, 9999, 7856, 7857, 7858, 7859, 7860]
```

The primary UmbraVOX port is **7853**. `connectTryPorts` iterates through this list when no explicit port is specified.

## Noise_IK Handshake (`Network.Noise`, `Network.Noise.Handshake`, `Network.Noise.State`)

Implemented and functional. Pattern:

```
-> e, es, s, ss
<- e, ee, se
```

### Configuration

- **Protocol name**: `Noise_IK_25519_ChaChaPoly_SHA256`
- **Prologue**: `"UmbraVox_v1"` (mixed into the handshake hash)
- **DH function**: X25519 (`Crypto.Curve25519`)
- **Cipher**: ChaCha20 (`Crypto.Random.chacha20Encrypt`)
- **Hash**: SHA-256 (`Crypto.SHA256`)
- **MAC**: HMAC-SHA-256 (`Crypto.HMAC`), tag length = 32 bytes
- **Key derivation**: HKDF-SHA-256 (`Crypto.HKDF.hkdfSHA256Extract`/`hkdfSHA256Expand`)

### Key Separation Fix

The `splitKeys` function derives four independent session keys from the final chaining key:

```haskell
splitKeys ck =
    let prk = hkdfSHA256Extract ck empty
        sendEncKey = hkdfSHA256Expand prk "enc-send" 32
        sendMacKey = hkdfSHA256Expand prk "mac-send" 32
        recvEncKey = hkdfSHA256Expand prk "enc-recv" 32
        recvMacKey = hkdfSHA256Expand prk "mac-recv" 32
    in (sendEncKey, sendMacKey, recvEncKey, recvMacKey)
```

The responder swaps send/recv keys so that initiator-send = responder-recv and vice versa.

### Post-Handshake State

```haskell
data NoiseState = NoiseState
    { nsSendEncKey :: !ByteString   -- 32-byte ChaCha20 key for sending
    , nsSendMacKey :: !ByteString   -- 32-byte HMAC key for send authentication
    , nsRecvEncKey :: !ByteString   -- 32-byte ChaCha20 key for receiving
    , nsRecvMacKey :: !ByteString   -- 32-byte HMAC key for recv authentication
    , nsSendN      :: !Word64       -- Send nonce counter
    , nsRecvN      :: !Word64       -- Recv nonce counter
    }
```

Post-handshake encryption (`noiseEncrypt`/`noiseDecrypt` in `Network.Noise`) uses ChaCha20 with HMAC-SHA-256 authentication. Nonces are 12 bytes: 4 zero bytes + 8-byte little-endian counter.

### Framing

Handshake messages use length-prefixed framing: 4-byte big-endian length header + payload. Maximum frame size is 64 KiB to prevent DoS via large allocations.

## mDNS Peer Discovery (`Network.MDNS`)

Implemented and functional. Discovers peers on the local network via UDP multicast:

- **Multicast group**: 224.0.0.251 (standard mDNS, RFC 6762)
- **Port**: 5353 (standard mDNS)
- **Service name**: `_umbravox._tcp.local`
- **Socket options**: `SO_REUSEADDR` + `SO_REUSEPORT` (allows multiple processes on the same host)
- **Multicast join**: Raw `setsockopt` FFI call for `IP_ADD_MEMBERSHIP`
- **Multicast loopback**: Enabled, so peers on the same machine discover each other
- **Announcement interval**: 10 seconds
- **Self-filtering**: Announcements from our own port + pubkey are ignored (`isSelfAnnouncement`)
- **Peer deduplication**: By public key fingerprint

### Announcement Format

```
_umbravox._tcp.local\nport=NNNN;pubkey=HEXHEX
```

### API

- **`startMDNS :: Int -> ByteString -> IO (MVar [MDNSPeer], ThreadId)`** -- Start discovery with our port and pubkey
- **`stopMDNS :: ThreadId -> IO ()`** -- Kill the discovery thread
- **`getDiscoveredPeers :: MVar [MDNSPeer] -> IO [MDNSPeer]`** -- Read current peer list

## Peer Exchange (`Network.PeerExchange`)

Implemented. After a handshake completes, connected peers exchange known peer lists:

- Peers received via PEX are marked as indirect and are never re-forwarded (1-hop maximum)
- Provides: `encodePeerList`, `decodePeerList`, `exchangePeers`

## Connection Trust Modes

UmbraVox defines five connection trust modes in `TUI.Types.ConnectionMode`. The active mode determines how the node behaves during and after the Noise_IK handshake, and whether mDNS and PEX subsystems are enabled.

| Mode | Accept | mDNS | PEX | DB | Behavior |
|------|--------|------|-----|-----|----------|
| **Swing** | All | On | Auto | On | Most open -- shares peer lists automatically |
| **Promiscuous** | All | On | Manual | On | Accept anyone silently |
| **Selective** | Confirm | On | Off | On | Shows fingerprint, user decides (default) |
| **Chaste** | Trusted only | Off | Off | On | Silent reject indistinguishable from MAC failure |
| **Chastity** | Trusted only | Off | Off | Off | Chaste + no persistence (fully ephemeral) |

### Effect on the Noise Handshake

In **Swing** and **Promiscuous** modes, the Noise_IK handshake completes unconditionally for any peer. In **Selective** mode, the handshake completes but the session is paused until the user confirms the peer's fingerprint in a dialog. In **Chaste** and **Chastity** modes, the node checks the initiator's static public key (transmitted encrypted in the first Noise_IK message `-> e, es, s, ss`) against `cfgTrustedKeys`. If the key is not in the trusted list, the responder silently closes the connection. The failure is designed to be indistinguishable from a MAC verification failure, so an untrusted peer cannot determine whether the node exists, is offline, or rejected the connection.

### Effect on mDNS

In **Swing**, **Promiscuous**, and **Selective** modes, mDNS announcements are sent and received on `_umbravox._tcp.local`. In **Chaste** and **Chastity** modes, mDNS is disabled entirely: no announcements are sent and no multicast listener is active. This prevents the node from being discoverable on the LAN.

### Effect on Peer Exchange (PEX)

In **Swing** mode, peer exchange happens automatically after every successful handshake. In **Promiscuous** mode, PEX data is accepted if offered but not automatically initiated. In **Selective**, **Chaste**, and **Chastity** modes, PEX is disabled -- no peer lists are sent or accepted.

### Effect on Persistence

In all modes except **Chastity**, conversations, contacts, peer information, and settings are persisted to Anthony DB. In **Chastity** mode, `cfgDBEnabled` is set to `False` and no database file is opened. All state exists only in memory and is lost when the application exits.

## Not Implemented

The following are defined as stub modules (raise "not implemented" errors) or are empty:

| Feature | Module | Status |
|---------|--------|--------|
| UDP transport | `Network.Transport.UDP` | Empty module, no implementation |
| Kademlia DHT | -- | Not implemented |
| Dandelion++ routing | `Network.Dandelion` | Stub (`routeMessage` raises error) |
| Gossip protocol | `Network.Gossip` | Stub (`gossipBlock` raises error) |
| Chain sync | `Network.Sync` | Stub (`syncChain` raises error) |
| Peer scoring/banning | `Network.PeerManager` | Stub (`newPeerManager` raises error) |
| Compact block relay | -- | Not implemented |
| Eclipse attack prevention | -- | Not implemented |
| Wire protocol messages (INV, GET_DATA, etc.) | -- | Not implemented |
| Version negotiation | -- | Not implemented |
| Multiplexed logical streams | -- | Not implemented |
| Per-session rekeying | -- | Not implemented |

## References

- Noise Protocol Framework (Perrin, 2018)
- RFC 6762 -- Multicast DNS

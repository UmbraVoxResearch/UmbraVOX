# UmbraVox: System Overview

UmbraVox is a peer-to-peer encrypted messaging system. Each participant runs a full node that communicates directly over TCP with Noise_IK transport encryption. Messages are protected by the Signal Double Ratchet protocol with a post-quantum outer wrapper using ML-KEM-768. Peers discover each other via mDNS on local networks and peer exchange on first connection.

The long-term design envisions a blockchain-based message transport layer with Ouroboros Praos consensus, Dandelion++ IP obfuscation, and 11-day chain truncation for protocol-level ephemeral messaging. These components are specified in the design documents but not yet implemented in code. The current MVP focuses on direct peer-to-peer encrypted chat.

The current verification strategy for the messaging MVP is a tiered hardening harness:

- **Core**: deterministic crypto, protocol, TUI simulation, codegen, and loopback messaging scenarios
- **TCP**: real localhost TCP end-to-end scenarios
- **Fault**: adversarial parser/transport/fault-injection scenarios
- **Recovery**: persistence and restart-oriented scenarios
- **Deferred**: preserved blockchain, storage, consensus, and economics stub suites outside the MVP fast gate
- **Soak**: longer stress scenarios, kept separate from the required gate

## Current Implementation

The following subsystems are functional:

- **Cryptographic primitives**: SHA-256, SHA-512, HMAC, HKDF, AES-256, AES-GCM, X25519, Ed25519, Keccak/SHA-3, ML-KEM-768, Poly1305, ChaCha20, VRF. All hand-implemented in pure Haskell using only GHC boot libraries.
- **Signal protocol**: X3DH key agreement, PQXDH (post-quantum extended X3DH with ML-KEM-768), Double Ratchet, Sender Keys, Session management.
- **Post-quantum wrapper**: AES-256-GCM encryption with ML-KEM-768-derived keys applied over Signal ciphertext.
- **Transport security**: Noise_IK handshake (X25519 + ChaChaPoly1305) for authenticated encrypted TCP connections.
- **Peer discovery**: mDNS/DNS-SD for LAN auto-discovery, peer exchange protocol for transitive peer sharing.
- **Persistence**: Anthony DB for storing peers, settings, conversations, and message history across restarts.
- **Identity management**: BIP39-based encrypted identity export/import, stealth addresses (DKSAP), QR code safety numbers.
- **Terminal UI**: Split-pane interface with F1-F5 menu bar (File, Contacts, Chat, Prefs, Help), dialog system, SIGWINCH resize handling.
- **Codegen pipeline**: 10 `.spec` files drive generation of pure Haskell modules, constant-time C source, and FFI bindings.
- **Formal verification**: 17 F* specifications covering all cryptographic primitives and key protocols, all verified.

The following subsystems exist as stubs or type definitions only:

- **Consensus**: Types, Block, Ledger, Protocol, LeaderElection, Validation, ForkChoice, Nonce, Truncation, Mempool. Type signatures and data structures are defined but implementations are placeholders.
- **Economics**: Token, Fees, Rewards, Penalty, Cycle, Onboarding. Stub implementations only.
- **Advanced networking**: Gossip, Dandelion++, chain sync, Kademlia DHT. Not yet implemented.
- **Additional transports**: UDP, Signal server relay, XMPP, Discord, Matrix, Blockchain. Stub backends in the transport abstraction.

## Encryption Pipeline (per message)

```
Plaintext
  |-> Signal Double Ratchet encrypt (AES-256-GCM + HMAC-SHA256)
  |-> PQ Outer Wrapper encrypt (ML-KEM-768 derived key -> AES-256-GCM)
  |-> Noise_IK transport encryption (X25519 + ChaChaPoly1305)
  |-> TCP transmission
```

## Architecture at a Glance (current)

```
Terminal UI (split-pane, F1-F5 menus, dialogs)
    |
Chat Session (encrypt/decrypt, contact management)
    |
Signal Protocol (PQXDH key agreement, Double Ratchet)
    |
PQ Wrapper (ML-KEM-768 outer encryption layer)
    |
Transport Layer (Noise_IK over TCP, mDNS discovery)
    |
Persistence (Anthony DB: peers, conversations, messages)
```

## Architecture at a Glance (design target)

```
Chat API (JSON-RPC over WebSocket)
    |
Crypto Engine (Signal sessions, PQ wrapper, key management)
    |
Mempool (validate, prioritize, evict)
    |
Consensus Engine (slot clock, VRF leader election, block production/validation)
    |
Chain Storage (append-only blocks, state DB, indexes)
    |
Network Layer (TCP transport, Noise encryption, gossip, Dandelion++)
```

## Connection Trust Modes

UmbraVox provides five connection trust modes, ordered from most open to most locked down. The active mode controls how the node handles incoming connections, mDNS discovery, peer exchange (PEX), and database persistence.

| Mode | Accept | mDNS | PEX | DB | Behavior |
|------|--------|------|-----|-----|----------|
| **Swing** | All | On | Auto | On | Most open -- shares peer lists automatically |
| **Promiscuous** | All | On | Manual | On | Accept anyone silently |
| **Selective** | Confirm | On | Off | On | Shows fingerprint, user decides (default) |
| **Chaste** | Trusted only | Off | Off | On | Silent reject indistinguishable from MAC failure |
| **Chastity** | Trusted only | Off | Off | Off | Chaste + no persistence (fully ephemeral) |

- **Swing** is intended for open social use where maximum connectivity is desired. Peer lists are exchanged automatically after each handshake.
- **Promiscuous** accepts all inbound connections without user confirmation but does not auto-share peer lists.
- **Selective** (the default) presents the remote peer's fingerprint and asks the user to accept or reject. mDNS is active for LAN discovery but PEX is disabled.
- **Chaste** only allows connections from peers whose public keys are in the trusted keys list. Untrusted connections are silently dropped in a way that is indistinguishable from a MAC verification failure, providing no information to the rejected peer.
- **Chastity** adds full ephemerality on top of Chaste: the database is disabled, so no conversations, contacts, or keys are written to disk.

The mode is configured in the TUI via F4 (Prefs) -> Settings, and is stored in `AppConfig.cfgConnectionMode`. The `ConnectionMode` enum is defined in `TUI.Types`.

## Core Design Principles

- **No central servers**: Every participant runs a full node
- **Defense in depth**: Signal Protocol + post-quantum wrapper + Noise_IK transport
- **Deniability**: No asymmetric signatures on message content
- **No external crypto libraries**: Pure Haskell reference implementations using only GHC boot libraries, with FFI to constant-time C planned for production
- **Formal verification**: F* specifications for all cryptographic primitives

## Test Coverage

The project uses a tiered hardening harness covering:

- NIST/RFC known-answer test vectors for all cryptographic primitives
- Signal protocol round-trip and session management tests
- Network transport and Noise handshake tests
- Real TCP chat scenarios
- Fault injection, malformed input, replay, truncation, and disconnect scenarios
- Persistence and restart-oriented scenarios
- TUI simulation tests (menu navigation, dialog workflows, keyboard input)
- Storage round-trip tests
- Integration and end-to-end encrypted messaging tests
- Fuzz and security tests

Consensus, economics, and blockchain-facing modules remain present as stubs/design targets and are intentionally outside the messaging-MVP hardening gate.

## F* Formal Specifications (17 total, all verified)

| Specification | Standard |
|---------------|----------|
| Spec.SHA256.fst | FIPS 180-4 |
| Spec.SHA512.fst | FIPS 180-4 |
| Spec.HMAC.fst | RFC 2104/4231 |
| Spec.HKDF.fst | RFC 5869 |
| Spec.AES256.fst | FIPS 197 |
| Spec.GaloisField.fst | SP 800-38D |
| Spec.GCM.fst | SP 800-38D |
| Spec.ChaCha20.fst | RFC 8439 |
| Spec.X25519.fst | RFC 7748 |
| Spec.Ed25519.fst | RFC 8032 |
| Spec.Keccak.fst | FIPS 202 |
| Spec.MLKEM768.fst | FIPS 203 |
| Spec.Poly1305.fst | RFC 8439 |
| Spec.DoubleRatchet.fst | Signal Protocol |
| Spec.X3DH.fst | Signal Protocol |
| Spec.PQXDH.fst | Signal Protocol + FIPS 203 |
| Spec.NoiseIK.fst | Noise Protocol Framework |

## Known Limitations (current)

- **Not constant-time**: Pure Haskell implementations are not constant-time due to GHC lazy evaluation and garbage collection. C FFI backends are planned.
- **No key zeroing**: Secret keys are held in ordinary heap ByteStrings with no explicit zeroing on deallocation.
- **CSPRNG thread safety**: The global CSPRNG uses a plain IORef, which is not safe under concurrent access.
- **No independent audit**: Cryptographic implementations have not been reviewed by an independent third party.

## Documentation Index

- [Quickstart Guide](QUICKSTART.md)
- [Language and Project Structure](02-language-and-structure.md)
- [Cryptographic Architecture](03-cryptography.md)
- [Consensus Mechanism](04-consensus.md) *(design only)*
- [Chain Truncation](05-truncation.md) *(design only)*
- [Token Economics](06-economics.md) *(design only)*
- [Message Format](07-message-format.md)
- [Dandelion++ IP Obfuscation](08-dandelion.md) *(design only)*
- [P2P Network Layer](09-network.md)
- [Security Model](10-security.md)
- [Node Architecture](11-node-architecture.md)
- [Development Phases](12-development-phases.md)
- [Module Map](ARCHITECTURE.md)

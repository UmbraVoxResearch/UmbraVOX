# UmbraVox

A decentralized, post-quantum encrypted messaging system.

UmbraVox is a peer-to-peer encrypted chat application where each participant runs a full node. It combines end-to-end encryption using the Signal protocol with a post-quantum protective layer (ML-KEM-768), Noise_IK transport security, and mDNS peer discovery on local networks.

## Current Status

**MVP functional.** Two clients can exchange post-quantum encrypted messages over TCP with Noise_IK transport security and Signal Double Ratchet + ML-KEM-768 encryption.

| Metric | Value |
|--------|-------|
| Tests | 1,082 across 70+ test modules |
| F* formal specifications | 17 (all verified) |
| Codegen spec files | 10 generating 30 Haskell + C + FFI outputs |
| Transport backends | TCP, Loopback (functional); UDP, Signal, XMPP, Discord, Matrix, Blockchain (stubs) |

### Implemented Features

- **TUI**: Split-pane terminal interface with F1-F5 menu system (File, Contacts, Chat, Prefs, Help)
- **E2E encryption**: Signal Double Ratchet with PQXDH key agreement (X3DH + ML-KEM-768)
- **Post-quantum wrapper**: AES-256-GCM with ML-KEM-768-derived keys over Signal ciphertext
- **Transport security**: Noise_IK handshake (X25519 + ChaChaPoly1305)
- **Discovery**: mDNS/DNS-SD LAN auto-discovery + peer exchange protocol
- **Persistence**: Anthony DB for peers, settings, conversations, and message history
- **Identity export**: Encrypted identity exports with BIP39 passphrases
- **Modular transport**: Pluggable transport abstraction (`TransportClass`) with multiple backends
- **Codegen pipeline**: 10 `.spec` files drive generation of pure Haskell, constant-time C, and FFI bindings
- **Formal verification**: 17 F* specifications covering all cryptographic primitives and protocols

### Key Modules

```
src/UmbraVox/
  Crypto/          Cryptographic primitives (SHA-256/512, AES-GCM, X25519,
                   Ed25519, ML-KEM-768, Keccak/SHA-3, Poly1305, HKDF, VRF)
  Crypto/Signal/   Signal protocol (X3DH, PQXDH, Double Ratchet, SenderKeys, Session)
  Network/         Transport (TCP, Loopback, stubs), Noise_IK, MDNS, PeerExchange,
                   PeerManager, Gossip, Dandelion, Protocol, Sync
  Chat/            Session management, Wire format, Message, Contacts, API
  Storage/         Anthony DB, ChainDB, StateDB, Index, Checkpoint, Schema
  Protocol/        Encoding, CBOR framing, QRCode, MessageFormat, WireFormat
  TUI/             Terminal UI (Types, Constants, Terminal, Layout, Render,
                   Dialog, Menu, Input, Actions, Handshake)
  Economics/       Token model, Fees, Rewards, Penalty, Cycle, Onboarding (stubs)
  Consensus/       Block, Ledger, Protocol, LeaderElection, Validation,
                   ForkChoice, Nonce, Truncation, Mempool (stubs)
  Tools/           Complexity checker, F* verifier, Reference fetcher
```

## Build Instructions

UmbraVox builds with GHC and Cabal inside a Nix shell. Only GHC boot libraries are used as dependencies (no third-party crypto libraries).

```bash
# Enter the development environment
nix-shell

# Build the project
cabal build

# Run the test suite
cabal test

# Run the application
cabal run umbravox
```

Additional build targets:

```bash
# Run F* formal verification (requires F* and Z3)
make verify

# Check cyclomatic complexity
make complexity

# Run all quality gates
make quality
```

## Encryption Pipeline

Each message passes through multiple encryption layers:

```
Plaintext
  |-> Signal Double Ratchet encrypt (AES-256-GCM + HMAC-SHA256)
  |-> PQ Outer Wrapper encrypt (ML-KEM-768 derived key -> AES-256-GCM)
  |-> Noise_IK transport encryption (X25519 + ChaChaPoly1305)
  |-> TCP transmission
```

## Expression and Legal Notice

This repository, and any related UmbraVox manuscripts, drafts, specifications, commentary, examples, proofs, or source text, are intended as expressive works. They may include political, philosophical, scientific, mathematical, literary, and technical speech.

Source code, algorithms, formal descriptions, pseudocode, protocol specifications, and implementation notes may also constitute protected expression in some jurisdictions. However, laws regulating cryptographic software, privacy technologies, export controls, publication, compilation, possession, distribution, importation, and operational deployment differ across countries and legal regimes.

Accordingly:

- this repository is provided for informational, research, educational, and expressive purposes
- nothing in this repository constitutes legal advice
- any person who compiles, ports, modifies, distributes, exports, deploys, or operates software derived from this work is solely responsible for compliance with applicable law
- the authors, contributors, and publishers make no representation that any implementation or use of this work is lawful in every jurisdiction

If you reduce any portion of this work to practice in executable form, distribute binaries, provide hosted services, or operate derived systems in the real world, you are responsible for determining and satisfying all legal, regulatory, licensing, and compliance obligations that apply where you live or operate.

## Security Notice

**THIS SOFTWARE HAS NOT BEEN INDEPENDENTLY PEER-REVIEWED OR AUDITED.**
The cryptographic implementations, while tested against NIST/RFC vectors
and formally specified in F*, have not been reviewed by an independent
third party. This software **SHOULD NOT BE TRUSTED** for protecting sensitive
communications until an independent security audit has been completed.

Use at your own risk. See [LEGAL-NOTICE.md](LEGAL-NOTICE.md) for full terms.

## Documentation

All major design documentation lives in [`doc/`](doc/):

| Document | Description |
|----------|-------------|
| [01 - Overview](doc/01-overview.md) | System overview and architecture |
| [02 - Language & Structure](doc/02-language-and-structure.md) | Haskell, project layout, dependency policy |
| [03 - Cryptography](doc/03-cryptography.md) | Primitives, Signal protocol, post-quantum wrapper |
| [04 - Consensus](doc/04-consensus.md) | Ouroboros Praos adaptation (design, not yet implemented) |
| [05 - Truncation](doc/05-truncation.md) | 11-day cycle design (not yet implemented) |
| [06 - Economics](doc/06-economics.md) | Token economics design (stubs only) |
| [07 - Message Format](doc/07-message-format.md) | Message block layout, CBOR serialization |
| [08 - Dandelion++](doc/08-dandelion.md) | IP obfuscation design (not yet implemented) |
| [09 - Network](doc/09-network.md) | P2P topology, transport security, discovery |
| [10 - Security](doc/10-security.md) | Threat model, adversary classes, deniability |
| [11 - Node Architecture](doc/11-node-architecture.md) | Concurrency, storage layout, resource estimates |
| [12 - Development Phases](doc/12-development-phases.md) | Development roadmap and verification strategy |
| [ARCHITECTURE](doc/ARCHITECTURE.md) | Module map and codebase structure |

## Known Limitations

- Pure Haskell crypto implementations are not constant-time (GHC lazy evaluation, GC). C FFI backends are planned but not yet implemented.
- No key material zeroing in Haskell (GC limitation). `SecureByteString` with FFI `explicit_bzero` is planned.
- Consensus, blockchain transport, and token economics modules are stubs/design only.
- The CSPRNG (`globalCSPRNG`) uses a plain `IORef` and is not thread-safe.
- No independent security audit has been performed.

## Future Development

- Constant-time C FFI backends for all cryptographic primitives
- Full consensus implementation (Ouroboros Praos adaptation)
- On-chain message transport with Dandelion++ IP obfuscation
- Independent third-party security audit
- FIPS 140-3 validated encryption modules

## Compliance Advisory

This project implements strong cryptography and decentralized communications
technology. Operators deploying this software must independently assess
compliance with applicable laws in their jurisdiction, including but not
limited to cryptography export controls, telecommunications regulations,
lawful intercept requirements, and data retention obligations.

See [LEGAL-NOTICE.md](LEGAL-NOTICE.md) for detailed legal information
including export controls, telecommunications, data retention, and token
economics notices. See [PUBLISHING-NOTE.md](PUBLISHING-NOTE.md) for the
expressive and research framing of this work.

## License

See [LICENSE](LICENSE).

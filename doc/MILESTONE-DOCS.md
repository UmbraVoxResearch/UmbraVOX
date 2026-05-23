# Milestone Documentation Index

Per-milestone documentation requirements (M26.3). Each section lists what was
implemented, where the code lives, where the tests are, relevant design docs,
and security audit findings.

Last updated: 2026-05-22

---

## M20: Codebase Quality + Architectural Improvements (v0.2.1)

### What was implemented
- M20.1: Eliminated unsafePerformIO globals, broke Network-TUI dependency via
  ListenerCallbacks, moved Bridge/Signal/Main.hs to app-signal-bridge/.
- M20.2: 11 `*Safe` crypto variants added, 24 stub modules documented with
  error handling hardening.
- M20.3: Consensus/economics stubs gated behind cabal flag, transport stubs
  documented, all test files wired, 7 attic docs archived.
- M20.4 (partial): 36 M-series TODOs audited, VM agent trust boundary hardened,
  IPC decode hardened with 64 KiB guard + telemetry, config hash pinning
  fail-closed.
- M20.5 (partial): All Makefile test targets VM-default, flake.nix VM-only
  devShell, smoke/dev boot split, VM cache/output policy.
- M20.6: ED-008e universal completeness PROVED (62 Qed), 475 Qed total across
  14 Coq files.
- M20.7: run_named_suite macro, consolidated gate wrappers, release packaging
  shared library, VM SSH/QEMU helper library.

### Key files
- `src/UmbraVox/Network/Listener.hs` (ListenerCallbacks decoupling)
- `app-signal-bridge/Main.hs` (extracted bridge entrypoint)
- `src/UmbraVox/Crypto/*.hs` (*Safe variants across all crypto modules)
- `src/UmbraVox/Consensus/` (gated stubs)
- `src/UmbraVox/Economics/` (gated stubs)
- `Makefile` (VM-default targets, run_named_suite macro)
- `flake.nix` (VM-only devShell)
- `coq/` (Coq proof files, 14 files, 475 Qed)

### Tests
- `test/Test/Hardening/` (Fault.hs, Recovery.hs, Soak.hs, TCP.hs)
- `test/Test/App/` (RuntimeLog tests)
- `test/Test/Storage/` (Encryption tests)
- Infrastructure tests: `make test-infra`

### Design docs
- `doc/VM-DEVELOPMENT.md`
- `doc/VM-ISOLATION.md`

### Security audit findings
- 36 M-series TODOs audited (M20.4.2)
- VM agent trust boundary hardened (M20.4.3)
- IPC decode hardened with 64 KiB guard (M20.4.4)
- Config hash pinning fail-closed (M20.4.6)

---

## M21: Stub Implementation + Codegen Pipeline (v0.3.0)

### What was implemented
- M21.1: VRF ECVRF-ED25519-SHA512, PQWrapper ML-KEM-768+AES-256-GCM,
  Signal.Session state management.
- M21.2: MessageFormat 1024-byte blocks with PKCS7 padding, WireFormat network
  envelope with routing metadata.
- M21.3: JSON-RPC 2.0 API, PeerManager scoring/banning, Dandelion++ stem/fluff
  privacy routing, P2P wire protocol.
- M21.4: All 18 .spec files generate valid C, FFI wrappers wired, differential
  testing (C vs Haskell), 7 F* specs added.

### Key files
- `src/UmbraVox/Crypto/VRF.hs` (ECVRF-ED25519-SHA512)
- `src/UmbraVox/Crypto/PQWrapper.hs` (ML-KEM-768 + AES-256-GCM)
- `src/UmbraVox/Crypto/Signal/` (Signal.Session state management)
- `src/UmbraVox/Protocol/MessageFormat.hs` (1024-byte block format)
- `src/UmbraVox/Protocol/WireFormat.hs` (network envelope)
- `src/UmbraVox/Chat/API.hs` (JSON-RPC 2.0)
- `src/UmbraVox/Network/PeerManager.hs` (peer scoring/banning)
- `src/UmbraVox/Network/Dandelion.hs` (Dandelion++ routing)
- `src/UmbraVox/Network/Protocol.hs` (P2P wire protocol)
- `codegen/Specs/` (18 .spec files)
- `csrc/generated/` (18 generated C files)
- `codegen/Main.hs`, `codegen/CryptoGen.hs`, `codegen/TestGen.hs` (codegen pipeline)

### Tests
- `test/Test/Crypto/VRF.hs`
- `test/Test/Crypto/PQWrapper.hs`
- `test/Test/Protocol/MessageFormat.hs`
- `test/Test/Protocol/WireFormat.hs`
- `test/Test/Network/PeerManager.hs`
- `test/Test/Network/Dandelion.hs`
- `test/Test/Network/Protocol.hs`
- `test/Test/Crypto/Differential/` (C vs Haskell differential tests)
- `test/Test/Codegen.hs`

### Design docs
- `doc/transport-providers.md`
- `doc/CRYPTO-SAFETY.md` (nonce constructions, HKDF domain strings)

### Security audit findings
- None specific to M21 (pre-audit milestone); subsequent audits (M23, M27)
  covered crypto and protocol correctness.

---

## M23: Protocol Security Audit (v0.3.1)

### What was implemented
- All 32 protocol findings resolved (11 critical/high, 12 medium, 9 low).
- All 15 crypto implementation findings resolved.
- Encrypted envelope with stealth addressing (cleartext source/dest IDs removed).
- RouteToken HKDF with channel + identity binding.
- Envelope AEAD: ChaCha20-Poly1305 for data, HMAC for handshake.
- Dandelion opaque forwarding (relay swaps token, never decrypts inner envelope).
- DoS mitigations: PoW challenge + rate limiting.
- MitM protections: channel binding, identity binding, key confirmation MAC.
- Token rotation: counter(100) + wall-clock(600s).
- Crypto fixes: Ed25519 cofactored verify, X25519 constant-time cswap, VRF sign
  convention + subgroup validation, MLKEM overflow guards, GCM nonce-reuse
  detection.

### Key files
- `src/UmbraVox/Protocol/WireFormat.hs` (v1 stealth addressing rewrite)
- `src/UmbraVox/Protocol/RouteToken.hs` (HKDF session routing tokens)
- `src/UmbraVox/Protocol/Handshake.hs` (PQXDH integration)
- `src/UmbraVox/Protocol/ProofOfWork.hs` (PoW challenge)
- `src/UmbraVox/Crypto/StealthAddress.hs` (stealth scan)
- `src/UmbraVox/Crypto/Ed25519.hs` (cofactored verify)
- `src/UmbraVox/Crypto/Curve25519.hs` (constant-time cswap)
- `src/UmbraVox/Crypto/VRF.hs` (sign convention, subgroup validation)
- `src/UmbraVox/Crypto/MLKEM.hs` (overflow guards)
- `src/UmbraVox/Crypto/GCM.hs` (nonce-reuse detection)
- `src/UmbraVox/Network/Dandelion.hs` (opaque forwarding)
- `src/UmbraVox/Network/RateLimit.hs` (rate limiting)

### Tests
- `test/Test/Protocol/RouteToken.hs`
- `test/Test/Protocol/WireFormat.hs`
- `test/Test/Network/DoSMitigation.hs`
- `test/Test/Crypto/StealthAddress.hs`
- `test/Test/Security/` (regression suites: RegressionM7, RegressionM8, etc.)

### Design docs
- `doc/PROTOCOL-AUDIT-v0.3.0.md` (protocol audit report)
- `doc/CRYPTO-AUDIT-v0.3.0.md` (cryptographic audit report)
- `doc/ENCRYPTED-ENVELOPE-DESIGN.md` (stealth addressing design)

### Security audit findings
- 32 protocol findings (11 critical/high, 12 medium, 9 low) -- all resolved
- 15 crypto implementation findings -- all resolved
- Full details in `doc/PROTOCOL-AUDIT-v0.3.0.md` and `doc/CRYPTO-AUDIT-v0.3.0.md`

---

## M24: DHT Network + Unified Discovery (v0.4.0)

### What was implemented
- Kademlia DHT: core types, routing table, RPC, iterative lookup, store with TTL.
- Static discovery: env var, DNS SRV/TXT, config file sources.
- Security model refactor: Chaste/Selective/Promiscuous/Swing modes.
- DHT integration: transport prefix, bootstrap, Dandelion++ stem-route,
  PeerManager scoring.
- TUI: discovery settings, peer source tags, manual PEX, dhtStatus/exchangePeers
  API.
- Security hardening: Sybil/eclipse/spam tests, node ID verification, adaptive
  parameters, F* routing table spec.

### Key files
- `src/UmbraVox/Network/DHT.hs` (DHT entrypoint)
- `src/UmbraVox/Network/DHT/Types.hs` (core DHT types, NodeId, KBucket)
- `src/UmbraVox/Network/DHT/RoutingTable.hs` (k-bucket routing table)
- `src/UmbraVox/Network/DHT/RPC.hs` (DHT RPC: PING, STORE, FIND_NODE, FIND_VALUE)
- `src/UmbraVox/Network/DHT/Lookup.hs` (iterative lookup)
- `src/UmbraVox/Network/DHT/Store.hs` (key-value store with TTL)
- `src/UmbraVox/Network/Discovery.hs` (unified discovery: env, DNS, config)
- `src/UmbraVox/Network/PeerManager.hs` (scoring, banning, source tags)
- `src/UmbraVox/Network/PeerExchange.hs` (PEX)
- `src/UmbraVox/Network/Presence.hs` (DHT presence)

### Tests
- `test/Test/Network/DHT.hs` (core DHT tests)
- `test/Test/Network/PeerManager.hs`
- `test/Test/Network/PeerExchange.hs`
- `test/Test/Security/Adversarial.hs` (Sybil/eclipse/spam tests)

### Design docs
- `doc/DHT-NETWORK-PLAN.md` (DHT design and implementation plan)

### Security audit findings
- Sybil resistance: node ID verification via HKDF binding.
- Eclipse attack mitigation: adaptive routing table parameters.
- Spam prevention: rate limiting + PoW challenge on DHT operations.

---

## M25: UmbraClaw Plugin (v0.5.0+)

### What was implemented
- Plugin scaffold: manifest, IPC subprocess, session management, executable.
- Protocol integration: authentication flow, message send/receive, contact sync,
  TUI indicators.
- Testing: IPC smoke test, wire-compatibility tests.

### Key files
- `src/UmbraVox/Bridge/UmbraClaw/Main.hs` (plugin main entrypoint)
- `src/UmbraVox/Bridge/UmbraClaw/Session.hs` (session management)
- `src/UmbraVox/Plugin/Registry.hs` (plugin registry)
- `src/UmbraVox/Plugin/Types.hs` (plugin types)
- `plugins/umbraclaw/manifest.uvx` (plugin manifest)
- `src/UmbraVox/Network/ProviderRuntime.hs` (IPC provider runtime)
- `src/UmbraVox/Network/ProviderCatalog.hs` (provider catalog)

### Tests
- `test/Test/Plugin/Registry.hs` (plugin registry tests)
- `test/Test/Plugin/EphemeralIntegration.hs` (IPC smoke test)

### Design docs
- `doc/PLUGIN-AUTHORING.md` (plugin authoring guide)
- `doc/transport-providers.md` (transport provider architecture)
- `plugins/README.md` (plugin directory overview)

### Security audit findings
- Plugin signed manifests (M27.6 finding, fixed)
- IPC chunked reading hardening (M27.6 finding, fixed)

---

## M26: Documentation Overhaul

### What was implemented
- M26.1: Stale docs moved to `doc/attic/`, `doc/README.md` and
  `doc/ARCHITECTURE.md` updated, Haddock generation via `make docs`.
- M26.2 (partial): Module dependency graph, API docs, plugin docs, test coverage
  report -- pending.
- M26.3: Per-milestone documentation requirements -- this file.

### Key files
- `doc/README.md` (documentation index)
- `doc/ARCHITECTURE.md` (active runtime and module layout)
- `doc/attic/` (archived stale docs)
- `doc/MILESTONE-DOCS.md` (this file)

### Tests
- N/A (documentation milestone)

### Design docs
- All docs under `doc/` are the output of this milestone.

### Security audit findings
- None (documentation-only milestone).

---

## M27: Security Review v0.4.1 (72 findings, 48 fixed)

### What was implemented
- 72 findings from 5 specialized review agents (privacy, crypto, security arch,
  build, supply chain). 48 fixed as of last update.
- M27.1 Critical (3/3): mDNS ephemeral rotating IDs, pure-haskell-crypto default
  False + compile guard, VM exec heredoc quoting.
- M27.2 High privacy (5/5): message padding buckets, DHT NodeId HKDF with
  ephemeral salt, PEX opaque session tokens, PQXDH Noise IK wrapping, truncated
  fingerprint confirmation.
- M27.3 High crypto (4/4): GCM input length cap, CSPRNG reseed guard,
  StealthAddress subgroup check, WireFormat 64-bit sequence counter.
- M27.4 High security (4/4): hand-rolled SQLite3 FFI, selective TOFU key
  pinning, Noise nonce overflow check, legacy plaintext rejection.
- M27.5 High build + supply chain (5/6): cc-options hardening, VM firewall
  default, /work tmpfs 0700, VM base image SHA-256 pins, curl-pipe-sh removal.
  Remaining: nixpkgs stable pin (HI-016).
- M27.6 Medium (25/25): Show redaction, route token rotation, Dandelion jitter,
  encrypted message type, KeyStore per-install salt, constantEq C FFI, key
  confirmation MAC, adaptive PoW, skipped key expiry, HKDF length prefixes,
  sequence high-water mark, API constantEq, IPC chunked reading, SSRF IPv6
  block, DHT store limit, codegen path allowlist, cabal.project.freeze, GPG
  signing mandatory, plugin signed manifests, test vector SHA-256, release
  manifest dynamic, reproducibility gate, isBundleFresh future rejection, GCM
  nonce tracker, RuntimeLog denylist redaction.

### Key files
- `src/UmbraVox/Network/MDNS.hs` (ephemeral rotating IDs)
- `src/UmbraVox/Crypto/GCM.hs` (input length cap, nonce tracker)
- `src/UmbraVox/Crypto/Random.hs` (CSPRNG reseed guard)
- `src/UmbraVox/Crypto/StealthAddress.hs` (subgroup check)
- `src/UmbraVox/Protocol/WireFormat.hs` (64-bit sequence counter)
- `src/UmbraVox/Protocol/RouteToken.hs` (token rotation)
- `src/UmbraVox/Network/Dandelion.hs` (jitter)
- `src/UmbraVox/Crypto/ConstantTime.hs` (constantEq C FFI)
- `src/UmbraVox/Crypto/KeyStore.hs` (per-install salt)
- `src/UmbraVox/Network/PeerExchange.hs` (opaque session tokens)
- `src/UmbraVox/Network/RateLimit.hs` (adaptive PoW)
- `src/UmbraVox/App/` (RuntimeLog denylist redaction)
- `csrc/constant_time.c` (C FFI for constant-time comparison)
- `csrc/sqlite3_shim.c` (hand-rolled SQLite3 FFI)
- `Makefile` (cc-options hardening)
- `nix/vm-image.nix` (VM firewall default, base image SHA-256 pins)

### Tests
- `test/Test/Security/` (all regression suites)
- `test/Test/Crypto/GCM.hs`
- `test/Test/Crypto/StealthAddress.hs`
- `test/Test/Network/MDNS.hs`
- `test/Test/Network/PeerExchange.hs`
- `test/Test/App/` (RuntimeLog tests)

### Design docs
- `doc/SECURITY-REVIEW-v0.4.1.md` (full 72-finding audit report)

### Security audit findings
- 72 total findings: 3 critical, 20 high, 32 medium, 15 low.
- 48 fixed, 1 remaining (HI-016 nixpkgs stable pin).
- Full breakdown at `doc/SECURITY-REVIEW-v0.4.1.md`.

---

## M28: Offline Message Delivery (stubs + scaffolding)

### What was implemented
- M28.1: Sender-side outbound queue type and module (per-peer FIFO).
- M28.2: Relay-assisted delivery stubs (stealth-addressed relay mailbox).
- M28.3: Test scaffolding (queue persistence, drain, overflow tests -- pending).
- Note: This milestone has stubs and scaffolding committed; full integration
  (persistent queue, relay protocol, TUI indicators) is pending.

### Key files
- `src/UmbraVox/Chat/OutboundQueue.hs` (outbound queue type and operations)
- `src/UmbraVox/Network/Relay.hs` (relay-assisted delivery stubs)

### Tests
- Test stubs planned under M28.3 (queue persistence, drain, overflow).
- No dedicated test files yet.

### Design docs
- `TODO.txt` M28 section describes the design: outbound queue with per-peer
  FIFO, relay mailbox with stealth-addressed storage, TTL-based expiry.

### Security audit findings
- None yet (milestone in early stages). Design calls for encrypted queue
  persistence and relay storage of only encrypted blobs with no identity linkage.

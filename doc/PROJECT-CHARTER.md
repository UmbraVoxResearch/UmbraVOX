# UmbraVOX Project Charter

## Mission

Build a post-quantum encrypted messaging system with formally verified
cryptographic foundations, reproducible assurance evidence, and honest
documentation of what is and is not proved.

## Principles

1. **Truth over metrics.** Do not optimize for lower assumption counts at the
   expense of hiding trust. A visible assumption is stronger than a hidden one.
2. **Auditability over completeness.** A reviewer-grade incomplete proof is
   more valuable than a complete proof that cannot be independently reproduced.
3. **Fail-closed by default.** Every check, test, and gate must fail closed.
   A skipped check is not a passed check.
4. **No overclaiming.** Document what is proved and what is NOT proved. State
   modeling limitations, stub status, and side-channel non-claims explicitly.
5. **Cleanroom boundaries.** External reference implementations are test oracles,
   not dependencies. Oracle code never enters the UmbraVOX source tree.

## Scope

### Cryptographic Primitives (pure Haskell)
- SHA-256, SHA-512, SHA-3/SHAKE (Keccak)
- AES-256-GCM, ChaCha20-Poly1305
- HMAC-SHA-256/512, HKDF
- X25519 (ECDH), Ed25519 (EdDSA)
- ML-KEM-768 (post-quantum KEM, FIPS 203)
- Poly1305 MAC

### Protocol Composition
- X3DH key agreement (Signal-like)
- PQXDH hybrid post-quantum key agreement
- Signal Double Ratchet
- Noise IK authenticated key exchange
- Sender Keys (group messaging)
- Verifiable Random Functions (VRF)
- Stealth Addresses

### Formal Verification
- F* specifications for all primitives and protocols (32 specs)
- Coq external evidence (primality, field arithmetic, curve properties)
- Assumption ledger with mechanical consistency checking
- Reviewer-grade assurance matrix and dependency graph

### Runtime
- Terminal UI (TUI) with rich text editing
- Ephemeral-by-default with plugin architecture (M17)
- VM-first development (NixOS QEMU guests)
- Multi-platform targets (Linux, BSDs, illumos)

## Assurance Layers

| Layer | What it proves | Tools | Status |
|-------|---------------|-------|--------|
| Formal model | Algorithm correctness | F* + Z3 | 32 specs, 0 admit, 25 active assume val |
| External certificates | Computational facts Z3 can't prove | Coq (Rocq 9.1.1) | 19 files, 613 Qed |
| Assumption registry | Trust boundary is documented | check-assumption-ledger.sh | 25 active entries (20 discharged), mechanically checked |
| Proof hygiene | No hidden trust, no misleading names | check-proof-hygiene.sh | 4 checks, 0 issues |
| Infrastructure tests | Build system, VM pipeline, docs | test-infrastructure.sh | 67 tests |
| Crypto audit | Per-module assurance grades A-F | audit/ reports | 6 reports |
| Differential testing | Runtime matches verified model | Multi-oracle cleanroom (v0.1.4+) | 36/36 suites PASS |
| Side-channel | Constant-time behavior | dudect, HACL* (future) | Not yet claimed |

## Non-Claims

This project does NOT claim:
- Cryptographic hardness is proved unconditionally
- All Haskell runtime behavior is proved inside F*
- Coq evidence automatically removes F* assumptions
- Ed25519 group law is fully machine-proved (universal GZnZ proofs cover field structure; full curve-point universality remains open)
- ML-KEM-768 has functional correctness proofs (parameter validation only)
- Constant-time behavior is proved for Haskell implementations
- The system is operationally secure in production environments
- Differential testing proves formal runtime equivalence

## Versioning

| Version | Focus |
|---------|-------|
| v0.1.0 | Formal verification milestone (0 admit, assumption ledger) |
| v0.1.1 | VM infrastructure hardening |
| v0.1.2 | Full crypto audit, hidden assumes surfaced, false theorems fixed |
| v0.1.3 | Assurance hardening (reviewer-grade framework, Coq evidence, X25519 reductions) |
| v0.1.4 | Cleanroom multi-oracle differential testing |
| v0.1.5 | 22 differential suites, 5 Coq files (187 Qed), Ed25519 malleability |
| v0.1.6 | VRFDLEQ.v, pre-release check, 6 Coq files (219 Qed) |
| v0.1.7 | sign_then_verify proved, 11 Coq files (415 Qed), ML-KEM confirmed, AFL++ |
| v0.1.8 | Documentation sweep and release |
| v0.2.0 | Signal-compatible chat bridge plugin, universal group law proofs (ED-003/ED-007/ED-008) |
| v0.4.x | Security model refactor, SQLite FFI migration, DHT/discovery, relay stubs, verified C infra |
| v0.5.0 | Unified ./uv build system (replaces Makefile), mke2fs -d disk images |
| v0.5.3 | Codegen pipeline, M28 delivery, CI pipelines (Linux/macOS/Windows), DoubleRatchet/Ed25519 fixes |
| v0.5.5–v0.5.9 | Code review fixes (MEDIUM/LOW), quality gates, generated-headers check, CT-branch analysis |
| v0.5.10–v0.5.13 | Static analysis (Go CVEs, shell quoting), Go refactoring, PQXDH tests |
| v0.5.14–v0.5.16 | Shell-to-Go migration (smoke, coqprime, release, signal-test, fstar-eval, vm-init tools) |
| v0.5.17 | GHC 9.14.1 upgrade (from 9.6.7), SecureBytes migration (15 secret fields, 18 files) |
| v0.5.18–v0.5.22 | Signal JAR classpath fix, F* assume val documentation, Signal Server fat JAR, path stripping |
| v0.6.0 | vmctl unified VM framework (5-phase), 4-tier NixOS hierarchy, YAML DSL, CI strategy, 851 tests |
| v0.6.1 | Coq proofs for 18 F* assume vals (Ed25519/X25519/VRF/SHA-256), verified C (HACL* + fiat-crypto), CycloneDX SBOM |
| v0.6.2 | Security audit v2 (91 findings), SecureBytes completion, ephemeral signing, SBOM, YAML wiring |
| v0.6.3 | Firecracker runtime path removed, stub removal, Go tests (netproxy, vmctl), CI hardening |
| post-v0.6.3 | Signal bridge WebSocket provisioning wired (M26.1–M26.2), CI hardening (M24/M28), nix tier consistency (M23) |

## Team

- Primary developer: cyanitol
- Formal verification: F* + Coq + expert agent review
- Crypto audit: specialized parallel agent teams
- Infrastructure: NixOS VM-first development model

## References

- Signal Protocol: X3DH spec (2016), Double Ratchet spec (2016), PQXDH spec (2023)
- NIST: FIPS 180-4 (SHA-2), FIPS 197 (AES), FIPS 202 (SHA-3), FIPS 203 (ML-KEM)
- RFCs: 7748 (X25519), 8032 (Ed25519), 8439 (ChaCha20-Poly1305), 5869 (HKDF)
- Formal tools: F* (Microsoft Research), Rocq/Coq 9.1.1, Z3 4.16.0
- Oracle libraries: HACL*/EverCrypt, libsodium, BoringSSL, Monocypher, libsignal

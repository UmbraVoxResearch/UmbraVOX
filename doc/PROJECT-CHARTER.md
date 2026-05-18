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
- F* specifications for all primitives and protocols (24 specs)
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
| Formal model | Algorithm correctness | F* + Z3 | 24 specs, 0 admit, 28 assume val |
| External certificates | Computational facts Z3 can't prove | Coq (Rocq 9.1.1) | 4 files, 171 Qed |
| Assumption registry | Trust boundary is documented | check-assumption-ledger.sh | 28 entries, mechanically checked |
| Proof hygiene | No hidden trust, no misleading names | check-proof-hygiene.sh | 4 checks, 0 issues |
| Infrastructure tests | Build system, VM pipeline, docs | test-infrastructure.sh | 67 tests |
| Crypto audit | Per-module assurance grades A-F | audit/ reports | 6 reports |
| Differential testing | Runtime matches verified model | Multi-oracle cleanroom (v0.1.4) | Planned |
| Side-channel | Constant-time behavior | dudect, HACL* (future) | Not yet claimed |

## Non-Claims

This project does NOT claim:
- Cryptographic hardness is proved unconditionally
- All Haskell runtime behavior is proved inside F*
- Coq evidence automatically removes F* assumptions
- Ed25519 group law is fully machine-proved
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
| v0.1.4 | Cleanroom multi-oracle differential testing (planned) |

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

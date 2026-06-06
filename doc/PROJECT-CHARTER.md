# UmbraVOX Project Charter

## Mission

Build a post-quantum encrypted messaging system for life-critical use, with
100% formally verified cryptographic foundations, reproducible assurance
evidence, and honest documentation of what is and is not proved.

Every production byte of crypto must trace to a machine-checked proof. That is
not a future aspiration — it is the definition of done for this project.

---

## Principles

1. **Truth over metrics.** Do not optimize for lower assumption counts at the
   expense of hiding trust. A visible assumption is stronger than a hidden one.
2. **Auditability over completeness.** A reviewer-grade incomplete proof is
   more valuable than a complete proof that cannot be independently reproduced.
3. **Fail-closed by default.** Every check, test, and gate must fail closed.
   A skipped check is not a passed check.
4. **No overclaiming.** Document what is proved and what is NOT proved. State
   modeling limitations, stub status, and side-channel non-claims explicitly.
5. **Cleanroom boundaries.** External reference implementations are test
   oracles, not dependencies. Oracle code never enters production build paths.
6. **Provenance over convenience.** Every production byte of crypto must trace
   to a machine-checked proof. Third-party code, however well-audited, is an
   oracle, not production.

---

## Architecture

### Production Crypto Pipeline (Target State)

Each cryptographic primitive must follow a complete five-stage chain before it
is eligible for production:

```
Stage 1: F* Pure Spec (.fst)
    |  Formally proves algorithm correctness against the mathematical spec.
    |  All values, loops, and invariants stated and checked by F* + Z3.
    v
Stage 2: F* Low* Implementation (.fst, Low* style)
    |  Formally derived from the pure spec.
    |  Carries an equivalence proof: every call to the Low* function produces
    |  the same output as the pure spec.  Memory safety and (where applicable)
    |  constant-time are encoded as type-level invariants.
    v
Stage 3: KaRaMeL C Extraction  →  csrc/extracted/<prim>.c
    |  Machine-checked derivation: KaRaMeL translates Low* to C while
    |  preserving the proof chain.  The extracted C carries no runtime
    |  F* or Coq dependency.  THIS IS THE PRODUCTION C FILE.
    v
Stage 4: CryptoGen FFI Binding  →  src/UmbraVox/Crypto/Generated/FFI/<prim>.hs
    |  CryptoGen reads the algorithm's .spec file and emits the Haskell FFI
    |  shim that calls csrc/extracted/<prim>.c.  CryptoGen is the single
    |  source of truth for all crypto provenance; the .spec file encodes
    |  the algorithm identity, parameter set, and C entry-point name.
    v
Stage 5: Expert Review Gate
    |  No primitive advances to production without independent expert review
    |  of the F* proof, the Low* equivalence lemma, and the extraction output.
    v
Production Build
```

The entry in `UmbraVox.cabal`'s `c-sources` stanza is changed from the
CryptoGen oracle file (`csrc/generated/<prim>.c`) to the extracted file
(`csrc/extracted/<prim>.c`) only after all five stages pass.

### Third-Party C Is Oracles, Not Production

| Library | Location | Role |
|---------|----------|------|
| HACL* | `csrc/hacl/` → will move to `contrib/hacl-oracle/` | Formally verified C from F*/Low*. Used as INTERIM production while our own extraction pipeline is being built; becomes differential oracle once our extracted C exists. |
| fiat-crypto | `csrc/fiat/` → will move to `contrib/fiat-oracle/` | Coq-extracted field arithmetic. Used as differential oracle for X25519/Ed25519 field operations. |
| PQClean, pq-crystals | oracle VMs in `contrib/` | Differential oracles for ML-KEM-768. Never in `c-sources`. |
| CryptoGen output | `csrc/generated/` | Algorithm reference, never production after extraction completes. |

### CryptoGen as Provenance Hub

CryptoGen `.spec` files encode each algorithm (name, parameter set, NIST/RFC
reference, entry-point symbols). CryptoGen generates three artifacts from each
spec:

1. **Oracle C** (`csrc/generated/<prim>.c`) — clean-room reference implementation.
2. **Haskell reference** — pure Haskell oracle for differential testing.
3. **FFI binding** (`src/UmbraVox/Crypto/Generated/FFI/<prim>.hs`) — calls
   `csrc/extracted/<prim>.c` in production; calls `csrc/generated/<prim>.c`
   during the oracle-only phase.

This makes every algorithm's provenance chain machine-readable and auditable
from a single `.spec` file.

### Three-Way Differential Testing

For each primitive that has entered the extraction pipeline:

```
Our extracted C  (csrc/extracted/)
        |
        | must agree byte-for-byte
        v
CryptoGen oracle C  (csrc/generated/)
        |
        | must agree byte-for-byte
        v
HACL* / fiat-crypto / PQClean oracle  (contrib/)
```

Any pairwise disagreement fails the test suite. Differential testing proves
output equivalence on the tested inputs; it does NOT prove timing safety.

---

## Scope

### Cryptographic Primitives

| Primitive | Standard | Phase |
|-----------|----------|-------|
| SHA-256 | FIPS 180-4 | Phase 2 (M36B.1) |
| SHA-512 | FIPS 180-4 | Phase 2 (M36B.2) |
| ChaCha20 | RFC 8439 | Phase 2 (M36B.3) |
| Poly1305 | RFC 8439 | Phase 2 (M36B.4) |
| Keccak / SHA-3 | FIPS 202 | Phase 2 (M36B.5) |
| HMAC | FIPS 198-1 | Phase 2 (M36B.6a) |
| HKDF | RFC 5869 | Phase 2 (M36B.6b) |
| X25519 | RFC 7748 | Phase 2 (M36B.7) |
| Ed25519 | RFC 8032 | Phase 2 (M36B.8) |
| AES-256-GCM | FIPS 197 + NIST SP 800-38D | Phase 3 (M36B.9) |
| VRF (ECVRF-ED25519-SHA512-TAI) | RFC 9381 | Phase 3 (M36B.10) |
| ML-KEM-768 | FIPS 203 | Phase 3 (M36B.11) |

### Protocol Composition

- X3DH key agreement (Signal-like)
- PQXDH hybrid post-quantum key agreement
- Signal Double Ratchet
- Noise IK authenticated key exchange
- Sender Keys (group messaging)
- Verifiable Random Functions (VRF)
- Stealth Addresses

### Formal Verification Infrastructure

- F* pure specifications for all primitives and protocols
- F* Low* implementations with machine-checkable equivalence proofs (target)
- KaRaMeL C extraction toolchain (target)
- Coq external evidence (primality, field arithmetic, curve properties)
- Assumption ledger with mechanical consistency checking
- Reviewer-grade assurance matrix and dependency graph

### Runtime

- Terminal UI (TUI) with rich text editing
- Ephemeral-by-default with plugin architecture
- VM-first development (NixOS QEMU guests)
- Multi-platform targets (Linux, BSDs, illumos/OmniOS/SmartOS/OpenIndiana)

---

## Current State (honest, as of v0.6.3)

### What is implemented

- **Production crypto runs through pure Haskell only.** `Crypto.SHA256`,
  `Crypto.MLKEM`, etc. are called directly. This is NOT constant-time.
- **FFI layer exists but is stub-only.** `src/UmbraVox/Crypto/Generated/FFI/`
  files are present and perform link probes. They are NOT wired to C in
  production builds.
- **F* pure specs: 32 files.** These prove algorithm correctness at the
  mathematical level using F* + Z3. They carry no Low* implementations.
- **ML-KEM-768 F* spec is stubs-only.** The NTT is `let ntt f = f`. This is
  not a proof of anything except parameter structure.
- **Coq proofs: 19 files, 613 Qed, 5 Axioms.** These cover Ed25519/X25519
  algebraic properties, primality, DLEQ for VRF, and structural lemmas.
- **ASSURANCE-MATRIX.md currently shows incorrect Axiom counts.** This is a
  known documentation gap to be corrected in Phase 1.
- **No `csrc/extracted/` directory.** No KaRaMeL-extracted C exists yet.
- **No F* Low* implementations.** Stage 2 of the production pipeline has not
  begun for any primitive.
- **KaRaMeL toolchain not integrated.** Stage 3 cannot run until M36A.
- **HACL* not vendored.** `csrc/hacl/` contains infrastructure and bridge
  stubs only, not the HACL* C sources themselves.
- **fiat-crypto not vendored.** `csrc/fiat/` contains infrastructure and
  vendoring docs only.
- **3-way differential testing: 8 of 11 primitives** have 2-way Haskell vs
  generated C tests. The third arm (HACL*/fiat-crypto) is not yet active.
- **Differential testing proves output equivalence, NOT timing safety.**

### What is NOT claimed

- Cryptographic hardness is proved unconditionally.
- All Haskell runtime behavior is proved inside F*.
- Coq evidence automatically removes F* assumptions.
- Ed25519 group law is fully machine-proved (universal GZnZ proofs cover
  field structure; full curve-point universality remains open).
- ML-KEM-768 has functional correctness proofs (parameter validation only).
- Constant-time behavior is proved for any Haskell implementation.
- The system is operationally secure in production environments.
- Differential testing proves formal runtime equivalence.
- HACL* or fiat-crypto proofs transfer to UmbraVOX-generated code.

---

## Assurance Layers

| Layer | What it proves | Status |
|-------|---------------|--------|
| F* pure specs | Algorithm correctness (math level) | 32 specs; ML-KEM-768 stubs-only; 0 admit; 25 active assume val |
| F* Low* impls | Same algorithm, extractable to C, with equivalence proof | NONE yet — M36B target |
| KaRaMeL extraction | Machine-checked C derivation from Low* | Not yet set up — M36A target |
| Coq proofs | External computational facts (primality, field laws, DLEQ) | 19 files, 613 Qed, 5 Axioms |
| Assumption ledger | Trust boundary documentation, mechanically checked | 31 active entries (4 discharged in Coq), check-assumption-ledger.sh |
| Proof hygiene | No hidden trust, no misleading names | check-proof-hygiene.sh, 4 checks, 0 issues |
| HACL* oracle C | Independent F*/Low*-derived reference | `csrc/hacl/` — not vendored yet; bridge stubs exist for SHA-256 |
| fiat-crypto oracle C | Coq-extracted field arithmetic reference | `csrc/fiat/` — not vendored yet |
| CryptoGen oracle C | Deterministic algorithm reference from .spec files | `csrc/generated/` — active in current builds as interim |
| Differential testing | Output equivalence on tested inputs (NOT timing) | 2-way for 8/11 primitives; 3-way target once HACL* vendored |
| Side-channel | Constant-time behavior | Not claimed for any current implementation |
| Infrastructure tests | Build system, VM pipeline, docs | test-infrastructure.sh, 67+ tests |
| Crypto audit | Per-module assurance grades A-F | 6 audit reports |

---

## Roadmap

### Phase 1 — Documentation Truthfulness and HACL* Interim (immediate)

Goals:
- Fix ASSURANCE-MATRIX.md Axiom counts to match actual Coq proofs.
- Document the current pure-Haskell production path explicitly and honestly.
- Wire HACL* as INTERIM production via CryptoGen FFI (replaces pure Haskell
  for hash and AEAD primitives while the extraction pipeline is built).
- Vendor HACL* sources into `csrc/hacl/` following `csrc/hacl/VENDORING.md`.
- Activate bridge files (`csrc/hacl/bridge_sha256.c` and equivalents) in
  `UmbraVox.cabal` for SHA-256, SHA-512, ChaCha20, Poly1305, Keccak, HMAC,
  HKDF.
- Vendor fiat-crypto sources into `csrc/fiat/` for X25519/Ed25519 field ops.
- Activate 3-way differential testing.
- Label all HACL*/fiat-crypto usage as INTERIM clearly in source and docs.

Deliverable: production crypto exits pure Haskell and has a formally verified
oracle arm in differential testing. No F* Low* work required for this phase.

### Phase 2 — KaRaMeL Toolchain + First Extractions (M36A/M36B)

Goals:
- M36A: Integrate KaRaMeL toolchain into builder VM. Establish `csrc/extracted/`
  directory and cabal wiring.
- M36B.1–M36B.8: Write F* Low* implementations for each primitive and extract
  via KaRaMeL. Order: SHA-256 → SHA-512 → ChaCha20 → Poly1305 → Keccak →
  HMAC → HKDF → X25519 → Ed25519.

Estimated effort per primitive:
- SHA-256/512: 2–3 weeks each (template from HACL*).
- ChaCha20: 2–3 weeks.
- Poly1305: 2–3 weeks.
- Keccak/SHA-3: 3–4 weeks.
- HMAC/HKDF: 1–2 weeks each (builds on SHA-256).
- X25519: 3–4 weeks (uses fiat-crypto field arithmetic).
- Ed25519: 4–6 weeks.

For each primitive: F* Low* impl → equivalence proof → KaRaMeL extraction →
`csrc/extracted/<prim>.c` → CryptoGen FFI binding update → expert review →
`c-sources` switch → HACL*/fiat-crypto demoted to `contrib/`.

### Phase 3 — AES-256-GCM, VRF, ML-KEM-768 (M36B.9–M36B.11)

Goals:
- M36B.9: AES-256-GCM. Evaluate Vale for hardware-accelerated verified C
  (AES-NI). Portable software AES has no existing verified Low* implementation;
  this may require Vale or novel F* Low* work. 4–8 weeks.
- M36B.10: VRF (ECVRF-ED25519-SHA512-TAI). Compose Ed25519 + SHA-512 Low*
  implementations from Phase 2. Builds on existing `VRFDLEQ.v` Coq proof.
  3–5 weeks.
- M36B.11: ML-KEM-768. Complete the F* pure spec (NTT, sampling, encode/decode,
  encaps/decaps). Write Low* implementation. Extract via KaRaMeL. This is
  pioneering work — no public Low* ML-KEM implementation exists. 8–16 weeks.
  Blocked on F* spec completion.

### Phase 4 — Oracle Migration (ongoing, concurrent with Phase 2–3)

As each primitive gets its own extracted C, the corresponding third-party source
moves from `csrc/` to `contrib/`:

- HACL*: `csrc/hacl/` → `contrib/hacl-oracle/` (primitive by primitive)
- fiat-crypto: `csrc/fiat/` → `contrib/fiat-oracle/`
- PQClean/pq-crystals: oracle VMs in `contrib/`

After migration, the `contrib/` entry is used only as a differential oracle in
tests, never in `c-sources`.

### Phase 5 — Protocol Composition and Application Layer

Goals:
- Formally verify X3DH, PQXDH, and Double Ratchet at the F* level with all
  assume vals discharged.
- Complete the Signal bridge registration path end-to-end.
- Integrate DHT/discovery and relay infrastructure.
- Multi-platform packaging (Linux, BSD, illumos).

### Phase 6 — Full Assurance Certification

Goals:
- All 11 cryptographic primitives in `csrc/extracted/`.
- All protocol F* specs with 0 active assume vals.
- Complete 3-way differential testing for all primitives.
- Independent external expert review of every proof chain.
- Release with a signed SBOM and reproducible build evidence.
- Publish assurance matrix as a static auditable artifact.

---

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
| v0.7.0 (target) | Phase 1 complete: HACL* and fiat-crypto vendored and wired as interim production; 3-way differential testing active for 8+ primitives; pure-Haskell crypto path replaced |
| v0.8.0 (target) | Phase 2 partial: KaRaMeL integrated; first extracted primitive (SHA-256) in csrc/extracted/ and wired to production |
| v1.0.0 (target) | Phase 2 complete: all 9 Phase-2 primitives in csrc/extracted/; HACL*/fiat-crypto demoted to contrib/ |
| v1.x (target) | Phase 3: AES-256-GCM, VRF, ML-KEM-768 extraction complete |
| v2.0.0 (target) | Phase 6: all primitives extracted, all assume vals discharged, certified release |

---

## Team

- Primary developer: cyanitol
- Formal verification: F* + Coq + expert agent review
- Crypto audit: specialized parallel agent teams
- Infrastructure: NixOS VM-first development model

---

## References

- Signal Protocol: X3DH spec (2016), Double Ratchet spec (2016), PQXDH spec (2023)
- NIST: FIPS 180-4 (SHA-2), FIPS 197 (AES), FIPS 202 (SHA-3), FIPS 203 (ML-KEM)
- RFCs: 7748 (X25519), 8032 (Ed25519), 8439 (ChaCha20-Poly1305), 5869 (HKDF), 9381 (ECVRF)
- Formal tools: F* (Microsoft Research), Rocq/Coq 9.1.1, Z3 4.16.0, KaRaMeL (target)
- Oracle libraries: HACL*/EverCrypt, fiat-crypto, PQClean, pq-crystals, libsodium, BoringSSL
- HACL* extraction: Low* subset of F*, KaRaMeL compiler (`github.com/FStarLang/karamel`)
- CryptoGen: internal codegen from .spec files (`app/codegen/`)

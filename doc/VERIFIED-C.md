# Verified C Integration Strategy

## 1. Overview

UmbraVOX C code comes from multiple sources with distinct trust properties.
The architecture distinguishes **interim production** (what runs today), **target
production** (what will run after M36B), and **differential oracles** (never
production; used for cross-checking).

### Current architecture (v0.7.x, interim)

- **HACL*** (`csrc/hacl/`) — **INTERIM PRODUCTION** for covered primitives.
  Verified C extracted from Low* (F* subset) via KaRaMeL. Machine-checked
  proofs of functional correctness, memory safety, and constant-time execution.
  Covers SHA-256/512, ChaCha20, Poly1305, Keccak, HMAC, HKDF.
  _Will transition to differential oracle in `contrib/hacl-oracle/` once
  `csrc/extracted/` is available (post-M36B)._

- **fiat-crypto** (`csrc/fiat/`) — **INTERIM PRODUCTION** for field arithmetic.
  Verified constant-time C extracted from Coq proofs. Covers Ed25519 and X25519
  field operations. Used by BoringSSL, Go stdlib, Linux kernel.
  _Will transition to differential oracle in `contrib/fiat-oracle/` once
  `csrc/extracted/` provides equivalent formally-verified field arithmetic._

- **CryptoGen** (`csrc/generated/`) — **DIFFERENTIAL ORACLE ONLY**. 18 C files
  generated from `.spec` files. Covers protocol formats, ML-KEM, AES-GCM, VRF,
  and primitives not covered by HACL*/fiat-crypto. Never used as production
  code; retained exclusively for differential testing.

- **Haskell reference** (`src/UmbraVox/Crypto/*.hs`) — **REFERENCE ORACLE
  ONLY**. Pure Haskell implementations of all primitives. Correct by
  construction but NOT constant-time. Never called in production.

### Target architecture (post-M36B)

- **`csrc/extracted/`** — OUR production C, KaRaMeL-extracted from UmbraVOX's
  own F* Low* specs. 100% formally verified C derived from in-tree proofs.
  This is the long-term production code path.

- **`contrib/hacl-oracle/`** — HACL* moves here as a differential oracle once
  `csrc/extracted/` covers its primitives.

- **`contrib/fiat-oracle/`** — fiat-crypto moves here as a differential oracle
  once `csrc/extracted/` covers field arithmetic.

- **`csrc/generated/`** — CryptoGen differential oracle (unchanged role).

```
Current (v0.7.x, interim):
  csrc/hacl/       → INTERIM production (HACL* formally verified by HACL* team)
  csrc/fiat/       → INTERIM production (fiat-crypto Coq-verified field arithmetic)
  csrc/generated/  → Differential oracle (CryptoGen output)

Target (post-M36B):
  csrc/extracted/      → OUR production (KaRaMeL-extracted from our F* Low* specs)
  contrib/hacl-oracle/ → HACL* differential oracle
  contrib/fiat-oracle/ → fiat-crypto differential oracle
  csrc/generated/      → CryptoGen differential oracle
```

---

## 2. Trust Hierarchy

```
Coq proofs (test/evidence/formal-proofs/coq/)
    |
    | algebraic correctness: field laws, group law, scalar mult,
    |   primality of 2^255-19, DLEQ for VRF
    v
fiat-crypto  ──────────────────────────────────────────────────►  verified field arithmetic C
    (GF(2^255-19): mul, square, add, sub, serialize)                Ed25519 base field, X25519
    Coq proof → extracted C (no runtime F*/Coq dependency)          (INTERIM production)

F* specs (test/evidence/formal-proofs/fstar/)
    |
    | functional correctness + memory safety + constant-time,
    |   proved via refinement lemmas, discharged by Z3
    v
HACL* (Low* → KaRaMeL → C)  ────────────────────────────────►  verified hash/AEAD C
    SHA-256, SHA-512, ChaCha20, Poly1305, Keccak/SHA-3,             (INTERIM production)
    HMAC, HKDF

UmbraVOX F* Low* specs (future, M36B)
    |
    | functional correctness + memory safety + constant-time
    | proved in-tree, extracted via KaRaMeL
    v
csrc/extracted/  ────────────────────────────────────────────►  TARGET production C
    (our own verified C, replaces HACL* and fiat-crypto
     primitive-by-primitive)

NIST / RFC standards
    |
    | manual encoding (two independent paths)
    |
    +──► .spec files (app/codegen/Specs/)
    |        |
    |        | CryptoGen.hs (mechanical, deterministic)
    |        v
    |    csrc/generated/*.c  ──────────────────────────────────►  differential oracle only
    |        |
    +──► F* specs            differential testing (3-way):
             |                   HACL* / fiat-crypto (interim production)
             | ./uv verify        codegen C (oracle)
             v                   Haskell reference (oracle)
         internal consistency
```

---

## 3. Oracle Coverage by Primitive

The following table shows differential oracle depth for each primitive.
"N-way" counts independent implementations compared in testing.

| Primitive | Interim production | Oracles active | Coverage |
|-----------|-------------------|----------------|----------|
| SHA-256 | HACL* (`csrc/hacl/`) | CryptoGen + Haskell ref | **3-way** |
| SHA-512 | HACL* (`csrc/hacl/`) | CryptoGen + Haskell ref | **3-way** |
| ChaCha20 | HACL* (`csrc/hacl/`) | CryptoGen + Haskell ref | **3-way** |
| Poly1305 | HACL* (`csrc/hacl/`) | CryptoGen + Haskell ref | **3-way** |
| Keccak / SHA-3 | HACL* (`csrc/hacl/`) | CryptoGen + Haskell ref | **3-way** |
| HMAC | HACL* (`csrc/hacl/`) | CryptoGen + Haskell ref | **3-way** |
| HKDF | HACL* (`csrc/hacl/`) | CryptoGen + Haskell ref | **3-way** |
| X25519 | fiat-crypto (`csrc/fiat/`) | CryptoGen + Haskell ref | **3-way** |
| Ed25519 | fiat-crypto (`csrc/fiat/`) | CryptoGen + Haskell ref | **3-way** |
| AES-256-GCM | _(none — see note)_ | Wycheproof vectors + Haskell ref | **2-way** |
| ML-KEM-768 | _(none — see note)_ | pq-crystals oracle + Haskell ref | **2-way** |
| VRF (ECVRF) | _(none — see note)_ | Haskell ref + RFC 9381 KAV | **1-way + KAV** |
| PQ wrapper | _(none)_ | — | — |
| Wire format | _(none)_ | — | — |
| Message format | _(none)_ | — | — |
| Network protocol | _(none)_ | — | — |
| Session state | _(none)_ | — | — |
| Dandelion | _(none)_ | — | — |

**Notes on coverage gaps:**

- **AES-256-GCM**: HACL* EverCrypt AES-256-GCM (M38.2) is the interim
  production implementation (`Generated.FFI.GCM`). Wycheproof vectors
  exercise both the Haskell oracle and the FFI path (via `runAesGcmVectorsFFI`
  in `Test.Crypto.Wycheproof`). Tests skip gracefully on platforms without
  AES-NI. `testGCMDifferential` in `Test.Crypto.Differential` provides
  4-vector Haskell-oracle vs FFI comparison.

- **ML-KEM-768**: FIPS 203 was published in 2024; no public verified Low*
  implementation exists. pq-crystals oracle provides cross-check; formal C
  assurance is ASSURANCE_PENDING.

- **VRF (ECVRF-ED25519-SHA512-ELL2)**: The Haskell implementation passes all
  three RFC 9381 Appendix A.2 known-answer test vectors — these vectors are
  the authoritative ground truth (the RFC itself), stronger than a typical
  2-way implementation differential. `Test.Crypto.VRF` covers 24 test cases
  including RFC KAV, round-trip, deterministic regression, and 11 negative
  tests.

  Second oracle plan: a standalone Python implementation using the
  `py_ecc` / `cryptography` library against RFC 9381 §4.1 (ECVRF-EDWARDS25519-SHA512-ELL2),
  invoked from `test/scripts/vrf-oracle.py`, would serve as a second oracle
  for differential testing. This is deferred until `csrc/extracted/vrf.c`
  (M36B.10) requires validation, at which point the RFC KAV vectors alone may
  not be sufficient and an independent implementation comparison is warranted.

  **Current assessment**: RFC KAV coverage plus Haskell round-trip testing
  provides adequate assurance for the interim period. A C-level oracle
  (e.g., libsodium's Ed25519 scalar-mult primitives composed per RFC 9381)
  is not justified until VRF enters the production FFI path.

---

## 4. Constant-Time Guarantees

| Source | CT guarantee | Mechanism |
|--------|-------------|-----------|
| fiat-crypto | By construction | Coq proof rules out data-dependent branches and variable-time memory access patterns in the field arithmetic |
| HACL* | By construction | Low* type system encodes CT as a type-level invariant; KaRaMeL preserves it on extraction; verified by the F* checker |
| CryptoGen | By discipline | `csrc/ct_helpers.h` provides branchless select (`ct_select32/64`), compare (`ct_eq32`), and swap (`ct_cswap32/64`, `ct_cmov32/64`). `csrc/constant_time.c` provides `constant_time_eq` with a `volatile` accumulator. The codegen quality gate (`./uv check`) enforces no data-dependent branches in generated files |
| Haskell reference | **None** | Pure Haskell — NOT constant-time. Reference oracle only; never called in production |

CryptoGen's CT guarantee is weaker than fiat-crypto and HACL*: it is a coding
discipline enforced by review and tooling, not a machine-checked proof.
Empirical agreement with HACL* and fiat-crypto oracles in differential testing
provides strong (but not formal) evidence that the generated C is semantically
correct; it does not prove CT.

---

## 5. Bridge Integration

The bridge pattern connects verified library entry points to the FFI signatures
expected by the Haskell layer.

### HACL* bridge

One bridge file exists: `csrc/hacl/bridge_sha256.c`. It wraps
`Hacl_SHA2_256_hash` to match the `sha256_hash` and `sha256_link_probe`
symbols imported by `src/UmbraVox/Crypto/Generated/FFI/SHA256.hs`. The same
pattern will be used for each remaining HACL* algorithm when the sources are
vendored.

```
Hacl_SHA2_256.c   (HACL* extracted)
    |
    v
csrc/hacl/bridge_sha256.c
    |  sha256_hash(output, input, len)
    |  sha256_link_probe() -> 1
    v
UmbraVox.Crypto.Generated.FFI.SHA256 (Haskell FFI)
```

When HACL* sources are vendored, `bridge_sha256.c` replaces
`csrc/generated/sha256.c` in the `c-sources` stanza of `UmbraVox.cabal`. The
Haskell FFI layer is unchanged. Additional bridges for SHA-512, ChaCha20,
Poly1305, Keccak, HMAC, and HKDF follow the same pattern.

### fiat-crypto bridge

fiat-crypto exposes operations in the `fiat_25519_*` namespace. A thin
namespace-mapping header (`ffi_bridge.h`, documented in `csrc/fiat/README.md`)
re-exports these as `umbravox_fe_*` for use by the X25519 and Ed25519 generated
C files. fiat-crypto requires no binary entry points of its own; it provides
field element operations that the scalar multiplication loops in `x25519.c` and
`ed25519extended.c` call directly after the namespace bridge is in place.

---

## 6. Production Linkage Gap (Current State)

As of v0.7.0, there are two compounding gaps in the production code path:

**Gap 1**: All `src/UmbraVox/Crypto/Generated/FFI/*.hs` modules call only
`*_link_probe` and then delegate to pure Haskell (`Crypto.SHA256`,
`Crypto.MLKEM`, etc.). The HACL* and fiat-crypto bridge functions exist in C
but are not called from any production path.

**Gap 2**: Production callers (`Protocol/`, `Network/`, `TUI/`) import
`UmbraVox.Crypto.SHA256` etc. directly — the NOT-constant-time Haskell
reference — bypassing the FFI layer entirely.

This means **all production crypto currently runs through variable-time pure
Haskell**.

**Resolution timeline**:

- **M38 (Phase 2)**: CryptoGen extended with HaclBridge backend; FFI modules
  regenerated to call HACL* directly; production callers updated to
  `Generated.FFI.*` (monadic interface change required).
- **M36B (Phases 4-5)**: Our own extracted C (`csrc/extracted/`) replaces
  HACL* primitive-by-primitive. HACL* and fiat-crypto move to oracle role.

Until M38 lands, any security claim that UmbraVOX uses constant-time crypto in
production is incorrect. This gap must be resolved before any production
deployment.

---

## 7. Assurance Grade by Primitive

The following table shows honest current vs. target assurance per primitive.

| Primitive | Current grade | Current limiter | Target grade (post-M36B) |
|-----------|--------------|-----------------|--------------------------|
| SHA-256 | INTERIM | Haskell ref in prod path (not CT); HACL* bridge not wired | Formally verified extracted C |
| SHA-512 | INTERIM | Same as SHA-256 | Formally verified extracted C |
| ChaCha20 | INTERIM | Same as SHA-256 | Formally verified extracted C |
| Poly1305 | INTERIM | Same as SHA-256 | Formally verified extracted C |
| Keccak / SHA-3 | INTERIM | Same as SHA-256 | Formally verified extracted C |
| HMAC | INTERIM | Same as SHA-256 | Formally verified extracted C |
| HKDF | INTERIM | Same as SHA-256 | Formally verified extracted C |
| X25519 | INTERIM | Haskell ref in prod path (not CT); fiat-crypto bridge not wired | Formally verified extracted C |
| Ed25519 | INTERIM | Same as X25519 | Formally verified extracted C |
| AES-256-GCM | ASSURANCE_PENDING | No verified C; Wycheproof + Haskell only; M38.2 | Vendor HACL* AES-GCM or Vale |
| ML-KEM-768 | ASSURANCE_PENDING | No verified Low* impl; pq-crystals oracle only | Formal C TBD post-FIPS 203 ecosystem |
| VRF (ECVRF) | ASSURANCE_PENDING | Haskell reference only; no second oracle | liboqs ECVRF oracle (M34.3) |
| Protocol codecs | BEST_EFFORT | Protocol-specific; no upstream verified counterpart | Differential testing against Haskell |

---

## 8. Formal Proof Coverage

The Coq proofs in `test/evidence/formal-proofs/coq/` (20 files including 14
Ed25519 files, `X25519DH.v`, `VRFDLEQ.v`, and `StructuralProofs.v`)
independently verify algebraic properties of the curve operations. These are
separate from the fiat-crypto Coq proofs but cover the same mathematical
structures, providing a second formal confirmation of correctness for the field
arithmetic path.

F* specs in `test/evidence/formal-proofs/fstar/` cover the same primitives as
HACL*, providing internal consistency checking via `./uv verify`. These specs
are the foundation from which `csrc/extracted/` will eventually be produced.

---

## 9. Current State vs. Target State Summary

| Component | Current state | Target state |
|-----------|--------------|--------------|
| `csrc/hacl/` | INTERIM production (not yet vendored) | Differential oracle in `contrib/hacl-oracle/` |
| `csrc/fiat/` | INTERIM production (not yet vendored) | Differential oracle in `contrib/fiat-oracle/` |
| `csrc/extracted/` | Does not exist (M36B) | OUR production C from in-tree F* specs |
| `csrc/generated/` | Differential oracle | Differential oracle (unchanged) |
| FFI modules | Call `*_link_probe` only; delegate to Haskell | Call HACL*/extracted C directly |
| Production callers | Import Haskell ref directly (not CT) | Import `Generated.FFI.*` (CT, via C) |
| Differential testing | 2-way (Haskell vs codegen C) | 3-way (+ HACL*/fiat-crypto/extracted) |

Vendoring instructions are in `csrc/hacl/VENDORING.md` and
`csrc/fiat/VENDORING.md`. After vendoring, the build system integration changes
are confined to the `c-sources` stanza in `UmbraVox.cabal` and the
`csrc/hacl/bridge_*.c` files; the Haskell FFI layer and test harness require no
modification to support the interim (HACL*-based) production path.

# Verified C Integration Strategy

## 1. Overview

UmbraVOX C code comes from three sources with distinct trust properties:

- **HACL*** (`csrc/hacl/`) — **PRIMARY** for covered primitives. Verified C
  extracted from Low* (F* subset) via KaRaMeL. Machine-checked proofs of
  functional correctness, memory safety, and constant-time execution. Covers
  SHA-256/512, ChaCha20, Poly1305, Keccak, HMAC, HKDF.

- **fiat-crypto** (`csrc/fiat/`) — **PRIMARY** for field arithmetic. Verified
  constant-time C extracted from Coq proofs. Covers Ed25519 and X25519 field
  operations. Used by BoringSSL, Go stdlib, Linux kernel.

- **CryptoGen** (`csrc/generated/`) — 18 C files generated from `.spec` files.
  **PRIMARY** for protocol formats, ML-KEM, AES-GCM, VRF, and any primitive
  not covered by HACL*/fiat-crypto. **RETAINED AS DIFFERENTIAL ORACLE** for
  all primitives — even those replaced by verified C. Three-way differential
  testing: verified C vs CryptoGen vs libsodium ensures all implementations
  agree.

- **fiat-crypto** (`csrc/fiat/`) — verified C generated from machine-checked
  Coq proofs for finite field arithmetic over GF(2^255-19). The same files are
  used by BoringSSL, the Go standard library, and the Linux kernel. They cover
  the field operations underlying X25519 and Ed25519. Files must be vendored
  following `csrc/fiat/VENDORING.md`.

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
    Coq proof → extracted C (no runtime F*/Coq dependency)

F* specs (test/evidence/formal-proofs/fstar/)
    |
    | functional correctness + memory safety + constant-time,
    |   proved via refinement lemmas, discharged by Z3
    v
HACL* (Low* → KaRaMeL → C)  ────────────────────────────────►  verified hash/AEAD C
    SHA-256, SHA-512, ChaCha20, Poly1305, Keccak/SHA-3,             oracle for csrc/generated/
    HMAC, HKDF

NIST / RFC standards
    |
    | manual encoding (two independent paths)
    |
    +──► .spec files (app/codegen/Specs/)
    |        |
    |        | CryptoGen.hs (mechanical, deterministic)
    |        v
    |    csrc/generated/*.c  ──────────────────────────────────►  active production C
    |        |
    +──► F* specs            differential testing (3-way):
             |                   Haskell oracle
             | ./uv verify        codegen C (active)
             v                   HACL* C / fiat-crypto (verified oracle)
         internal consistency
```

---

## 3. Primitive Coverage

| Primitive | Generated C file | HACL* oracle | fiat-crypto oracle | Coq proof |
|-----------|-----------------|:------------:|:------------------:|:---------:|
| SHA-256 | `sha256.c` | Yes (`Hacl_SHA2_256`) | — | — |
| SHA-512 | `sha512.c` | Yes (`Hacl_SHA2_512`) | — | — |
| ChaCha20 | `chacha20.c` | Yes (`Hacl_Chacha20`) | — | — |
| Poly1305 | `poly1305.c` | Yes (`Hacl_Poly1305_32`) | — | — |
| Keccak / SHA-3 | `keccak.c` | Yes (`Hacl_SHA3`) | — | — |
| HMAC | `hmac.c` | Yes (`Hacl_HMAC`) | — | — |
| HKDF | `hkdf.c` | Yes (`Hacl_HKDF`) | — | — |
| X25519 | `x25519.c` | — | Yes (`fiat_25519_64`) | `X25519DH.v` |
| Ed25519 (extended) | `ed25519extended.c` | — | Yes (`fiat_25519_64`) | `Ed25519*.v` (14 files) |
| AES-256 | `aes256.c` | No (Vale/AES-NI only) | — | — |
| ML-KEM-768 | `mlkem768.c` | No (FIPS 203 too recent) | — | — |
| PQ wrapper | `pqwrapper.c` | — | — | — |
| VRF (ECVRF) | `vrf.c` | — | Partial (field ops) | `VRFDLEQ.v` |
| Wire format | `wireformat.c` | — | — | — |
| Message format | `messageformat.c` | — | — | — |
| Network protocol | `networkprotocol.c` | — | — | — |
| Session state | `sessionstate.c` | — | — | — |
| Dandelion | `dandelion.c` | — | — | — |

**Summary:** After integration, 8 of the 18 generated C files have a formally
verified oracle. The remaining 10 are tested exclusively via differential
testing against the Haskell reference implementation and NIST/RFC Known Answer
Tests.

The three primitives with no verified oracle (AES-256, ML-KEM-768, and the
protocol format codecs) are structurally constrained:

- **AES-256**: Constant-time software AES requires hardware intrinsics (AES-NI).
  HACL* delegates this to Vale, which emits architecture-specific assembly.
  There is no portable verified C for AES-256 in either HACL* or fiat-crypto.
- **ML-KEM-768**: FIPS 203 was published in 2024. No public verified Low*
  implementation exists as of this writing. See `doc/hacl-evaluation.md` §2.
- **Protocol format codecs** (wire, message, network, session, dandelion):
  These are protocol-specific and have no upstream verified library counterpart.

---

## 4. Constant-Time Guarantees

| Source | CT guarantee | Mechanism |
|--------|-------------|-----------|
| fiat-crypto | By construction | Coq proof rules out data-dependent branches and variable-time memory access patterns in the field arithmetic |
| HACL* | By construction | Low* type system encodes CT as a type-level invariant; KaRaMeL preserves it on extraction; verified by the F* checker |
| CryptoGen | By discipline | `csrc/ct_helpers.h` provides branchless select (`ct_select32/64`), compare (`ct_eq32`), and swap (`ct_cswap32/64`, `ct_cmov32/64`). `csrc/constant_time.c` provides `constant_time_eq` with a `volatile` accumulator. The codegen quality gate (`./uv check`) enforces no data-dependent branches in generated files |

CryptoGen's CT guarantee is weaker than the other two: it is a coding
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

## 6. Assurance Implications

After full integration (HACL* and fiat-crypto vendored and wired):

- **8 of 18** generated C files will have a formally verified oracle with a
  machine-checked proof chain: fiat-crypto via Coq for field arithmetic;
  HACL* via F* for hash functions, AEAD stream cipher, MAC, and key derivation.

- **10 of 18** files (AES-256, ML-KEM-768, PQ wrapper, VRF, wire format,
  message format, network protocol, session state, dandelion, and the HKDF and
  HMAC passthrough wrappers for formats) are tested exclusively via differential
  testing against Haskell oracles validated on NIST/RFC Known Answer Tests.

- Differential testing uses a three-way comparison:
  ```
  Haskell oracle  vs  codegen C (active)  vs  HACL* / fiat-crypto (verified)
  ```
  Any pairwise disagreement is reported as a test failure. Because HACL* and
  fiat-crypto carry machine-checked proof chains, agreement on all test inputs
  empirically links the codegen C to formally verified implementations. This is
  strong evidence of correctness but not a formal equivalence proof; the gap is
  documented in `doc/hacl-evaluation.md` §4 and `doc/assurance-matrix.md`.

- The Coq proofs in `test/evidence/formal-proofs/coq/` (20 files including
  14 Ed25519 files, `X25519DH.v`, `VRFDLEQ.v`, and `StructuralProofs.v`)
  independently verify algebraic properties of the curve operations. These are
  separate from the fiat-crypto Coq proofs but cover the same mathematical
  structures, providing a second formal confirmation of correctness for the
  field arithmetic path.

### Current state vs. target state

| Component | Current state | Target state |
|-----------|--------------|--------------|
| `csrc/hacl/` sources | Not vendored (infrastructure only) | Vendored from `cryspen/hacl-packages` |
| `csrc/fiat/` sources | Not vendored (infrastructure only) | Vendored from `mit-plv/fiat-crypto` |
| `csrc/hacl/bridge_sha256.c` | Present (bridge model) | Present and active in build |
| Differential 3-way tests | 2-way (Haskell vs codegen C) | 3-way (+ HACL*/fiat-crypto) |

Vendoring instructions are in `csrc/hacl/VENDORING.md` and
`csrc/fiat/VENDORING.md`. After vendoring, the build system integration changes
are confined to the `c-sources` stanza in `UmbraVox.cabal` and the
`csrc/hacl/bridge_*.c` files; the Haskell FFI layer and test harness require no
modification.

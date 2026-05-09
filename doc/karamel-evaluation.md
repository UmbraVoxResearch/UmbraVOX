# KaRaMeL Extraction Evaluation for UmbraVOX

Evaluation date: 2026-05-08

## 1. What Is KaRaMeL?

KaRaMeL (formerly KreMLin) is a compiler from a subset of F* called Low* to C.
The pipeline works as follows:

1. **F* specification (Spec layer)**: Pure functional models of algorithms.
   These use sequences, natural numbers, and mathematical constructs. They are
   not extractable to C.

2. **Low* implementation**: An imperative subset of F* that uses stack- and
   heap-allocated buffers, machine integers, and explicit memory management via
   the `FStar.HyperStack` memory model. Low* code looks like C written in F*
   syntax. It is proven equivalent to the Spec layer via refinement lemmas.

3. **KaRaMeL extraction**: The Low* code is compiled to readable, idiomatic C.
   The generated C is guaranteed to be memory-safe and functionally correct by
   construction, because the F* type checker has already verified those
   properties before extraction.

The critical distinction: **Spec-level F* cannot be extracted by KaRaMeL.**
KaRaMeL operates exclusively on Low* code. A Spec module and a Low* module are
two separate artifacts connected by an equivalence proof.

The canonical example of this pipeline is HACL*, which contains Spec-level
specifications of cryptographic primitives alongside Low* implementations that
are proven equivalent to those specs and then extracted to C via KaRaMeL.

## 2. Current F* Spec State

### 2.1 Summary

UmbraVOX contains 17 F* specification files under
`test/evidence/formal-proofs/fstar/`. All are handwritten formal models. All
are green under the active F* + Z3 toolchain via `make verify`.

### 2.2 Are They Written in the Low* Subset?

**No.** Every specification uses constructs that are outside the Low* fragment:

- `FStar.Seq` (immutable functional sequences, not extractable buffers)
- `FStar.UInt8`, `FStar.UInt32`, `FStar.UInt64` at the specification level
  (these types are shared with Low*, but the surrounding code uses them in a
  purely functional style with `Seq` rather than with stack/heap buffers)
- Pure recursive functions over sequences (e.g., `process_blocks` in
  Spec.Poly1305, `process_blocks_count` in Spec.SHA256)
- `nat` and `pos` types for unbounded arithmetic (e.g., field elements in
  Spec.Poly1305, Spec.X25519, Spec.Ed25519)
- `Seq.create`, `Seq.append`, `Seq.slice`, `Seq.snoc` -- functional sequence
  operations with no C equivalent
- `List.Tot` for constant tables
- `assert_norm` for compile-time normalization

These are all standard for F* Spec-level code. They are not defects. Spec-level
code is *supposed* to be mathematical and readable, not imperative. But they
are categorically outside what KaRaMeL can extract.

### 2.3 Assume Statements

The specs contain 217 total `assume` occurrences across 17 files, plus 13
`assume val` declarations. Distribution by category:

| Category | Files | assume count | assume val count |
|---|---|---|---|
| Hash functions | SHA-256, SHA-512, Keccak | 5 + 4 + 24 = 33 | 0 |
| Symmetric ciphers | AES-256, ChaCha20, GCM, Poly1305 | 14 + 11 + 26 + 7 = 58 | 11 + 1 = 12 |
| Key derivation | HMAC, HKDF | 5 + 6 = 11 | 0 |
| Asymmetric | X25519, Ed25519, ML-KEM-768 | 32 + 35 + 15 = 82 | 1 |
| Field arithmetic | GaloisField | 8 | 0 |
| Protocols | X3DH, PQXDH, Double Ratchet, Noise IK | 5 + 4 + 7 + 6 = 22 | 0 |

Every `assume` is either:

- A KAT vector assertion that would require full symbolic normalization of the
  algorithm (prohibitively slow in the F* type checker)
- A length/bound obligation that has not been fully discharged
- An abstract function placeholder (e.g., `bitwise_and` in Spec.Poly1305,
  quarter-round operations in Spec.ChaCha20)

Each of these would need to be eliminated before any extraction path -- whether
KaRaMeL or hand-written C with equivalence proofs -- could claim full formal
linkage.

### 2.4 Library Dependencies

All specs depend on standard `FStar.*` library modules:

- `FStar.Seq` (universal)
- `FStar.UInt8`, `FStar.UInt32`, `FStar.UInt64` (most specs)
- `FStar.Mul` (most specs)
- `FStar.Int.Cast` (SHA-256, GCM, others)
- `FStar.List.Tot` (constant tables)

These are standard F* library dependencies. They are not obstacles to
verification, but they are further confirmation that the code lives at the Spec
layer, not the Low* layer.

## 3. Extractability Assessment

### 3.1 Hash Functions (SHA-256, SHA-512, Keccak)

**Current state:** Pure functional specifications using `FStar.Seq`. SHA-256
is the most complete, with explicit schedule computation, compression rounds,
padding, and serialization. Keccak has 24 `assume` statements, the highest
among hash specs.

**KaRaMeL extractability:** Not directly extractable. Would require writing a
separate Low* implementation module (e.g., `Hacl.SHA256.fst`) that uses
`FStar.Buffer` or `LowStar.Buffer`, `FStar.HyperStack`, stack allocation, and
imperative loops. The Low* module would then need a refinement proof showing
functional equivalence to the existing Spec.

**HACL* precedent:** HACL* already contains verified Low* implementations of
SHA-256, SHA-512, and SHA-3/Keccak that extract to C. These could potentially
be reused directly rather than writing new Low* code from scratch.

**Effort estimate:** 2-4 person-weeks per primitive if writing from scratch.
Significantly less if adopting HACL* implementations directly.

### 3.2 Symmetric Ciphers (AES-256, ChaCha20, GCM, Poly1305)

**Current state:** AES-256 has a complete S-box, key expansion, and
cipher/inverse cipher specification. ChaCha20 has 11 `assume val` declarations
for quarter-round operations, making it effectively a skeleton with axiomatized
core operations. GCM is the most assumption-heavy symmetric spec (26 assumes).
Poly1305 uses unbounded natural number arithmetic (`nat`) for field elements.

**KaRaMeL extractability:** Same fundamental obstacle -- all are Spec-level
code. Additional complications:

- ChaCha20's `assume val` declarations mean even the spec-level semantics are
  not fully defined; Low* code would need to fill in the axiomatized functions
- Poly1305 uses `nat` for GF(2^130-5) arithmetic; Low* would need multi-limb
  representations (typically 5x26-bit or 5x51-bit limbs)
- GCM composes AES block encryption with GHASH; a Low* version would need both
  AES and GF(2^128) Low* implementations as dependencies
- AES-256 S-box lookup tables could be a constant-time concern (cache timing)

**HACL* precedent:** HACL* contains verified Low* implementations of ChaCha20,
Poly1305, and ChaCha20-Poly1305 AEAD. It does not contain AES or AES-GCM due
to the difficulty of writing constant-time AES in software (HACL* relies on
AES-NI hardware intrinsics via Vale/EverCrypt for AES).

**Effort estimate:** ChaCha20 + Poly1305: 2-3 person-weeks (or adopt HACL*).
AES-256 + GCM: 4-8 person-weeks and fundamentally harder due to constant-time
constraints.

### 3.3 Asymmetric Primitives (X25519, Ed25519, ML-KEM-768)

**Current state:** X25519 and Ed25519 are the most assumption-heavy specs (32
and 35 assumes respectively). They use unbounded `nat` for field arithmetic
mod p = 2^255 - 19. ML-KEM-768 uses polynomial arithmetic over Zq[X]/(X^256+1)
with 15 assumes.

**KaRaMeL extractability:** These are the hardest primitives to bring to Low*:

- X25519/Ed25519 require multi-limb big-integer representations with carry
  propagation, modular reduction, and carefully structured multiplication
  (e.g., schoolbook or Karatsuba). HACL* uses 5x51-bit limbs for Curve25519.
- Ed25519 additionally requires extended coordinate point arithmetic, scalar
  multiplication, and RFC 8032 encoding/decoding.
- ML-KEM-768 requires NTT-domain polynomial arithmetic, which is feasible in
  Low* but would be a significant engineering effort with no existing HACL*
  precedent for FIPS 203.

**HACL* precedent:** HACL* contains verified Low* implementations of X25519
and Ed25519. No existing HACL* implementation of ML-KEM-768 (FIPS 203 is too
recent for the current HACL* codebase).

**Effort estimate:** X25519: 3-5 person-weeks (or adopt HACL*). Ed25519: 4-6
person-weeks (or adopt HACL*). ML-KEM-768: 8-16 person-weeks with no existing
precedent to draw from.

### 3.4 Protocols (X3DH, PQXDH, Double Ratchet, Noise IK)

**Current state:** Protocol-level specifications that compose cryptographic
primitives. Relatively light on assumes (4-7 each).

**KaRaMeL extractability:** Protocol-level code is almost never extracted via
KaRaMeL in practice. Reasons:

- Protocols involve state machines, I/O, network communication, and
  application-level logic that do not fit the Low* memory model well
- The value of verified extraction is highest for the leaf cryptographic
  primitives, not the compositions
- HACL* does not extract protocol-level code; EverCrypt provides a
  multiplexing API over primitive implementations
- Protocol correctness is better addressed through computational security
  proofs (e.g., in CryptoVerif or ProVerif) rather than extraction

**Recommendation:** Do not pursue KaRaMeL extraction for protocol specs. Keep
them as Spec-level formal models.

## 4. Prerequisites for Extraction

### 4.1 What Would Need to Change in the Specs

The existing Spec files would **not change**. They would remain as-is and
serve as the reference specifications. What would be needed is:

1. **New Low* implementation modules** for each target primitive (e.g.,
   `Hacl.SHA256.fst`, `Hacl.ChaCha20.fst`). These are separate files that
   import the Spec modules and prove equivalence.

2. **Refinement proofs** showing that each Low* implementation computes the
   same function as the corresponding Spec module on all inputs.

3. **Reduction of `assume` statements** in the Spec layer. Assumes in the Spec
   layer propagate as trusted assumptions through any refinement proof. KAT
   assumes are tolerable (they are validated by runtime tests), but structural
   assumes and `assume val` declarations would weaken the extraction guarantee.

4. **Memory management annotations** in the Low* layer: `FStar.HyperStack`
   framing lemmas, buffer liveness invariants, modifies clauses.

### 4.2 Tooling Requirements

The following would need to be added to the nix-shell environment:

| Tool | Purpose | Current status |
|---|---|---|
| F* | Type checker and verifier | Already in nix-shell |
| Z3 | SMT backend for F* | Already in nix-shell |
| KaRaMeL | Low* to C compiler | Not in nix-shell |
| HACL* library | Low* crypto implementations and Spec compatibility | Not in nix-shell |
| clang/gcc | C compilation of extracted output | Likely already available |
| Vale | x86/ARM assembly for AES-NI, if hardware AES is desired | Not in nix-shell |

### 4.3 Estimated Total Effort

| Primitive | Low* from scratch | Adopt HACL* | Notes |
|---|---|---|---|
| SHA-256 | 2-4 weeks | 1-2 weeks | Well-understood; HACL* version mature |
| SHA-512 | 2-4 weeks | 1-2 weeks | Analogous to SHA-256 |
| Keccak | 3-5 weeks | 1-2 weeks | HACL* has verified Keccak |
| ChaCha20 | 2-3 weeks | 1 week | HACL* version is mature |
| Poly1305 | 2-3 weeks | 1 week | HACL* version is mature |
| AES-256 | 4-8 weeks | N/A | No HACL* software AES; need Vale for AES-NI |
| GCM | 3-5 weeks | N/A | Depends on AES; same Vale constraint |
| X25519 | 3-5 weeks | 1-2 weeks | HACL* Curve25519 is mature |
| Ed25519 | 4-6 weeks | 1-2 weeks | HACL* Ed25519 is mature |
| ML-KEM-768 | 8-16 weeks | N/A | No existing verified Low* implementation |
| HMAC | 1-2 weeks | 1 week | Generic over hash; straightforward |
| HKDF | 1-2 weeks | 1 week | Generic over HMAC; straightforward |
| **Total** | **35-63 weeks** | **~12-17 weeks** (partial) | HACL* path excludes AES, GCM, ML-KEM |

These are estimates for a developer experienced with F*/Low*/KaRaMeL. For a
team learning the toolchain, multiply by 2-3x.

## 5. Recommendation

### Should UmbraVOX Pursue KaRaMeL Extraction?

**Not as a near-term priority.** The reasoning:

1. **The specs are models, not implementations.** The entire existing F* corpus
   is Spec-level code. KaRaMeL extraction requires a second, separate Low*
   implementation layer that does not exist yet. The specs would provide the
   reference target, but the actual extraction work would be writing new code
   from scratch.

2. **The assurance roadmap already identifies a more practical path.** The
   `assurance-roadmap.md` document correctly identifies the near-term priority
   as replacing generated C link probes with real hand-written C semantic
   implementations (item F1 in TODO), then adding differential testing against
   the Haskell oracle. This delivers independent semantic diversity without the
   toolchain complexity of KaRaMeL.

3. **HACL* adoption is a middle path worth considering.** Rather than writing
   Low* from scratch or hand-writing C, UmbraVOX could adopt HACL*'s
   already-extracted C for the primitives HACL* covers (SHA-2, Keccak,
   ChaCha20, Poly1305, X25519, Ed25519). This would give verified C with
   minimal effort. The gap would be AES-256, GCM, and ML-KEM-768.

4. **ML-KEM-768 has no verified extraction precedent.** FIPS 203 is recent
   enough that no public verified Low* implementation exists. UmbraVOX would
   be pioneering work here, which is high-risk for a messaging application.

5. **The `assume` debt matters.** With 217 assumes across the spec corpus,
   the formal layer is not yet strong enough to serve as a trustworthy
   extraction source. Reducing assumes in the Spec layer is a prerequisite
   for either the KaRaMeL path or any path that claims formal linkage.

### Recommended Path

1. **Near-term (MVP + 1):** Hand-write C implementations for the core
   primitives. Differentially test against the Haskell oracle. This is the
   F1 path already identified in the roadmap.

2. **Medium-term:** Evaluate HACL* adoption for primitives where HACL* already
   has mature, extracted C. This would replace hand-written C for those
   primitives with formally verified C at modest integration cost.

3. **Long-term:** If the project reaches a point where publication-grade formal
   assurance is required, invest in Low* implementations for the remaining
   primitives (AES-256, GCM, ML-KEM-768) and complete the KaRaMeL pipeline.

4. **Continuous:** Reduce `assume` statements in the Spec layer regardless of
   which extraction path is chosen. This strengthens the formal model and is
   prerequisite to any path.

## 6. Comparison Table

| Dimension | KaRaMeL extraction path | Hand-written C path (F1) |
|---|---|---|
| **What gets produced** | Verified C extracted from Low* F* code | Hand-written C tested against Haskell oracle |
| **Formal guarantee** | Memory safety and functional correctness by construction | No formal guarantee; relies on differential testing |
| **Prerequisite work** | Write entire Low* layer (~35-63 person-weeks); reduce assumes; add KaRaMeL to toolchain | Write C implementations; add differential test harness |
| **Estimated effort** | 35-63 person-weeks from scratch; 12-17 weeks partial via HACL* | 8-16 person-weeks for core primitives |
| **Toolchain complexity** | High: F*, Z3, KaRaMeL, possibly Vale for AES-NI | Low: standard C compiler, existing test infrastructure |
| **Constant-time evidence** | Strong: Low* type system enforces secret independence | Weak unless manually audited or tested with tools like ctgrind |
| **Coverage of ML-KEM-768** | No existing precedent; pioneering effort | Straightforward to implement and test |
| **Coverage of AES/GCM** | Requires Vale for constant-time AES-NI; no pure-software HACL* path | Can use platform AES-NI intrinsics directly |
| **Maintenance burden** | Must maintain F*, Z3, KaRaMeL toolchain versions in lockstep | Standard C maintenance |
| **HACL* hybrid option** | Adopt HACL* extracted C for covered primitives | Hand-write all primitives |
| **Matches current roadmap** | Identified as post-MVP option | Identified as near-term next step (F1) |
| **Risk** | High: large effort, unfamiliar toolchain, no ML-KEM precedent | Moderate: standard engineering, no formal proof |
| **When to choose** | When publication-grade formal assurance is a hard requirement | When independent semantic diversity is the goal |

## 7. Summary

The UmbraVOX F* specifications are well-structured Spec-level formal models.
They follow HACL* naming conventions and use standard F* library modules. They
are not Low* implementations and cannot be extracted to C via KaRaMeL in their
current form.

Extraction would require writing a complete second layer of Low* implementation
code, proving it equivalent to the existing Spec layer, and integrating the
KaRaMeL toolchain. This is a substantial engineering investment that is not
justified for the current MVP phase.

The more practical near-term path is hand-written C with differential testing
(the F1 path already in the roadmap), potentially supplemented by adopting
HACL*'s already-verified C for primitives where HACL* coverage exists. The
KaRaMeL path should remain a documented long-term option for when the project's
assurance requirements justify the investment.

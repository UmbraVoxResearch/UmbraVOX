# HACL* Adoption Evaluation (M6.3.5 + M6.3.6)

Evaluation date: 2026-05-08

## 1. What Is HACL*

HACL* is a formally verified cryptographic library written in Low* (a subset
of F*) and extracted to C via KaRaMeL. It provides constant-time, memory-safe
C implementations of common cryptographic primitives with machine-checked
proofs of functional correctness.

The HACL* pipeline:

1. **F* Spec layer** -- Pure functional models of algorithms (mathematical,
   not extractable).
2. **Low* implementation** -- Imperative F* code using stack/heap buffers and
   machine integers. Proven equivalent to the Spec layer via refinement
   lemmas.
3. **KaRaMeL extraction** -- Low* compiled to readable, idiomatic C.
   Memory safety and functional correctness are guaranteed by construction.

The extracted C files are the deliverable. They are ordinary C that can be
compiled with any standard C compiler, with no runtime dependency on F* or
KaRaMeL.

## 2. Coverage Assessment

| Primitive | HACL* Has? | Maturity | Notes |
|---|---|---|---|
| SHA-256 | Yes | Mature | Spec + Low* + extracted C |
| SHA-512 | Yes | Mature | Same pipeline as SHA-256 |
| Keccak/SHA-3 | Yes | Mature | Includes SHAKE variants |
| ChaCha20 | Yes | Mature | Scalar and vectorized (128/256-bit) variants |
| Poly1305 | Yes | Mature | Multi-limb implementation (32-bit and 64-bit) |
| X25519 | Yes | Mature | 5x51-bit limb representation |
| Ed25519 | Yes | Mature | Extended coordinates |
| AES-256 | No | N/A | Only via Vale/AES-NI (hardware intrinsics) |
| AES-GCM | No | N/A | Same constraint; requires Vale for constant-time |
| HMAC | Yes | Generic | Over any HACL*-supported hash |
| HKDF | Yes | Generic | Over HMAC |
| ML-KEM-768 | No | N/A | FIPS 203 too recent; no public verified Low* implementation |

HACL* covers 9 of the 12 UmbraVOX primitives. The three gaps (AES-256,
AES-GCM, ML-KEM-768) are structurally difficult:

- **AES-256 / AES-GCM**: Constant-time software AES is impractical due to
  table-lookup timing channels. HACL* delegates AES to Vale, which emits
  verified x86/ARM assembly using AES-NI hardware instructions. This is
  correct but architecture-specific.
- **ML-KEM-768**: FIPS 203 was published in 2024. No public verified Low*
  implementation exists as of this evaluation.

## 3. Adoption Options

### Option A: Replace generated C with HACL* extracted C

Use HACL*'s already-extracted `.c` files as the active C implementation for
covered primitives. Keep codegen C for AES-256, AES-GCM, and ML-KEM-768.

Advantages:

- Immediate access to formally verified, constant-time C.
- No need to write Low* code ourselves.
- HACL* C files are self-contained and portable.

Disadvantages:

- Requires integrating HACL* headers and build system into the Cabal/Nix
  pipeline.
- HACL* API signatures may differ from the current generated C API; FFI
  wrappers would need adaptation.
- Creates a dependency on the HACL* release cadence for updates.
- Does not cover AES-256, AES-GCM, or ML-KEM-768.

### Option B: Reference HACL* as comparison oracle

Keep codegen C as the active implementation. Use HACL* extracted C as a
second oracle alongside the Haskell oracle for differential testing.

Three-way comparison: Haskell (reference) vs codegen C (active) vs HACL* C
(verified oracle).

Advantages:

- No changes to the current build pipeline or FFI boundary.
- Adds a formally verified third data point to differential testing.
- Can be adopted incrementally, one primitive at a time.
- Compatible with the existing assurance roadmap.

Disadvantages:

- Does not give the active C path constant-time or formal guarantees.
- HACL* is a test dependency only, not a production component.
- Still requires HACL* to be available in the build environment.

### Recommendation

**Option B (comparison oracle) for now**, with Option A as a documented
upgrade path for when constant-time properties become a hard requirement.

Rationale:

1. The current assurance roadmap prioritizes independent semantic diversity
   (Category 3 in `assurance-roadmap.md`), not proof-linked production
   semantics (Category 4). Option B serves Category 3 directly.
2. The codegen C path is already wired, tested, and understood. Replacing it
   with HACL* C would be a larger integration effort with limited near-term
   assurance gain beyond what differential testing already provides.
3. Option A remains viable as a future step. The HACL* coverage table above
   and the build integration notes below are sufficient to execute it when
   the project reaches that stage.

### Integration path for Option B

1. Add HACL* extracted C sources to `nix-shell` or as a Nix derivation.
2. Write thin C test harnesses that call HACL* functions with the same test
   vectors used for the codegen C differential tests.
3. Compare HACL* output against both Haskell and codegen C output.
4. Report any three-way disagreements as test failures.
5. No changes to `Makefile`, `cabal`, or production FFI wiring needed.

## 4. F*-to-C Correspondence Argument (M6.3.6)

### Current correspondence chain

The UmbraVOX codebase has three parallel representations of each
cryptographic primitive. They are connected by informal correspondence, not
by machine-checked refinement proofs.

```
NIST / RFC standard (authoritative definition)
    |
    | (manual encoding, two independent paths)
    |
    +---> .spec file (custom DSL)
    |         |
    |         | (CryptoGen.hs -- mechanical translation)
    |         v
    |     Generated C code
    |         |
    |         | (differential testing)
    |         v
    |     Verified against Haskell oracle
    |
    +---> F* formal model (handwritten, same algorithms)
              |
              | (F* type checker + Z3)
              v
          Internal consistency verified (make verify)
```

### What this chain establishes

1. **The .spec files encode the same algorithms as the F* specs.** Both were
   written by the same author from the same NIST/RFC sources. They use the
   same variable names, round structures, and constant tables. This is an
   informal but inspectable correspondence.

2. **The codegen mechanically translates .spec to C.** The `CryptoGen.hs`
   code generator is deterministic. Given the same `.spec` input, it
   produces the same C output. The translation is syntactic and auditable.

3. **Differential testing verifies C output matches Haskell.** The test
   suite runs both the Haskell reference implementation and the generated C
   (via FFI) on NIST/RFC test vectors and random inputs, then compares byte
   outputs. Any semantic divergence is caught.

4. **The Haskell implementations match NIST test vectors.** The Haskell
   oracle is validated against authoritative Known Answer Tests from NIST
   CAVP and RFC appendices.

5. **The F* specs model the same algorithms.** The F* formal models are
   internally consistent (all green under `make verify`). They model the
   same algorithms as the Haskell and C paths, using the same constants,
   round structures, and data flows.

### What this chain does NOT establish

The chain does **not** include a machine-checked proof that the `.spec` to C
path produces semantically equivalent output to the F* spec. The gap is:

```
F* spec  <--- no formal link --->  .spec file  ---> Generated C
```

The `.spec` file and the F* spec were written independently (though from the
same standard). There is no tool or proof that they define the same function
on all inputs. The differential testing provides strong empirical evidence of
agreement, but it is not a proof.

This gap is documented in `doc/assurance-matrix.md` under "Standards
Provenance" and in the "Assumption gaps" column of the Primitive Matrix.

### Strengthening options (ordered by increasing effort)

1. **Shared test vectors in F* specs.** Add the same NIST/RFC KAT vectors
   used in the Haskell and C test suites as `assert` lemmas in the F*
   specs. If F* can normalize the spec function on those inputs to the
   expected output, this confirms the F* model agrees with the test suite on
   those points. Some KAT lemmas are already present (assumption-backed);
   converting them to fully discharged lemmas strengthens the link.

2. **Three-way HACL* differential testing.** Use HACL* extracted C as a
   formally verified third oracle (Option B above). If HACL* output agrees
   with both the Haskell oracle and the codegen C on all test inputs, and
   HACL* has a machine-checked proof chain from F* spec to extracted C, then
   the codegen C is empirically equivalent to a formally verified
   implementation.

3. **Structural audit of .spec vs F* alignment.** A manual, documented
   audit comparing each `.spec` file against its corresponding F* spec,
   confirming that the algorithm structure, constants, and data flow match.
   This is not a proof but is stronger than the current implicit
   correspondence.

4. **KaRaMeL extraction path.** Write Low* implementations of UmbraVOX
   primitives, prove them equivalent to the existing F* specs via
   refinement lemmas, and extract them to C via KaRaMeL. This replaces
   the `.spec` to C path entirely with a machine-checked F* to C path.
   See `doc/attic/karamel-evaluation.md` for the full cost/benefit analysis;
   estimated effort is 35-63 person-weeks from scratch or 12-17 weeks
   partial via HACL* adoption.

### Current recommendation

Pursue options 1-3 in the near term. These are incremental, compatible with
the existing build pipeline, and collectively provide a strong (though not
formally complete) correspondence argument. Option 4 remains a documented
long-term path for when publication-grade formal assurance is required.

## 5. Summary

HACL* covers 9 of the 12 UmbraVOX primitives with mature, formally verified
C implementations. The three gaps (AES-256, AES-GCM, ML-KEM-768) have no
existing HACL* coverage due to structural constraints.

The recommended near-term adoption is as a **comparison oracle** for
differential testing (Option B), not as a replacement for the codegen C path.
This adds a formally verified data point to the existing test suite without
disrupting the build pipeline.

The F*-to-C correspondence argument is currently informal: the F* specs and
the `.spec` files were written from the same standards, the codegen is
mechanical, and differential testing confirms byte-level agreement on test
vectors. The gap -- no machine-checked proof linking F* to `.spec` -- is
documented and can be narrowed through shared KAT vectors, HACL* three-way
testing, and structural audit. Full closure requires the KaRaMeL extraction
path, which is a long-term option.

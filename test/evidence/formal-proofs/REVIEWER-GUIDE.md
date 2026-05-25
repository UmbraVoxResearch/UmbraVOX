# Formal Verification Reviewer Guide

This document helps an independent reviewer understand and reproduce the
formal verification evidence in UmbraVOX.

**Current baseline:** 0 `admit()`, 25 `assume val` across 24 F\* specs,
444 Coq `Qed` across 13 verified files, 67 infrastructure tests.

---

## Quick Start

```bash
# Enter the reproducible tool environment
nix-shell shell.nix

# Hygiene checks only (seconds)
./uv assurance-fast

# Full evidence suite (minutes)
./uv assurance
```

`./uv assurance-fast` runs grep-based invariant checks (no admit, assumption
counts, Qed counts) and is suitable for CI or a first-pass audit.

`./uv assurance` additionally runs F\* type-checking, Coq compilation, and
the Haskell infrastructure test suite.

---

## Tool Requirements

| Tool               | Version                        | Purpose                              |
|--------------------|--------------------------------|--------------------------------------|
| F\*                | 2026.03.24\~dev                | SMT-backed verification via Z3       |
| Z3                 | 4.16.0                         | SMT solver (backend for F\*)         |
| Rocq/Coq           | 9.1.1 (stdlib 9.0.0)          | Kernel-checked computational proofs  |
| GHC                | 9.14.1                         | Haskell compiler                     |
| Cabal              | (bundled with GHC)             | Haskell build/test runner            |

All tools are provided by `nix-shell shell.nix`. No manual installation is
required.

---

## Why Two Proof Systems

UmbraVOX uses both F\* and Coq because each covers ground the other cannot.

**F\* (refinement types + SMT)**
- Good for protocol composition, type-level invariants, and KAT vectors
  via `assert_norm`.
- Z3 handles linear arithmetic, bitvectors, and uninterpreted functions
  well.
- Limited by Z3's inability to do nonlinear arithmetic over large finite
  fields (e.g., 2^255 - 19).

**Coq (kernel-checked computation)**
- Good for primality certificates (`vm_compute` on 255-bit numbers), field
  arithmetic, and ring/field tactics.
- Proofs are checked by a small trusted kernel, not an external solver.
- Used for evidence that Z3 cannot discharge: primality of curve primes,
  Fermat-witness computations, field-order verification.

The two systems are complementary, not redundant. F\* covers protocol-level
properties; Coq covers number-theoretic foundations.

---

## Interpreting the 25 assume val Declarations

The 25 `assume val` declarations are **not** 25 independent holes in the
verification. They collapse into fewer independent trust roots. See
`ASSUMPTION-GRAPH.md` for the full dependency analysis.

| Category                        | Count | Notes                                         |
|---------------------------------|-------|-----------------------------------------------|
| Cryptographic hardness          | 7     | Unprovable without an adversary model          |
| X25519 curve arithmetic         | 3     | DH commutativity + prime_is_prime              |
| Ed25519 algebraic geometry      | 9     | Need Coq ring/field tactics to discharge       |
| Cross-toolchain boundary (F\*/GHC) | 6  | Semantic gap between F\* specs and Haskell runtime |

The cryptographic-hardness assumptions are inherently unprovable in a
proof assistant (they are conjectured hardness of mathematical problems).
The curve-arithmetic and algebraic-geometry assumptions have known discharge
paths via Coq. The cross-toolchain assumptions reflect the gap between
F\*'s specification language and GHC's runtime semantics.

---

## Claims This Repository Does NOT Make

To set correct expectations for an audit:

- **Does not prove cryptographic hardness unconditionally.** The security
  of X25519/Ed25519/MLKEM768 rests on conjectured hardness of ECDLP/MLWE.
- **Does not prove real-world operational security.** Timing attacks,
  side-channel leakage, and memory safety of the compiled binary are out
  of scope.
- **Does not prove all Haskell runtime behavior inside F\*.** The 6
  cross-toolchain `assume val` declarations represent an explicit trust
  boundary.
- **Does not claim Coq evidence automatically removes F\* assumptions.**
  Each discharge must be reviewed individually for semantic alignment.
- **Does not claim Ed25519 group law is fully proved.** `Ed25519GroupLaw.v`
  is a DRAFT; `Ed25519GroupUniversal.v` proves universal group-law properties
  over GZnZ field structure, but full curve-point universality remains open.
- **Does not claim MLKEM768 functional correctness.** The F\* spec covers
  parameter validation only, not the full encapsulation/decapsulation model.
- **Does not claim the primality of 2^255 - 19 is formally proved in Coq.**
  Pocklington conditions are verified computationally, but the theorem
  formalization is absent from the Coq stdlib.

---

## How to Audit the Trust Boundary

A recommended audit workflow:

1. **Run `./uv assurance-fast`** -- verifies hygiene invariants (zero
   admits, expected assumption count, expected Qed count). Takes seconds.

2. **Read `ASSURANCE-MATRIX.md`** -- per-module verification status, showing
   which modules are fully verified, partially verified, or stub-only.

3. **Read `ASSUMPTION-GRAPH.md`** -- dependency clusters and independent
   trust roots. This shows how assumptions relate and which clusters can
   be discharged together.

4. **Check `ASSUMPTIONS.md`** -- the full ledger of all `assume val`
   declarations with categories, justifications, and discharge paths.

5. **Run `./uv assurance`** -- the full reproducible evidence suite. This
   type-checks all F\* specs, compiles all Coq proofs, and runs the
   Haskell infrastructure tests.

After step 5, the reviewer has confirmed that the stated baseline holds
on their own machine.

---

## Known Limitations

- **High z3rlimit in some F\* specs.** Some modules require `z3rlimit`
  up to 200000. This means Z3 may take significant time on those modules,
  and verification time is sensitive to Z3 version.

- **Coq build time.** The Coq proofs require approximately 30 seconds to
  build, dominated by `vm_compute` on 255-bit Fermat witnesses.

- **MLKEM768 is parameter-validation only.** The MLKEM768 spec contains
  stubs, not a functional model of encapsulation/decapsulation.

- **ChaCha20 KAT placeholder.** `kat_block` is a placeholder; the real
  KAT was added as `kat_block_rfc8439` and currently validates only the
  first 4 bytes of the RFC 8439 test vector.

- **Primality certificate script timeout.** The `runghc` primality
  certificate script times out on the host machine; it runs successfully
  inside the Nix VM environment.

<!-- SPDX-License-Identifier: Apache-2.0 -->

# DO-178C Coverage Analysis — HPC Expression Coverage

**Standard:** DO-178C DAL A
**Tool:** GHC HPC (Haskell Program Coverage)
**Date of initial run:** 2026-05-12
**Test suite:** umbravox-test `required` tag (fast messaging-MVP gate)
**Report:** `dist-newstyle/.../hpc/vanilla/html/hpc_index.html`

---

## 1. What DO-178C MC/DC Requires

Modified Condition/Decision Coverage (MC/DC) is the structural coverage
criterion mandated by DO-178C for DAL A software. It requires:

1. Every decision (compound Boolean expression) must have been evaluated
   to both true and false outcomes.
2. Every condition (atomic Boolean sub-expression) within a decision must
   independently affect the decision outcome — i.e., by toggling that one
   condition while holding all others constant, the decision outcome changes.
3. Every entry and exit point of every function must be covered.

MC/DC is stricter than branch coverage (which only requires each branch
to be taken and not taken at least once) because it requires demonstrating
independence of each condition within a decision.

---

## 2. What HPC Provides

GHC's HPC tool instruments Haskell source code at the expression level and
reports three coverage categories per module:

| HPC metric              | Meaning                                                      |
|-------------------------|--------------------------------------------------------------|
| Top-level declarations  | Percentage of top-level functions/definitions exercised      |
| Alternatives used       | Percentage of case branches, guards, and pattern-match arms  |
| Expressions used        | Percentage of expression nodes in the AST that were reduced  |

HPC's **alternatives used** metric maps directly onto branch coverage for
Haskell pattern matching and guards. This is the closest available analog
to DO-178C structural coverage for a purely functional language compiled by
GHC; no mature MC/DC-precise tool exists for Haskell as of GHC 9.6.

HPC's **expressions used** metric is even finer-grained: it tracks each
syntactic expression node, catching dead code within a branch that branch
coverage would miss.

---

## 3. Gap Analysis: HPC vs True MC/DC

### 3.1 Where HPC is equivalent to MC/DC

For Haskell code, the following constructs are fully equivalent between
HPC coverage and MC/DC:

- **Simple guards**: `| cond = expr` — HPC tracks each guard and whether
  it was both True and False. This is identical to MC/DC for single-condition
  decisions.
- **Pattern match alternatives**: Each arm of a `case` expression is a
  decision. HPC requires each arm to be reached, equivalent to branch
  coverage and MC/DC for single-outcome decisions.
- **Single-condition `if` expressions**: `if b then x else y` with a
  simple `b` maps directly to MC/DC.

### 3.2 Where HPC falls short of MC/DC

- **Compound Boolean guards**: `| cond1 && cond2 = expr` — HPC tracks
  whether the guard was entered (True) or fell through (False), but does
  not independently vary `cond1` and `cond2`. A dedicated condition table
  is needed to demonstrate independence.
- **Short-circuit evaluation**: `(&&)` and `(||)` short-circuit; the
  second operand may never be evaluated even when the guard is reached.
  HPC's expression coverage will show this as an uncovered expression node,
  flagging it for review.
- **`if` conditions within expressions**: `if` used in the middle of an
  expression tree; HPC tracks each sub-expression but not individual
  conditions within a compound predicate in the sub-expression.

### 3.3 Haskell-specific mitigations

Haskell's type system and purity eliminate whole classes of MC/DC-relevant
defects common in C:

- No pointer aliasing: decisions involving pointer equality do not arise.
- Exhaustive pattern matching enforced by `-Wall` and `-Wincomplete-uni-patterns`:
  unhandled cases are compile-time errors, not runtime gaps.
- Referential transparency: a condition `f x` has a deterministic value;
  side effects cannot change its meaning between evaluations.

These properties reduce (but do not eliminate) the number of cases where
full MC/DC analysis adds value beyond what HPC provides.

---

## 4. Plan for Closing the Gap

### Phase 1 (current): HPC expression coverage baseline

- Run `make mcdc-report` to generate the per-module expression coverage report.
- Target: all crypto and protocol modules reach ≥ 90% expression coverage.
- Modules below 90% are listed in section 5.

### Phase 2: Manual condition tables for complex guards

For each function containing a compound Boolean guard or `&&`/`||` chain
in the assurance boundary (crypto and protocol modules), produce a condition
table documenting:

- The decision expression
- Each condition within it
- The test cases that independently toggle each condition

These tables live in `test/evidence/condition-tables/` and are referenced
by the assurance matrix in `doc/assurance-matrix.md`.

Priority: functions flagged by HPC as having "always True" or "always False"
guards — these are dead-condition candidates requiring additional test cases.

### Phase 3: Tool evaluation

Evaluate whether GHC's `-fprof-auto` combined with external analysis, or
a separate Haskell instrumentation framework, can provide condition-level
tracing sufficient for automated MC/DC verification. This is tracked in
the assurance roadmap (`doc/assurance-roadmap.md`).

---

## 5. Initial HPC Report: Crypto Module Coverage

Run: `make mcdc-report` (2026-05-12, `required` test suite)

All percentages are **expression coverage** (finest-grained HPC metric).
Modules with < 90% expression coverage are marked with a gap note.

### 5.1 Core crypto primitives (library modules)

| Module                          | Expr % | Alts % | Decls % | Gap note                                      |
|---------------------------------|--------|--------|---------|-----------------------------------------------|
| UmbraVox.Crypto.AES             | 98%    | 73%    | 100%    | 7 alternatives not taken (error/edge paths)   |
| UmbraVox.Crypto.BIP39           | 99%    | 50%    | 100%    | 1 alternative not taken (empty-wordlist guard)|
| UmbraVox.Crypto.ChaChaPoly      | 92%    | 61%    | 100%    | 5 alternatives not taken (auth-fail paths)    |
| UmbraVox.Crypto.ConstantTime    | 100%   | 100%   | 100%    | Full coverage                                 |
| UmbraVox.Crypto.Curve25519      | 98%    | 93%    | 100%    | 1 alternative (rejection-sampling edge)       |
| UmbraVox.Crypto.Ed25519         | 95%    | 80%    | 100%    | 8 alternatives (invalid-signature paths)      |
| UmbraVox.Crypto.Export          | 98%    | 75%    | 100%    | 1 alternative not taken                       |
| UmbraVox.Crypto.GCM             | 98%    | 78%    | 100%    | 4 alternatives (tag-verify fail paths)        |
| UmbraVox.Crypto.HKDF            | 99%    | 92%    | 100%    | 1 alternative not taken                       |
| UmbraVox.Crypto.HMAC            | 100%   | 100%   | 100%    | Full coverage                                 |
| UmbraVox.Crypto.Keccak          | 98%    | 66%    | 100%    | 4 alternatives (padding/length variants)      |
| UmbraVox.Crypto.KeyStore        | 85%    | 87%    | 63%    | **GAP**: 37% of top-level decls untested      |
| UmbraVox.Crypto.MLKEM           | 97%    | 89%    | 77%    | 15 decls untested (decapsulate error paths)   |
| UmbraVox.Crypto.Poly1305        | 98%    | 83%    | 100%    | 1 alternative not taken                       |
| UmbraVox.Crypto.PQWrapper       | 100%   | —      | 100%    | Full coverage (thin wrapper, no branches)     |
| UmbraVox.Crypto.Random          | 90%    | 70%    | 95%    | 7 alternatives (entropy-source fallback)      |
| UmbraVox.Crypto.RatchetPersist  | 94%    | 66%    | 100%    | 2 alternatives (serialise error paths)        |
| UmbraVox.Crypto.SHA256          | 100%   | 100%   | 100%    | Full coverage                                 |
| UmbraVox.Crypto.SHA512          | 100%   | 100%   | 100%    | Full coverage                                 |
| UmbraVox.Crypto.StealthAddress  | 93%    | 58%    | 72%    | 7 alternatives (scanning edge cases)          |
| UmbraVox.Crypto.VRF             | 100%   | —      | 100%    | Full coverage (thin wrapper)                  |
| UmbraVox.Crypto.Warning         | n/a    | n/a    | n/a    | Types-only module, no executable code         |

### 5.2 Signal protocol modules

| Module                              | Expr % | Alts % | Decls % | Gap note                                         |
|-------------------------------------|--------|--------|---------|--------------------------------------------------|
| UmbraVox.Crypto.Signal.X3DH         | 97%    | 80%    | 100%    | 2 alternatives (key-type mismatch paths)         |
| UmbraVox.Crypto.Signal.PQXDH        | 99%    | 100%   | 93%    | 1 decl untested (legacy-compat branch)           |
| UmbraVox.Crypto.Signal.DoubleRatchet| 96%    | 80%    | 71%    | **GAP**: 29% decls + 10 alternatives untested    |
| UmbraVox.Crypto.Signal.SenderKeys   | (test) | (test) | (test) | Library module covered via test module           |
| UmbraVox.Crypto.Signal.Session      | 100%   | —      | 75%    | 1 decl untested (session-close path)             |

### 5.3 Generated FFI bridge modules

All `UmbraVox.Crypto.Generated.*` and `UmbraVox.Crypto.Generated.FFI.*`
modules are thin wrappers generated by the codegen pipeline. They each
reached 100% expression coverage under the required suite.

The corresponding C implementations in `csrc/generated/` are not covered
by HPC (C is not instrumented by GHC). Separate coverage tooling
(gcov/lcov) is required for those translation units. This is a pending
gap tracked in the assurance matrix.

---

## 6. Modules Requiring Additional Tests (< 90% expression)

Based on the initial run, the following crypto-assurance-boundary modules
are below the 90% expression coverage target:

| Module                                | Expr % | Priority | Recommended action                            |
|---------------------------------------|--------|----------|-----------------------------------------------|
| UmbraVox.Crypto.KeyStore              | 85%    | High     | Add tests for store-full and eviction paths   |
| UmbraVox.Crypto.Signal.DoubleRatchet | 96%    | Medium   | Add out-of-order and skip-counter test cases  |
| UmbraVox.Crypto.StealthAddress        | 93%    | Medium   | Add scanning with mismatched ephemeral keys   |
| UmbraVox.Crypto.Random                | 90%    | Medium   | Add entropy-source fallback simulation        |

Note: DoubleRatchet expression coverage is 96% but only 71% of top-level
declarations are exercised. The untested declarations are the skip-counter
management paths that handle out-of-order message delivery; these require
multi-step ratchet advancement test scenarios.

---

## 7. How to Regenerate This Report

```
nix-shell --run "make mcdc-report"
```

The `mcdc-report` target:
1. Runs `cabal configure --enable-coverage` to pre-generate the autogen
   headers needed by the C FFI sources.
2. Builds all targets with `cabal build all -j1 --enable-coverage` using
   single-threaded compilation to avoid a race condition in the Cabal/GHC
   parallel build where the C object `.tmp` file is renamed before the
   autogen directory is populated.
3. Runs the `required` test suite.
4. Invokes `hpc report` with the correct `--hpcdir` paths for both the
   library mix directory and the test-binary mix directory.
5. Prints the per-module expression coverage table to stdout.
6. Reports the path to the HTML report for browser inspection.

The HTML report at `dist-newstyle/.../hpc/vanilla/html/hpc_index.html`
provides annotated source with red/green highlighting of covered and
uncovered expression nodes.

---

## 8. Relation to Assurance Matrix

This document feeds directly into the coverage evidence lane described in
`doc/assurance-matrix.md`. The HPC HTML report and the `.tix` file are
evidence artifacts. They should be captured alongside the formal
verification logs in the `make evidence` bundle.

The condition tables required for Phase 2 (section 4) will be linked from
the assurance matrix as supplementary evidence for the MC/DC gap closure.

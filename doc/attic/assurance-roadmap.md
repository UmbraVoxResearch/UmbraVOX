# UmbraVOX Cryptographic Assurance Roadmap

UmbraVOX is currently a research-oriented messaging MVP with an active Haskell
reference/runtime path, a partially green formal F* assurance layer, and
generated C/FFI surfaces that are wired into the build but not yet active as
independent cryptographic semantics.

This document describes the intended path from the current
reference-oriented implementation toward a stronger cryptographic assurance
model.

The goal is not to overstate current security claims. The goal is to make the
current assurance boundary explicit, keep the messaging MVP testable, and move
incrementally from reference semantics toward independently checked,
constant-time-capable production cryptography.

Runtime troubleshooting logs are part of that same boundary discipline. Even
with payload redaction and restrictive file permissions, they still capture
sensitive operational metadata and should not be treated as production-safe
telemetry.

## Current Assurance Model

- The active confidentiality and integrity path is the layered runtime path:
  Signal Double Ratchet, PQ outer wrapping, and Noise transport.
- Haskell is the active semantic implementation path for the current crypto
  runtime.
- F* is the intended formal assurance layer, and the current full verifier run
  is green under the active toolchain. That green result means the handwritten
  model suite is internally consistent again; it is not yet a machine-checked
  refinement proof from those models to the active Haskell runtime.
- Generated Haskell modules are currently wrapper-style modules over the active
  handwritten Haskell implementations.
- Generated C artifacts are currently link-probe stubs, not active crypto
  implementations.
- Generated FFI modules currently call the C link probes and then delegate back
  to the active Haskell implementations.

## What The Current C/FFI Bridge Does

Today’s generated C/FFI bridge has real value, but its value is narrow.

- It proves that the generated C artifacts compile and link into the package.
- It proves that the generated FFI bridge modules are exposed, callable, and
  stable in the build graph.
- It helps prevent the preserved generated namespaces from silently rotting.

Today’s generated C/FFI bridge does not yet provide:

- an independent cryptographic implementation
- independent semantic diversity
- meaningful constant-time evidence
- meaningful FFI memory-safety evidence
- proof that generated C semantics match the Haskell runtime semantics

That distinction matters. A linked probe is build hygiene. It is not
cryptographic assurance.

## Assurance Taxonomy

### 1. Linkage Smoke Tests

This is the current generated C/FFI category.

- C symbols exist
- Cabal wiring is correct
- the bridge modules call into the compiled package successfully

Value:

- catches symbol, package, or linker drift early

Limit:

- does not validate crypto semantics

### 2. Wrapper Parity

This is also present today.

- generated wrappers return the same bytes as the active Haskell implementation

Value:

- confirms wrapper wiring has not accidentally changed algorithms or signatures

Limit:

- still not independent assurance if both sides execute the same implementation

### 3. Active Semantic Diversity

This is the first materially stronger assurance category and is not present yet.

- one active Haskell implementation
- one active C implementation
- both execute real semantics
- both are compared systematically through differential tests

Value:

- can catch implementation bugs that one implementation alone would miss

Limit:

- still requires strong FFI boundary hardening and side-channel discipline

### 4. Proof-Linked Production Semantics

This is the strongest practical target for UmbraVOX.

- F* specifications define intended behavior
- Haskell remains the semantic oracle and readable reference implementation
- C becomes the production-oriented low-level implementation path
- the C path is tied back to the F* layer through proof linkage, equivalence
  arguments, or a stronger verified generation/extraction story

Value:

- strongest practical combination of readability, testability, and production
  assurance

Limit:

- highest implementation and evidence burden

## Recommended Near-Term Roadmap

1. Keep Haskell as the active reference/runtime path.
2. Preserve the green F* gate while reducing assumption-heavy trusted areas and
   strengthening the standards-equivalence story.
3. Keep generated C/FFI claims narrow and truthful.
4. Convert generated C from probe-only output to real semantic output.
5. Convert generated FFI from probe-plus-delegate wrappers to real foreign
   crypto calls.
6. Add true differential testing between Haskell and C.
7. Harden the FFI boundary with malformed-input, ownership, lifetime,
   concurrency, and error-path tests.
8. Add explicit side-channel and key-handling evidence before any production or
   life-critical claim expansion.

## Post-MVP Refactor Option

After the messaging MVP assurance bar is actually met, UmbraVOX can evaluate a
stronger long-term refactor for selected primitives:

- keep the current handwritten F* specifications as the formal source layer
- evaluate whether those primitives should be rewritten into an extractable
  Low*/KaRaMeL-compatible form
- compare the current `.spec -> generated C` path against an
  `F* -> KaRaMeL/Low* -> C` path for traceability, constant-time discipline,
  and equivalence-testing fit
- keep Haskell as the semantic oracle even if extracted C becomes the active
  low-level implementation path

This is explicitly a post-MVP option, not part of the current MVP-critical
closeout path.

## Target Assurance Model

The strongest practical target for UmbraVOX is a triple-check model:

1. F* specifications define the intended cryptographic and protocol semantics.
2. Haskell remains the active reference implementation and differential oracle.
3. C becomes the production-oriented low-level implementation path, with a
   narrow and hardened FFI boundary.

The C path does not become meaningful merely because it compiles or links. It
becomes meaningful only when it implements real semantics, is tested against
the Haskell oracle, is tied back to F* specifications or equivalence
arguments, and is covered by malformed-input, boundary, concurrency,
persistence, and side-channel-oriented hardening tests.

## Claim Discipline

Until the stronger target is reached, the repo should remain explicit about the
current boundary:

- the active Haskell path is the current semantic crypto path
- the F* assurance layer is active but currently incomplete
- the generated C/FFI bridge is a truthful build/link and wrapper-participation
  surface, not yet an independently assured production crypto path
- publication-grade, life-critical, or DO-style assurance claims must remain
  bounded by the evidence that is actually green

## Bounded MVP Assurance Statement

The following bounded statement describes the assurance posture of the
current MVP release. It is intended to be the strongest honest claim
supported by the evidence in this repository.

What the MVP provides:

- Active confidentiality and integrity through the layered Haskell runtime
  path: Signal Double Ratchet, PQ outer wrapping (PQXDH), and Noise IK
  transport.
- App-layer AEAD encryption for persistent message content and conversation
  names at rest, using AES-256-GCM with HKDF-derived per-identity storage
  keys. SQLite format unchanged; no SQLCipher dependency.
- 17 handwritten F* formal specifications, all green under the active
  toolchain. These are internally consistent models, not machine-checked
  refinement proofs against the Haskell runtime.
- Runtime diagnostic logging with metadata redaction and restrictive file
  permissions (0600). Logging is disabled by default and should not be
  treated as production-safe telemetry.
- Strong runtime test coverage across crypto primitives, protocol layers,
  transport hardening, fault injection, and persistence recovery.

What the MVP does not provide:

- Constant-time cryptographic implementations (pure Haskell reference code).
- Guaranteed key material zeroing.
- Machine-checked refinement proof from F* models to Haskell runtime.
- Independent, semantically real generated C execution path.
- Independent security audit.
- Maintained guest VM images for in-guest release verification.
- Native build runners for non-Linux targets (Windows, macOS, BSD remain
  source releases).

Residual metadata exposure:

- Peer public keys, IP addresses, ports, timestamps, settings, and
  conversation IDs are stored as plaintext in the SQLite database. This
  metadata is needed for database queries and session restore. See
  `doc/persistence-model.md` for rationale.

## Post-MVP Target Statement

The stronger target model for UmbraVOX beyond the MVP is a triple-check
assurance architecture:

1. F* specifications define intended cryptographic and protocol semantics.
2. Haskell remains the semantic oracle and readable reference implementation.
3. C becomes the production-oriented low-level implementation path, with a
   narrow and hardened FFI boundary.
4. Differential testing between Haskell oracle and C implementation provides
   independent cross-language assurance.
5. Side-channel discipline, key-handling evidence, and constant-time
   properties are demonstrated through targeted hardening tests.

This target requires replacing the current generated C link probes with
real semantic implementations, adding true FFI boundary hardening, and
establishing a refinement or correspondence argument from F* models to
the active runtime code.

The post-MVP target is tracked in `TODO.txt` under Future Planned items
F1-F4.

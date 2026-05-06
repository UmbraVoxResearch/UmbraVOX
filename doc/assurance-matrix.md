# UmbraVOX Assurance And Equivalence Matrix

This document is the repo's compact evidence ledger for cryptographic
assurance. It is intentionally narrower and more mechanical than
[`assurance-roadmap.md`](assurance-roadmap.md).

Use it to answer:

- what standard each primitive targets
- what implementation is active at runtime
- what generated/codegen surfaces exist
- what the F* layer currently proves
- what tests currently back the primitive
- what trust gaps still remain

## Reading This Matrix

- `Active Haskell`: the handwritten Haskell implementation is part of the
  active runtime path.
- `Generated Haskell`: wrapper-style generated namespace over the handwritten
  Haskell implementation.
- `Generated C`: currently a build/link probe or, if that changes later, a
  real semantic implementation.
- `Generated FFI`: current bridge status for the generated foreign layer.
- `F* status`: handwritten formal model status, not a proof that Haskell
  refines the model.
- `Assumption gaps`: important `assume`-based or otherwise trusted parts of the
  current evidence package.

## Primitive Matrix

| Primitive | Standard target | Active Haskell | Generated Haskell | Generated C | Generated FFI | F* status | Tests/evidence | Assumption gaps | Current bottom line |
|---|---|---|---|---|---|---|---|---|---|
| SHA-256 | FIPS 180-4 | Yes | Wrapper | Link probe | Bridge back to Haskell | Green | NIST KATs, boundary cases, fuzz, full `make verify` | KAT lemmas in F* remain assumption-backed; no Haskell refinement proof | Strong runtime evidence, green handwritten model, no independent C semantics |
| SHA-512 | FIPS 180-4 | Yes | Wrapper | Link probe | Bridge back to Haskell | Green | NIST KATs, boundary cases, fuzz, full `make verify` | Padding divisibility and KAT proof story still partly trusted in F* | Strong runtime evidence, green handwritten model, no independent C semantics |
| HMAC | RFC 2104 / RFC 4231 | Yes | Wrapper | Link probe | Bridge back to Haskell | Green | RFC KATs, fuzz, full `make verify` | PRF/security lemmas and some KAT structure remain assumption-heavy | Good implementation/test evidence, weaker formal security claim than wording should imply |
| HKDF | RFC 5869 | Yes | Wrapper | Link probe | Bridge back to Haskell | Green | RFC KATs, fuzz, full `make verify` | Counter-byte and output-structure obligations still rely partly on trusted assumptions | Good implementation/test evidence, incomplete formal equivalence/security story |
| AES-256 | FIPS 197 | Yes | Wrapper | Link probe | Bridge back to Haskell | Green | NIST KATs, round-trip, fuzz, full `make verify` | No machine-checked refinement from F* to Haskell; no active generated C semantics | Good runtime/reference evidence, no independent production C path |
| GF(2^128) | SP 800-38D | Yes (inside GCM) | N/A | N/A | N/A | Green | Covered through GCM tests and F* lemmas | Formal layer does not by itself prove active runtime refinement | Internal formal component is green, still tied to handwritten model only |
| AES-GCM | SP 800-38D | Yes | Wrapper | Link probe | Bridge back to Haskell | Green | NIST vectors, tamper tests, fuzz, full `make verify` | GCM formal model still uses important assumptions; current F* type still does not fully bind AES semantics to keyed AES-256 behavior | Runtime path is strong; formal story is green but not publication-grade complete |
| ChaCha20 | RFC 8439 | Yes | Wrapper | Link probe | Bridge back to Haskell | Green | RFC KATs, edge cases, fuzz, full `make verify` | No refinement proof to Haskell; no active generated C semantics | Good runtime/reference evidence, no independent C semantics |
| X25519 | RFC 7748 | Yes | Wrapper | Link probe | Bridge back to Haskell | Green | RFC vectors, DH properties, fuzz, full `make verify` | No machine-checked refinement to Haskell | Good runtime/reference evidence, still one semantic implementation path |
| Ed25519 | RFC 8032 | Yes | N/A | N/A | N/A | Green | RFC vectors, sign/verify tests, fuzz, full `make verify` | No machine-checked refinement to Haskell | Good runtime/reference evidence, handwritten model only |
| Keccak / SHA-3 / SHAKE | FIPS 202 | Yes | Wrapper | Link probe | Bridge back to Haskell | Green | NIST vectors, SHAKE tests, fuzz, full `make verify` | No machine-checked refinement to Haskell | Good runtime/reference evidence, no independent C semantics |
| Poly1305 | RFC 8439 | Yes | Wrapper | Link probe | Bridge back to Haskell | Green | RFC vector, parity tests, full `make verify` | No machine-checked refinement to Haskell | Good runtime/reference evidence, no independent C semantics |
| ML-KEM-768 | FIPS 203 | Yes | Wrapper | Link probe | Bridge back to Haskell | Green | Self-consistency tests, round-trip, parity tests, full `make verify` | Evidence is much stronger on implementation/test side than on standards-correspondence proofs | Good MVP evidence, still not an independently assured low-level path |

## Protocol Matrix

| Protocol | Standard / source target | Active runtime | F* status | Tests/evidence | Main caveat |
|---|---|---|---|---|---|
| X3DH | Signal / X3DH design | Yes | Green | End-to-end, integration, verifier | No Haskell refinement proof |
| PQXDH | Project protocol target | Yes | Green | Integration, fuzz, verifier | Project-specific model, not standards-generated |
| Double Ratchet | Signal Double Ratchet | Yes | Green | Bidirectional, out-of-order, tamper, verifier | No Haskell refinement proof |
| Noise IK | Noise framework | Yes | Green | Handshake tests, transport tests, verifier | No machine-checked refinement to runtime code |

## Proof-Layer Matrix

| Layer | SHA-256 | SHA-512 | HMAC | HKDF | GCM | Meaning today |
|---|---|---|---|---|---|---|
| Bounds / shape | Present | Present | Partial | Partial | Partial | Local well-formedness and length/index reasoning |
| Word-op identities | Present | Present | Partial | N/A | Partial | Rotations, shifts, modular adds, helper identities |
| Message schedule equivalence | Present structurally | Present structurally | N/A | N/A | N/A | Current F* structure matches the intended recurrence |
| Round-state equivalence | Present structurally | Present structurally | N/A | N/A | Partial | Current chunked rounds mirror the standard recurrence |
| Compression equivalence | Present structurally | Present structurally | N/A | N/A | Partial | Stronger than mere loop shape, but still within handwritten model only |
| Whole hash / AEAD / KDF equivalence | Partial | Partial | Partial | Partial | Partial | Some obligations are still assumption-backed or only indirectly tested |
| KAT agreement | Tested strongly; F* partly assumed | Tested strongly; F* partly assumed | Tested strongly; F* partly assumed | Tested strongly; F* partly assumed | Tested strongly; F* partly assumed | Haskell test layer is stronger than the formal KAT layer |
| Haskell refinement proof | Missing | Missing | Missing | Missing | Missing | Not currently machine-checked |
| Independent C semantics | Missing | Missing | Missing | Missing | Missing | Current C/FFI surface is not an independent semantic path |

## Standards Provenance

- The F* files are handwritten formal models.
- They are not generated from NIST or RFC prose.
- The `.spec` pipeline is separate from the F* layer.
- The `.spec` pipeline currently generates wrapper-style Haskell and
  link-probe/bridge C/FFI surfaces, not the active runtime semantics.

## Current Assurance Statement

The strongest honest statement supported by the current repo is:

- the active runtime cryptographic behavior is implemented in handwritten
  Haskell
- the handwritten F* model suite is currently green under the active toolchain
- runtime tests and vectors are strong across the MVP crypto surface
- generated C/FFI is wired into the build and tested for bridge/linkage parity
  only
- there is not yet a machine-checked refinement proof from F* to Haskell
- there is not yet an independent, semantically real generated C execution path

## Next Evidence Upgrades

1. Reduce `assume`-based trust in GCM, HMAC, and HKDF.
2. Add explicit equivalence lemmas for SHA-256 and SHA-512 that tie:
   - message schedule
   - round-state transitions
   - compression output
   - whole-hash padding/length encoding
   to the standard recurrences.
3. Narrow or remove any remaining DAL/DO-style wording that exceeds the
   evidence in this matrix.
4. Replace generated C link probes with real semantic implementations before
   claiming independent cross-language assurance.
5. Add a real refinement/correspondence story from handwritten F* models to the
   active handwritten Haskell implementations if publication-grade formal
   claims are required.

<!-- SPDX-License-Identifier: Apache-2.0 -->

# F* Irreducible Cryptographic Axioms

## Overview

The UmbraVOX F* specifications contain **0 admit() calls** — every proof obligation
is either Z3-discharged or declared as an explicit `assume val` with documentation.

Most `assume val` declarations are **eliminable** (KAT normalization barriers, abstract
function signatures) and are being progressively replaced with concrete implementations.
This document registers only the **irreducible** axioms — cryptographic hardness
assumptions and cross-toolchain boundaries that fundamentally cannot be proved in F*.

These represent the permanent trust boundary of the formal verification.

---

## Irreducible Axioms (12)

### Cryptographic Hardness Assumptions (7)

These encode standard computational hardness assumptions from the cryptographic
literature. They are unprovable in any constructive proof system without an explicit
adversary model.

| # | File | Axiom | Property | Reference |
|---|------|-------|----------|-----------|
| 1 | Spec.ChaChaPoly | `tag_forgery_ct_axiom` | Poly1305 UF-CMA: tag collision probability <= (L+1)/p < 2^-106 | Bernstein 2005, "The Poly1305-AES message-authentication code" |
| 2 | Spec.DoubleRatchet | `hmac_non_fixpoint` | HMAC-SHA256(ck, 0x01) != ck (PRF non-fixpoint; requires pre-image resistance) | Bellare-Canetti-Krawczyk 1996, FIPS 198-1 |
| 3 | Spec.DoubleRatchet | `hmac_collision_resistance` | HMAC-SHA256(ck, 0x01) != HMAC-SHA256(ck, 0x02) (collision resistance on distinct inputs) | FIPS 180-4, FIPS 198-1 |
| 4 | Spec.Ed25519 | `distinct_messages_distinct_sigs` | Distinct messages produce distinct signatures (SHA-512 CR + discrete log hardness) | RFC 8032, Bernstein et al. 2012 |
| 5 | Spec.StealthAddress | `unlinkability` | Stealth addresses for the same recipient are unlinkable (DDH on Curve25519) | Bernstein 2006, "Curve25519: new Diffie-Hellman speed records" |
| 6 | Spec.VRF | `vrf_strong_uniqueness` | VRF output is uniquely bound to the secret key (DL hardness on Ed25519) | RFC 9381 Section 3 |
| 7 | Spec.VRF | `vrf_collision_resistance` | VRF hash-to-curve + scalar multiplication is collision-resistant | RFC 9381, SHA-512 CR, Ed25519 DL hardness |

### Cross-Toolchain Boundaries (4)

These model the semantic boundary between the F* verification toolchain and the
Haskell runtime. They are irreducible because F* and GHC have no shared extraction
or linking path.

| # | File | Axiom | Property | Validation |
|---|------|-------|----------|------------|
| 8 | Spec.SHA256.Refinement | `bs_of_seq` | ByteString-to-Seq conversion at F*/GHC boundary | Semantic model definition |
| 9 | Spec.SHA256.Refinement | `seq_of_bs` | Seq-to-ByteString conversion at F*/GHC boundary | Semantic model definition |
| 10 | Spec.SHA256.Refinement | `bs_seq_roundtrip` | seq_of_bs(bs_of_seq(s)) = s (roundtrip) | Semantic model definition |
| 11 | Spec.SHA256.Refinement | `haskell_sha256` | Uninterpreted Haskell SHA-256 at F* boundary | NIST FIPS 180-4 KAT; Haskell property tests |

### External Validation

All irreducible axioms have empirical validation paths:

- **Cryptographic hardness**: Published security proofs in peer-reviewed venues
  (Bernstein 2005/2006, Bellare-Canetti-Krawczyk 1996, RFC 8032/9381).
- **Cross-toolchain**: Haskell differential test suite validates behavioral
  equivalence between the F* model and the Haskell implementation against
  NIST/RFC test vectors.

### Eliminable Axioms (not listed here)

The remaining ~69 `assume val` declarations are eliminable by:
1. Making abstract functions concrete (inline SHA-512, ChaCha20, AES-256 implementations)
2. Increasing Z3 resource limits for deep normalization
3. Formalizing Ed25519 group law in F* (following Bernstein-Birkner-Joye-Lange-Peters 2008)

These are tracked as ongoing work in M13 and do not represent trust boundary items.

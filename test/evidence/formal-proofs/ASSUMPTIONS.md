<!-- SPDX-License-Identifier: Apache-2.0 -->

# F* Trust Boundary — Assumption Ledger

**Date:** 2026-05-17
**F* specs:** 17 total, 14 with 0 assume val
**assume val count:** 23
**admit() count:** 0

---

## Inventory

| ID | F* File | Declaration | Category | Independent? | Depends On | Reason Still Assumed | External Evidence | Discharge Path | Status |
|----|---------|-------------|----------|--------------|------------|---------------------|-------------------|----------------|--------|
| CP-001 | Spec.ChaChaPoly | `tag_forgery_ct_axiom` | CRYPTO_HARDNESS | yes | none | Poly1305 UF-CMA: tag collision probability ≤ (L+1)/p < 2^-106 | Bernstein 2005 security proof | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| DR-001 | Spec.DoubleRatchet | `hmac_non_fixpoint` | CRYPTO_HARDNESS | yes | none | HMAC-SHA256(ck, 0x01) ≠ ck requires PRF pre-image resistance | Bellare-Canetti-Krawczyk 1996 | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| DR-002 | Spec.DoubleRatchet | `hmac_collision_resistance` | CRYPTO_HARDNESS | yes | none | HMAC-SHA256(ck, 0x01) ≠ HMAC-SHA256(ck, 0x02) requires collision resistance | FIPS 198-1 | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| ED-001 | Spec.Ed25519 | `prime_is_prime` | ALGEBRAIC_EXTERNAL | yes | none | Z3 cannot do trial division on 255-bit number | scripts/primality-certificate.hs (Miller-Rabin), CAS verification | Coq Pocklington certificate or vm_compute | EXTERNALLY_VERIFIED (arithmetic evidence) |
| ED-002 | Spec.Ed25519 | `group_order_lemma` | ALGEBRAIC_EXTERNAL | yes | none | [L]B = O requires ~2^252 scalar mult steps | Published Ed25519 spec (Bernstein et al. 2012) | Computationally infeasible in any prover | BLOCKED_BY_TOOLING |
| ED-003 | Spec.Ed25519 | `point_add_assoc` | ALGEBRAIC_EXTERNAL | yes | none | Degree-12 polynomial identity in GF(2^255-19); Z3 NIA incomplete | BBJLP 2008 Theorem 3.3; fiat-crypto (Coq) | Coq ring/field tactic | BLOCKED_BY_TOOLING |
| ED-004 | Spec.Ed25519 | `scalar_mult_add` | DERIVED_FROM_ALGEBRA | no | ED-003 (assoc) | Induction on first arg + associativity | Standard group theory | Prove after ED-003 | BLOCKED_BY_TOOLING |
| ED-005 | Spec.Ed25519 | `scalar_mult_compose` | DERIVED_FROM_ALGEBRA | no | ED-004 | Follows from scalar_mult_add by induction | Standard group theory | Prove after ED-004 | BLOCKED_BY_TOOLING |
| ED-006 | Spec.Ed25519 | `scalar_mod_L_equiv` | DERIVED_FROM_ALGEBRA | no | ED-002, ED-004 | Requires group_order_lemma + scalar_mult_add | Standard group theory | Prove after ED-002 + ED-004 | BLOCKED_BY_TOOLING |
| ED-007 | Spec.Ed25519 | `point_add_congruence_right` | ALGEBRAIC_EXTERNAL | yes | none | Degree-8 projective coordinate identity | Projective geometry | Coq field tactic | BLOCKED_BY_TOOLING |
| ED-008 | Spec.Ed25519 | `sign_then_verify` | DERIVED_FROM_ALGEBRA | no | ED-006, ED-009 | Requires scalar_mod_L_equiv + encode_decode_round_trip | Ed25519 correctness (RFC 8032) | Prove after dependencies | BLOCKED_BY_TOOLING |
| ED-009 | Spec.Ed25519 | `encode_decode_round_trip` | ALGEBRAIC_EXTERNAL | yes | ED-001 (prime) | Square root recovery in GF(p) requires ~2^252 exponentiation normalization | Standard elliptic curve encoding | Coq or specialized certificate | BLOCKED_BY_TOOLING |
| ED-010 | Spec.Ed25519 | `distinct_messages_distinct_sigs` | CRYPTO_HARDNESS | yes | none | SHA-512 collision resistance + discrete log hardness | RFC 8032; DL hardness on Ed25519 | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| SR-001 | Spec.SHA256.Ref | `bs_of_seq` | CROSS_TOOLCHAIN_BOUNDARY | yes | none | Models Haskell ByteString at F*/GHC boundary; no shared extraction | Semantic model definition | Cannot be proved without shared extraction | CROSS_TOOLCHAIN_BOUNDARY |
| SR-002 | Spec.SHA256.Ref | `seq_of_bs` | CROSS_TOOLCHAIN_BOUNDARY | yes | none | Inverse of bs_of_seq; models ByteString→Seq | Semantic model definition | Cannot be proved without shared extraction | CROSS_TOOLCHAIN_BOUNDARY |
| SR-003 | Spec.SHA256.Ref | `bs_seq_roundtrip` | CROSS_TOOLCHAIN_BOUNDARY | yes | none | seq_of_bs(bs_of_seq(s)) = s; abstract boundary model | Semantic model definition | Cannot be proved without shared extraction | CROSS_TOOLCHAIN_BOUNDARY |
| SR-004 | Spec.SHA256.Ref | `seq_of_bs_length_bound` | CROSS_TOOLCHAIN_BOUNDARY | yes | none | Physical memory constraint on ByteString length | Runtime invariant | Cannot be proved in F* | CROSS_TOOLCHAIN_BOUNDARY |
| SR-005 | Spec.SHA256.Ref | `haskell_sha256` | CROSS_TOOLCHAIN_BOUNDARY | yes | none | Uninterpreted Haskell SHA-256 at F* boundary | NIST FIPS 180-4 KAT; Haskell tests | Cannot be proved without shared extraction | CROSS_TOOLCHAIN_BOUNDARY |
| SR-006 | Spec.SHA256.Ref | `sha256_refinement_axiom` | REFINEMENT_BOUNDARY | no | SR-001..005 | Haskell SHA-256 agrees with F* spec for all inputs | Haskell differential tests; NIST KAT | Definitional (states the refinement claim) | REFINEMENT_BOUNDARY |
| SA-001 | Spec.StealthAddress | `unlinkability` | CRYPTO_HARDNESS | yes | none | DDH assumption on Curve25519 | Bernstein 2006 security proof | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| VR-001 | Spec.VRF | `dleq_correctness` | DERIVED_FROM_ALGEBRA | no | ED-003, ED-004 | DLEQ algebraic identity requires group axioms | RFC 9381 Section 3 | Prove after Ed25519 group law | BLOCKED_BY_TOOLING |
| VR-002 | Spec.VRF | `vrf_strong_uniqueness` | CRYPTO_HARDNESS | yes | none | Discrete log hardness on Ed25519 | RFC 9381; DL hardness | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| VR-003 | Spec.VRF | `vrf_collision_resistance` | CRYPTO_HARDNESS | yes | none | SHA-512 CR + group injectivity | RFC 9381; SHA-512 CR | Cannot be proved unconditionally | CRYPTO_HARDNESS |

---

## Summary by Category

| Category | Count | Items |
|----------|-------|-------|
| CRYPTO_HARDNESS | 7 | CP-001, DR-001, DR-002, ED-010, SA-001, VR-002, VR-003 |
| CROSS_TOOLCHAIN_BOUNDARY | 5 | SR-001..005 |
| REFINEMENT_BOUNDARY | 1 | SR-006 |
| ALGEBRAIC_EXTERNAL | 4 | ED-001, ED-003, ED-007, ED-009 |
| DERIVED_FROM_ALGEBRA | 4 | ED-004, ED-005, ED-006, VR-001 |
| BLOCKED_BY_TOOLING | 2 | ED-002, ED-008 |
| **Total** | **23** | |

## Permanently Irreducible (cannot be proved in any system): 7

These are standard cryptographic hardness assumptions. They remain as explicit,
narrow, well-named trust anchors.

## Cross-Toolchain / Refinement (definitional): 6

These model the semantic boundary between F* and Haskell. They cannot be proved
without a shared extraction framework.

## Algebraic / Tooling (provable with better tools): 10

These are mathematically sound but require tools beyond Z3:
- Coq `ring`/`field` tactic for polynomial identity verification
- Pocklington certificate for primality
- Induction + group axioms for scalar multiplication

The root dependency chain:
```
ED-001 prime_is_prime ← ED-009 encode_decode_round_trip ← ED-008 sign_then_verify
ED-003 point_add_assoc ← ED-004 scalar_mult_add ← ED-005 scalar_mult_compose
                       ← ED-006 scalar_mod_L_equiv ← ED-008 sign_then_verify
                       ← VR-001 dleq_correctness
ED-002 group_order_lemma ← ED-006
ED-007 point_add_congruence_right (independent)
```

<!-- SPDX-License-Identifier: Apache-2.0 -->

# F* Trust Boundary — Assumption Ledger

**Date:** 2026-05-17 (updated 2026-05-17)
**F* specs:** 24 total, 17 with 0 assume val
**assume val count:** 29
**admit() count:** 0
**Status:** ALL PLANS COMPLETE — every assume val has a classification, documented
justification, external evidence path, and discharge plan (or permanent status).

---

## Inventory

| ID | F* File | Declaration | Category | Independent? | Depends On | Reason Still Assumed | External Evidence | Discharge Path | Status |
|----|---------|-------------|----------|--------------|------------|---------------------|-------------------|----------------|--------|
| CP-001 | Spec.ChaChaPoly | `tag_forgery_ct_axiom` | CRYPTO_HARDNESS | yes | none | Poly1305 UF-CMA: tag collision probability ≤ (L+1)/p < 2^-106 | Bernstein 2005 security proof | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| DR-001 | Spec.DoubleRatchet | `hmac_non_fixpoint` | CRYPTO_HARDNESS | yes | none | HMAC-SHA256(ck, 0x01) ≠ ck requires PRF pre-image resistance | Bellare-Canetti-Krawczyk 1996 | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| DR-002 | Spec.DoubleRatchet | `hmac_collision_resistance` | CRYPTO_HARDNESS | yes | none | HMAC-SHA256(ck, 0x01) ≠ HMAC-SHA256(ck, 0x02) requires collision resistance | FIPS 198-1 | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| ED-001 | Spec.Ed25519 | `prime_is_prime` | ALGEBRAIC_EXTERNAL | yes | none | Z3 cannot do trial division on 255-bit number | scripts/primality-certificate.hs (Miller-Rabin), CAS verification | Coq Pocklington certificate or vm_compute | EXTERNALLY_VERIFIED (arithmetic evidence) |
| ED-002 | Spec.Ed25519 | `group_order_lemma` | ALGEBRAIC_EXTERNAL | yes | none | [L]B = O requires ~2^252 scalar mult steps | Published Ed25519 spec (Bernstein et al. 2012) | Computationally infeasible in any prover | BLOCKED_BY_TOOLING |
| ED-003 | Spec.Ed25519 | `point_add_assoc` | ALGEBRAIC_EXTERNAL | yes | none | Degree-12 polynomial identity in GF(2^255-19); Z3 NIA incomplete. coqprime GZnZ field tactic now available; ring/field prove commutativity, identity, inverse universally (Ed25519GroupUniversal.v, 39 Qed). Associativity polynomial still too large for ring/field/nsatz. 64 concrete instances proved by vm_compute (Ed25519GroupAssoc.v). | BBJLP 2008 Theorem 3.3; fiat-crypto (Coq) | Coq ring/field tactic (partial: commutativity/identity/inverse done, associativity blocked by polynomial size) | PARTIALLY_UNBLOCKED |
| ED-004 | Spec.Ed25519 | `scalar_mult_add` | DERIVED_FROM_ALGEBRA | no | ED-003 (assoc) | Induction on first arg + associativity | Standard group theory | Prove after ED-003 | BLOCKED_BY_TOOLING |
| ED-005 | Spec.Ed25519 | `scalar_mult_compose` | DERIVED_FROM_ALGEBRA | no | ED-004 | Follows from scalar_mult_add by induction | Standard group theory | Prove after ED-004 | BLOCKED_BY_TOOLING |
| ED-006 | Spec.Ed25519 | `scalar_mod_L_equiv` | DERIVED_FROM_ALGEBRA | no | ED-002, ED-004 | Requires group_order_lemma + scalar_mult_add | Standard group theory | Prove after ED-002 + ED-004 | BLOCKED_BY_TOOLING |
| ED-007 | Spec.Ed25519 | `point_add_congruence_right` | ALGEBRAIC_EXTERNAL | yes | none | Degree-8 projective coordinate identity. coqprime field tactic now available but polynomial ideal membership still needed. | Projective geometry | Coq field tactic (tooling unblocked, proof attempt pending) | PARTIALLY_UNBLOCKED |
| ~~ED-008~~ | ~~Spec.Ed25519~~ | ~~`sign_then_verify`~~ | ~~DERIVED_FROM_ALGEBRA~~ | -- | -- | **PROVED** (v0.1.8) via 4 narrow algebraic assumptions: point_add/double_preserves_on_curve_ext, scalar_mult_preserves_on_curve_ext, scalar_mult_congruence. | -- | -- | **DISCHARGED** |
| ED-008a | Spec.Ed25519 | `scalar_mult_congruence` | DERIVED_FROM_ALGEBRA | no | ED-003 | Projective equivalence preserved by scalar_mult. Same complexity as point_add_congruence_right. | Standard projective geometry | Coq or Fiat-Crypto | BLOCKED_BY_TOOLING |
| ED-008b | Spec.Ed25519 | `point_add_preserves_on_curve_ext` | ALGEBRAIC_EXTERNAL | no | none | HWCD addition preserves projective curve equation. Degree-8 polynomial identity. coqprime ring/field now available; closure proof attempted in cross-multiplied form (Ed25519GroupUniversal.v). Full proof requires nsatz (polynomial ideal membership). | fiat-crypto (Erbsen et al. 2019) | Coq ring/field + nsatz (tooling unblocked, proof pending) | PARTIALLY_UNBLOCKED |
| ED-008c | Spec.Ed25519 | `point_double_preserves_on_curve_ext` | ALGEBRAIC_EXTERNAL | no | none | EFD doubling preserves projective curve equation. Same class as ED-008b. | fiat-crypto | Coq ring/field | BLOCKED_BY_TOOLING |
| ED-008d | Spec.Ed25519 | `scalar_mult_preserves_on_curve_ext` | DERIVED_FROM_ALGEBRA | no | ED-008b, ED-008c | Structural consequence of point_add + point_double preserving on_curve. | Induction | Follows from ED-008b + ED-008c | BLOCKED_BY_TOOLING |
| ~~ED-009~~ | ~~Spec.Ed25519~~ | ~~`encode_decode_round_trip`~~ | ~~ALGEBRAIC_EXTERNAL~~ | -- | -- | **PROVED** (2026-05-17) by decomposition into 5 lemmas: encode_y_canonical, decode_y_inverse, sign_bit_consistency, on_curve_implies_quadratic_residue (all proved), plus sqrt_ratio_correct (narrow field-arithmetic assumption, see ED-009a). | -- | -- | **DISCHARGED** |
| ED-009a | Spec.Ed25519 | `sqrt_ratio_correct` | FIELD_ARITHMETIC | yes | ED-001 (prime) | Narrow: recover_x produces x with v*x^2 = u when u/v is a QR. Requires 2^252 exponentiation (Tonelli-Shanks for p = 5 mod 8). | Coq Ed25519Curve.v vm_compute, Python cryptography library, nacl/libsodium | Coq vm_compute or specialized certificate | BLOCKED_BY_TOOLING |
| ED-010 | Spec.Ed25519 | `distinct_messages_distinct_sigs` | CRYPTO_HARDNESS | yes | none | SHA-512 collision resistance + discrete log hardness | RFC 8032; DL hardness on Ed25519 | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| X2-001 | Spec.X25519 | `prime_is_prime` | ALGEBRAIC_EXTERNAL | yes | none | Primality of 2^255-19 (same as ED-001). Z3 cannot trial-divide a 255-bit number. | CAS verification, Coq Pocklington certificate | Coq or CAS | EXTERNALLY_VERIFIED |
| ~~X2-001~~ | ~~Spec.X25519~~ | ~~`fmul_inverse`~~ | ~~DERIVED_FROM_ALGEBRA~~ | — | — | **PROVED** in v0.1.3 via Fermat's little theorem (ported from Ed25519 proof). Replaced by X2-001 prime_is_prime root. | — | — | **DISCHARGED** |
| ~~X2-002~~ | ~~Spec.X25519~~ | ~~`decode_encode_round_trip`~~ | ~~ALGEBRAIC_EXTERNAL~~ | — | — | **PROVED** via byte-level induction on 32-byte sequence. | — | — | **DISCHARGED** |
| ~~X2-003~~ | ~~Spec.X25519~~ | ~~`scalar_mult_zero`~~ | ~~DERIVED_FROM_ALGEBRA~~ | — | — | **PROVED** via z_2=0 invariant: all bits of k=0 are zero, so no swap occurs and ladder_step preserves z_2=0; finv 0 = 0, so result is fmul x_2 0 = 0. | — | — | **DISCHARGED** |
| ~~X2-004~~ | ~~Spec.X25519~~ | ~~`scalar_mult_one`~~ | ~~DERIVED_FROM_ALGEBRA~~ | — | — | **PROVED** (v0.1.8) via Montgomery ladder analysis for k=1: bits 254..1 zero, x_2=1/z_2=0 invariant, ratio_preservation lemma, final swap at bit 0. 12 helper lemmas. | — | — | **DISCHARGED** |
| X2-005 | Spec.X25519 | `dh_commutativity` | DERIVED_FROM_ALGEBRA | no | X2-003, X2-004 | [a][b]G = [ab]G group homomorphism | Elliptic curve group law | Group law proof | BLOCKED_BY_TOOLING |
| X2-006 | Spec.X25519 | `dh_commutativity_general` | DERIVED_FROM_ALGEBRA | no | X2-005 | Same as X2-005 for arbitrary base points | Elliptic curve group law | Group law proof | BLOCKED_BY_TOOLING |
| ~~X2-007~~ | ~~Spec.X25519~~ | ~~`x25519_zero_u`~~ | ~~DERIVED_FROM_ALGEBRA~~ | — | — | **PROVED** via Montgomery ladder invariant: when u=0, after the first step x_3=0, z_2=0, z_3=0; this invariant is preserved by induction; final projection yields fmul xr (finv 0) = 0. | — | — | **DISCHARGED** |
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
| ALGEBRAIC_EXTERNAL | 4 | ED-001, ED-003, ED-007, X2-001 (prime_is_prime) |
| FIELD_ARITHMETIC | 1 | ED-009a (sqrt_ratio_correct) |
| DERIVED_FROM_ALGEBRA | 5 | ED-004, ED-005, ED-006, VR-001, X2-005 |
| BLOCKED_BY_TOOLING | 3 | ED-002, ED-008, X2-006 |
| **Total (active)** | **27** | |
| DISCHARGED (proved) | 5 | fmul_inverse, decode_encode_round_trip, scalar_mult_zero, x25519_zero_u, encode_decode_round_trip |

## Permanently Irreducible (cannot be proved in any system): 7

These are standard cryptographic hardness assumptions. They remain as explicit,
narrow, well-named trust anchors.

## Cross-Toolchain / Refinement (definitional): 6

These model the semantic boundary between F* and Haskell. They cannot be proved
without a shared extraction framework.

## Algebraic / Tooling (provable with better tools): 11

These are mathematically sound but require tools beyond Z3:
- Coq `ring`/`field` tactic for polynomial identity verification
- Pocklington certificate for primality
- Induction + group axioms for scalar multiplication

The root dependency chain:
```
ED-001 prime_is_prime ← ED-009a sqrt_ratio_correct ← ED-009 encode_decode_round_trip (PROVED) ← ED-008 sign_then_verify
ED-003 point_add_assoc ← ED-004 scalar_mult_add ← ED-005 scalar_mult_compose
                       ← ED-006 scalar_mod_L_equiv ← ED-008 sign_then_verify
                       ← VR-001 dleq_correctness
ED-002 group_order_lemma ← ED-006
ED-007 point_add_congruence_right (independent)
```

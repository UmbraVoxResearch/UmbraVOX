<!-- SPDX-License-Identifier: Apache-2.0 -->

# F* Irreducible Cryptographic Axioms

## Overview

The UmbraVOX F* specifications contain **0 admit() calls** and **30 assume val
declarations** across 24 specs (17 specs have 0 assume val). 9 Coq files
provide **350 Qed proofs** (0 Admitted) backing Ed25519 curve, field, scalar,
prime, constant, group associativity instances, DLEQ, and sqrt-ratio properties.

**Key v0.1.8 milestones:**
- `sign_then_verify`: PROVED (was the broadest assume val)
- `encode_decode_round_trip`: PROVED (sqrt_ratio_correct isolated)
- 64 concrete associativity instances machine-checked in Coq
- Sqrt-ratio formula verified for 3 points via Coq vm_compute

This document registers only the **irreducible** axioms — cryptographic hardness
assumptions, cross-toolchain boundaries, and algebraic facts requiring tools
beyond Z3. For the complete inventory of all 30 assume vals with categories
and dependency analysis, see `test/evidence/formal-proofs/ASSUMPTIONS.md`.

These represent the permanent trust boundary of the formal verification.

---

## Irreducible Axioms (12)

### Cryptographic Hardness Assumptions (8)

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
| 8 | Spec.VRF | `dleq_correctness` | DLEQ proof verifies correctly for honest prover (DL relation on Ed25519) | RFC 9381 Section 5, Chaum-Pedersen 1992 |

### Cross-Toolchain Boundaries (4)

These model the semantic boundary between the F* verification toolchain and the
Haskell runtime. They are irreducible because F* and GHC have no shared extraction
or linking path.

| # | File | Axiom | Property | Validation |
|---|------|-------|----------|------------|
| 9 | Spec.SHA256.Refinement | `bs_of_seq` | ByteString-to-Seq conversion at F*/GHC boundary | Semantic model definition |
| 10 | Spec.SHA256.Refinement | `seq_of_bs` | Seq-to-ByteString conversion at F*/GHC boundary | Semantic model definition |
| 11 | Spec.SHA256.Refinement | `bs_seq_roundtrip` | seq_of_bs(bs_of_seq(s)) = s (roundtrip) | Semantic model definition |
| 12 | Spec.SHA256.Refinement | `haskell_sha256` | Uninterpreted Haskell SHA-256 at F* boundary | NIST FIPS 180-4 KAT; Haskell property tests |

### External Validation

All irreducible axioms have empirical validation paths:

- **Cryptographic hardness**: Published security proofs in peer-reviewed venues
  (Bernstein 2005/2006, Bellare-Canetti-Krawczyk 1996, RFC 8032/9381).
- **Cross-toolchain**: Haskell differential test suite validates behavioral
  equivalence between the F* model and the Haskell implementation against
  NIST/RFC test vectors.

### Remaining Assume Vals (18, not listed above)

The remaining 18 `assume val` declarations (beyond the 12 irreducible axioms):

1. **Ed25519 group theory** (8): `point_add_assoc`, `scalar_mult_add`,
   `scalar_mult_compose`, `scalar_mod_L_equiv`, `point_add_congruence_right`,
   `group_order_lemma`, `prime_is_prime` (Ed25519), `sqrt_ratio_correct`.
   Discharge path: Coq ring/field tactics (group theory), Coq vm_compute
   (primality, sqrt-ratio).
   Note: `sign_then_verify` and `encode_decode_round_trip` were PROVED in v0.1.8.
2. **Ed25519 on-curve preservation** (3): `point_add_preserves_on_curve_ext`,
   `point_double_preserves_on_curve_ext`, `scalar_mult_preserves_on_curve_ext`.
   Added in v0.1.8 to prove sign_then_verify. Discharge path: Coq or Fiat-Crypto
   (HWCD/EFD completeness, degree-8 polynomial identity).
3. **Ed25519 congruence** (1): `scalar_mult_congruence`.
   Projective equivalence under scalar_mult. Same complexity as point_add_congruence_right.
4. **X25519 curve arithmetic** (4): `prime_is_prime` (X25519), `scalar_mult_one`,
   `dh_commutativity`, `dh_commutativity_general`.
   Note: `fmul_inverse`, `decode_encode_round_trip`, `scalar_mult_zero`, `x25519_zero_u`
   were PROVED in v0.1.4-v0.1.5.
5. **Cross-toolchain boundary** (2): `seq_of_bs_length_bound`,
   `sha256_refinement_axiom` — model the F*/GHC semantic gap.

### Dependency Root Analysis

The Ed25519 group theory axioms form a dependency chain formerly rooted in
Fermat's Little Theorem (`fmul_inverse: a * a^(p-2) = 1 mod p`).

**`fmul_inverse` is now PROVED** via `FStar.Math.Fermat.fermat` (the standard
library's mechanized proof of Fermat's Little Theorem). The only remaining
trusted fact for this chain is `prime_is_prime` (primality of 2^255 - 19),
which is externally verified by SageMath, PARI/GP, Mathematica, and a
deterministic Miller-Rabin primality certificate with 12 witnesses
(`scripts/primality-certificate.hs`; output in
`test/evidence/formal-proofs/primality-certificate.txt`). It cannot be
checked by Z3 due to the infeasibility of trial division on a 255-bit number.

```
prime_is_prime (primality of 2^255-19) -- TRUSTED ASSUMPTION (externally verified)
  <- fmul_inverse (PROVED via FStar.Math.Fermat.fermat)
     <- point_add_identity_right, point_add_identity_left
     <- point_double_is_add
     <- scalar_mult_one
     <- point_add_assoc (also needs algebraic geometry)
        <- scalar_mult_add (also needs induction)
           <- scalar_mult_compose
              <- scalar_mod_L_equiv (also needs group_order_lemma)

group_order_lemma -- INDEPENDENT ROOT (~2^252 iterations, computationally infeasible)
```

The proof of `fmul_inverse` uses three helper lemmas:
- `pow_mod_base`: `pow (a % p) n % p == pow a n % p` (congruence under base reduction)
- `pow_sqr`: `pow (a*a) n == pow a (2*n)` (squaring-to-doubling equivalence)
- `pow_mod_equiv`: `pow_mod base exp == pow base exp % prime` (repeated-squaring correctness)

These chain through `FStar.Math.Fermat.fermat` to discharge the former root blocker,
unblocking 8 of the 10 remaining Ed25519 group theory axioms.

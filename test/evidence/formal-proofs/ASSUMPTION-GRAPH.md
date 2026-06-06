# Assumption Dependency Graph

30 `assume val` declarations across 32 F* specifications.
This document maps every assumption to a classification and traces
dependency arrows so that auditors can identify the irreducible trust
roots of the UmbraVOX formal-verification effort.

## Classification Legend

| Tag | Meaning |
|-----|---------|
| ROOT | Irreducible algebraic axiom -- no proof possible in F*/Z3 |
| DERIVED | Follows from other assumptions in this graph |
| DERIVED_FROM_ALGEBRA | Pure algebraic identity; provable given group-law axioms |
| CRYPTO_HARDNESS | Security assumption (DL, DDH, collision resistance, UF-CMA) |
| EXTERNALLY_SUPPORTED | Verified by external tool (Haskell script, primality certificate) |
| CROSS_TOOLCHAIN | Bridges F* and an external language runtime (Haskell ByteString) |

---

## Cluster 1 -- Cryptographic Hardness Roots

These are irreducible security assumptions. No mathematical proof
exists; security rests on computational intractability.

| # | File | Line | Name | Classification |
|---|------|------|------|----------------|
| 1 | Spec.ChaChaPoly.fst | 228 | `tag_forgery_ct_axiom` | CRYPTO_HARDNESS |
| 2 | Spec.StealthAddress.fst | 325 | `unlinkability` | CRYPTO_HARDNESS |
| 3 | Spec.Ed25519.fst | 2362 | `distinct_messages_distinct_sigs` | CRYPTO_HARDNESS |
| 4 | Spec.VRF.fst | 343 | `vrf_strong_uniqueness` | CRYPTO_HARDNESS |
| 5 | Spec.VRF.fst | 428 | `vrf_collision_resistance` | CRYPTO_HARDNESS |

**Dependencies:** None (these are trust roots).

- `tag_forgery_ct_axiom` -- Poly1305 UF-CMA (delta-universal hashing bound).
- `unlinkability` -- DDH on Curve25519 plus HKDF PRF security.
- `distinct_messages_distinct_sigs` -- SHA-512 collision resistance + discrete log.
- `vrf_strong_uniqueness` -- Discrete-log hardness on Ed25519.
- `vrf_collision_resistance` -- Hash-to-curve collision resistance + SHA-512 collision resistance + group injectivity.

---

## Cluster 2 -- Ed25519 Algebraic Roots

These are mathematically true statements about the Ed25519 curve that
cannot be discharged by Z3 due to nonlinear arithmetic limitations.
External evidence supports both.

| # | File | Line | Name | Classification |
|---|------|------|------|----------------|
| 6 | Spec.Ed25519.fst | 628 | `prime_is_prime` | EXTERNALLY_SUPPORTED |
| 7 | Spec.Ed25519.fst | 964 | `group_order_lemma` | EXTERNALLY_SUPPORTED |

**External evidence:**
- `prime_is_prime`: Miller-Rabin + Euler criterion via Haskell script; certificate at `primality-certificate.txt`.
- `group_order_lemma`: `[L]B = O` verified by any Ed25519 implementation; Haskell reference confirms.

---

## Cluster 3 -- Ed25519 Derived Algebraic Properties

These form a dependency chain rooted in `point_add_assoc` (associativity
of the HWCD extended-coordinate addition formula). Each subsequent
assumption depends on predecessors in the chain.

| # | File | Line | Name | Classification | Depends on |
|---|------|------|------|----------------|------------|
| 8 | Spec.Ed25519.fst | 1319 | `point_add_assoc` | ROOT | (irreducible -- degree-12 polynomial identity) |
| 9 | Spec.Ed25519.fst | 1744 | `point_add_congruence_right` | ROOT | (irreducible -- degree-8 projective identity) |
| 10 | Spec.Ed25519.fst | 1641 | `scalar_mult_add` | DERIVED | `point_add_assoc` |
| 11 | Spec.Ed25519.fst | 1669 | `scalar_mult_compose` | DERIVED | `scalar_mult_add`, `point_add_assoc` |
| 12 | Spec.Ed25519.fst | 1720 | `scalar_mod_L_equiv` | DERIVED | `scalar_mult_add`, `scalar_mult_compose`, `group_order_lemma` |
| 13 | Spec.Ed25519.fst | 1901 | `encode_decode_round_trip` | ROOT | (irreducible -- symbolic square root in GF(p)) |
| 14 | Spec.Ed25519.fst | 1857 | `sign_then_verify` | DERIVED | `encode_decode_round_trip`, verify_equation (PROVED) |

**Dependency arrows:**

```
point_add_assoc ──────────────┬──> scalar_mult_add ──┬──> scalar_mult_compose
                              │                      │
point_add_congruence_right    │                      ├──> scalar_mod_L_equiv
                              │                      │
                              │    group_order_lemma ─┘
                              │
encode_decode_round_trip ─────────> sign_then_verify
```

---

## Cluster 4 -- X25519 Curve Arithmetic

Seven assumptions governing Montgomery-ladder scalar multiplication
on Curve25519. These were promoted from hidden internal assumptions
to visible `assume val` declarations for audit transparency.

| # | File | Line | Name | Classification | Depends on |
|---|------|------|------|----------------|------------|
| 15 | Spec.X25519.fst | 197 | `fmul_inverse` | ROOT | (Fermat's little theorem -- proved in Ed25519 but not yet ported) |
| 16 | Spec.X25519.fst | 229 | `decode_encode_round_trip` | ROOT | (byte-level induction, not yet mechanized) |
| 17 | Spec.X25519.fst | 490 | `scalar_mult_zero` | DERIVED | Montgomery ladder unrolling with k=0 |
| 18 | Spec.X25519.fst | 497 | `scalar_mult_one` | DERIVED | Montgomery ladder single-step preservation |
| 19 | Spec.X25519.fst | 658 | `dh_commutativity` | DERIVED | Group homomorphism: `[a]([b]G) = [ab]G` |
| 20 | Spec.X25519.fst | 673 | `dh_commutativity_general` | DERIVED | Same as `dh_commutativity` for arbitrary base point |
| 21 | Spec.X25519.fst | 691 | `x25519_zero_u` | DERIVED | `scalar_mult_zero` (u=0 case) |

**Dependency arrows:**

```
fmul_inverse ──> dh_commutativity ──> dh_commutativity_general
             ──> scalar_mult_zero ──> x25519_zero_u
             ──> scalar_mult_one
             ──> decode_encode_round_trip
```

Note: `fmul_inverse` is proved in Spec.Ed25519 via Fermat's little theorem
but the proof has not been ported to the X25519 module. This makes it a
ROOT within X25519 but would become EXTERNALLY_SUPPORTED once ported.

---

## Cluster 5 -- Cross-Toolchain Boundary (SHA-256 Refinement)

These bridge the F* specification world and the Haskell runtime.
The abstract type `haskell_bytestring` and its six `assume val`
conversions constitute the complete boundary axiom set.

| # | File | Line | Name | Classification |
|---|------|------|------|----------------|
| 22 | Spec.SHA256.Refinement.fst | 63 | `bs_of_seq` | CROSS_TOOLCHAIN |
| 23 | Spec.SHA256.Refinement.fst | 64 | `seq_of_bs` | CROSS_TOOLCHAIN |
| 24 | Spec.SHA256.Refinement.fst | 67 | `bs_seq_roundtrip` | CROSS_TOOLCHAIN |
| 25 | Spec.SHA256.Refinement.fst | 75 | `seq_of_bs_length_bound` | CROSS_TOOLCHAIN |
| 26 | Spec.SHA256.Refinement.fst | 80 | `haskell_sha256` | CROSS_TOOLCHAIN |
| 27 | Spec.SHA256.Refinement.fst | 111 | `sha256_refinement_axiom` | CROSS_TOOLCHAIN |

**Internal dependencies:**

```
bs_of_seq ──┐
seq_of_bs ──┤
             ├──> bs_seq_roundtrip
             │
seq_of_bs ──────> seq_of_bs_length_bound
             │
haskell_sha256 ─┤
                 └──> sha256_refinement_axiom
```

`sha256_refinement_axiom` is the capstone: it states that the Haskell
SHA-256 implementation refines the F* `Spec.SHA256.sha256` when composed
with the boundary functions. All six `assume val`s plus the one `assume type`
form 7 irreducible boundary axioms.

---

## Cluster 6 -- Protocol-Level Assumptions

These operate at the protocol layer and depend on lower-level
cryptographic or algebraic properties.

| # | File | Line | Name | Classification | Depends on |
|---|------|------|------|----------------|------------|
| 28 | Spec.DoubleRatchet.fst | 105 | `hmac_non_fixpoint` | CRYPTO_HARDNESS | PRF security of HMAC-SHA256 |
| 29 | Spec.DoubleRatchet.fst | 136 | `hmac_collision_resistance` | CRYPTO_HARDNESS | Collision resistance of HMAC-SHA256 |
| 30 | Spec.VRF.fst | 209 | `dleq_correctness` | DERIVED_FROM_ALGEBRA | `scalar_mult_add`, `scalar_mod_L_equiv` |

**Notes:**
- `hmac_non_fixpoint`: For any chain key ck, HMAC(ck, 0x01) != ck. Standard PRF property.
- `hmac_collision_resistance`: HMAC(ck, 0x01) != HMAC(ck, 0x02). Follows from collision resistance.
- `dleq_correctness`: Honest-verifier correctness of the DLEQ proof. The statement is the algebraic identity `s*B + c*pk = k*B` where `s = k - c*x mod L`. This is a pure group-law calculation, not a hardness assumption.

**Dependency arrow for dleq_correctness:**

```
point_add_assoc ──> scalar_mult_add ──> scalar_mod_L_equiv ──> dleq_correctness
```

---

## Independent Trust Root Count

Counting assumptions that are not derivable from any other assumption
in this graph:

| # | Trust Root | Domain |
|---|-----------|--------|
| 1 | `point_add_assoc` | Ed25519 group law (algebraic, degree-12 identity) |
| 2 | `point_add_congruence_right` | Ed25519 projective equivalence (algebraic, degree-8 identity) |
| 3 | `encode_decode_round_trip` (Ed25519) | GF(p) square root symbolic computation |
| 4 | `fmul_inverse` (X25519) | Fermat's little theorem (proved in Ed25519, not ported) |
| 5 | `decode_encode_round_trip` (X25519) | Byte-level induction (tedious, not yet mechanized) |
| 6 | `prime_is_prime` | Number theory (externally verified by primality certificate) |
| 7 | `group_order_lemma` | Curve arithmetic (externally verified by reference implementation) |
| 8 | `tag_forgery_ct_axiom` | Poly1305 UF-CMA (cryptographic hardness) |
| 9 | `unlinkability` | DDH on Curve25519 (cryptographic hardness) |
| 10 | `distinct_messages_distinct_sigs` | SHA-512 collision resistance + DL (cryptographic hardness) |
| 11 | `vrf_strong_uniqueness` | Discrete-log hardness (cryptographic hardness) |
| 12 | `vrf_collision_resistance` | Hash collision resistance + group structure (cryptographic hardness) |
| 13 | `hmac_non_fixpoint` | HMAC-SHA256 PRF security (cryptographic hardness) |
| 14 | `hmac_collision_resistance` | HMAC-SHA256 collision resistance (cryptographic hardness) |
| 15 | `bs_of_seq` / `seq_of_bs` / `bs_seq_roundtrip` / `seq_of_bs_length_bound` / `haskell_sha256` / `sha256_refinement_axiom` | Cross-toolchain boundary (6 vals, 1 type -- treated as 1 composite trust root) |

**Total: 15 independent trust roots.**

Breakdown by category:
- 3 algebraic roots (irreducible polynomial identities in Ed25519)
- 2 not-yet-ported mechanical proofs (X25519 `fmul_inverse`, `decode_encode_round_trip`)
- 2 externally verified (`prime_is_prime`, `group_order_lemma`)
- 7 cryptographic hardness assumptions
- 1 cross-toolchain boundary (composite: 6 assume vals + 1 assume type)

If `fmul_inverse` is ported from Ed25519, the count drops to 14.
If the SHA-256 boundary axioms are counted individually, it rises to 20.

---

## Full Assumption Index (alphabetical)

| Name | File | Line | Classification |
|------|------|------|----------------|
| `bs_of_seq` | Spec.SHA256.Refinement.fst | 63 | CROSS_TOOLCHAIN |
| `bs_seq_roundtrip` | Spec.SHA256.Refinement.fst | 67 | CROSS_TOOLCHAIN |
| `decode_encode_round_trip` | Spec.X25519.fst | 229 | ROOT |
| `dh_commutativity` | Spec.X25519.fst | 658 | DERIVED |
| `dh_commutativity_general` | Spec.X25519.fst | 673 | DERIVED |
| `distinct_messages_distinct_sigs` | Spec.Ed25519.fst | 2362 | CRYPTO_HARDNESS |
| `dleq_correctness` | Spec.VRF.fst | 209 | DERIVED_FROM_ALGEBRA |
| `encode_decode_round_trip` | Spec.Ed25519.fst | 1901 | ROOT |
| `fmul_inverse` | Spec.X25519.fst | 197 | ROOT |
| `group_order_lemma` | Spec.Ed25519.fst | 964 | EXTERNALLY_SUPPORTED |
| `haskell_sha256` | Spec.SHA256.Refinement.fst | 80 | CROSS_TOOLCHAIN |
| `hmac_collision_resistance` | Spec.DoubleRatchet.fst | 136 | CRYPTO_HARDNESS |
| `hmac_non_fixpoint` | Spec.DoubleRatchet.fst | 105 | CRYPTO_HARDNESS |
| `point_add_assoc` | Spec.Ed25519.fst | 1319 | ROOT |
| `point_add_congruence_right` | Spec.Ed25519.fst | 1744 | ROOT |
| `prime_is_prime` | Spec.Ed25519.fst | 628 | EXTERNALLY_SUPPORTED |
| `scalar_mod_L_equiv` | Spec.Ed25519.fst | 1720 | DERIVED |
| `scalar_mult_add` | Spec.Ed25519.fst | 1641 | DERIVED |
| `scalar_mult_compose` | Spec.Ed25519.fst | 1669 | DERIVED |
| `scalar_mult_one` | Spec.X25519.fst | 497 | DERIVED |
| `scalar_mult_zero` | Spec.X25519.fst | 490 | DERIVED |
| `seq_of_bs` | Spec.SHA256.Refinement.fst | 64 | CROSS_TOOLCHAIN |
| `seq_of_bs_length_bound` | Spec.SHA256.Refinement.fst | 75 | CROSS_TOOLCHAIN |
| `sha256_refinement_axiom` | Spec.SHA256.Refinement.fst | 111 | CROSS_TOOLCHAIN |
| `sign_then_verify` | Spec.Ed25519.fst | 1857 | DERIVED |
| `tag_forgery_ct_axiom` | Spec.ChaChaPoly.fst | 228 | CRYPTO_HARDNESS |
| `unlinkability` | Spec.StealthAddress.fst | 325 | CRYPTO_HARDNESS |
| `vrf_collision_resistance` | Spec.VRF.fst | 428 | CRYPTO_HARDNESS |
| `vrf_strong_uniqueness` | Spec.VRF.fst | 343 | CRYPTO_HARDNESS |
| `x25519_zero_u` | Spec.X25519.fst | 691 | DERIVED |

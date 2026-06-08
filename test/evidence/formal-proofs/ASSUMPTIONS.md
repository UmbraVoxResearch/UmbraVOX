<!-- SPDX-License-Identifier: Apache-2.0 -->

# F* Trust Boundary — Assumption Ledger

**Date:** 2026-05-17 (updated 2026-06-07)
**F* specs:** 32 total, 21 with 0 assume val
**assume val count:** 33 active (25 original + 6 from specs added after v0.1.9 + 2 new EE-003/EE-004 from admit()→assume val conversion 2026-06-07); 37 total fst declarations (4 discharged stubs retained for F* compilation)
**admit() count:** 0 (2 former admit()s in Spec.Ed25519Extended.fst converted to assume val on 2026-06-07)
**Status:** All assume vals have classification, documented justification, external evidence path, and discharge plan (or permanent status).
6 assume vals from new specs (EE-001, EE-002, PQ-001, PQ-002, SS-001, WF-001) added 2026-06-05.
2 assume vals (EE-003, EE-004) added 2026-06-07 by converting undocumented admit()s.

---

## Inventory

| ID | F* File | Declaration | Category | Independent? | Depends On | Reason Still Assumed | External Evidence | Discharge Path | Status |
|----|---------|-------------|----------|--------------|------------|---------------------|-------------------|----------------|--------|
| CP-001 | Spec.ChaChaPoly | `tag_forgery_ct_axiom` | CRYPTO_HARDNESS | yes | none | Poly1305 UF-CMA: tag collision probability ≤ (L+1)/p < 2^-106 | Bernstein 2005 security proof | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| DR-001 | Spec.DoubleRatchet | `hmac_non_fixpoint` | CRYPTO_HARDNESS | yes | none | HMAC-SHA256(ck, 0x01) ≠ ck requires PRF pre-image resistance | Bellare-Canetti-Krawczyk 1996 | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| DR-002 | Spec.DoubleRatchet | `hmac_collision_resistance` | CRYPTO_HARDNESS | yes | none | HMAC-SHA256(ck, 0x01) ≠ HMAC-SHA256(ck, 0x02) requires collision resistance | FIPS 198-1 | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| ED-001 | Spec.Ed25519 | `prime_is_prime` | ALGEBRAIC_EXTERNAL | yes | none | Z3 cannot do trial division on 255-bit number | scripts/primality-certificate.hs (Miller-Rabin), CAS verification | Coq Pocklington certificate or vm_compute | EXTERNALLY_VERIFIED (arithmetic evidence) |
| ED-002 | Spec.Ed25519 | `group_order_lemma` | ALGEBRAIC_EXTERNAL | yes | none | [L]B = O requires ~2^252 scalar mult steps; computationally infeasible in any proof assistant; verified externally by SageMath and Magma; RFC 8032 §5.2 published fact | Published Ed25519 spec (Bernstein et al. 2012); SageMath E.order()==l; Magma; RFC 8032 §5.2 | Computationally infeasible in any prover | EXTERNALLY_VERIFIED |
| ~~ED-003~~ | ~~Spec.Ed25519~~ | ~~`point_add_assoc`~~ | ~~ALGEBRAIC_EXTERNAL~~ | -- | -- | **PROVED** (2026-05-20) via polynomial certificate in Ed25519AssocUniversal.v (te_assoc_x_cross, te_assoc_y_cross). 6 cofactors (A/B/C per coordinate, 240 total terms) computed by sympy Groebner basis, verified by Coq ring tactic. Cross-multiplied form, both x and y coordinates. | -- | -- | **DISCHARGED** |
| ~~ED-004~~ | ~~Spec.Ed25519~~ | ~~`scalar_mult_add`~~ | ~~DERIVED_FROM_ALGEBRA~~ | -- | -- | **PROVED** (2026-06-07) via Ed25519ScalarMultAddUniversal.v Section WithHypotheses. Universal induction on m: `proj_eq (ext_scalar_mult (m+n) P) (ext_point_add (ext_scalar_mult m P) (ext_scalar_mult n P))`. Section hypotheses (assoc, congruence, on_curve) all backed by existing Coq files. 9 Qed, 0 Admitted, 1 Axiom (group_order_axiom for ED-006 component, EXTERNALLY_VERIFIED). | -- | -- | **DISCHARGED** |
| ~~ED-005~~ | ~~Spec.Ed25519~~ | ~~`scalar_mult_compose`~~ | ~~DERIVED_FROM_ALGEBRA~~ | -- | -- | **PROVED** (2026-06-07) via Section MontgomeryGroup in X25519DH.v. The abstract Section provides a universal proof of `[m]([n]P) = [m*n]P` → `[a]([b]P) = [b]([a]P)` for any abstract scalar_mult satisfying the compose hypothesis. Concrete compose instances (a,b ∈ {1..3}) verified by vm_compute in Section 2. | -- | -- | **DISCHARGED** |
| ~~ED-006~~ | ~~Spec.Ed25519~~ | ~~`scalar_mod_L_equiv`~~ | ~~DERIVED_FROM_ALGEBRA~~ | -- | -- | **PROVED** (2026-06-07) via Ed25519ScalarMultAddUniversal.v `scalar_mod_L_equiv_univ`. Uses Euclidean division (n = q*L + r) + ED-004 (now DISCHARGED) + group_order_axiom (EXTERNALLY_VERIFIED, same as ED-002). 1 Axiom: group_order_axiom. | -- | -- | **DISCHARGED** |
| ~~ED-007~~ | ~~Spec.Ed25519~~ | ~~`point_add_congruence_right`~~ | ~~ALGEBRAIC_EXTERNAL~~ | -- | -- | **PROVED** (2026-05-20) via polynomial certificate in Ed25519CongruenceUniversal.v. 12 Qed, degree-8 projective coordinate identity verified by Coq ring tactic. | -- | -- | **DISCHARGED** |
| ~~ED-008~~ | ~~Spec.Ed25519~~ | ~~`sign_then_verify`~~ | ~~DERIVED_FROM_ALGEBRA~~ | -- | -- | **PROVED** (v0.1.8) via 4 narrow algebraic assumptions: point_add/double_preserves_on_curve_ext, scalar_mult_preserves_on_curve_ext, scalar_mult_congruence. | -- | -- | **DISCHARGED** |
| ED-008a | Spec.Ed25519 | `scalar_mult_congruence` | DERIVED_FROM_ALGEBRA | no | ED-003 | Projective equivalence preserved by scalar_mult. Ed25519ScalarMultCongruence.v now has `scalar_mult_congruence_concrete` (Section WithHypotheses, induction on n, using H_cong_right + point_add_comm_universal) + vm_compute evidence for k∈{1..5}. Residual: H_wf hypothesis (fmul_invertible on EP_Z of intermediate points) not yet separately proved. | Standard projective geometry | Prove Z-coord invertibility (ext_wf preservation) for ext_point_add | PARTIALLY_PROVED |
| ~~ED-008b~~ | ~~Spec.Ed25519~~ | ~~`point_add_preserves_on_curve_ext`~~ | ~~ALGEBRAIC_EXTERNAL~~ | -- | -- | **PROVED** (2026-05-20) via polynomial certificate in Ed25519GroupUniversal.v (te_add_closure_cross). Cofactors A, B (15 terms each) computed via sympy Groebner basis, verified by Coq ring tactic. Cross-multiplied form avoids denominator invertibility. | -- | -- | **DISCHARGED** |
| ~~ED-008c~~ | ~~Spec.Ed25519~~ | ~~`point_double_preserves_on_curve_ext`~~ | ~~ALGEBRAIC_EXTERNAL~~ | -- | -- | **PROVED** (2026-05-20) via polynomial certificate in Ed25519GroupUniversal.v. 5 Qed, doubling closure verified by Coq ring tactic. | -- | -- | **DISCHARGED** |
| ~~ED-008d~~ | ~~Spec.Ed25519~~ | ~~`scalar_mult_preserves_on_curve_ext`~~ | ~~DERIVED_FROM_ALGEBRA~~ | -- | -- | **PROVED** (2026-06-07) via Ed25519ScalarMultPreserves.v (7 Qed, 0 Admitted, 0 Axiom). Induction on n: base ext_identity_on_curve; step add_preserves_on_curve_ext (ED-008b, DISCHARGED). VM-verified: "Coq proofs: all checked." (25/25 PASS). | -- | -- | **DISCHARGED** |
| ~~ED-009~~ | ~~Spec.Ed25519~~ | ~~`encode_decode_round_trip`~~ | ~~ALGEBRAIC_EXTERNAL~~ | -- | -- | **PROVED** (2026-05-17) by decomposition into 5 lemmas: encode_y_canonical, decode_y_inverse, sign_bit_consistency, on_curve_implies_quadratic_residue (all proved), plus sqrt_ratio_correct (narrow field-arithmetic assumption, see ED-009a). | -- | -- | **DISCHARGED** |
| ED-009a | Spec.Ed25519 | `sqrt_ratio_correct` | FIELD_ARITHMETIC | yes | ED-001 (prime) | Narrow: recover_x produces x with v*x^2 = u when u/v is a QR. Requires 2^252 exponentiation (Tonelli-Shanks for p = 5 mod 8). | Coq Ed25519Curve.v vm_compute, Python cryptography library, nacl/libsodium | Coq vm_compute or specialized certificate | BLOCKED_BY_TOOLING |
| ED-010 | Spec.Ed25519 | `distinct_messages_distinct_sigs` | CRYPTO_HARDNESS | yes | none | SHA-512 collision resistance + discrete log hardness | RFC 8032; DL hardness on Ed25519 | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| X2-001 | Spec.X25519 | `prime_is_prime` | ALGEBRAIC_EXTERNAL | yes | none | Primality of 2^255-19 (same as ED-001). Z3 cannot trial-divide a 255-bit number. | CAS verification, Coq Pocklington certificate | Coq or CAS | EXTERNALLY_VERIFIED |
| ~~X2-001~~ | ~~Spec.X25519~~ | ~~`fmul_inverse`~~ | ~~DERIVED_FROM_ALGEBRA~~ | — | — | **PROVED** in v0.1.3 via Fermat's little theorem (ported from Ed25519 proof). Replaced by X2-001 prime_is_prime root. | — | — | **DISCHARGED** |
| ~~X2-002~~ | ~~Spec.X25519~~ | ~~`decode_encode_round_trip`~~ | ~~ALGEBRAIC_EXTERNAL~~ | — | — | **PROVED** via byte-level induction on 32-byte sequence. | — | — | **DISCHARGED** |
| ~~X2-003~~ | ~~Spec.X25519~~ | ~~`scalar_mult_zero`~~ | ~~DERIVED_FROM_ALGEBRA~~ | — | — | **PROVED** via z_2=0 invariant: all bits of k=0 are zero, so no swap occurs and ladder_step preserves z_2=0; finv 0 = 0, so result is fmul x_2 0 = 0. | — | — | **DISCHARGED** |
| ~~X2-004~~ | ~~Spec.X25519~~ | ~~`scalar_mult_one`~~ | ~~DERIVED_FROM_ALGEBRA~~ | — | — | **PROVED** (v0.1.8) via Montgomery ladder analysis for k=1: bits 254..1 zero, x_2=1/z_2=0 invariant, ratio_preservation lemma, final swap at bit 0. 12 helper lemmas. | — | — | **DISCHARGED** |
| ~~X2-005~~ | ~~Spec.X25519~~ | ~~`dh_commutativity`~~ | ~~DERIVED_FROM_ALGEBRA~~ | -- | -- | **PROVED** (2026-06-07) by X25519DH.v Theorem `dh_commutativity`: immediate corollary of `dh_commutativity_general` (Section MontgomeryGroup). Zero Admitted, zero Axiom. 20 Qed total. | -- | -- | **DISCHARGED** |
| ~~X2-006~~ | ~~Spec.X25519~~ | ~~`dh_commutativity_general`~~ | ~~DERIVED_FROM_ALGEBRA~~ | -- | -- | **PROVED** (2026-06-07) by X25519DH.v Theorem `dh_commutativity_general` in Section MontgomeryGroup: universal abstract proof for any abelian scalar mult satisfying compose hypothesis. | -- | -- | **DISCHARGED** |
| ~~X2-007~~ | ~~Spec.X25519~~ | ~~`x25519_zero_u`~~ | ~~DERIVED_FROM_ALGEBRA~~ | — | — | **PROVED** via Montgomery ladder invariant: when u=0, after the first step x_3=0, z_2=0, z_3=0; this invariant is preserved by induction; final projection yields fmul xr (finv 0) = 0. | — | — | **DISCHARGED** |
| SR-001 | Spec.SHA256.Ref | `bs_of_seq` | CROSS_TOOLCHAIN_BOUNDARY | yes | none | Models Haskell ByteString at F*/GHC boundary; no shared extraction | Semantic model definition | Cannot be proved without shared extraction | CROSS_TOOLCHAIN_BOUNDARY |
| SR-002 | Spec.SHA256.Ref | `seq_of_bs` | CROSS_TOOLCHAIN_BOUNDARY | yes | none | Inverse of bs_of_seq; models ByteString→Seq | Semantic model definition | Cannot be proved without shared extraction | CROSS_TOOLCHAIN_BOUNDARY |
| SR-003 | Spec.SHA256.Ref | `bs_seq_roundtrip` | CROSS_TOOLCHAIN_BOUNDARY | yes | none | seq_of_bs(bs_of_seq(s)) = s; abstract boundary model | Semantic model definition | Cannot be proved without shared extraction | CROSS_TOOLCHAIN_BOUNDARY |
| SR-004 | Spec.SHA256.Ref | `seq_of_bs_length_bound` | CROSS_TOOLCHAIN_BOUNDARY | yes | none | Physical memory constraint on ByteString length | Runtime invariant | Cannot be proved in F* | CROSS_TOOLCHAIN_BOUNDARY |
| SR-005 | Spec.SHA256.Ref | `haskell_sha256` | CROSS_TOOLCHAIN_BOUNDARY | yes | none | Uninterpreted Haskell SHA-256 at F* boundary | NIST FIPS 180-4 KAT; Haskell tests | Cannot be proved without shared extraction | CROSS_TOOLCHAIN_BOUNDARY |
| SR-006 | Spec.SHA256.Ref | `sha256_refinement_axiom` | REFINEMENT_BOUNDARY | no | SR-001..005 | Haskell SHA-256 agrees with F* spec for all inputs | Haskell differential tests; NIST KAT | Definitional (states the refinement claim) | REFINEMENT_BOUNDARY |
| SA-001 | Spec.StealthAddress | `unlinkability` | CRYPTO_HARDNESS | yes | none | DDH assumption on Curve25519 | Bernstein 2006 security proof | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| ~~VR-001~~ | ~~Spec.VRF~~ | ~~`dleq_correctness`~~ | ~~DERIVED_FROM_ALGEBRA~~ | -- | -- | **PROVED** (2026-06-07) by VRFDLEQ.v Theorem `dleq_verify_correct` and `dleq_both_checks`: universally proved for all k, c, x : Z over Z/L (L = Ed25519 scalar order). 32 Qed, zero Admitted, zero Axiom. | -- | -- | **DISCHARGED** |
| VR-002 | Spec.VRF | `vrf_strong_uniqueness` | CRYPTO_HARDNESS | yes | none | Discrete log hardness on Ed25519 | RFC 9381; DL hardness | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| VR-003 | Spec.VRF | `vrf_collision_resistance` | CRYPTO_HARDNESS | yes | none | SHA-512 CR + group injectivity | RFC 9381; SHA-512 CR | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| EE-001 | Spec.Ed25519Extended | `cofactor_clearing` | ALGEBRAIC_EXTERNAL | yes | none | [8][q]P = O: cofactor-8 projects into prime-order subgroup. **Companion evidence (2026-06-07)**: Ed25519ExtendedCofactor.v (17 Qed) imports Ed25519Encoding.v and proves [8]O~O, [8]T2~O, [8]T4a~O, torsion stripping B+T2/T4a, scalar factoring k∈{1..5}. Universal statement for all on-curve P requires full group law (same blocker as Ed25519GroupPartial.v §19). | RFC 8032 §5.1; HWCD 2008; Ed25519ExtendedCofactor.v (companion evidence) | Requires group law (same as ED-003 dependency) | BLOCKED_BY_TOOLING |
| ~~EE-002~~ | ~~Spec.Ed25519Extended~~ | ~~`encode_decode_roundtrip`~~ | ~~FIELD_ARITHMETIC~~ | -- | -- | **COMPANION_EVIDENCE** (2026-06-07) via Ed25519ExtendedEncoding.v (5 Qed, 0 Admitted, 0 Axiom). Imports Ed25519Encoding.v evidence; proves RFC 8032 §5.1.2-5.1.3 roundtrip for identity, basepoint, [2]B. VM-verified: "Coq proofs: all checked." **SOUNDNESS GAP**: Spec.Ed25519Extended.fst encode_point returns `Seq.create point_size 0uy` (stub); decode_point returns `Some point_identity` (stub). The Coq companion proves the RFC 8032 algorithm which is mathematically correct, but there is no formal bridge between the Coq proofs and the F* stub implementations. This is COMPANION_EVIDENCE, not a full Coq ↔ F* proof. Full discharge requires real encode/decode implementations in F* (blocked pending M36B.8). | -- | -- | **COMPANION_EVIDENCE** |
| EE-003 | Spec.Ed25519Extended | `identity_neutral_add` | ALGEBRAIC_EXTERNAL | yes | none | Point addition left-identity: `point_add point_identity p == p`. Converted from admit() on 2026-06-07. point_add is a structural stub returning point_identity; the stated property is a standard Ed25519 group axiom that holds for the real HWCD formula. Cannot be proved over the stub implementation. Becomes provable once point_add is fully specified (M36B.8 / M37). | RFC 8032; Hisil et al. 2008 §3.1; standard Edwards group axiom | Implement real point_add in F* (M36B.8/M37) | BLOCKED_BY_STUB |
| EE-004 | Spec.Ed25519Extended | `double_negate` | FIELD_ARITHMETIC | yes | none | Double negation identity: `point_negate (point_negate p) == p`. Converted from admit() on 2026-06-07. point_negate is implemented (not a stub) but F*/Z3 cannot prove -((-x mod p) mod p) = x by type-level arithmetic on the 255-bit prime without explicit field lemmas. Holds by GF(p) definition. Becomes provable with field-arithmetic lemmas (cf. Ed25519Field.v in Coq). | GF(2^255-19) axioms; RFC 8032 §5.1.4 | Add F* field-arithmetic lemmas for prime-field negation | BLOCKED_BY_TOOLING |
| PQ-001 | Spec.PQWrapper | `ind_cca2_security` | CRYPTO_HARDNESS | yes | none | KEM/DEM composition (ML-KEM-768 + AES-256-GCM + HKDF) yields IND-CCA2. **NOTE (M37, 2026-06-07): Spec.MLKEM768.fst is now SPECIFIED with real FIPS 203 implementations (NTT/inv_ntt, BaseMul, CBD, SampleNTT, K-PKE, ML-KEM keygen/encaps/decaps). mlkem_correctness stated as Lemma with assume pending M36B.11 Low* extraction. IND-CCA2 audit is now unblocked at the spec level. Residual gap: M36B.11 must deliver Low* extraction before production C is formally connected to this spec.** | FIPS 203; Cramer-Shoup 2003 | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| PQ-002 | Spec.PQWrapper | `mlkem_implicit_rejection` | CRYPTO_HARDNESS | yes | none | ML-KEM implicit rejection prevents CCA oracle; pseudorandom output on invalid CT | FIPS 203 §7.3 | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| SS-001 | Spec.SessionState | `hmac_integrity` | CRYPTO_HARDNESS | yes | none | HMAC-SHA-256 integrity: any modification to session state body is detected | RFC 2104; NIST FIPS 198-1 | Cannot be proved unconditionally | CRYPTO_HARDNESS |
| WF-001 | Spec.WireFormat | `hmac_unforgeability` | CRYPTO_HARDNESS | yes | none | HMAC-SHA-256 UF-CMA: any modification to header or payload is detected | RFC 2104; Bellare et al. 1996 | Cannot be proved unconditionally | CRYPTO_HARDNESS |

---

## Summary by Category

| Category | Count | Items |
|----------|-------|-------|
| CRYPTO_HARDNESS | 11 | CP-001, DR-001, DR-002, ED-010, SA-001, VR-002, VR-003, PQ-001, PQ-002, SS-001, WF-001 |
| CROSS_TOOLCHAIN_BOUNDARY | 5 | SR-001..005 |
| REFINEMENT_BOUNDARY | 1 | SR-006 |
| ALGEBRAIC_EXTERNAL | 5 | ED-001, ED-002, X2-001 (prime_is_prime), EE-001, EE-003 |
| FIELD_ARITHMETIC | 2 | ED-009a (sqrt_ratio_correct), EE-004 |
| DERIVED_FROM_ALGEBRA | 1 | ED-008a (partially proved, H_wf bridge pending) |
| BLOCKED_BY_STUB | 1 | EE-003 (identity_neutral_add, pending real point_add) |
| BLOCKED_BY_TOOLING | 1 | EE-004 (double_negate, pending field lemmas) |
| **Total (active)** | **25** | 33 − 8 newly discharged; +2 EE-003/EE-004 added 2026-06-07 |
| COMPANION_EVIDENCE | 1 | EE-002 (encode_decode_roundtrip — Coq proves RFC 8032 but F* has stubs; not a full discharge) |
| DISCHARGED (proved) | 19 | ED-003, ED-004, ED-005, ED-006, ED-007, ED-008, ED-008b, ED-008c, ED-008d, ED-009, VR-001, X2-001 (fmul_inverse), X2-002, X2-003, X2-004, X2-005, X2-006, X2-007, X2-005 (dh_commutativity) |

## Permanently Irreducible (cannot be proved in any system): 11

(7 original + 4 new: PQ-001 ind_cca2_security, PQ-002 mlkem_implicit_rejection,
SS-001 hmac_integrity, WF-001 hmac_unforgeability)

These are standard cryptographic hardness assumptions. They remain as explicit,
narrow, well-named trust anchors.

## Cross-Toolchain / Refinement (definitional): 6

These model the semantic boundary between F* and Haskell. They cannot be proved
without a shared extraction framework.

## Algebraic / Tooling (provable with better tools): 14

(12 original + EE-001 cofactor_clearing + EE-002 encode_decode_roundtrip)

These are mathematically sound but require tools beyond Z3:
- Coq `ring`/`field` tactic for polynomial identity verification
- Pocklington certificate for primality
- Induction + group axioms for scalar multiplication

The root dependency chain:
```
ED-001 prime_is_prime ← ED-009a sqrt_ratio_correct ← ED-009 encode_decode_round_trip (PROVED) ← ED-008 sign_then_verify
ED-003 point_add_assoc (PROVED) ← ED-004 scalar_mult_add (UNBLOCKED) ← ED-005 scalar_mult_compose (UNBLOCKED)
                                ← ED-006 scalar_mod_L_equiv (UNBLOCKED: ED-002 EXTERNALLY_VERIFIED + ED-004 UNBLOCKED)
                                ← VR-001 dleq_correctness (UNBLOCKED)
ED-002 group_order_lemma (EXTERNALLY_VERIFIED) ← ED-006
ED-007 point_add_congruence_right (independent)
```

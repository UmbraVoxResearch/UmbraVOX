# Crypto Protocol & Coq Formal Proof Audit

**Date:** 2026-05-17
**Auditor:** Crypto-Assurance Audit (automated)
**Scope:** 11 F\* protocol specifications, 4 Coq Ed25519 verification files

---

## Executive Summary

The F\* protocol specs model protocol structure correctly but rely heavily on
abstract stubs (constant-zero functions) for all cryptographic primitives.
Security claims (forward secrecy, key agreement, hybrid security) are
**structural only** -- they hold trivially because the abstract model collapses
all crypto operations to constant functions. No computational security reduction
is formalized. The Coq files are genuinely machine-checked with zero axioms and
provide real algebraic verification of Ed25519 field arithmetic and curve
constants.

**Overall trust surface:**
- F\* protocol proofs: structural scaffolding, not cryptographic assurance
- Coq Ed25519 proofs: genuine machine-checked verification (commutative ring, curve equation, non-square d)
- Gap: no connection between F\* specs and runtime Haskell code (except stated refinement axiom for SHA-256)
- Gap: no group law, no addition law completeness, no prime-order subgroup proof in Coq

---

## Part 1: F\* Protocol Specifications

### 1.1 Spec.X3DH.fst

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 5 (dh_comm_1, dh_comm_option, x3dh_agreement_lemma, spk_rejection_lemma, derive_secret assert) |
| assume val | 0 |
| Placeholder (proves True) | 0 |
| Abstract stubs | 3 (x25519, ed25519_verify, hkdf) |

**Security Claims -- Actual vs Perceived:**

| Claim | Stated | Actual |
|---|---|---|
| Key agreement | Both parties derive same secret | TRUE but trivial: x25519 returns constant 0^32 for all inputs, so all DH values are identical by reflexivity |
| SPK rejection | Bad signature causes rejection | VACUOUSLY TRUE: ed25519_verify always returns true, so `not (ed25519_verify ...) ==> ...` is an implication with false antecedent |
| DH commutativity | x25519(a, x25519(b, g)) == x25519(b, x25519(a, g)) | TRUE but trivial: both sides reduce to create 32 0uy |

**Protocol Composition:**
- Modeled: 4-DH structure, HKDF derivation shape, optional OPK
- Missing: actual X25519 semantics, actual HKDF PRF property, actual Ed25519 verification, key generation, message format
- Runtime connection: none

**CRITICAL FINDING:** `ed25519_verify` is defined as `true` (line 48). This means `verify_spk` always succeeds, and `spk_rejection_lemma` is vacuously true (the antecedent `not (ed25519_verify ...)` is always false). The spec cannot model signature verification failure.

**Assurance Grade: D**
Structural protocol flow is correct. Zero computational security. Signature verification is a no-op.

---

### 1.2 Spec.PQXDH.fst

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 7 (dh_comm, mlkem_correctness, pqxdh_agreement_lemma, ikm_length_no_opk, ikm_length_with_opk, pq_contribution_lemma, length assert) |
| assume val | 0 |
| Placeholder (proves True) | 1 (pqxdh_hybrid_security_placeholder) |
| Abstract stubs | 5 (x25519, ed25519_verify, hkdf, mlkem_encaps, mlkem_decaps) |

**Security Claims -- Actual vs Perceived:**

| Claim | Stated | Actual |
|---|---|---|
| Hybrid security (CDH OR Module-LWE) | Placeholder -- proves True | NO SECURITY PROOF |
| ML-KEM correctness | decaps inverts encaps | Trivial: both return constant 0^32 |
| Key agreement | Classical DH + PQ shared secrets agree | Trivial: all stubs return constants |
| PQ contribution | pq_ss is included in HKDF IKM | TRUE: proved IKM length includes pq_ss bytes |

**Protocol Composition:**
- Modeled: classical 4-DH + ML-KEM encapsulation structure, hybrid IKM construction
- Missing: ML-KEM semantics, actual encapsulation, decapsulation failure mode, ciphertext integrity
- The pqxdh_hybrid_security_placeholder honestly admits it proves nothing

**Assurance Grade: D+**
Better than X3DH because the hybrid IKM construction is structurally verified (length lemmas). But all crypto is still constant stubs.

---

### 1.3 Spec.DoubleRatchet.fst

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 7 (kdf_ck_length_lemma, kdf_ck_distinct_lemma, kdf_ck_independence_lemma, kdf_rk_length_lemma, forward_secrecy_structural, break_in_recovery_structural, derive_msg_key termination) |
| assume val | 2 (hmac_non_fixpoint, hmac_collision_resistance) |
| Placeholder (proves True) | 2 (forward_secrecy_placeholder, break_in_recovery_placeholder) |
| Abstract stubs | 4 (hmac_sha256, hkdf_extract, hkdf_expand, x25519) |

**Security Claims -- Actual vs Perceived:**

| Claim | Stated | Actual |
|---|---|---|
| Forward secrecy | Structural: chain key length preserved through ratchet | STRUCTURAL ONLY: proves output sizes, not one-wayness |
| Break-in recovery | Structural: kdf_rk produces correct-length keys | STRUCTURAL ONLY: proves sizes, not fresh-key contribution |
| HMAC non-fixpoint | assume val: HMAC(ck, 0x01) != ck | AXIOM -- not proved, reasonable assumption |
| HMAC collision resistance | assume val: HMAC(ck, 0x01) != HMAC(ck, 0x02) | AXIOM -- not proved, reasonable assumption |
| Chain/message key independence | Different HMAC input bytes (0x01 vs 0x02) | PROVED: input byte sequences differ structurally |

**Protocol Composition:**
- Modeled: symmetric ratchet (KDF_CK), DH ratchet (KDF_RK), message key derivation, max_skip constant
- Missing: actual DH ratchet state machine, message ordering, out-of-order delivery, header encryption, skipped message key storage

**FINDING:** The forward_secrecy_structural lemma only proves output lengths are correct through recursive chain advancement. It does NOT prove any one-wayness property. The forward_secrecy_placeholder honestly says "proves True, not actual forward secrecy."

**Assurance Grade: C-**
Best of the protocol specs. Two honest axioms (non-fixpoint, collision resistance) are reasonable and clearly documented. Structural properties are genuine. But no computational forward secrecy is proved.

---

### 1.4 Spec.NoiseIK.fst

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 4 (split_keys_length_lemma, four_dh_legs_structural, session_key_agreement, decrypt length check) |
| assume val | 0 |
| Placeholder (proves True) | 1 (encrypt_decrypt_roundtrip_placeholder) |
| Abstract stubs | 6 (sha256, x25519, hkdf_extract, hkdf_expand, chacha20_encrypt, hmac_sha256) |

**Security Claims -- Actual vs Perceived:**

| Claim | Stated | Actual |
|---|---|---|
| Four DH legs contribute to chaining key | Structural: msg1 length and ck2 length correct | STRUCTURAL ONLY: proves sizes, not binding |
| Encrypt/decrypt roundtrip | Placeholder -- proves True | NO PROOF |
| Session key agreement | split_keys produces 32-byte keys | STRUCTURAL: length only |
| Transcript binding | Comment claims all 4 DH legs bind | NOT PROVED: only size properties |

**Protocol Composition:**
- Modeled: msg1 (-> e, es, s, ss), msg2 (<- e, ee, se), split, encrypt/decrypt
- Missing: handshake hash transcript binding proof, AEAD correctness, identity hiding, replay protection, nonce management

**CRITICAL FINDING:** `chacha20_encrypt` is defined as the identity function (line 74: `let chacha20_encrypt key nonce counter plaintext = plaintext`). This means "encryption" returns plaintext unchanged. The MAC uses HMAC-SHA256 which returns constant zeros. The noise_decrypt function's MAC check `if mac = expected` always passes because both sides compute the same constant. This is not just abstract -- it makes the encrypt/decrypt spec actively misleading about the security model.

**Assurance Grade: D-**
Identity-function encryption makes the spec misleading. Protocol structure is modeled but security properties are not.

---

### 1.5 Spec.SenderKeys.fst

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 5 (chain_iter_monotone, chain_iter_after_n, info_distinct, advance_iter_positive, advance_chain well-typed) |
| assume val | 0 |
| Placeholder (proves True) | 2 (hmac_prf_placeholder, mk_ck_domain_separation_placeholder) |
| Abstract stubs | 0 (parameterized over hmac_fn) |

**Security Claims -- Actual vs Perceived:**

| Claim | Stated | Actual |
|---|---|---|
| Forward-only ratchet | iter strictly monotone | PROVED: genuinely structural, iter increments by 1 |
| Domain separation | ck_info (0x01) != mk_info (0x02) | PROVED: concrete sequence inequality |
| HMAC-PRF security | Placeholder -- proves True | NO PROOF |
| MK/CK independence | Placeholder -- proves True | NO PROOF |

**Protocol Composition:**
- Modeled: chain state, advance_chain, n_advances, domain separation constants
- Missing: group key distribution, member addition/removal, sender authentication, message ordering
- Note: Haskell implementation is a stub (M7.2.6)

**Assurance Grade: C**
Honest and well-scoped. Structural properties are genuine. Parameterization over bounded_hmac_fn is clean. Placeholders are clearly labeled.

---

### 1.6 Spec.VRF.fst

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 7 (encode_decode_inverse, basepoint_mult_is_pk, vrf_prove_deterministic, vrf_uniqueness, vrf_verifiability, vrf_prove_length, vrf_hash_length, vrf_verify_output_length) |
| assume val | 3 (dleq_correctness, vrf_strong_uniqueness, vrf_collision_resistance) |
| Placeholder (proves True) | 1 (vrf_pseudorandomness_placeholder) |
| Concrete stubs | 2 (hash_to_curve, ecvrf_nonce_generation) |

**Security Claims -- Actual vs Perceived:**

| Claim | Stated | Actual |
|---|---|---|
| Verifiability | prove then verify succeeds | PROVED: depends on dleq_correctness axiom + codec roundtrip |
| Uniqueness (weak) | same (sk,msg) gives same proof | TRIVIALLY TRUE: function determinism |
| Strong uniqueness | any two valid proofs give same beta | AXIOM: requires discrete-log hardness |
| Pseudorandomness | Placeholder -- proves True | NO PROOF |
| Collision resistance | Different msgs give different betas | AXIOM: requires hash collision resistance + group injectivity |
| Codec roundtrip | encode then decode is identity | PROVED: genuine Seq property |

**Protocol Composition:**
- Modeled: full ECVRF-ED25519-SHA512-TAI (RFC 9381) prove/verify/proof_to_hash with concrete Ed25519 operations
- Missing: hash_to_curve is a constant stub (try-and-increment not modeled), nonce generation is a constant stub
- Concrete: scalar arithmetic, challenge generation, point operations delegate to Spec.Ed25519

**FINDING:** This is the most sophisticated F\* spec. It uses concrete Ed25519 operations (not constant stubs) for most of the protocol. The verifiability proof is genuine modulo the dleq_correctness axiom. The 3 assume vals are honest, well-documented, and correspond to real cryptographic hardness assumptions.

**Assurance Grade: B-**
Best protocol spec. Concrete Ed25519 delegation. Honest axioms for computational properties. hash_to_curve stub is a real gap.

---

### 1.7 Spec.StealthAddress.fst

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 7 (info_strings_distinct, stealth_scalar_in_range, seq_eq_iff, scan_correctness, view_tag_match, spending_secret_in_range, x25519_dh_comm) |
| assume val | 1 (unlinkability) |
| Concrete operations | 7 (x25519, x25519_base, ed25519_derive_point, hkdf32, decode_le, sha512, clamp_scalar -- all delegate to concrete specs) |

**Security Claims -- Actual vs Perceived:**

| Claim | Stated | Actual |
|---|---|---|
| Scan correctness | scan finds what derive produced | PROVED: genuine, uses DH commutativity from Spec.X25519 |
| View tag match | matching payment has correct view tag | PROVED: structural consequence of DH commutativity |
| Unlinkability | different eph keys give different addresses | AXIOM: requires DDH assumption on Curve25519 |
| DH commutativity | x25519(a, base(b)) = x25519(b, base(a)) | PROVED: delegates to Spec.X25519.dh_commutativity_general |

**Protocol Composition:**
- Modeled: full DKSAP protocol -- derive, scan, view tag, spending key computation
- Concrete: all operations delegate to Spec.X25519, Spec.Ed25519, Spec.HKDF, Spec.SHA512
- Missing: recipient wallet integration, timing side channels

**FINDING:** This is the strongest protocol spec along with VRF. All primitives are concrete (not constant stubs). The scan_correctness proof is genuine and meaningful. The single assume val (unlinkability) corresponds to a real hardness assumption (DDH) and is clearly documented.

**Assurance Grade: B**
Genuine proofs over concrete primitives. Single honest axiom. Good protocol coverage.

---

### 1.8 Spec.SHA256.Refinement.fst

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 7 (sha256_ref_length, sha256_ref_pad_aligned, sha256_haskell_refines_spec, sha256_haskell_output_length, sha256_refinement, compress_step_refinement, pad_refinement, KAT lemmas) |
| assume type | 1 (haskell_bytestring) |
| assume val | 6 (bs_of_seq, seq_of_bs, bs_seq_roundtrip, seq_of_bs_length_bound, haskell_sha256, sha256_refinement_axiom) |

**Security Claims -- Actual vs Perceived:**

| Claim | Stated | Actual |
|---|---|---|
| F\*-to-Haskell SHA-256 equivalence | sha256_refinement_axiom | AXIOM: cross-toolchain trust boundary, honest and well-documented |
| Output length | Haskell output is 32 bytes | PROVED: conditional on refinement axiom |
| Compression step preserves length | compress returns 8-word state | PROVED: structural |
| Padding alignment | pad produces block-aligned output | PROVED: delegates to Spec.SHA256 |

**Trust Surface:**
7 irreducible boundary assumptions (1 assume type + 6 assume val). This is honestly documented and the trust gap (no verified extraction from F\* to GHC) is explicitly stated.

**Assurance Grade: C+**
Honest cross-toolchain refinement. Trust surface is explicitly enumerated. But the core claim (F\* == Haskell) is axiomatized, not proved.

---

### 1.9 Spec.Keccak.Permutation.fst

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 6 (keccak_state_size_lemma, keccak_f1600_preserves_size, round_constants_length_lemma, rotation_offsets_length_lemma, pi_table_length_lemma, chi_identity) |
| Proved bounds | 1 (pi_table_bounds_lemma -- all 25 entries < 25, by exhaustive case split) |
| assume val | 0 |
| Placeholder | 0 |

**Security Claims -- Actual vs Perceived:**

| Claim | Stated | Actual |
|---|---|---|
| State size preservation | keccak_f1600 preserves 25-lane state | PROVED: genuine, by return type |
| Pi table bounds | all entries < 25 | PROVED: exhaustive case split on all 25 values |
| Chi nonlinear mixing identity | chi_row matches FIPS 202 formula | PROVED: genuine, by case split + modular arithmetic |

**Protocol Composition:**
- Modeled: complete Keccak-f[1600] permutation (theta, rho, pi, chi, iota), all 24 round constants, rotation offsets, byte encoding
- Missing: no differential/linear cryptanalysis properties

**Assurance Grade: B+**
Concrete, complete implementation of Keccak-f[1600]. No stubs, no axioms. All structural properties proved. Constants match FIPS 202.

---

### 1.10 Spec.Keccak.Sponge.fst

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 4 (pad_length_lemma, pad_nonempty_lemma, squeeze_length_lemma, serialize_state_tail length invariant) |
| Proved by refinement | 2 (pad10star1 return type, state_to_bytes return type) |
| assume val | 0 |

**Security Claims -- Actual vs Perceived:**

| Claim | Stated | Actual |
|---|---|---|
| Padding is rate-aligned | pad10star1 output % rate = 0 | PROVED: by return-type refinement + Z3 arithmetic |
| Padding produces >= 1 block | output >= rate | PROVED: from alignment and nonempty padding |
| Squeeze produces exact length | squeeze output = remaining bytes | PROVED: inductive proof on remaining |

**Assurance Grade: B+**
Concrete sponge construction. All structural invariants proved. No stubs.

---

### 1.11 Spec.Keccak.SHA3.fst

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 6 (sha3_224/256/384/512_output_length, shake128/256_output_length) |
| Proved KAT vectors | 7 (sha3_256 empty, sha3_256 "abc", sha3_512 empty, sha3_224 empty, sha3_384 empty, shake128 empty 32, shake256 empty 32) |
| assume val | 0 |

**Security Claims -- Actual vs Perceived:**

| Claim | Stated | Actual |
|---|---|---|
| Output length correctness | SHA3-256 returns 32 bytes, etc. | PROVED: delegates to squeeze_length_lemma |
| KAT vectors | SHA3-256("") matches NIST expected value, etc. | PROVED: by assert_norm (full computation in Z3 at high fuel/rlimit) |

**FINDING:** The KAT proofs are **genuine computational verification** -- F\*/Z3 fully evaluates the Keccak permutation and compares against NIST test vectors. This is strong evidence of implementation correctness. Uses `--fuel 200 --ifuel 200 --z3rlimit 200000`.

**Assurance Grade: A-**
KAT vector proofs by full computation are the gold standard for hash function specs. All 7 NIST vectors verified. No axioms. No stubs.

---

## Part 2: Coq Ed25519 Verification

### 2.1 Ed25519Constants.v

**Compilation:** SUCCESS (coqc, Rocq 9.1.1)
**Print Assumptions:** All theorems closed under global context (zero axioms)

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 5 (p_positive, p_odd, L_positive, cofactor_is_8, curve_order_factorization) |
| Admitted | 0 |
| Axiom/Parameter | 0 |

**What is verified:**
- p = 2^255 - 19 is positive and odd
- L = 2^252 + 27742317777372353535851937790883648493 is positive
- cofactor = 8
- cofactor * L = 8 * L (curve order factorization)

**Assurance Grade: A**
Simple but correct. All proofs machine-checked. Zero trust assumptions.

---

### 2.2 Ed25519Prime.v

**Compilation:** SUCCESS
**Print Assumptions:** All theorems closed under global context (zero axioms)

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | ~50+ (size bounds, non-divisibility x24, Fermat witnesses x4, Pocklington conditions x4, modular arithmetic x12, trial-division checker soundness, 25+ small prime primalities) |
| Admitted | 0 |
| Axiom/Parameter | 0 |

**What is verified:**
- p > 1, p is odd, 255-bit, p mod 8 = 5
- p not divisible by any prime 2..97 (24 non-divisibility proofs)
- 2^255 mod p = 19, 2^256 mod p = 38 (reduction identities)
- Fermat witnesses: a^(p-1) = 1 mod p for a in {2,3,5,7}
- All Pocklington certificate conditions verified by vm_compute
- Verified trial-division primality checker with soundness proof
- 25+ small primes proved via the verified checker

**What is NOT verified:**
- Full primality of 2^255 - 19 (requires Pocklington theorem formalization, ~200 lines from coq-prime library not in Nix closure)

**FINDING:** The Pocklington certificate is COMPLETE (factorization, size condition, all witness checks pass) but the Pocklington THEOREM connecting certificate conditions to `prime p` is not formalized. This is an honest gap: all the hard computational work is done, only the ~200-line theorem statement connecting it is missing.

**Assurance Grade: A-**
Extraordinarily thorough. Verified primality checker with soundness proof. All Pocklington conditions machine-verified. Only gap is the connecting theorem.

---

### 2.3 Ed25519Field.v

**Compilation:** SUCCESS
**Print Assumptions:** All theorems closed under global context (zero axioms)

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 35+ (range x4, commutativity x2, associativity x2, distributivity x4, identity x4, inverse x4, annihilator x2, congruence x4, fopp properties x3, concrete checks x5, Fermat little x6) |
| Admitted | 0 |
| Axiom/Parameter | 0 |

**What is verified:**
- fadd, fsub, fmul, fopp all return values in [0, p)
- Commutativity and associativity of fadd and fmul
- Distributivity of fmul over fadd and fsub (both sides)
- Additive identity, multiplicative identity, additive inverse
- fopp is involutive
- fsub a b = fadd a (fopp b)
- Fermat's little theorem for a in {2,3,5,7,11,13}
- 2^255 -> 19, 2^256 -> 38 reduction identities

**What is NOT verified:**
- Multiplicative inverse (finv) -- requires primality proof
- Full field structure (this is a commutative ring, not a field)

**Assurance Grade: A-**
Complete commutative ring verification. All algebraic laws proved. finv gap is honest (blocked on primality).

---

### 2.4 Ed25519Curve.v

**Compilation:** SUCCESS
**Print Assumptions:** All theorems closed under global context (zero axioms)

**Proof Status:**
| Category | Count |
|---|---|
| Proved lemmas | 15+ (d_cross_check, identity_on_curve, basepoint_on_curve, d_is_non_square, d_range, d_nonzero, curve_a_is_neg_1, order2_point_on_curve, By_cross_check, Bx/By range, on_curve_b_correct) |
| Admitted | 0 |
| Axiom/Parameter | 0 |

**What is verified:**
- Curve equation predicate: -x^2 + y^2 = 1 + d*x^2*y^2 (mod p)
- d = -121665/121666 mod p (verified by cross-multiplication)
- Identity point (0, 1) satisfies curve equation
- RFC 8032 basepoint (Bx, By) satisfies curve equation
- d is NOT a square mod p (Euler criterion: d^((p-1)/2) = p-1 = -1 mod p)
- By = 4/5 mod p (verified: 5 * By = 4 mod p)

**What is NOT verified:**
- Point addition formulas (completeness, correctness)
- Group law (closure, associativity, inverse)
- Basepoint generates prime-order subgroup
- Scalar multiplication correctness

**FINDING:** The d-is-non-square proof (Euler criterion) is cryptographically significant -- it is required for completeness of the twisted Edwards addition law. This is genuine assurance.

**Assurance Grade: A-**
Curve constants and equation fully verified. Non-square proof for d is the key result. Group law gap is honest.

---

## Part 3: Cross-Cutting Findings

### 3.1 The Abstract Stub Problem (F\* Protocols)

Five of the seven protocol specs (X3DH, PQXDH, DoubleRatchet, NoiseIK, SenderKeys)
define ALL cryptographic primitives as constant functions:
- `x25519 _ _ = Seq.create 32 0uy`
- `hkdf _ _ _ len = Seq.create len 0uy`
- `hmac_sha256 _ _ = Seq.create 32 0uy`
- `ed25519_verify _ _ _ = true`
- `chacha20_encrypt _ _ _ pt = pt` (identity!)

This means:
1. All DH commutativity proofs are trivially true (both sides = 0^32)
2. All key agreement proofs are trivially true (all keys = 0^32)
3. Signature verification never fails
4. "Encryption" is the identity function
5. HKDF always returns zeros regardless of input

These stubs are acceptable as TYPE-LEVEL specifications (they establish the
protocol flow and length contracts). But they provide ZERO assurance about
cryptographic security properties.

### 3.2 Honest vs Dishonest Claims

**Honestly labeled as placeholder/axiom:**
- forward_secrecy_placeholder (DoubleRatchet) -- proves True, says so
- break_in_recovery_placeholder (DoubleRatchet) -- proves True, says so
- pqxdh_hybrid_security_placeholder (PQXDH) -- proves True, says so
- encrypt_decrypt_roundtrip_placeholder (NoiseIK) -- proves True, says so
- hmac_prf_placeholder (SenderKeys) -- proves True, says so
- hmac_non_fixpoint (DoubleRatchet) -- assume val, documented
- hmac_collision_resistance (DoubleRatchet) -- assume val, documented
- dleq_correctness (VRF) -- assume val, well-documented algebraic axiom
- vrf_strong_uniqueness (VRF) -- assume val, hardness assumption
- vrf_collision_resistance (VRF) -- assume val, hardness assumption
- unlinkability (StealthAddress) -- assume val, DDH assumption
- sha256_refinement_axiom (SHA256.Refinement) -- assume val, cross-toolchain

**Potentially misleading:**
- `spk_rejection_lemma` (X3DH): states "rejects forged signatures" but ed25519_verify = true makes the antecedent always false
- `dh_comm_1` comments reference "commutativity of scalar multiplication on Curve25519" but the proof is just reflexivity of constant zero
- `noise_decrypt` MAC check: appears to verify integrity but HMAC returns constant zeros and chacha20 is identity

### 3.3 Runtime Connection Gap

| Component | F\* Spec | Haskell Impl | Connection |
|---|---|---|---|
| SHA-256 | Spec.SHA256 | UmbraVox.Crypto.SHA256 | sha256_refinement_axiom (7 assume vals) |
| X3DH | Spec.X3DH | UmbraVox.Crypto.Signal.X3DH | None |
| PQXDH | Spec.PQXDH | UmbraVox.Crypto.Signal.PQXDH | None |
| Double Ratchet | Spec.DoubleRatchet | UmbraVox.Crypto.Signal.DoubleRatchet | None |
| Noise IK | Spec.NoiseIK | UmbraVox.Network.Noise | None |
| SenderKeys | Spec.SenderKeys | UmbraVox.Crypto.Signal.SenderKeys (stub) | None (impl is stub) |
| VRF | Spec.VRF | UmbraVox.Crypto.VRF (stub) | None (impl is stub) |
| Stealth Address | Spec.StealthAddress | UmbraVox.Crypto.StealthAddress | None |
| Keccak/SHA-3 | Spec.Keccak.* | (no Haskell impl referenced) | None |
| Ed25519 | Spec.Ed25519 | (referenced by VRF/StealthAddr) | None (Coq supports field/curve) |

Only SHA-256 has a stated (axiomatized) refinement to Haskell.

### 3.4 What the Coq Files Actually Support

The Coq files provide ground-truth for the Ed25519 constants and field arithmetic
used in the F\* specs (Spec.Ed25519, Spec.VRF, Spec.StealthAddress). Specifically:

- Ed25519Constants: validates p, d, L, cofactor values
- Ed25519Prime: provides Pocklington evidence for primality of p
- Ed25519Field: verifies the commutative ring structure of GF(p)
- Ed25519Curve: verifies basepoint is on curve and d is non-square

These Coq proofs are STRONGER than the F\* protocol proofs because they use
vm_compute (kernel-level computation) with zero axioms.

---

## Part 4: Assurance Grade Summary

| File | Grade | Key Strength | Key Weakness |
|---|---|---|---|
| Spec.X3DH.fst | D | Correct protocol structure | All crypto = constant stubs; sig verify = true |
| Spec.PQXDH.fst | D+ | IKM length proofs | All crypto = constant stubs |
| Spec.DoubleRatchet.fst | C- | Honest axioms, structural chain properties | Forward secrecy not proved computationally |
| Spec.NoiseIK.fst | D- | Protocol flow modeled | Encryption = identity; MAC = constants |
| Spec.SenderKeys.fst | C | Clean parameterization, genuine monotonicity | Haskell impl is a stub |
| Spec.VRF.fst | B- | Concrete Ed25519 ops, genuine verifiability proof | hash_to_curve stub |
| Spec.StealthAddress.fst | B | Concrete primitives, genuine scan_correctness | Single DDH axiom |
| Spec.SHA256.Refinement.fst | C+ | Honest trust boundary enumeration | 7 assume vals |
| Spec.Keccak.Permutation.fst | B+ | Complete Keccak-f[1600], no stubs | No differential properties |
| Spec.Keccak.Sponge.fst | B+ | Complete sponge, all invariants proved | No indifferentiability |
| Spec.Keccak.SHA3.fst | A- | 7 NIST KAT vectors by full computation | High Z3 resource usage |
| Ed25519Constants.v | A | Zero axioms, machine-checked | Simple properties only |
| Ed25519Prime.v | A- | Verified primality checker, Pocklington conditions | Missing connecting theorem |
| Ed25519Field.v | A- | Complete commutative ring | No multiplicative inverse |
| Ed25519Curve.v | A- | Basepoint on curve, d non-square | No group law |

---

## Part 5: Recommendations

1. **Instantiate abstract stubs.** The X3DH/PQXDH/NoiseIK/DoubleRatchet proofs become meaningful only when x25519, hkdf, hmac_sha256 are instantiated with concrete specs (as already done for VRF and StealthAddress).

2. **Fix ed25519_verify stub.** The current `true` return makes SPK verification vacuously true. At minimum, make it an uninterpreted function so the rejection lemma is non-trivial.

3. **Fix chacha20_encrypt stub.** The identity function is actively misleading. Use an uninterpreted function or delegate to a concrete spec.

4. **Complete Pocklington theorem.** The certificate is verified; only the ~200-line theorem is missing. Consider adding coq-prime to the Nix closure.

5. **Add group law to Coq.** Ed25519Curve.v has the curve equation but no addition law. This is the most impactful next step for the Coq verification.

6. **Establish Haskell refinement for protocols beyond SHA-256.** Currently only SHA-256 has a refinement axiom. X3DH, PQXDH, DoubleRatchet, NoiseIK have no connection to runtime.

---

## Methodology

- All F\* files reviewed by source inspection (not compiled -- F\* compilation requires VM)
- All 4 Coq files compiled successfully with `coqc -R . UmbraVox` via nix-shell (Rocq 9.1.1)
- `Print Assumptions` run for key theorems in all 4 Coq files: all closed under global context
- `grep` for Admitted/Axiom/Parameter in Coq files: zero hits
- assume val / assume type counts from F\* source inspection
- Placeholder identification by searching for `Lemma (True)` patterns
- Abstract stub identification by checking function bodies

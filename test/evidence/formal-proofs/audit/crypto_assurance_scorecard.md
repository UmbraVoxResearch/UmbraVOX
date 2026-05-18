# Cryptographic Assurance Scorecard

Date: 2026-05-17
Source: 4 audit reports (crypto_constants_audit, crypto_curve_kdf_audit, crypto_protocol_coq_audit, crypto_symmetric_audit)

---

## Per-Module Grading Criteria

- **A**: No admits, no untracked assumptions, strong non-vacuous theorems, constants verified, KAT present, runtime correspondence clear
- **B**: Minor tracked assumptions, useful theorems, constants verified, KAT present
- **C**: Significant assumptions, mostly structural proofs, limited runtime connection
- **D**: Many assumptions, theorem names stronger than statements, model-only
- **F**: False theorem, impossible precondition, misleading claim, stubs

---

## Scorecard

| Module | Grade | admit | assume val | Qed | KAT vectors | Runtime link | Key gap |
|--------|-------|-------|------------|-----|-------------|--------------|---------|
| Ed25519Constants.v | A | 0 | 0 | 5/5 closed | N/A | N/A (Coq constants) | Simple properties only |
| Ed25519Prime.v | A- | 0 | 0 | 50+ closed | N/A | N/A (Coq primality) | Pocklington connecting theorem missing |
| Ed25519Field.v | A- | 0 | 0 | 35+ closed | N/A | N/A (Coq ring) | No multiplicative inverse (blocked on primality) |
| Ed25519Curve.v | A- | 0 | 0 | 15+ closed | N/A | N/A (Coq curve) | No group law, no addition law completeness |
| Spec.Keccak.SHA3.fst | A- | 0 | 0 | 13 | 7 NIST vectors (full computation) | None | High Z3 resource usage |
| Spec.SHA256.fst | A- | 0 | 0 | 14+ | 3 NIST vectors (full digest) | Axiomatized refinement | No formal Haskell correspondence |
| Spec.AES256.fst | A- | 0 | 0 | 13+ | 2 FIPS 197 C.3 (full block) | Comment table only | Only 1 key/pt pair; S-box not per-entry verified against FIPS |
| Spec.GCM.fst | A- | 0 | 0 | 10+ | 2 NIST (TC14, TC16 full) | Comment table only | No AAD KAT; gcm_encrypt_decrypt_same_tag ensures True |
| Spec.Ed25519.fst | B+ | 0 | 10 | 15+ | 6 (2 RFC 8032 TVs) | Explicit table, verified | 10 assume vals (all standard); sign_then_verify assumed |
| Spec.X25519.fst | B+ | 0 | 4 | 10+ | 4 (2 RFC 7748 TVs + ECDH) | Explicit table, verified | DH commutativity assumed; scalar_mult_one not proved |
| Spec.Keccak.Permutation.fst | B+ | 0 | 0 | 6+ | None (indirect via SHA3) | None | No differential/linear properties |
| Spec.Keccak.Sponge.fst | B+ | 0 | 0 | 6 | None (indirect via SHA3) | None | No indifferentiability |
| Spec.SHA512.fst | B+ | 0 | 0 | 7+ | 2 (full digest) | Comment table only | Constants not individually assert_norm'd |
| Spec.Poly1305.fst | B+ | 0 | 0 | 7 | 1 RFC 8439 (full tag) | Comment table only | 2 Lemma(True) placeholders |
| Spec.GaloisField.fst | B+ | 0 | 0 | 9 | None (indirect via GCM) | Consumed by GCM | gf_mul_comm Z3-fragile |
| Spec.HMAC.fst | B+ | 0 | 0 (1 inline) | 6 | 3 RFC 4231 | Comment note | 1 Lemma(True) PRF placeholder; missing TC3-TC7 |
| Spec.ChaCha20.fst | B | 0 | 0 | 6+ | 2 partial (4/64 bytes each) + 1 vacuous | Comment table only | KATs check only 6.25% of block output; vacuous kat_block_placeholder |
| Spec.ChaChaPoly.fst | B | 0 | 1 | 8 | 0 concrete (data present, not evaluated) | Comment table only | No concrete AEAD KAT; tag_forgery_ct_axiom |
| Spec.HKDF.fst | B | 0 | 0 (1 inline) | 8 | 3 (1 RFC 5869 TC) | Comment table | Only 1 of 3 RFC test cases; no SHA-512 KAT |
| Spec.StealthAddress.fst | B | 0 | 1 | 7 | 0 | Concrete primitive delegation | Single DDH axiom (unlinkability) |
| Spec.VRF.fst | B- | 0 | 3 | 7+ | 0 | Concrete Ed25519 delegation | hash_to_curve is constant stub; 3 crypto axioms |
| Spec.SHA256.Refinement.fst | C+ | 0 | 6 (+1 assume type) | 7 | 0 | Axiomatized (sha256_refinement_axiom) | 7 assume vals; core equivalence claim is axiomatized |
| Spec.SenderKeys.fst | C | 0 | 0 | 5 | 0 | None (Haskell impl is stub) | 2 Lemma(True) placeholders; Haskell impl is a stub |
| Spec.DoubleRatchet.fst | C- | 0 | 2 | 7 | 0 | None | Forward secrecy not computationally proved; all crypto = constant stubs |
| Spec.PQXDH.fst | D+ | 0 | 0 | 7 | 0 | None | All crypto = constant stubs; hybrid security proves True |
| Spec.X3DH.fst | D | 0 | 0 | 5 | 0 | None | All crypto = constant stubs; ed25519_verify = true makes SPK rejection vacuous |
| Spec.NoiseIK.fst | D- | 0 | 0 | 4 | 0 | None | chacha20_encrypt = identity; HMAC = constant zeros; encrypt/decrypt roundtrip proves True |
| Spec.MLKEM768.fst | F | 0 | 0 | 0 genuine | 0 | Zero correspondence | ALL functions are identity/constant stubs; ALL 8 Lemma(True) vacuous; parameters only |

---

## Modules with Grade A / A-

**Ed25519Constants.v (A)** -- Zero axioms, all 5 theorems machine-checked by Coq vm_compute. Simple but fully verified: p positive/odd, L positive, cofactor = 8, curve order factorization.

**Ed25519Prime.v (A-)** -- Extraordinary thoroughness: verified trial-division checker with soundness proof, 24 non-divisibility proofs, 4 Fermat witnesses, complete Pocklington certificate conditions. All via vm_compute with zero axioms. Gap: the Pocklington theorem connecting certificate conditions to `prime p` is not formalized (~200 lines from coq-prime not in Nix closure).

**Ed25519Field.v (A-)** -- Complete commutative ring verification with 35+ lemmas. Covers commutativity, associativity, distributivity, identity, inverse (additive), congruence, and Fermat's little theorem for 6 witnesses. Gap: no multiplicative inverse proof (blocked on primality).

**Ed25519Curve.v (A-)** -- Basepoint on curve, d non-square (Euler criterion via vm_compute), identity on curve. The d-is-non-square proof is cryptographically significant for addition law completeness. Gap: no group law, no point addition formulas.

**Spec.Keccak.SHA3.fst (A-)** -- Gold standard for hash specs. 7 NIST KAT vectors verified by full Z3 computation (SHA3-224/256/384/512 + SHAKE128/256). No axioms, no stubs. Gap: high Z3 resource requirements (fuel 200, rlimit 200000).

**Spec.SHA256.fst (A-)** -- 3 NIST KATs with full 32-byte digest comparison via assert_norm. All 8 IV values and all 64 K constants individually verified by assert_norm. Complete algorithmic structure. Gap: no formal Haskell refinement in this file (separate Refinement file uses axiom).

**Spec.AES256.fst (A-)** -- Full encrypt/decrypt roundtrip proved by induction over 14 rounds. All component inverses proved (S-box 256-entry roundtrip, ShiftRows, MixColumns). FIPS 197 C.3 KAT with full block comparison. Gap: only 1 key/pt test vector; S-box verified by roundtrip, not per-entry against FIPS table.

**Spec.GCM.fst (A-)** -- Full AEAD roundtrip, tag integrity, GCTR involutivity, GHASH linearity. 2 NIST KATs (TC14, TC16) with concrete AES-256 evaluation. Strong design: parameterized over block cipher, KATs instantiate with verified AES-256. Gap: no AAD-exercising KAT; `gcm_encrypt_decrypt_same_tag` has `ensures True`.

---

## Modules with Grade B+ / B / B-

**Spec.Ed25519.fst (B+)** -- 10 assume vals, all standard (computational boundaries, algebraic geometry beyond SMT, cryptographic axioms). Field arithmetic fully proved. Basepoint on curve via assert_norm. 6 KAT tests from 2 RFC 8032 test vectors. Explicit runtime correspondence table verified by grep.

**Spec.X25519.fst (B+)** -- 4 assume vals (primality, scalar_mult_one, DH commutativity x2, zero-u). Full field arithmetic, Montgomery ladder spec, 4 RFC 7748 KATs including ECDH agreement. Explicit runtime correspondence.

**Spec.Keccak.Permutation.fst (B+)** -- Complete Keccak-f[1600] with no stubs, no axioms. All 24 round constants, rotation offsets. Pi table bounds proved by exhaustive case split.

**Spec.Keccak.Sponge.fst (B+)** -- Concrete sponge construction with all structural invariants proved (pad alignment, nonempty, squeeze length). No stubs, no axioms.

**Spec.SHA512.fst (B+)** -- Mirrors SHA-256 structure. 2 full-digest KATs. Gap: K table (80 entries) and IV values (8 entries) not individually verified by assert_norm unlike SHA-256.

**Spec.Poly1305.fst (B+)** -- Full RFC 8439 KAT via assert_norm. Correct field arithmetic, clamping bound. 2 Lemma(True) placeholders (le_roundtrip, UF-CMA) -- UF-CMA is fundamentally unprovable in F*.

**Spec.GaloisField.fst (B+)** -- Complete algebraic properties for GF(2^128). Distributivity proof (key for GHASH linearity). Commutativity proved but Z3-fragile (fuel 129, rlimit 4000).

**Spec.HMAC.fst (B+)** -- Correct RFC 2104 construction. 3 RFC 4231 KATs. Bounded variants eliminate inline assume for concrete instances. PRF placeholder is unavoidable (computational assumption).

**Spec.ChaCha20.fst (B)** -- Encrypt/decrypt roundtrip proved by induction (strong). Quarter-round KAT verified. Gap: block-function KATs check only 4/64 bytes (6.25%); vacuous kat_block_placeholder (seq_of_hex returns empty, making precondition false).

**Spec.ChaChaPoly.fst (B)** -- AEAD correctness (roundtrip) fully proved. Tag mutation detected structurally. 1 assume val (tag_forgery_ct_axiom) for CT tampering is irreducible crypto assumption. Gap: no concrete AEAD KAT despite having test vector data.

**Spec.HKDF.fst (B)** -- Correct RFC 5869 structure. Bounded variants clean. 3 KAT lemmas for 1 test case. Gap: only 1 of 3 RFC test cases; no SHA-512 HKDF KAT despite SHA-512 being primary protocol KDF.

**Spec.StealthAddress.fst (B)** -- Strongest protocol spec alongside VRF. All primitives concrete (delegates to Spec.X25519, Spec.Ed25519, Spec.HKDF). scan_correctness genuinely proved via DH commutativity. Single assume val (DDH unlinkability).

**Spec.VRF.fst (B-)** -- Most sophisticated protocol spec. Concrete Ed25519 operations for most of protocol. Verifiability proof genuine modulo dleq_correctness axiom. 3 honest crypto axioms. Gap: hash_to_curve is constant stub (try-and-increment not modeled).

---

## Modules with Grade C

**Spec.SHA256.Refinement.fst (C+)** -- Honest cross-toolchain refinement attempt. Trust surface explicitly enumerated (7 assume vals + 1 assume type). Core claim (F* SHA-256 == Haskell SHA-256) is axiomatized, not proved.

**Spec.SenderKeys.fst (C)** -- Clean parameterization over bounded_hmac_fn. Genuine monotonicity proof (iter strictly increasing). Domain separation proved concretely. 2 Lemma(True) placeholders. Haskell implementation is itself a stub.

**Spec.DoubleRatchet.fst (C-)** -- Best of the "abstract stub" protocol specs. 2 honest assume vals (HMAC non-fixpoint, collision resistance). Structural chain properties genuine. Forward secrecy and break-in recovery are structural only (prove output sizes, not one-wayness).

---

## Modules with Grade D / F

**Spec.PQXDH.fst (D+)** -- Slightly better than X3DH: hybrid IKM length proofs are structurally genuine. But all crypto = constant stubs. Hybrid security placeholder proves True.

**Spec.X3DH.fst (D)** -- Correct 4-DH protocol flow modeled. All crypto = constant stubs (x25519 returns zeros, hkdf returns zeros). ed25519_verify defined as `true`, making spk_rejection_lemma vacuously true (antecedent always false). DH commutativity is reflexivity of constant zero.

**Spec.NoiseIK.fst (D-)** -- Protocol flow modeled but actively misleading stubs: chacha20_encrypt is the identity function (returns plaintext unchanged), HMAC returns constant zeros. MAC check always passes because both sides compute the same constant. encrypt_decrypt_roundtrip placeholder proves True.

**Spec.MLKEM768.fst (F)** -- ALL cryptographic functions are identity/constant stubs. ntt returns input unchanged, cbd returns all-zeros, keygen/encaps/decaps return trivial values. ALL 8 "Lemma(True)" are vacuous (prove True). Zero KAT vectors. Zero runtime correspondence. Only genuine content: parameter validation (~10 assert_norm lines confirming FIPS 203 Table 2 values). File is honest about being stubs (header, _stub suffixes).

---

## Top 5 Remediation Priorities (Highest Impact)

### 1. ML-KEM-768: Implement real NTT/CBD/K-PKE functions (Grade F -> B potential)

The Haskell MLKEM module has 507+ lines of full FIPS 203 implementation but the F* spec is entirely stubs. Implementing even the NTT forward/inverse with roundtrip proof and one NIST KAT would move this from F to C+. This is the single largest assurance gap in the project -- a post-quantum primitive with zero formal verification.

### 2. Instantiate protocol stubs with concrete primitives (X3DH/PQXDH/NoiseIK/DoubleRatchet: D -> B potential)

Five protocol specs define x25519, hkdf, hmac_sha256 as constant-zero functions. VRF and StealthAddress already demonstrate the pattern: delegate to concrete Spec.X25519/Spec.HKDF/Spec.HMAC. Applying this pattern to the remaining protocols would make their agreement/commutativity proofs non-trivial and their security lemmas non-vacuous.

### 3. Fix ed25519_verify and chacha20_encrypt stubs (D-/D -> C+ potential)

In Spec.X3DH.fst, `ed25519_verify` is defined as `true`, making signature rejection proofs vacuously true. In Spec.NoiseIK.fst, `chacha20_encrypt` is the identity function, making "encryption" return plaintext. At minimum, make these uninterpreted functions so the proof obligations are non-trivial. Best: delegate to concrete specs.

### 4. Expand ChaCha20 KAT coverage from 6.25% to full block (B -> A- potential)

Current block-function KATs check only 4 of 64 output bytes. An error in bytes 4-63 would be undetected. Expanding to full 64-byte comparison (or at minimum first 16 + last 16) and adding an encryption-level RFC 8439 Section 2.4.2 KAT would significantly strengthen confidence.

### 5. Add HKDF-SHA-512 KAT and remaining RFC 5869 test cases (B -> B+ potential)

SHA-512-based HKDF is the primary KDF for the protocol, but zero SHA-512 HKDF KAT vectors exist. Only 1 of 3 RFC 5869 test cases is covered. Adding TC2 (longer inputs) and TC3 (zero-length salt) would exercise edge cases.

---

## Release Blockers (Must Be Addressed Before Claiming Formal Assurance)

### Hard blockers (claiming assurance without addressing these is misleading)

1. **ML-KEM-768 stub functions** -- Cannot claim "formally verified post-quantum cryptography" when all PQ functions are identity stubs. Either implement real proofs or explicitly exclude ML-KEM from assurance claims.

2. **NoiseIK identity-function encryption** -- `chacha20_encrypt` returning plaintext unchanged means the "AEAD" spec has no confidentiality. Any documentation claiming Noise IK is formally modeled must note the encryption stub.

3. **X3DH ed25519_verify = true** -- SPK verification always succeeds. The spk_rejection_lemma is vacuously true. Cannot claim X3DH signature verification is formally specified.

### Soft blockers (weaken assurance claims if not addressed)

4. **No concrete AEAD KAT in ChaChaPoly** -- Test vector data is present but never evaluated. The AEAD roundtrip is proved structurally, not by concrete computation.

5. **GCM has no AAD KAT** -- Both NIST test cases use empty associated data. An error in AAD processing would be undetected.

6. **SHA-512 constants not individually verified** -- Unlike SHA-256, the 80 K entries and 8 IV values lack per-index assert_norm verification. A typo in any constant would pass F* without detection.

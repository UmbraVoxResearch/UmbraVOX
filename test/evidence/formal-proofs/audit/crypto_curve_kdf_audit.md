# Crypto Curve & KDF F* Specification Audit

Audit date: 2026-05-17
Auditor: Claude (crypto-assurance mode)
Files under audit:
- `test/evidence/formal-proofs/fstar/Spec.Ed25519.fst`
- `test/evidence/formal-proofs/fstar/Spec.X25519.fst`
- `test/evidence/formal-proofs/fstar/Spec.HMAC.fst`
- `test/evidence/formal-proofs/fstar/Spec.HKDF.fst`
- `test/evidence/formal-proofs/fstar/Spec.MLKEM768.fst`

---

## 1. Spec.Ed25519.fst

### 1.1 admit / assume val / Lemma(True) Counts

| Category       | Count |
|----------------|-------|
| `admit`        | 0     |
| `assume val`   | 10    |
| `assume` (inline) | 0  |
| `Lemma (True)` | 0     |

### 1.2 assume val Inventory

| # | ID | Statement | Classification | Standard/Suspicious/Vacuous |
|---|-----|-----------|---------------|----------------------------|
| ED-001 | `prime_is_prime` | `is_prime (2^255 - 19)` | Computational boundary -- Z3 cannot trial-divide a 255-bit number | **Standard.** Externally verified by multiple CAS. Primality certificate at `scripts/primality-certificate.hs`. Identical assumption in Spec.X25519. |
| ED-002 | `group_order_lemma` | `[L]B = O` (basepoint has order L) | Computational boundary -- evaluating 2^252 scalar multiplications | **Standard.** Fundamental parameter of Ed25519, trivially confirmed by any implementation. Not provable in any SMT-based prover. |
| ED-003 | `point_add_assoc` | Associativity of point addition (extended coords) | Algebraic geometry beyond first-order SMT -- degree-12 polynomial identity | **Standard.** Proven in fiat-crypto (Coq), Bernstein et al. 2008. F* lacks Grobner basis tactics. Thorough justification in comments. |
| ED-004 | `scalar_mult_add` | `[a+b]P = [a]P + [b]P` | Depends on ED-003 (associativity) | **Standard.** Group homomorphism property. Dependency chain documented. |
| ED-005 | `scalar_mult_compose` | `[a]([b]P) = [a*b]P` | Depends on ED-004 | **Standard.** Z-module action. Dependency chain documented. |
| ED-006 | `scalar_mod_L_equiv` | `[(n mod L)]B = [n]B` | Composite: depends on ED-002, ED-004, ED-005 | **Standard.** Follows from group order. Proof sketch is correct. |
| ED-007 | `point_add_congruence_right` | Projective equivalence is a congruence for point_add | Algebraic -- degree-8 polynomial identity in projective coords | **Standard.** True for all projective coordinate systems. Requires same Grobner basis machinery as ED-003. |
| ED-008 | `sign_then_verify` | `verify(pk(sk), msg, sign(sk, msg)) = true` | Composite: depends on verify_equation (PROVED) + encode_decode_round_trip | **Standard.** The algebraic core (verify_equation) IS proved. Remaining gap is encode/decode round-trip. |
| ED-009 | `encode_decode_round_trip` | Encode then decode recovers the point (with on_curve_ext guard) | Square root recovery in GF(p) -- exponent ~2^252 | **Standard.** Requires Tonelli-Shanks correctness proof with infeasible exponent. Precondition guard correctly added. |
| ED-010 | `distinct_messages_distinct_sigs` | Different messages yield different signatures (under CR) | Cryptographic security assumption | **Standard cryptographic axiom.** Not provable in any formal system -- requires computational indistinguishability. Conditional on SHA-512 collision resistance. |

**Summary:** 10 assume vals. All are **standard** -- either computational boundaries (Z3 cannot normalize 2^252 iterations), algebraic geometry beyond first-order SMT (polynomial identities), or cryptographic security assumptions. None are suspicious or vacuous. Dependency chains are explicitly documented.

### 1.3 Exported Theorem Analysis

| Theorem | Actual Claim | Consumer-Perceived Claim | Vacuity Check |
|---------|-------------|------------------------|---------------|
| `basepoint_on_curve` | `on_curve Bx By == true` via assert_norm | Basepoint satisfies twisted Edwards equation | **Non-vacuous.** assert_norm forces full 255-bit computation. |
| `identity_on_curve` | `on_curve 0 1 == true` | Identity is on curve | **Non-vacuous.** Concrete evaluation. |
| `point_add_identity_right` | `encode_point(P+O) == encode_point(P)` | Right identity for addition | **Non-vacuous.** Full algebraic proof with case split on z=0. |
| `point_add_identity_left` | `encode_point(O+P) == encode_point(P)` | Left identity | **Non-vacuous.** Follows from commutativity + right identity. |
| `point_add_comm` | `point_add P Q == point_add Q P` | Addition is commutative | **Non-vacuous.** Syntactic equality (stronger than projective). |
| `point_double_is_add` | `encode_point(2P) == encode_point(P+P)` | Doubling consistent with addition | **Non-vacuous.** Requires on_curve_ext precondition. Full proof with degenerate cases. |
| `scalar_mult_zero` | `[0]P = O` | Zero scalar gives identity | **Non-vacuous.** Direct evaluation. |
| `scalar_mult_one` | `[1]P = P` | Unit scalar preserves point | **Non-vacuous.** Multi-step proof through non-canonical identity. |
| `fmul_inverse` | `a * a^(-1) = 1` for a != 0 | Field inverse correctness | **Non-vacuous.** Proved via FLT (depends on prime_is_prime). |
| `verify_equation` | `[S]B = R + [k]A` when S = (r+ka) mod L | Verification equation holds algebraically | **Non-vacuous but depends on 4 assume vals** (ED-004,005,006,007). The proof correctly chains them. |
| `ed25519_kat1_pubkey` | assert_norm evaluation matches RFC 8032 TV1 | KAT passes | **Non-vacuous.** Full computation. |
| `ed25519_kat1_sign` | assert_norm evaluation matches RFC 8032 TV1 | KAT passes | **Non-vacuous.** |
| `ed25519_kat1_verify` | assert_norm evaluation matches RFC 8032 TV1 | KAT passes | **Non-vacuous.** |
| KAT2 (pubkey/sign/verify) | Same as above for TV2 | KAT passes | **Non-vacuous.** |
| `verify_equation_lhs_rhs_agree` | precondition == postcondition | Named statement of verification equation | **VACUOUS in isolation** -- postcondition equals precondition. However, its purpose is as a type-level name, not as standalone assurance. The real proof is in `verify_equation`. |

### 1.4 Constants Audit

#### p = 2^255 - 19
- **F* value:** `normalize_term (pow2 255 - 19)` -- CORRECT per RFC 7748/8032
- **Coq value:** `2^255 - 19` in Ed25519Constants.v -- MATCHES

#### d = -121665/121666 mod p
- **F* computation:** `(prime - 121665) * finv (121666 % prime) % prime` -- CORRECT
- **Coq literal:** `37095705934669439343138083508754565189542113879843219016388785533085940283555`
- **Coq cross-check:** `d * 121666 = -121665 mod p` verified by vm_compute -- MATCHES
- **F* d computes the same value** (via assert_norm in basepoint_on_curve)

#### L = 2^252 + 27742317777372353535851937790883648493
- **F* value:** `normalize_term (pow2 252 + 27742317777372353535851937790883648493)` -- CORRECT per RFC 8032
- **Coq value:** Same in Ed25519Constants.v -- MATCHES

#### cofactor = 8
- **Coq:** `ed25519_cofactor = 8`, `cofactor * L = 8 * (2^252 + ...)` -- CORRECT
- **F*:** Not explicitly stated but implied by clamping (clears 3 low bits = multiply by 8)

#### Basepoint Bx
- **RFC 8032 / libsodium canonical value:** `15112221349535400772501151409588531511454012693041857206046113283949847762202`
- **Coq literal (Ed25519Curve.v line 138):** `15112221349535400772501151409588531511454012693041857206046113283949847762202` -- **MATCHES exactly**
- **F* computation:** `basepoint_x` is computed via `recover_x` from `basepoint_y = 4 * finv(5)`. The F* spec does NOT contain Bx as a literal; it computes it from the curve equation and adjusts parity. The `basepoint_on_curve` lemma (assert_norm) confirms the computed Bx satisfies the curve equation. **Cross-verification with Coq is provided by Coq's `basepoint_on_curve` lemma using the same literal.**

#### Basepoint By
- **RFC 8032 canonical value:** `46316835694926478169428394003475163141307993866256225615783033603165251855960`
- **Coq literal (Ed25519Curve.v line 121):** `46316835694926478169428394003475163141307993866256225615783033603165251855960` -- **MATCHES exactly**
- **Coq cross-check:** `By * 5 = 4 mod p` verified by vm_compute -- CORRECT (By = 4/5 mod p)
- **F* computation:** `basepoint_y = fmul 4 (finv (5 % prime))` -- This is `4 * 5^(p-2) mod p = 4/5 mod p` -- **MATCHES**

#### Curve equation verification: -Bx^2 + By^2 = 1 + d*Bx^2*By^2 mod p
- **Coq:** `basepoint_on_curve` proved by vm_compute with the exact Bx, By values -- **VERIFIED**
- **F*:** `basepoint_on_curve` proved by assert_norm with computed Bx, By -- **VERIFIED**
- **Independent check:** The Coq file uses the SAME Bx/By literals as RFC 8032. The F* file computes them from first principles (y=4/5, x from curve equation). Both arrive at the same point, confirmed by both provers.

#### F* vs Coq constant consistency
- p: identical (2^255 - 19)
- d: F* computes, Coq provides literal + cross-multiplication proof. **Same value.**
- By: F* computes as 4*finv(5), Coq provides literal + cross-multiplication proof. **Same value.**
- Bx: F* computes via recover_x, Coq provides literal + on_curve proof. **Same value** (Coq provides the ground truth literal; F* computes and verifies via assert_norm).
- L: identical

### 1.5 KAT Vector Coverage

| KAT | Source | What is tested | Status |
|-----|--------|---------------|--------|
| TV1 pubkey | RFC 8032 Section 7.1 | `ed25519_public_key(sk) == pk` | **PROVED** (assert_norm) |
| TV1 sign | RFC 8032 Section 7.1 | `ed25519_sign(sk, "") == sig` | **PROVED** (assert_norm) |
| TV1 verify | RFC 8032 Section 7.1 | `ed25519_verify(pk, "", sig) == true` | **PROVED** (assert_norm) |
| TV2 pubkey | RFC 8032 Section 7.1 | `ed25519_public_key(sk2) == pk2` | **PROVED** (assert_norm) |
| TV2 sign | RFC 8032 Section 7.1 | `ed25519_sign(sk2, "r") == sig2` | **PROVED** (assert_norm) |
| TV2 verify | RFC 8032 Section 7.1 | `ed25519_verify(pk2, "r", sig2) == true` | **PROVED** (assert_norm) |

Coverage: 2 of 3 RFC 8032 Section 7.1 test vectors (TV1: empty message, TV2: 1-byte message). TV3 (2-byte message with 1023-byte secret key context) is not included.

### 1.6 Runtime Correspondence to src/UmbraVox/Crypto/Ed25519.hs

The F* file documents an explicit correspondence table (lines 2379-2408). Verified by grep:

| F* function | Haskell function | Structural match |
|-------------|-----------------|-----------------|
| `prime` | `p` | Identical formula |
| `group_order` | `groupL` | Identical formula |
| `curve_d` | `curveD` | Same computation: `(-121665) * modInv 121666 p mod p` |
| `fadd/fsub/fmul` | `fAdd/fSub/fMul` | Identical `(a op b) mod prime` |
| `finv` | `modInv` | Both use `a^(p-2) mod p` |
| `point_add` | `pointAdd` | Same HWCD'08 formula, same variable names |
| `point_double` | `pointDouble` | Same EFD dbl-2008-hwcd formula |
| `scalar_mult` | `scalarMul` | Same double-and-add from MSB |
| `basepoint` | `basepoint` | Same derivation (y=4/5, recover x) |
| `clamp_scalar` | `clampScalar` | Same byte operations |
| `ed25519_sign` | `ed25519Sign` | Same 7-step algorithm |
| `ed25519_verify` | `ed25519Verify` | Same algorithm |

**Assessment:** The correspondence is complete and accurate. The F* spec is a faithful transliteration of the Haskell code.

### 1.7 Assurance Grade: B+

**Justification:**
- Field arithmetic is fully proved (closure, axioms, inverse via FLT)
- Basepoint on curve: proved (both F* assert_norm and Coq vm_compute)
- Group operation properties: identity (proved), commutativity (proved), doubling=addition (proved)
- Associativity: assumed (irreducible -- needs Grobner basis)
- Scalar multiplication properties: partially assumed (depend on associativity)
- Sign-then-verify: algebraic core proved, encode/decode round-trip assumed
- KAT vectors: 6 tests covering 2 RFC test vectors, all proved by normalization
- Constants: all verified against RFC and cross-checked with Coq proofs
- 10 assume vals, all standard and well-justified

**What prevents grade A:** The sign_then_verify top-level correctness theorem is still an assume val. The dependency on point_add_assoc (which requires a Grobner basis tactic not available in F*) cascades through 4 other assume vals. These are mathematically sound but not machine-checked.

---

## 2. Spec.X25519.fst

### 2.1 admit / assume val / Lemma(True) Counts

| Category       | Count |
|----------------|-------|
| `admit`        | 0     |
| `assume val`   | 4     |
| `assume` (inline) | 0  |
| `Lemma (True)` | 0     |

### 2.2 assume val Inventory

| # | ID | Statement | Classification | Standard/Suspicious/Vacuous |
|---|-----|-----------|---------------|----------------------------|
| X-001 | `prime_is_prime` | `is_prime (2^255 - 19)` | Same as ED-001 | **Standard.** Shared with Ed25519. |
| X-002 | `scalar_mult_one` | `scalar_mult 1 u == u` | Montgomery ladder with k=1 | **Standard.** Requires showing one iteration preserves u. Should be provable but not done. |
| X-003 | `dh_commutativity` | `x25519(a, x25519(b, G)) == x25519(b, x25519(a, G))` | Algebraic -- group homomorphism property | **Standard.** The fundamental ECDH property. Requires full group law proof. |
| X-004 | `dh_commutativity_general` | Same as X-003 for arbitrary base point | Generalization of X-003 | **Standard.** |
| X-005 | `x25519_zero_u` | `x25519(sk, 0) == 0` | All-zero input maps to all-zero output | **Standard.** Point at infinity stays at infinity. Should be provable (similar to scalar_mult_zero). |

### 2.3 Exported Theorem Analysis

| Theorem | Actual Claim | Vacuity Check |
|---------|-------------|---------------|
| `fmul_inverse` | `a * a^(-1) = 1` | **Non-vacuous.** Full FLT proof. |
| `scalar_mult_zero` | `scalar_mult 0 u == 0` | **Non-vacuous.** Full inductive proof (get_bit_zero + ladder invariant). |
| `decode_encode_round_trip` | `decode(encode(n)) = n` for n < 2^256 | **Non-vacuous.** Full inductive proof. |
| `clamp_idempotent` | `clamp(clamp(s)) == clamp(s)` | **Non-vacuous.** Bitvector reasoning. |
| `clamp_bit254_set` | Bit 254 always set after clamping | **Non-vacuous.** |
| `clamp_multiple_of_8` | Low 3 bits always clear after clamping | **Non-vacuous.** |
| Field axioms (6 lemmas) | Closure, identity, commutativity, associativity, distributivity | **Non-vacuous.** All proved. |
| KAT vectors (4 lemmas) | RFC 7748 Section 6.1 TV1, TV2, shared secret (alice + bob) | **Non-vacuous.** All by assert_norm. |

### 2.4 Constants Audit

| Constant | Value | RFC Reference | Status |
|----------|-------|--------------|--------|
| p | `2^255 - 19` | RFC 7748 Section 4.1 | **CORRECT** |
| a24 | `121666` | `(486662 + 2) / 4` per RFC 7748 | **CORRECT** |
| basepoint | `9` | RFC 7748 Section 4.1 | **CORRECT** |

### 2.5 KAT Vector Coverage

| KAT | Source | Status |
|-----|--------|--------|
| TV1: alice_sk * basepoint = alice_pk | RFC 7748 Section 6.1 | **PROVED** |
| TV2: bob_sk * bob_u = expected | RFC 7748 Section 6.1 | **PROVED** |
| Shared secret (Alice side) | RFC 7748 Section 6.1 | **PROVED** |
| Shared secret (Bob side) | RFC 7748 Section 6.1 | **PROVED** |

Coverage: Both RFC 7748 Section 6.1 test vectors + full ECDH agreement test. **Complete for Section 6.1.**

### 2.6 Runtime Correspondence to src/UmbraVox/Crypto/Curve25519.hs

Explicit correspondence table at lines 1009-1028. Verified:
- All field operations, encoding/decoding, clamping, Montgomery ladder, and top-level x25519 function have matching Haskell counterparts.
- The Haskell `x25519` returns `Maybe ByteString` (checking for all-zero output), while the F* spec returns `coordinate` unconditionally. This is a minor interface difference -- the F* spec does not model the all-zero rejection check as `None`.

### 2.7 Assurance Grade: B+

**Justification:**
- Field arithmetic fully proved including FLT-based inverse
- Montgomery ladder spec is complete and matches RFC 7748 pseudocode
- scalar_mult_zero fully proved (non-trivial inductive proof)
- decode/encode round-trip fully proved
- Clamping properties proved (idempotent, bit constraints)
- 4 complete KAT vectors from RFC 7748
- 4 assume vals (primality, scalar_mult_one, DH commutativity x2, zero-u)
- scalar_mult_one and x25519_zero_u should be provable but are not (minor gap)

**What prevents grade A:** DH commutativity (the key security property) is assumed. This is standard -- it requires the full group law proof.

---

## 3. Spec.HMAC.fst

### 3.1 admit / assume val / Lemma(True) Counts

| Category       | Count |
|----------------|-------|
| `admit`        | 0     |
| `assume val`   | 0     |
| `assume` (inline) | 1  |
| `Lemma (True)` | 1     |

### 3.2 assume Inventory

| # | ID | Statement | Classification | Standard/Suspicious/Vacuous |
|---|-----|-----------|---------------|----------------------------|
| HM-001 | `prepare_key` inline assume | `Seq.length hashed <= block_size` in the unbounded hash path | Length invariant for abstract hash function | **Standard but avoidable.** The bounded variant `prepare_key_bounded` handles this correctly. The assume only exists in the generic/unconstrained path. The concrete HMAC-SHA-256/512 functions use the bounded path. |
| HM-002 | `hmac_prf_placeholder` | `Lemma (True)` | Placeholder for PRF security | **VACUOUS.** Explicitly documented as placeholder. Proves True, not actual PRF indistinguishability. This is a computational security assumption that cannot be proved in F*. |

### 3.3 Exported Theorem Analysis

| Theorem | Actual Claim | Vacuity Check |
|---------|-------------|---------------|
| `hmac_structure_lemma` | HMAC = H(opad_key \|\| H(ipad_key \|\| msg)) | **Non-vacuous.** Unfolds definition. |
| `prepare_key_length` | Prepared key has block_size length | **Non-vacuous.** By construction. |
| `hmac_sha256_kat_tc1` | RFC 4231 TC1 SHA-256 | **Non-vacuous.** assert_norm. |
| `hmac_sha512_kat_tc1` | RFC 4231 TC1 SHA-512 | **Non-vacuous.** assert_norm. |
| `hmac_sha256_kat_tc2` | RFC 4231 TC2 SHA-256 | **Non-vacuous.** assert_norm. |
| `hmac_prf_placeholder` | `True` | **VACUOUS.** Documented as placeholder. |

### 3.4 Constants Audit

| Constant | Value | Reference | Status |
|----------|-------|-----------|--------|
| ipad | `0x36` repeated | RFC 2104 | **CORRECT** |
| opad | `0x5c` repeated | RFC 2104 | **CORRECT** |
| SHA-256 block size | 64 | FIPS 180-4 | **CORRECT** |
| SHA-512 block size | 128 | FIPS 180-4 | **CORRECT** |
| SHA-256 hash length | 32 | FIPS 180-4 | **CORRECT** |
| SHA-512 hash length | 64 | FIPS 180-4 | **CORRECT** |

### 3.5 KAT Vector Coverage

| KAT | Source | Status |
|-----|--------|--------|
| TC1 HMAC-SHA-256 ("Hi There") | RFC 4231 | **PROVED** |
| TC1 HMAC-SHA-512 ("Hi There") | RFC 4231 | **PROVED** |
| TC2 HMAC-SHA-256 ("Jefe"/"what do ya want...") | RFC 4231 | **PROVED** |

Coverage: 3 of 7 RFC 4231 test cases. TC1 and TC2 cover short-key and short-message cases. Missing: TC3 (long key), TC4 (long data), TC5 (truncation), TC6/TC7 (key > block size).

### 3.6 Runtime Correspondence to src/UmbraVox/Crypto/HMAC.hs

The Haskell HMAC module exports `hmacSHA256` and `hmacSHA512`. The F* spec defines the same functions (`hmac_sha256`, `hmac_sha512`) with identical HMAC construction (RFC 2104 two-pass hash with ipad/opad XOR). Block sizes match (64 for SHA-256, 128 for SHA-512).

### 3.7 Assurance Grade: B+

**Justification:**
- HMAC construction correctly specified per RFC 2104
- Constants correct
- Bounded variants eliminate the inline assume for concrete instances
- 3 KAT vectors proved
- 1 vacuous placeholder for PRF security (unavoidable -- computational assumption)
- No substantive assume vals

**What prevents grade A:** The generic `prepare_key` has an inline assume. The PRF placeholder proves True instead of stating the actual property (even as an axiom). Missing TC3-TC7 KAT coverage.

---

## 4. Spec.HKDF.fst

### 4.1 admit / assume val / Lemma(True) Counts

| Category       | Count |
|----------------|-------|
| `admit`        | 0     |
| `assume val`   | 0     |
| `assume` (inline) | 1  |
| `Lemma (True)` | 0     |

### 4.2 assume Inventory

| # | ID | Statement | Classification | Standard/Suspicious/Vacuous |
|---|-----|-----------|---------------|----------------------------|
| HK-001 | `hkdf_expand` inline assume | `Seq.length expanded >= len` in the unbounded path | Length invariant for expand_loop output | **Standard but avoidable.** The bounded variant `hkdf_expand_bounded` handles this correctly via type-level length tracking. Concrete instances (hkdf_sha256, hkdf_sha512) use the bounded path. |

### 4.3 Exported Theorem Analysis

| Theorem | Actual Claim | Vacuity Check |
|---------|-------------|---------------|
| `hkdf_structure_lemma` | `hkdf = expand(extract(...))` | **Non-vacuous.** |
| `hkdf_output_length` | Output length = requested length | **Non-vacuous.** By return type. |
| `extract_default_salt_lemma` | Empty salt defaults to zeros | **Non-vacuous.** |
| `expand_max_blocks_lemma` | Max 255 blocks output | **Non-vacuous.** |
| `ceil_div_bounds_lemma` | `1 <= ceil(len/hash_len) <= 255` | **Non-vacuous.** Arithmetic proof. |
| `hkdf_sha256_kat_tc1_extract` | RFC 5869 TC1 extract | **Non-vacuous.** assert_norm. |
| `hkdf_sha256_kat_tc1_expand` | RFC 5869 TC1 expand | **Non-vacuous.** assert_norm. |
| `hkdf_sha256_kat_tc1_full` | RFC 5869 TC1 full pipeline | **Non-vacuous.** assert_norm. |

### 4.4 Constants Audit

| Constant | Value | Reference | Status |
|----------|-------|-----------|--------|
| SHA-256 hash_len | 32 | FIPS 180-4 | **CORRECT** |
| SHA-512 hash_len | 64 | FIPS 180-4 | **CORRECT** |
| Max output blocks | 255 | RFC 5869 Section 2.3 | **CORRECT** |
| Counter starts at 1 | Yes | RFC 5869 Section 2.3 | **CORRECT** |
| Empty salt default | hash_len zeros | RFC 5869 Section 2.2 | **CORRECT** |

### 4.5 KAT Vector Coverage

| KAT | Source | Status |
|-----|--------|--------|
| TC1 Extract (HMAC-SHA-256) | RFC 5869 | **PROVED** |
| TC1 Expand (HMAC-SHA-256) | RFC 5869 | **PROVED** |
| TC1 Full (HMAC-SHA-256) | RFC 5869 | **PROVED** |

Coverage: 1 of 3 RFC 5869 test cases (all SHA-256). Missing: TC2 (longer inputs), TC3 (zero-length salt/info). No HKDF-SHA-512 KAT vectors despite SHA-512 being the primary KDF for the protocol.

### 4.6 Runtime Correspondence to src/UmbraVox/Crypto/HKDF.hs

The Haskell module exports `hkdfExtract`, `hkdfExpand`, `hkdf` with both SHA-256 and SHA-512 variants. The F* spec mirrors these exactly:
- `hkdf_extract` <-> `hkdfExtract`
- `hkdf_expand` <-> `hkdfExpand`
- `hkdf` <-> `hkdf`
- SHA-256 and SHA-512 concrete instances match

### 4.7 Assurance Grade: B

**Justification:**
- HKDF construction correctly specified per RFC 5869
- Bounded variants eliminate the inline assume for concrete instances
- 3 KAT lemmas for 1 test case
- Structural properties proved
- No assume vals

**What prevents grade A:** Only 1 of 3 RFC 5869 test cases. No SHA-512 KAT vectors (critical given SHA-512 is the primary HKDF variant). The inline assume in the generic path is a minor blemish.

---

## 5. Spec.MLKEM768.fst

### 5.1 admit / assume val / Lemma(True) Counts

| Category       | Count |
|----------------|-------|
| `admit`        | 0     |
| `assume val`   | 0     |
| `assume` (inline) | 0  |
| `Lemma (True)` | **8** |

### 5.2 Stub/Vacuous Lemma Inventory

**ALL core functions are identity/constant stubs.** The file is explicit about this in its header.

| # | Function/Lemma | What it claims to prove | What it actually proves | Classification |
|---|---------------|------------------------|------------------------|---------------|
| ML-001 | `ntt` | NTT forward transform | **Returns input unchanged (identity)** | STUB |
| ML-002 | `inv_ntt` | Inverse NTT | **Returns input unchanged (identity)** | STUB |
| ML-003 | `ntt_roundtrip_stub` | `inv_ntt(ntt(f)) == f` | **Proves True (identity composed with identity)** | VACUOUS |
| ML-004 | `cbd` | Centered binomial distribution | **Returns all-zeros** | STUB |
| ML-005 | `cbd_bound_stub` | CBD coefficients bounded by eta | **Proves True (0 <= eta trivially)** | VACUOUS |
| ML-006 | `sample_ntt` | Rejection sampling from XOF | **Returns all-zeros** | STUB |
| ML-007 | `sample_ntt_range_stub` | All coefficients in [0,q) | **Proves True (0 < 3329 trivially)** | VACUOUS |
| ML-008 | `compress_decompress_approx_stub` | Compress/decompress roundtrip bound | **Proves True (no bound checked)** | VACUOUS |
| ML-009 | `kpke_keygen` | K-PKE key generation | **Returns (d, d)** | STUB |
| ML-010 | `kpke_encrypt` | K-PKE encryption | **Returns msg** | STUB |
| ML-011 | `kpke_decrypt` | K-PKE decryption | **Returns ct** | STUB |
| ML-012 | `kpke_correctness_stub` | K-PKE correctness | **Proves True** | VACUOUS |
| ML-013 | `mlkem_keygen` | ML-KEM key generation | **Returns (seed, seed)** | STUB |
| ML-014 | `mlkem_encaps` | ML-KEM encapsulation | **Returns (m, m)** | STUB |
| ML-015 | `mlkem_decaps` | ML-KEM decapsulation | **Returns ct** | STUB |
| ML-016 | `mlkem_correctness_stub` | ML-KEM correctness | **Proves True** | VACUOUS |
| ML-017 | `mlkem_ss_length_stub` | Shared secret length = 32 | **Proves True** | VACUOUS |
| ML-018 | `mlkem_keygen_ek_size_stub` | Encap key size = 1184 | **Proves True** | VACUOUS |
| ML-019 | `kat_keygen_lengths_stub` | KAT keygen lengths | **Proves True** | VACUOUS |
| ML-020 | `kat_encaps_lengths_stub` | KAT encaps lengths | **Proves True** | VACUOUS |
| ML-021 | `kat_implicit_rejection_stub` | Implicit rejection | **Proves True** | VACUOUS |

### 5.3 What IS genuinely proved

Only the parameter validation section contains real proofs:

| Assertion | Status |
|-----------|--------|
| `mlkem_k = 3` | **PROVED** (assert_norm) |
| `mlkem_n = 256` | **PROVED** (assert_norm) |
| `mlkem_q = 3329` | **PROVED** (assert_norm) |
| `mlkem_eta1 = 2` | **PROVED** (assert_norm) |
| `mlkem_eta2 = 2` | **PROVED** (assert_norm) |
| `mlkem_du = 10` | **PROVED** (assert_norm) |
| `mlkem_dv = 4` | **PROVED** (assert_norm) |
| `mlkem_q > 1` | **PROVED** (assert_norm) |

These confirm the ML-KEM-768 parameters match FIPS 203 Table 2. The `compress` and `decompress` functions have correct formulas but are never connected to any functional proof.

### 5.4 Constants Audit

| Constant | Value | FIPS 203 Table 2 | Status |
|----------|-------|-----------------|--------|
| n | 256 | 256 | **CORRECT** |
| k | 3 | 3 | **CORRECT** |
| q | 3329 | 3329 | **CORRECT** |
| eta1 | 2 | 2 | **CORRECT** |
| eta2 | 2 | 2 | **CORRECT** |
| du | 10 | 10 | **CORRECT** |
| dv | 4 | 4 | **CORRECT** |
| encap_key_size | 1184 | 384*k+32 = 1184 | **CORRECT** |
| decap_key_size | 2400 | 768*k+96 = 2400 | **CORRECT** |
| ciphertext_size | 1088 | 32*(du*k+dv) = 1088 | **CORRECT** |
| shared_secret_size | 32 | 32 | **CORRECT** |
| zeta | 17 | primitive 256th root of unity mod 3329 | **CORRECT** (17^128 = -1 mod 3329) |
| barrett_const | 20159 | floor(2^26/3329) = 20159 | **CORRECT** |

**Note:** `q` is claimed to be prime but only `q > 1` is proved. Primality of 3329 is not verified (it is easy to verify: 3329 is prime).

### 5.5 KAT Vector Coverage

**NONE.** All KAT lemmas prove True. Zero test vectors are included.

### 5.6 Runtime Correspondence to src/UmbraVox/Crypto/MLKEM.hs

The Haskell MLKEM module is a full implementation with NTT, CBD, K-PKE, and ML-KEM algorithms (507+ lines). The F* spec is a stub that shares only parameter names. There is **zero functional correspondence** between the F* stubs and the Haskell implementation.

| F* | Haskell | Match |
|----|---------|-------|
| `ntt` (identity) | `ntt` (full butterfly NTT) | **NO MATCH** |
| `kpke_keygen` (returns input) | `kpkeKeyGen` (full Algorithm 12) | **NO MATCH** |
| `mlkem_encaps` (returns input) | `mlkemEncaps` (full Algorithm 16) | **NO MATCH** |
| All other functions | Full FIPS 203 algorithms | **NO MATCH** |

### 5.7 Assurance Grade: F

**Justification:**
- Parameters are correctly specified and verified by assert_norm
- ALL cryptographic functions are identity/constant stubs
- ALL lemmas are vacuous (prove True)
- ZERO functional correctness proofs
- ZERO KAT vectors
- ZERO correspondence to the Haskell implementation
- The file is HONEST about being stubs (header, _stub suffixes, comments)
- The only genuine content is parameter validation (~10 lines)

**This file provides zero assurance for ML-KEM-768 functional correctness.** It is a parameter-validation scaffold, not a specification. The honesty of the documentation prevents a lower-than-F grade (which would imply active deception).

---

## Summary Table

| File | assume val | assume (inline) | Lemma(True) | KAT vectors | Grade |
|------|-----------|-----------------|-------------|-------------|-------|
| Spec.Ed25519.fst | 10 | 0 | 0 | 6 (2 TV) | **B+** |
| Spec.X25519.fst | 4 | 0 | 0 | 4 (2 TV + ECDH) | **B+** |
| Spec.HMAC.fst | 0 | 1 | 1 | 3 (2 TC) | **B+** |
| Spec.HKDF.fst | 0 | 1 | 0 | 3 (1 TC) | **B** |
| Spec.MLKEM768.fst | 0 | 0 | 8 | 0 | **F** |

### Trust Surface Summary

**Irreducible assumptions (mathematically valid, not machine-checkable in F*):**
1. Primality of 2^255 - 19 (ED-001/X-001) -- externally verified
2. Group order [L]B = O (ED-002) -- externally verified
3. Point addition associativity (ED-003) -- proven in fiat-crypto (Coq)
4. DH commutativity (X-003/X-004) -- follows from group law

**Derived assumptions (follow from irreducible ones):**
5. scalar_mult_add (ED-004) -- needs associativity
6. scalar_mult_compose (ED-005) -- needs scalar_mult_add
7. scalar_mod_L_equiv (ED-006) -- composite
8. point_add_congruence_right (ED-007) -- projective equivalence
9. sign_then_verify (ED-008) -- algebraic core proved, needs encode/decode round-trip
10. encode_decode_round_trip (ED-009) -- needs square root recovery proof

**Cryptographic assumptions (inherently unprovable):**
11. distinct_messages_distinct_sigs (ED-010) -- collision resistance
12. HMAC PRF security (HM-002) -- computational assumption

**Total trust surface:** 4 irreducible mathematical axioms + 6 derived axioms + 2 cryptographic assumptions. All are standard and well-documented. No suspicious or vacuous assume vals.

**Critical gap:** ML-KEM-768 has zero functional assurance. This is the most significant finding of the audit.

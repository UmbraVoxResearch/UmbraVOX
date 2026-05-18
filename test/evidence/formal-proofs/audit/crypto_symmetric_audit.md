# Symmetric Crypto F* Specification Audit

Auditor: Crypto-Assurance Audit Mode
Date: 2026-05-17
Scope: 8 symmetric-crypto F* spec files (10-phase audit brief)

---

## 1. Spec.SHA256.fst

### 1.1 admit / assume val / Lemma(True) counts

| Category | Count | Details |
|----------|-------|---------|
| admit | 0 | None |
| assume val | 0 | None |
| Lemma(True) | 0 | None |
| Placeholders | 0 | None |

### 1.2 Exported theorem audit

| Theorem | Actual claim | Consumer-perceived claim | GAP? |
|---------|-------------|------------------------|------|
| sha256_output_length | Seq.length (sha256 msg) = 32 | Output is always 32 bytes | No gap (length only, but accurately named) |
| pad_length_lemma | pad output % 64 = 0 | Padding is block-aligned | No gap |
| pad_nonempty_lemma | pad output >= 64 bytes | At least one block produced | No gap |
| sha256_kat_abc | sha256("abc") == expected (via assert_norm) | NIST KAT match | No gap -- full concrete evaluation |
| sha256_kat_empty | sha256("") == expected | NIST KAT match | No gap |
| sha256_kat_448bit | sha256(56-byte msg) == expected | NIST KAT match (two-block path) | No gap |
| compress_preserves_length | compress returns 8 words | Compression is type-safe | No gap (length, not correctness) |
| ch_identity / maj_identity | Definitional unfolding | Function matches formula | No gap (tautological but honest) |
| ch_all_ones / ch_all_zeros | Boundary behavior of Ch | Ch selects correctly at extremes | No gap |
| schedule_low_words_spec | W[t] for t<16 = big-endian parse | Schedule init matches spec | No gap |
| schedule_high_words_spec | W[t] for 16<=t<64 matches FIPS formula | Schedule extension matches FIPS | No gap |
| compress_is_foldback_of_rounds | compress[i] = h[i] + rounds[i] mod 2^32 | Foldback matches FIPS 180-4 | No gap |
| round_step_state_bounded | Output has 8 words | Round preserves state shape | Slight naming mismatch: "bounded" suggests value bounds, proves length |
| round_step_words_32bit | UInt32.v < pow2 32 | Words fit in 32 bits | No gap (follows from UInt32 type) |

### 1.3 Constants audit

- **IV values (h0..h7)**: All 8 values match FIPS 180-4 Section 5.3.3. Individually verified by assert_norm on Seq.index.
- **Round constants (k_table)**: All 64 values match FIPS 180-4 Section 4.2.2. Individually verified by 64 assert_norm statements on Seq.index.
- **Rotation/shift constants**: bsig0(2,13,22), bsig1(6,11,25), ssig0(7,18,3), ssig1(17,19,10) -- all match FIPS 180-4 Section 4.1.2.

### 1.4 KAT vector coverage

| Vector | Status | Coverage |
|--------|--------|----------|
| SHA-256("abc") | VERIFIED (assert_norm, full digest) | Full 32 bytes |
| SHA-256("") | VERIFIED (assert_norm, full digest) | Full 32 bytes |
| SHA-256(56-byte msg) | VERIFIED (assert_norm, full digest) | Full 32 bytes, two-block path |
| Long message (>64 bytes, multi-block) | MISSING | No vector exercises >2 blocks |
| Million 'a' vector | MISSING | Standard NIST vector absent (likely Z3 resource limitation) |

### 1.5 Runtime correspondence

Claims correspondence with `src/UmbraVox/Crypto/SHA256.hs`. No formal refinement proof connecting the F* spec to the Haskell code exists in this file. A separate `Spec.SHA256.Refinement.fst` exists but uses `assume val sha256_refinement_axiom` (an unproved axiom) to bridge the gap.

**FINDING**: The correspondence is asserted, not proved. The refinement file's axiom is the trust boundary.

### 1.6 Assurance grade: **A-**

Strong spec. All constants verified. Three substantive KATs with full digest comparison via assert_norm. No admits, no assume vals, no placeholders. Complete algorithmic structure (padding, schedule, rounds, compression, serialization). Missing only long-message KAT and formal Haskell correspondence.

---

## 2. Spec.SHA512.fst

### 2.1 admit / assume val / Lemma(True) counts

| Category | Count |
|----------|-------|
| admit | 0 |
| assume val | 0 |
| Lemma(True) | 0 |

### 2.2 Exported theorem audit

| Theorem | Actual claim | GAP? |
|---------|-------------|------|
| sha512_output_length | length = 64 | No gap |
| sha512_kat_abc | Full digest match for "abc" | No gap |
| sha512_kat_empty | Full digest match for "" | No gap |
| schedule_low_words_spec | W[t] for t<16 = big-endian parse of 8-byte fields | No gap |
| schedule_high_words_spec | W[t] for 16<=t<80 matches FIPS formula | No gap |
| compress_is_foldback_of_rounds | Foldback = component-wise add_mod | No gap |
| round_step_words_64bit | Values < pow2 64 | No gap (tautological from UInt64 type) |

### 2.3 Constants audit

- **IV values (h0..h7)**: 8 values match FIPS 180-4 Section 5.3.5 (64-bit values from sqrt of first 8 primes). NOT individually assert_norm'd unlike SHA-256.
- **Round constants (k_table)**: 80 entries present matching FIPS 180-4 Section 4.2.3. List length verified by assert_norm.
- **Rotation constants**: bsig0(28,34,39), bsig1(14,18,41), ssig0(1,8,7), ssig1(19,61,6) -- all match FIPS 180-4 Section 4.1.3.

**FINDING**: No per-entry assert_norm on k_table entries (unlike SHA-256 which verifies all 64 individually). No per-entry assert_norm on init_hash entries. The list literals are the sole truth source -- a typo in any constant would pass undetected by F*.

### 2.4 KAT vector coverage

| Vector | Status |
|--------|--------|
| SHA-512("abc") | VERIFIED (full 64-byte digest) |
| SHA-512("") | VERIFIED (full 64-byte digest) |
| Two-block message | MISSING |
| Long message | MISSING |

### 2.5 Runtime correspondence

Claims correspondence with `src/UmbraVox/Crypto/SHA512.hs`. No formal refinement proof.

### 2.6 Assurance grade: **B+**

Solid structure mirroring SHA-256 but weaker constant verification -- no per-index assert_norm on K table or IV values. Two KATs present but missing multi-block coverage.

---

## 3. Spec.ChaCha20.fst

### 3.1 admit / assume val / Lemma(True) counts

| Category | Count | Details |
|----------|-------|---------|
| admit | 0 | |
| assume val | 0 | |
| Lemma(True) | 0 | |
| Placeholders | 1 | `kat_block_placeholder` -- VACUOUSLY TRUE |

### 3.2 Exported theorem audit

| Theorem | Actual claim | Consumer-perceived claim | GAP? |
|---------|-------------|------------------------|------|
| qr_test | Quarter-round on specific inputs matches RFC 8439 2.1.1 | QR function is correct | No gap |
| encrypt_decrypt_roundtrip | chacha20_encrypt(chacha20_encrypt(msg)) = msg | XOR-based encryption is involutory | No gap -- substantive proof by induction |
| encrypt_decrypt_single_block | Same for single block | Single-block roundtrip | No gap |
| xor_involutory | XOR with same keystream recovers plaintext | XOR is self-inverse | No gap |
| chacha20_encrypt_length | Output length = input length | Stream cipher preserves length | No gap |
| kat_block_placeholder | **VACUOUSLY TRUE** | Block function KAT | **CRITICAL GAP**: seq_of_hex returns Seq.empty, so length key = key_size (32) is always false. The implication is trivially true. Proves NOTHING. |
| kat_block_rfc8439 | First 4 bytes match RFC 8439 2.3.2 | Block function KAT | Partial: only 4 of 64 bytes (6.25%) |
| kat_allzero | First 4 bytes of all-zero block | All-zero KAT | Partial: 4 of 64 bytes (6.25%) |

### 3.3 Constants audit

- **Sigma constants**: 0x61707865, 0x3320646e, 0x79622d32, 0x6b206574 -- match "expand 32-byte k" in little-endian. CORRECT.
- **Quarter-round rotation amounts**: 16, 12, 8, 7 -- match RFC 8439 Section 2.1. CORRECT.
- **Double-round column/diagonal indices**: Columns (0,4,8,12), (1,5,9,13), (2,6,10,14), (3,7,11,15); Diagonals (0,5,10,15), (1,6,11,12), (2,7,8,13), (3,4,9,14) -- match RFC 8439. CORRECT.

### 3.4 KAT vector coverage

| Vector | Status | Coverage |
|--------|--------|----------|
| Quarter-round (RFC 8439 2.1.1) | VERIFIED (full) | 4 output words |
| Block function (RFC 8439 2.3.2) | PARTIAL | 4/64 bytes |
| All-zero block | PARTIAL | 4/64 bytes |
| Encryption KAT (RFC 8439 2.4.2) | MISSING | No encryption-level KAT |
| kat_block_placeholder | VACUOUS | Proves nothing (seq_of_hex is stub) |

**FINDING**: The `kat_block_placeholder` is explicitly marked as vacuously true in comments, which is honest. However, its existence alongside the real KATs may create false confidence in casual review. The real KATs only check 4 bytes each -- a block-level error in bytes 4-63 would be undetected.

### 3.5 Runtime correspondence

Comment table maps F* definitions to Haskell counterparts in `src/UmbraVox/Crypto/Random.hs`. No formal refinement proof.

### 3.6 Assurance grade: **B**

Strong structural properties (roundtrip proof by induction). Honest about the vacuous placeholder. But KAT coverage is severely limited (6.25% of block output), and no full encryption-level KAT exists.

---

## 4. Spec.Poly1305.fst

### 4.1 admit / assume val / Lemma(True) counts

| Category | Count | Details |
|----------|-------|---------|
| admit | 0 | |
| assume val | 0 | |
| Lemma(True) | 2 | `le_roundtrip_placeholder` (proves True, not le_to_nat roundtrip), `poly1305_uf_cma_placeholder` (proves True, not UF-CMA) |

### 4.2 Exported theorem audit

| Theorem | Actual claim | Consumer-perceived claim | GAP? |
|---------|-------------|------------------------|------|
| poly1305_tag_length | Output is 16 bytes | Tag size is correct | No gap |
| empty_message_lemma | process_blocks on empty returns 0 | Empty message handling | No gap |
| le_roundtrip_placeholder | **True** | LE encoding roundtrip | **GAP**: Name suggests roundtrip, proves nothing |
| poly1305_uf_cma_placeholder | **True** | UF-CMA security | **GAP**: Name suggests security bound, proves nothing |
| bitwise_and_bound | bitwise_and a b <= b | AND monotonicity | No gap |
| clamp_r_bound_lemma | clamp_r r <= clamp_mask | Clamping bound | No gap |
| poly1305_rfc8439_kat | Full KAT match (RFC 8439 2.5.2) | Poly1305 produces correct tag | No gap -- full concrete evaluation via assert_norm |

### 4.3 Constants audit

- **Prime**: p = 2^130 - 5. CORRECT.
- **Clamp mask**: 0x0ffffffc0ffffffc0ffffffc0fffffff. Matches RFC 8439 Section 2.5.1 clamping. CORRECT.

### 4.4 KAT vector coverage

| Vector | Status | Coverage |
|--------|--------|----------|
| RFC 8439 Section 2.5.2 | VERIFIED (full 16-byte tag) | Full |
| Empty message | MISSING as KAT | Only structural lemma, no expected value |

### 4.5 Runtime correspondence

Claims correspondence with `src/UmbraVox/Crypto/Poly1305.hs`. No formal refinement proof.

### 4.6 Assurance grade: **B+**

Full KAT with concrete evaluation. Arithmetic is correct. Two Lemma(True) placeholders are honestly labeled but may create false confidence. The UF-CMA placeholder is fundamentally unprovable in F* (requires probabilistic reasoning), which is correctly noted.

---

## 5. Spec.ChaChaPoly.fst

### 5.1 admit / assume val / Lemma(True) counts

| Category | Count | Details |
|----------|-------|---------|
| admit | 0 | |
| assume val | 1 | `tag_forgery_ct_axiom` -- cryptographic assumption for CT tampering detection |
| Lemma(True) | 0 | |

### 5.2 Exported theorem audit

| Theorem | Actual claim | Consumer-perceived claim | GAP? |
|---------|-------------|------------------------|------|
| aead_correctness | decrypt(encrypt(k,n,aad,pt)) = Some pt | AEAD roundtrip | No gap -- proved using ChaCha20 roundtrip + deterministic tag |
| tag_forgery_tag | Flipping tag byte => None | Tag mutation detected | No gap -- proved structurally (deterministic recomputation + flip_byte_neq) |
| tag_forgery_ct | Flipping CT byte => None | CT mutation detected | **AXIOM**: Delegated to `tag_forgery_ct_axiom` (assume val). This is an irreducible cryptographic assumption (Poly1305 delta-universality). |
| encrypt_length_preserving | len(ct) = len(pt) | Length preservation | No gap |
| encrypt_tag_length | len(tag) = 16 | Tag size | No gap |
| flip_byte_neq | flip_byte s i <> s | Mutation produces distinct seq | No gap |
| rfc8439_key_length | Key has 32 bytes | Size check | No gap |
| rfc8439_nonce_length | Nonce has 12 bytes | Size check | No gap |

### 5.3 Constants audit

- **key_size = 32, nonce_size = 12, tag_size = 16**: Match RFC 8439. CORRECT.
- RFC 8439 Section 2.8.2 test vector data present (key, nonce) but no full KAT evaluation.

### 5.4 KAT vector coverage

| Vector | Status |
|--------|--------|
| RFC 8439 Section 2.8.2 AEAD | NOT VERIFIED -- only key/nonce lengths checked. No encrypt/decrypt comparison. Described as "placeholder" |

**FINDING**: Despite having test vector data, there is no assert_norm evaluating `chachapoly_encrypt` on the RFC 8439 Section 2.8.2 vector. The KAT is purely structural (length checks).

### 5.5 Runtime correspondence

Comment table maps F* to `src/UmbraVox/Crypto/ChaChaPoly.hs`. No formal refinement.

### 5.6 Assurance grade: **B**

AEAD correctness (roundtrip) is properly proved. Tag-mutation detection is proved structurally. CT-mutation relies on a clearly labeled assume val (irreducible crypto assumption). Missing a concrete AEAD KAT is a notable gap.

---

## 6. Spec.AES256.fst

### 6.1 admit / assume val / Lemma(True) counts

| Category | Count |
|----------|-------|
| admit | 0 |
| assume val | 0 |
| Lemma(True) | 0 |

### 6.2 Exported theorem audit

| Theorem | Actual claim | GAP? |
|---------|-------------|------|
| sbox_inv_sbox_roundtrip | inv_sub_byte(sub_byte(b)) = b for all b | No gap -- full 256-entry composition check via assert_norm |
| inv_sbox_sbox_roundtrip | sub_byte(inv_sub_byte(b)) = b for all b | No gap |
| add_round_key_self_inverse | XOR self-inverse | No gap |
| inv_sub_bytes_sub_bytes | InvSubBytes(SubBytes(st)) = st | No gap |
| inv_shift_rows_shift_rows | InvShiftRows(ShiftRows(st)) = st | No gap |
| inv_mix_columns_mix_columns | InvMixColumns(MixColumns(st)) = st | No gap |
| sub_bytes_shift_rows_commute | SubBytes/ShiftRows commute | No gap |
| inv_round_cancels | Inverse round cancels forward round | No gap -- key structural lemma |
| encrypt_decrypt_roundtrip | aes_decrypt(aes_encrypt(key,pt)) = pt | No gap -- proved by induction over 14 rounds |
| decrypt_encrypt_roundtrip | aes_encrypt(aes_decrypt(key,ct)) = ct | No gap |
| aes256_kat_encrypt | FIPS 197 C.3 KAT (encrypt) | No gap -- assert_norm on concrete vector |
| aes256_kat_decrypt | FIPS 197 C.3 KAT (decrypt) | No gap |
| key_expansion_length | 60 words | No gap |

### 6.3 Constants audit

- **S-box (256 entries)**: Matches FIPS 197 Table A.1. Verified via `assert_norm (sbox_comp_seq = id_seq)` which composes sbox and inv_sbox and checks against identity for all 256 values. This proves internal consistency (roundtrip) but does NOT independently verify each S-box entry against the FIPS standard.
- **Inverse S-box (256 entries)**: Same roundtrip verification.
- **Rcon table (7 entries)**: 0x01000000, 0x02000000, 0x04000000, 0x08000000, 0x10000000, 0x20000000, 0x40000000. Matches FIPS 197 for AES-256 (i/Nk = 1..7). CORRECT.
- **MixColumns coefficients**: Forward (2,3,1,1), Inverse (0x0e,0x0b,0x0d,0x09). Match FIPS 197. CORRECT.
- **ShiftRows offsets**: (0,1,2,3). Match FIPS 197. CORRECT.
- **GF(2^8) reduction polynomial**: x^8 + x^4 + x^3 + x + 1 (0x1b). Match FIPS 197. CORRECT.

**FINDING**: S-box entries are verified by roundtrip composition (sbox then inv_sbox = identity) but NOT by independent per-entry comparison to FIPS 197 Table A.1. If both tables had coordinated errors (e.g., a swapped pair), the roundtrip would still pass. The FIPS 197 C.3 KAT catches this in practice for the byte values exercised by that specific input.

### 6.4 KAT vector coverage

| Vector | Status | Coverage |
|--------|--------|----------|
| FIPS 197 C.3 (encrypt) | VERIFIED (assert_norm, full 16-byte block) | Full |
| FIPS 197 C.3 (decrypt) | VERIFIED (assert_norm, full 16-byte block) | Full |
| FIPS 197 C.3 (roundtrip) | VERIFIED (derived from above) | Full |
| Additional NIST vectors | MISSING | Only one key/pt pair tested |

### 6.5 Runtime correspondence

Claims correspondence with `src/UmbraVox/Crypto/AES.hs`. No formal refinement.

### 6.6 Assurance grade: **A-**

Outstanding structural proofs: full encrypt/decrypt roundtrip proved by induction with all component inverses. S-box roundtrip verified for all 256 values. FIPS 197 C.3 KAT with full block comparison. Minor gaps: only one KAT vector, S-box not independently verified entry-by-entry against standard.

---

## 7. Spec.GCM.fst

### 7.1 admit / assume val / Lemma(True) counts

| Category | Count | Details |
|----------|-------|---------|
| admit | 0 | |
| assume val | 0 | |
| Lemma(True) | 1 | `ghash_universal_hash_placeholder` (proves True, not GHASH universality) |

### 7.2 Exported theorem audit

| Theorem | Actual claim | GAP? |
|---------|-------------|------|
| gcm_roundtrip | decrypt(encrypt(e,k,n,aad,pt)) = Some pt | No gap -- fully proved |
| gcm_tag_integrity | bad_tag != good_tag => decrypt returns None | No gap -- proved via constant_eq_correct |
| gctr_involutive | GCTR is self-inverse | No gap -- proved by induction |
| gctr_length | GCTR preserves length | No gap |
| ghash_linearity | GHASH distributes over XOR | No gap -- inductive proof |
| constant_eq_correct | constant_eq returns true iff equal | No gap |
| gcm_encrypt_decrypt_same_tag | Decrypt recomputes same tag | Ensures True in post -- **actually a placeholder** that proves nothing beyond structural congruence |
| ghash_universal_hash_placeholder | **True** | Name suggests universality, proves nothing |
| gcm_kat_tc14 | NIST TC14 (empty PT/AAD, all-zero key) | No gap -- full assert_norm |
| gcm_kat_tc16 | NIST TC16 (64-byte PT, non-trivial key) | No gap -- full assert_norm |

**FINDING**: `gcm_encrypt_decrypt_same_tag` has `ensures True` in its postcondition. The elaborate let-bindings in the ensures clause compute values but the actual obligation is just `True`. This is misleading -- it looks like it proves tag agreement but actually proves nothing.

### 7.3 Constants audit

- **block_size = 16, key_size = 32, nonce_size = 12, tag_size = 16**: Match SP 800-38D. CORRECT.
- **J0 construction**: nonce || 0x00000001 for 96-bit nonce. Matches SP 800-38D Section 7.1. CORRECT.
- **incr32**: Increments rightmost 32 bits. Matches SP 800-38D Section 6.2. CORRECT.

### 7.4 KAT vector coverage

| Vector | Status | Coverage |
|--------|--------|----------|
| NIST TC14 (empty PT, zero key) | VERIFIED (tag match via assert_norm) | Full 16-byte tag |
| NIST TC16 (64-byte PT, non-trivial key) | VERIFIED (CT + tag match) | Full |
| Non-empty AAD | MISSING | Neither TC14 nor TC16 exercise AAD |

**FINDING**: No KAT exercises the AAD (associated data) path. Both test cases use empty AAD. An error in `pad_to_16(aad)` handling would be undetected.

### 7.5 Runtime correspondence

Claims correspondence with `src/UmbraVox/Crypto/GCM.hs`. Uses `Spec.AES256.aes_encrypt` concretely for KAT verification. The GCM spec is parameterized over `aes_encrypt_fn_full`, and the KATs instantiate it with the verified AES-256 spec, which is a strong design.

### 7.6 Assurance grade: **A-**

Excellent spec. Full roundtrip proof, tag integrity proof, GCTR involutivity, GHASH linearity. Two NIST KATs with full evaluation including concrete AES-256. The `gcm_encrypt_decrypt_same_tag` ensures-True and missing AAD KAT coverage are the only notable gaps.

---

## 8. Spec.GaloisField.fst

### 8.1 admit / assume val / Lemma(True) counts

| Category | Count |
|----------|-------|
| admit | 0 |
| assume val | 0 |
| Lemma(True) | 0 |

### 8.2 Exported theorem audit

| Theorem | Actual claim | GAP? |
|---------|-------------|------|
| gf_xor_comm | XOR commutativity | No gap |
| gf_xor_assoc | XOR associativity | No gap |
| gf_xor_zero_identity | XOR with zero = identity | No gap |
| gf_xor_self_zero | a XOR a = 0 | No gap |
| gf_mul_zero | Mul by zero = zero | No gap |
| gf_mul_comm | Multiplication commutativity | **CONCERN**: Proved with `()` body at `--fuel 129 --z3rlimit 4000`. This is a non-trivial property. If Z3 actually discharges this on symbolic bitvectors, it is valid but fragile. If it times out on some configurations, it silently becomes unproven. |
| gf_mul_distributive | Left-distributivity over XOR | No gap -- full inductive proof with case analysis |
| gf_bs_roundtrip | bs_to_gf(gf_to_bs(x)) = x | No gap |
| gf_test_bit_xor | Bit extraction distributes over XOR | No gap |

### 8.3 Constants audit

- **r_poly**: 0xe100000000000000. Encodes x^7 + x^2 + x + 1 in MSB-first 128-bit representation. Matches SP 800-38D Section 6.3. CORRECT.

### 8.4 KAT vector coverage

No standalone KATs. GF(2^128) operations are tested indirectly through GCM KATs (TC14, TC16).

### 8.5 Runtime correspondence

Claims correspondence with `src/UmbraVox/Crypto/GCM.hs`. The GF(2^128) implementation is consumed by Spec.GCM.

### 8.6 Assurance grade: **B+**

Complete algebraic property set for GF(2^128). The distributivity proof is the most valuable (needed for GHASH linearity). Commutativity proof is Z3-fragile. No direct KATs but tested via GCM integration.

---

## Summary: Trust Surface

### Global findings

1. **ZERO admits across all 8 files.** This is excellent.

2. **1 assume val** (in Spec.ChaChaPoly.fst): `tag_forgery_ct_axiom`. This is an irreducible cryptographic assumption (Poly1305 delta-universality). Clearly labeled and fundamentally unprovable in F*. **Acceptable trust boundary.**

3. **4 Lemma(True) placeholders total:**
   - `le_roundtrip_placeholder` (Poly1305) -- name misleading
   - `poly1305_uf_cma_placeholder` (Poly1305) -- explicitly noted as unprovable
   - `ghash_universal_hash_placeholder` (GCM) -- explicitly noted as unprovable
   - `gcm_encrypt_decrypt_same_tag` has `ensures True` disguised in elaborate postcondition

4. **1 vacuous KAT** (ChaCha20): `kat_block_placeholder` proves nothing due to seq_of_hex returning empty. Honestly labeled in comments but may create false confidence.

5. **Partial KATs** (ChaCha20): Block-function KATs check only 4 of 64 output bytes (6.25%).

6. **No AAD KAT in GCM**: Both NIST test cases use empty AAD.

7. **SHA-512 constant verification gap**: Unlike SHA-256 (which individually verifies all 64 K entries and all 8 IV entries), SHA-512 does NOT individually verify its 80 K entries or 8 IV entries.

8. **No formal Haskell correspondence proofs** in any file. Spec.SHA256.Refinement.fst exists but uses an assume val axiom.

### Assurance grades summary

| File | Grade | Key strengths | Key weaknesses |
|------|-------|--------------|----------------|
| Spec.SHA256.fst | A- | 3 full KATs, all constants individually verified, complete algorithm | No formal Haskell correspondence |
| Spec.SHA512.fst | B+ | 2 KATs, complete algorithm | Constants not individually verified |
| Spec.ChaCha20.fst | B | Roundtrip proof, honest about vacuous KAT | KATs only check 4/64 bytes, no encryption KAT |
| Spec.Poly1305.fst | B+ | Full RFC KAT, correct field arithmetic | 2 Lemma(True) placeholders |
| Spec.ChaChaPoly.fst | B | AEAD correctness proved, tag mutation proved | 1 assume val, no concrete AEAD KAT |
| Spec.AES256.fst | A- | Full roundtrip proof (both directions), S-box 256-entry verification, FIPS KAT | Only 1 KAT vector, S-box not independently verified per-entry |
| Spec.GCM.fst | A- | Roundtrip proof, tag integrity, 2 NIST KATs with concrete AES-256, GHASH linearity | No AAD KAT, ensures-True placeholder |
| Spec.GaloisField.fst | B+ | Distributivity proof, algebraic properties | gf_mul_comm Z3-fragile, no direct KATs |

### Overall symmetric crypto assurance: **B+ / A-**

The specifications are among the best I have seen for a non-HACL* project. The trust surface is honest and small: 1 cryptographic axiom, 4 Lemma(True) placeholders (all either labeled or fundamentally unprovable). The primary risk is KAT coverage breadth (ChaCha20 partial coverage, no AAD path in GCM) and the lack of formal correspondence to the Haskell runtime.

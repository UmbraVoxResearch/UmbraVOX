# F* Assume Reduction Plan

Status: planning (no .fst modifications without explicit approval)

## 1. Assume Inventory

217 `assume` + 13 `assume val` across 17 specs (per karamel-evaluation.md).

### 1.1 By Category

| Category | Count | Description | Disposition |
|---|---|---|---|
| KAT assumes | ~45 | Test-vector equality (e.g., HMAC L168, SHA256 L654, HKDF L223) | Retain -- validated by runtime KATs |
| Structural assumes | ~95 | Length bounds, index safety, divisibility (e.g., GCM L91, L126, L206; HKDF L78, L80; SHA256 L186, L615; SHA512 L160, L169) | Prove -- replace with lemmas or assert_norm |
| Axiomatized functions | 13 | `assume val` declarations (ChaCha20: 11, Ed25519: 1, Poly1305: 1) | Implement -- fill with actual definitions |
| Security assumes | ~5 | PRF/indistinguishability claims (HMAC L135-137) | Track -- these are computational assumptions, not proof obligations |
| Correctness property assumes | ~59 | Encrypt/decrypt roundtrip, GHASH linearity, tag-length (e.g., GCM L287-311, L330-335) | Prove -- discharge as F* lemmas |

### 1.2 By Spec File

| Spec | assume | assume val | Dominant category |
|---|---|---|---|
| Spec.GCM | 26 | 0 | Structural (length/alignment) + correctness properties |
| Spec.Ed25519 | 35 | 1 | Structural (field arithmetic bounds) |
| Spec.X25519 | 32 | 0 | Structural (field arithmetic bounds) |
| Spec.Keccak | 24 | 0 | Structural (permutation indexing) |
| Spec.MLKEM768 | 15 | 0 | Structural (polynomial/NTT bounds) |
| Spec.AES256 | 14 | 0 | Structural (S-box/round indexing) |
| Spec.ChaCha20 | 11 | 11 | Axiomatized functions |
| Spec.GaloisField | 8 | 0 | Structural (field element bounds) |
| Spec.DoubleRatchet | 7 | 0 | Mixed structural/KAT |
| Spec.Poly1305 | 7 | 1 | Structural + 1 axiomatized (bitwise_and) |
| Spec.HKDF | 6 | 0 | Structural (L62, L78, L80) + KAT (L223, L230, L237) |
| Spec.NoiseIK | 6 | 0 | Correctness properties |
| Spec.SHA256 | 5 | 0 | Structural (L186, L615) + KAT (L654, L670, L697) |
| Spec.HMAC | 5 | 0 | Structural (L44, L47) + KAT (L168, L192, L221) |
| Spec.X3DH | 5 | 0 | Correctness properties |
| Spec.SHA512 | 4 | 0 | Structural (L160, L169) + KAT (L521, L540) |
| Spec.PQXDH | 4 | 0 | Correctness properties |

## 2. Reduction Priority

### P0: GCM, HMAC, HKDF (highest assurance debt per assurance-matrix.md)

**Spec.GCM** (26 assumes) -- largest structural debt in the symmetric suite.
- L91, L100: append-length for counter block construction. Provable via `Seq.lemma_len_append`.
- L126, L162: block-processing index/length bounds. Need loop invariant lemmas.
- L198, L200, L252, L254: `length * 8 < pow2 64` bounds. Provable via `assert_norm` for bounded inputs or precondition strengthening.
- L206, L260, L330, L335: GHASH input divisibility. Need padding-alignment lemmas.
- L287-311: encrypt/decrypt roundtrip and tag-length properties. Need explicit correctness lemmas over the encrypt/decrypt pair.

**Spec.HMAC** (5 assumes) -- small count but security-critical.
- L44: `Seq.length (h key) <= block_size`. Needs hash-output-length lemma as precondition on `hash_fn` type (add refinement: `h` returns `seq UInt8.t` of known length).
- L47: `Seq.length key <= block_size`. Redundant -- the else branch already guarantees `not (length key > block_size)`, so this is provable by negation. Replace with `()`.
- L168, L192, L221: KAT assumes. Retain (validated by runtime RFC 4231 tests).

**Spec.HKDF** (6 assumes) -- structural obligations block output-length proof.
- L62: `counter >= 0 /\ counter < pow2 8`. Provable from the precondition `counter >= 1 /\ counter <= 256` via `assert_norm (256 < pow2 8)`.
- L78: `n >= 1 /\ n <= 255`. Provable from `len > 0 /\ len <= 255 * hash_len` by arithmetic lemma.
- L80: `Seq.length expanded >= len`. Needs inductive lemma over `expand_loop` showing output length = `(n - counter + 1) * hash_len`.
- L223, L230, L237: KAT assumes. Retain (validated by runtime RFC 5869 tests).

### P1: SHA-256, SHA-512 (core hash foundations)

**Spec.SHA256** (5 assumes)
- L186: `len * 8 < pow2 64`. Add as precondition or prove via `assert_norm` for bounded message lengths.
- L615: `Seq.length (pad msg) >= block_size`. Provable from padding construction -- pad always adds at least 9 bytes.
- L654, L670, L697: KAT assumes. Retain.

**Spec.SHA512** (4 assumes)
- L160: `len * 8 >= 0 /\ len * 8 < pow2 64`. Same pattern as SHA-256.
- L169: `Seq.length padded % block_size = 0`. Provable from padding logic.
- L521, L540: KAT assumes. Retain.

### P2: ChaCha20, Poly1305, AES-256 (symmetric ciphers)

**Spec.ChaCha20** (11 assume val) -- highest axiomatization debt. All 11 `assume val` declarations (L59-L176) must be replaced with actual function definitions: `qr_test`, `double_round`, `le_bytes_to_uint32`, `create_16`, `serialize_state`, `seq_map2`, `chacha20_encrypt`, `encrypt_decrypt_roundtrip`, `seq_of_hex`, `kat_block`, `kat_allzero`.

**Spec.Poly1305** (7 assume + 1 assume val) -- `bitwise_and` at L72 needs implementation over `nat`. Structural assumes are field-element bounds.

**Spec.AES256** (14 assumes) -- S-box indexing and round-key length. Mostly provable with explicit length lemmas over the key schedule.

### P3: X25519, Ed25519, ML-KEM-768 (asymmetric)

These carry the highest raw counts (32, 35, 15) but are dominated by field-arithmetic bounds that require careful multi-step proofs over modular arithmetic. Ed25519 L374 `assume val sha512` should import from Spec.SHA512 instead.

### P4: Protocols (X3DH, PQXDH, Double Ratchet, Noise IK)

Low counts (4-7 each), mostly correctness properties. Lowest priority -- these compose already-verified primitives.

## 3. Reduction Strategies

| Strategy | Applies to | Example |
|---|---|---|
| Replace with `assert_norm` | Small arithmetic bounds | HKDF L62: `assert_norm (256 < pow2 8)` |
| Strengthen preconditions | Hash output length | HMAC L44: refine `hash_fn` type to guarantee output length |
| Discharge via negation | Redundant branch guards | HMAC L47: else branch implies `length key <= block_size` |
| Add inductive lemma | Loop output length | HKDF L80: prove `expand_loop` output length by induction on `n - counter` |
| Add append/slice lemmas | Sequence construction | GCM L91: `Seq.lemma_len_append prefix new_ctr_bytes` |
| Implement axiomatized functions | `assume val` | ChaCha20: define `double_round`, `serialize_state`, etc. |
| Import cross-module | External hash dependency | Ed25519 L374: import `Spec.SHA512.sha512` |
| Retain intentionally | KAT vectors, security axioms | All KAT assumes; HMAC PRF assumption (L135) |

### Assumes to retain permanently

- All KAT assumes (~45 total): these assert equality of algorithm output with known-answer test vectors. Full symbolic normalization of SHA-256/512/GCM through the F* type checker is prohibitively expensive. Runtime KAT tests validate these independently.
- HMAC PRF assumption (L135-137): this is a computational security claim (Bellare-Canetti-Krawczyk 1996), not a proof obligation.

## 4. Impact on Assurance Matrix

| Current gap (assurance-matrix.md) | After P0 reduction | After P0+P1 |
|---|---|---|
| "KAT lemmas remain assumption-backed" (SHA-256) | No change (KATs retained) | No change |
| "Counter-byte and output-structure obligations" (HKDF) | Fully discharged | Discharged |
| "PRF/security lemmas assumption-heavy" (HMAC) | Structural assumes eliminated; PRF axiom retained; assurance-matrix wording correctable | Same |
| "GCM formal model uses important assumptions" (GCM) | Structural assumes eliminated; correctness properties proved | Same |
| "Padding divisibility partly trusted" (SHA-512) | No change | Discharged |
| Proof-Layer Matrix "Partial" entries for HMAC/HKDF/GCM | Upgradable to "Present" for bounds/shape | Same |

Completing P0 alone eliminates the top three items from assurance-matrix.md "Next Evidence Upgrades" item 1. The structural assume count drops from ~95 to ~58. The axiomatized function count remains at 13 until P2 (ChaCha20) is addressed.

### Net effect on total assume count

| Phase | Assumes eliminated | Remaining |
|---|---|---|
| Baseline | -- | 217 + 13 assume val |
| After P0 | ~20 structural + correctness | ~197 + 13 |
| After P1 | ~4 structural | ~193 + 13 |
| After P2 | ~22 structural + 12 assume val | ~171 + 1 |
| After P3 | ~67 structural + 1 assume val | ~104 + 0 |
| After P4 | ~14 correctness | ~90 (all KAT + security) |

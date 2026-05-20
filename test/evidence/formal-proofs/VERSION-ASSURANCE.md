# VERSION-ASSURANCE.md

Assurance delta between UmbraVOX releases.
Tracks what changed, what did NOT change, and how to verify.

---

## v0.1.8 Assurance Delta

**Theme:** Formal proof breakthroughs — sign_then_verify and encode_decode_round_trip proved

**What changed since v0.1.4:**
- sign_then_verify: PROVED (was the broadest assume val in the project)
- encode_decode_round_trip: PROVED (sqrt_ratio_correct isolated as narrow field-arithmetic fact)
- 11 Coq files, 415 Qed (up from 171): Ed25519GroupAssoc (64 instances), Ed25519SqrtRatio (3 concrete verifications), Ed25519GroupPartial (projective equivalence), Ed25519GroupUniversal (universal group-law proofs via GZnZ)
- ML-KEM-768: confirmed as REAL implementation (not stubs), sampleNTT fix
- AFL++ fuzzing harnesses: 3 harnesses (GCM, Ed25519, X25519) ready
- TUI local screenshots: 8/8 captured and validated (R1.4-6 complete)
- 36/36 differential suites, protocol self-consistency traces

**7 permanently irreducible crypto hardness assumptions:**
1. tag_forgery_ct_axiom (Poly1305 UF-CMA, Bernstein 2005)
2. hmac_non_fixpoint (HMAC-SHA256 PRF non-fixpoint, FIPS 198-1)
3. hmac_collision_resistance (HMAC-SHA256 collision resistance, FIPS 180-4)
4. distinct_messages_distinct_sigs (Ed25519 SHA-512 CR + DL, RFC 8032)
5. unlinkability (Stealth address DDH on Curve25519, Bernstein 2006)
6. vrf_strong_uniqueness (VRF DL hardness, RFC 9381)
7. vrf_collision_resistance (VRF hash+DL hardness, RFC 9381)

These can never be proved unconditionally in any proof system.

**What this version does NOT claim:**
- Does not prove universal Ed25519 group law associativity for all points (universal proofs cover GZnZ field structure, concrete instances cover 64 test cases)
- Does not claim constant-time behavior
- Does not claim protocol-level interop with libsignal

---

## v0.1.4 Assurance Delta

**Theme:** Multi-oracle cleanroom differential testing — runtime correspondence evidence

**What changed since v0.1.3:**
- 21 differential test suites: 9 primitive + 4 negative + 8 metamorphic, ALL PASS
- Primitives verified against official RFC/NIST vectors: SHA-256, SHA-512, SHA-3,
  HMAC-SHA-256, HKDF-SHA-256, X25519, Ed25519, AES-256-GCM, ChaCha20-Poly1305
- 18 negative tests: wrong-key, bitflip, truncated, wrong-nonce, wrong-aad
- 8 metamorphic properties: AEAD roundtrip, sign-verify, commutativity, determinism
- Security fix: X25519 input validation (rejected 31-byte keys)
- Constant fixes: Ed25519 KAT public key corrected in F* spec + JSON vectors,
  AES-GCM tag corrected (wrong NIST test case ID)
- Multi-oracle infrastructure: 9 oracle pins, supply chain security doc
- ASSURANCE-MATRIX updated with Differential and Negative columns

**What was NOT changed:**
- assume val count: 28 (unchanged from v0.1.3)
- admit() count: 0 (invariant)
- Coq Qed count: 171 (unchanged)
- No theorems weakened or removed

**What this version claims:**
- Reproducible, cleanroom runtime correspondence evidence for 9 cryptographic
  primitives against official test vectors
- Fail-closed behavior verified for 4 AEAD/signature primitives
- Self-consistency verified via 8 metamorphic properties

**What this version does NOT claim:**
- Does not claim differential testing proves formal runtime equivalence
- Does not claim side-channel safety (timing, cache, etc.)
- Does not claim protocol-level interop (libsignal oracle not yet built)
- Does not claim ML-KEM-768 correctness (stubs only)

**Verification:**
```
cabal test umbravox-test --test-options='differential-oracle'
make assurance-fast
```

---

## v0.1.3 Assurance Delta

**Theme:** Assurance hardening — reviewer-grade evidence framework

### What changed since v0.1.2

**New evidence artifacts:**
- `ASSURANCE-MATRIX.md`: per-module risk table (module, proof status, assume count, risk tier)
- `ASSUMPTION-GRAPH.md`: dependency clusters, 15 independent trust roots
- `REVIEWER-GUIDE.md`: reproduction steps, claims NOT made, scope limitations

**New automation:**
- `make assurance-fast`: 5 hygiene checks (runs in seconds)
- `make assurance`: full release-grade suite (runs in minutes)
- `check-assumption-ledger.sh`: 6 consistency checks (assume val counts, duplicates, orphans, format)
- `check-proof-hygiene.sh`: 4 audit hygiene checks (admit-free, assume accounting, Coq Admitted, AUDIT NOTE markers)
- `assurance-fast` wired into `make quality` gate

**Organizational changes:**
- `Ed25519GroupLaw.v` moved to `coq/draft/` (aspirational, not evidence)
- `ASSUMPTIONS.md` updated with 7 X25519 entries (30 total)
- HMAC/HKDF `AUDIT NOTE` markers standardized across all in-body assumes

### What was NOT changed

- `assume val` count: **30** (unchanged from v0.1.2)
- `admit()` count: **0** (invariant maintained)
- Coq `Qed` count: **153** (unchanged)
- No theorems weakened or removed
- No `Admitted` lemmas in Coq (0 across all versions)

### Verification commands

```
make assurance-fast    # hygiene checks (seconds)
make assurance         # full release-grade suite (minutes)
```

---

## v0.1.2 Assurance Delta (from v0.1.1)

**Theme:** Full crypto proof audit — hidden assumes surfaced, false theorems fixed

Audit of all F* specs and Coq proofs found:
- Hidden `assume val` directives not tracked in the assumption ledger
- `encode_decode_round_trip` was false as stated (missing `on_curve_ext` precondition) — fixed
- Misleading proof names across 12 F* specs — all renamed/corrected
- `assume val` count grew from 23 to 30 as hidden assumes were surfaced and registered
- Coq proofs expanded from 5 to 153 `Qed` (Ed25519Prime.v Pocklington certificate, Ed25519Field.v)
- Axiom registry accounting corrected; evidence harness SKIP tracking fixed
- Inventory consistency tests added (65 -> 67)

All issues documented in commit history (`v0.1.1..v0.1.2`).

---

## Baseline Counts

| Metric | v0.1.1 | v0.1.2 | v0.1.3 | v0.1.4 | v0.1.5 | v0.1.6 | v0.1.7 | v0.1.8 |
|--------|--------|--------|--------|--------|--------|--------|--------|--------|
| F* `admit()` | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| F* `assume val` | 23 | 30 | 28 | 28 | 28 | 28 | 30 | 30 |
| Coq `Qed` | 5 | 153 | 171 | 171 | 187 | 219 | 350 | 415 |
| Coq files | 1 | 3 | 4 | 4 | 5 | 6 | 9 | 11 |
| Coq `Admitted` (verified) | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| Infra tests | 65 | 67 | 67 | 67 | 67 | 67 | 67 | 67 |
| Assurance checks | -- | -- | 5/5 | 5/5 | 5/5 | 8/8 | 8/8 | 8/8 |
| Differential suites | -- | -- | -- | 21/21 | 22/22 | 22/22 | 36/36 | 36/36 |
| TUI screenshots | -- | -- | -- | -- | -- | -- | 8/8 | 8/8 |
| Proved theorems | -- | -- | -- | -- | -- | -- | sign_then_verify, encode_decode_round_trip | -- |
| Irreducible (permanent) | 7 | 7 | 7 | 7 | 7 | 7 | 7 | 7 |

---

## Reading this table

- **`admit()`**: Proof holes in F*. Must remain 0 in all releases.
- **`assume val`**: Unverified axioms in F*. Each is registered in `ASSUMPTIONS.md` with justification.
- **`Qed`**: Machine-checked Coq proofs. Number should only increase.
- **`Admitted`**: Unfinished Coq proofs accepted without proof. Must remain 0.
- **Infra tests**: Infrastructure and integration tests in `test/`.
- **Assurance checks**: Automated hygiene checks from `make assurance-fast`.

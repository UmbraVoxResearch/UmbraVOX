# VERSION-ASSURANCE.md

Assurance delta between UmbraVOX releases.
Tracks what changed, what did NOT change, and how to verify.

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

| Metric | v0.1.1 | v0.1.2 | v0.1.3 |
|--------|--------|--------|--------|
| F* `admit()` | 0 | 0 | 0 |
| F* `assume val` | 23 | 30 | 30 |
| Coq `Qed` | 5 | 153 | 153 |
| Coq `Admitted` (verified) | 0 | 0 | 0 |
| Infra tests | 65 | 67 | 67 |
| Assurance checks | -- | -- | 5/5 PASS |

---

## Reading this table

- **`admit()`**: Proof holes in F*. Must remain 0 in all releases.
- **`assume val`**: Unverified axioms in F*. Each is registered in `ASSUMPTIONS.md` with justification.
- **`Qed`**: Machine-checked Coq proofs. Number should only increase.
- **`Admitted`**: Unfinished Coq proofs accepted without proof. Must remain 0.
- **Infra tests**: Infrastructure and integration tests in `test/`.
- **Assurance checks**: Automated hygiene checks from `make assurance-fast`.

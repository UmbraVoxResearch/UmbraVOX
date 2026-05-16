<!-- SPDX-License-Identifier: Apache-2.0 -->

# MC/DC Condition Tables — Crypto Module Compound Guards

**Standard:** DO-178C DAL A (MC/DC)
**Date:** 2026-05-14
**Scope:** All compound Boolean guards (`&&`, `||`) in `src/UmbraVox/Crypto/`

---

## Guard 1: AES.hs — `rcon` bounds check

**File:** `src/UmbraVox/Crypto/AES.hs:120`
**Function:** `rcon :: Int -> Word32`
**Decision:** `i >= 1 && i <= length rconTable`
**Conditions:** 2

| Test | Cond A: `i >= 1` | Cond B: `i <= 10` | Decision | Independence |
|------|-------------------|--------------------|----------|--------------|
| T1: i=0  | F | T | F | A toggles decision (vs T3) |
| T2: i=11 | T | F | F | B toggles decision (vs T3) |
| T3: i=5  | T | T | T | baseline |
| T4: i=1  | T | T | T | boundary |
| T5: i=10 | T | T | T | boundary |

**Coverage:** Fully achievable with integer boundary values.
**Test:** AES test suite exercises `rcon` for all 10 round constants (i=1..10). Add edge tests for i=0 and i=11.

---

## Guard 2: Ed25519.hs — point recovery zero check

**File:** `src/UmbraVox/Crypto/Ed25519.hs:235`
**Function:** `recoverXForDecode :: Integer -> Bool -> Maybe Integer`
**Decision:** `u `mod` p == 0 && not xSign`
**Conditions:** 2

| Test | Cond A: `u mod p == 0` | Cond B: `not xSign` | Decision | Independence |
|------|------------------------|----------------------|----------|--------------|
| T1: u=0, xSign=False   | T | T | T | baseline |
| T2: u=0, xSign=True    | T | F | F | B toggles decision (vs T1) |
| T3: u=7, xSign=False   | F | T | F | A toggles decision (vs T1) |
| T4: u=7, xSign=True    | F | F | F | neither |

**Coverage:** Requires Ed25519 point where `u mod p == 0`. This corresponds to y-coordinates where `y^2 - 1 == 0 (mod p)`, i.e. y = +/-1. The identity point (0,1) triggers this.
**Test:** Ed25519 test suite includes identity point decompression. Add explicit test with y=1 and both sign bits.

---

## Guard 3: MLKEM.hs — rejection sampling (d1)

**File:** `src/UmbraVox/Crypto/MLKEM.hs:345`
**Function:** `sampleNTT` inner `rejection` loop
**Decision:** `d1 < _Q && n < 256`
**Conditions:** 2

| Test | Cond A: `d1 < 3329` | Cond B: `n < 256` | Decision | Independence |
|------|----------------------|--------------------|----------|--------------|
| T1: d1=100, n=0    | T | T | T | accept coefficient |
| T2: d1=3329, n=0   | F | T | F | A toggles (vs T1) |
| T3: d1=100, n=256  | T | F | F | B toggles (vs T1) |
| T4: d1=3329, n=256 | F | F | F | neither |

**Coverage:** T1-T2 are exercised by any ML-KEM key generation (SHAKE-128 stream produces both valid and rejected coefficients). T3 requires polynomial with exactly 256 coefficients filled — occurs at the end of every successful `sampleNTT` call.
**Test:** ML-KEM FIPS 203 test vectors exercise full key generation including rejection sampling.

---

## Guard 4: MLKEM.hs — rejection sampling (d2)

**File:** `src/UmbraVox/Crypto/MLKEM.hs:348`
**Function:** `sampleNTT` inner `rejection` loop
**Decision:** `d2 < _Q && n' < 256`
**Conditions:** 2

Identical structure to Guard 3 but for the second coefficient extracted per 3-byte group.

| Test | Cond A: `d2 < 3329` | Cond B: `n' < 256` | Decision | Independence |
|------|----------------------|---------------------|----------|--------------|
| T1: d2 valid, n'<256   | T | T | T | accept |
| T2: d2 >= 3329, n'<256 | F | T | F | A toggles |
| T3: d2 valid, n'=256   | T | F | F | B toggles |

**Coverage:** Same reasoning as Guard 3. Both guards are exercised by every `sampleNTT` call with sufficient SHAKE-128 output.
**Test:** Covered by ML-KEM FIPS 203 KAT vectors.

---

## Guard 5: Keccak.hs — sponge rate validation

**File:** `src/UmbraVox/Crypto/Keccak.hs:241`
**Function:** `sponge :: Int -> Word8 -> Int -> ByteString -> ByteString`
**Decision:** `rate <= 0 || rate >= 200`
**Conditions:** 2

| Test | Cond A: `rate <= 0` | Cond B: `rate >= 200` | Decision | Independence |
|------|---------------------|-----------------------|----------|--------------|
| T1: rate=-1  | T | F | T | A causes rejection |
| T2: rate=200 | F | T | T | B causes rejection |
| T3: rate=136 | F | F | F | valid rate (SHA-3-256) |
| T4: rate=0   | T | F | T | boundary |
| T5: rate=199 | F | F | F | boundary |

**Coverage:** T3 is exercised by every SHA-3/Keccak test. T1/T4 and T2 require explicit invalid-rate tests.
**Test:** Keccak test suite includes rate validation tests (M8.3.4). Add boundary tests for rate=0 and rate=200 if not already present.

---

## Summary

| Guard | File:Line | Conditions | MC/DC Pairs | Test Coverage |
|-------|-----------|------------|-------------|---------------|
| 1 | AES.hs:120 | 2 | i=0, i=11, i=5 | True path covered via key expansion; false path is defensive (private function) |
| 2 | Ed25519.hs:235 | 2 | identity point + sign bits | True path structurally rare; false path is defensive (private function) |
| 3 | MLKEM.hs:345 | 2 | rejection sampling | Covered by KAT vectors |
| 4 | MLKEM.hs:348 | 2 | rejection sampling | Covered by KAT vectors |
| 5 | Keccak.hs:241 | 2 | rate=-1, rate=200, rate=136 | True path structurally unreachable via public API; defensive guard (private function) |

**Total compound guards:** 5
**Total atomic conditions:** 10
**MC/DC test pairs needed:** 15 (3 per 2-condition decision)
**Already covered by existing tests:** Guards 3, 4 (ML-KEM KAT vectors)
**Defensive guards (structurally unreachable false paths):** Guards 1, 2, 5

## Defensive Guard Analysis

Guards 1, 2, and 5 are **private functions** not exported from their modules. Their
false-condition paths are structurally unreachable through the public API:

- **Guard 1 (AES rcon):** Key expansion calls `rcon (i `div` nk)` where `nk=8` for
  AES-256 and `i` ranges over the expansion schedule. The quotient is always 1..7,
  well within the valid range 1..10. The `otherwise` branch (return 0) exists as
  defense-in-depth but cannot be triggered via `aesEncrypt`/`aesDecrypt`.

- **Guard 2 (Ed25519 recoverXForDecode):** The condition `u mod p == 0` requires a
  specific y-coordinate (y=1, the identity point). This path is exercised by the
  identity point but is extremely rare in normal signature verification.
  `recoverXForDecode` is private to Ed25519.hs.

- **Guard 5 (Keccak sponge):** The rate parameter is hardcoded by each SHA-3 variant
  (sha3_256 uses rate=136, shake128 uses rate=168, etc.). All hardcoded rates are
  valid (8 < rate < 200, multiple of 8). The guard exists to catch programming
  errors in future variant additions.

Under DO-178C, defensive guards with structurally prevented false paths are
acceptable when documented. The guard code is retained for safety, and the
MC/DC analysis documents that the false-condition independence pairs cannot
be demonstrated through the public API without exposing private internals.

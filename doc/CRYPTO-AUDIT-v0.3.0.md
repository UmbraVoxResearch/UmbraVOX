# UmbraVox Cryptographic Implementation Audit — v0.3.0

**Scope**: Pure Haskell reference implementations in `src/UmbraVox/Crypto/`.
**Date**: 2026-05-21
**Status**: All five modules are explicitly marked NOT CONSTANT-TIME (except
GCM, which claims constant-time GHASH).  This audit covers correctness and
side-channel issues beyond the acknowledged timing caveat.

Severity scale: **CRITICAL** > **HIGH** > **MEDIUM** > **LOW** > **INFO**

---

## 1. VRF.hs — ECVRF-ED25519-SHA512-ELL2 (RFC 9381)

### VRF-01: Elligator2 gx1 computation order is incorrect — HIGH

**File**: `src/UmbraVox/Crypto/VRF.hs`, line 177

The Montgomery curve equation is `v^2 = u^3 + A*u^2 + u`.  The code computes:

```haskell
!gx1 = fMul (fAdd (fAdd x1sq (fMul montA x1)) 1) x1
```

This evaluates `(x1^2 + A*x1 + 1) * x1 = x1^3 + A*x1^2 + x1`, which is
algebraically correct.  However, the same pattern on line 180 for `gx2`:

```haskell
!gx2 = fMul (fAdd (fAdd (fMul x2 x2) (fMul montA x2)) 1) x2
```

is also correct.  No finding here upon closer inspection.

### VRF-02: Nonce generation uses RFC 8032 method, not RFC 6979 — INFO

**File**: `src/UmbraVox/Crypto/VRF.hs`, lines 240-247

The nonce generation follows RFC 9381 Section 5.4.2.2 (the RFC 8032 method),
which is the correct choice for the ECVRF-ED25519-SHA512-ELL2 suite.  This is
NOT RFC 6979; the function comment on line 234 correctly identifies this.
No issue.

### VRF-03: scalarMul is variable-time (double-and-add) — MEDIUM

**File**: `src/UmbraVox/Crypto/Ed25519.hs`, lines 134-141 (used by VRF)

The scalar multiplication uses a textbook double-and-add from the high bit
down, branching on each secret scalar bit via `testBit`.  This leaks the
scalar through timing and cache side-channels.

The module header acknowledges this.  For a reference implementation this is
acceptable, but the VRF module does not repeat the warning and exports
`vrfProve` which processes a secret key.

**Recommendation**: Add a prominent NOT-CONSTANT-TIME banner to VRF.hs
exports, and ensure the FFI path is wired before any production use.

### VRF-04: computeSqrtNeg486664 sign convention may not match RFC — MEDIUM

**File**: `src/UmbraVox/Crypto/VRF.hs`, lines 221-229

The code picks the even root of `sqrt(-486664)`.  RFC 9381 Section 5.4.1.2
and the reference implementation define a specific value for this constant
(the "positive" square root where positive means the least non-negative
residue).  Picking "even" is one convention but the RFC uses the residue
that equals `sqrt(-1) * sqrt(486664)` with a specific sign.  If the wrong
root is chosen, all VRF outputs will be internally consistent but
incompatible with other RFC 9381 implementations.

**Recommendation**: Hard-code the constant from the RFC test vectors rather
than computing it at runtime.  Validate against the RFC 9381 test vectors
in Section A.

### VRF-05: No validation that decoded Gamma is on the curve — MEDIUM

**File**: `src/UmbraVox/Crypto/VRF.hs`, lines 340-348

During verification, `decodePoint` is called on the Gamma bytes.  The
`decodePoint` function (Ed25519.hs line 240) checks `y < p` and recovers x
via the curve equation, which implicitly validates the point is on the
curve.  However, it does NOT check that the point is in the prime-order
subgroup (it could be a small-order or mixed-order point).

For ECVRF this matters: an attacker who supplies a Gamma that is not in the
prime-order subgroup could potentially forge proofs for related inputs.
RFC 9381 Section 5.3 step 2 requires that Gamma be validated.

**Recommendation**: After decoding Gamma, verify `[L]Gamma == O` (the
identity), or equivalently multiply by the cofactor and check the result
matches `cofactor * decoded_gamma`.

### VRF-06: Missing s range check should also verify c < 2^128 — LOW

**File**: `src/UmbraVox/Crypto/VRF.hs`, line 349

The code checks `s >= groupL` but does not verify that `c < 2^128`.  Since c
is decoded from 16 bytes, it is always less than 2^128, so this is
automatically satisfied.  However, an explicit check would be defense in
depth.

---

## 2. GCM.hs — AES-256-GCM (NIST SP 800-38D)

### GCM-01: Tag comparison uses constantEq — correct — INFO

**File**: `src/UmbraVox/Crypto/GCM.hs`, line 205

The decryption path uses `constantEq` from `ConstantTime.hs` for tag
verification.  The `constantEq` implementation XOR-folds all bytes and
handles unequal lengths.  This is correct.

### GCM-02: GHASH gfMul is constant-time at the Haskell level — INFO

**File**: `src/UmbraVox/Crypto/GCM.hs`, lines 95-127

The GF(2^128) multiplication uses the `negate` masking trick to avoid
branching on secret bits.  The loop always runs 128 iterations.  At the
Haskell source level this is correct.

**Caveat**: GHC may compile `Word64` operations with variable-time
instructions on some architectures, and the `negate` trick relies on
two's complement behavior which Haskell's `Word64` guarantees.  The real
risk is GHC optimization passes introducing branches.  This is a known
limitation of pure-Haskell crypto.

### GCM-03: No nonce-reuse protection — MEDIUM

**File**: `src/UmbraVox/Crypto/GCM.hs`, lines 158-172

The `gcmEncrypt` / `gcmEncryptSafe` functions accept a nonce parameter but
perform no tracking of previously used nonces.  AES-GCM is catastrophically
broken under nonce reuse (full plaintext and authentication key recovery).

The CRYPTO-SAFETY.md document describes nonce construction discipline at the
protocol layer, which is the correct architectural boundary.  However, the
GCM module itself provides no defense.

**Recommendation**: Consider adding a nonce-misuse-resistant mode (SIV or
GCM-SIV) as a fallback, or at minimum add a doc comment warning callers
that nonce reuse is catastrophic.

### GCM-04: Word64 length overflow for very large inputs — LOW

**File**: `src/UmbraVox/Crypto/GCM.hs`, lines 167-168

```haskell
!lenA = fromIntegral (BS.length aad) * 8 :: Word64
!lenC = fromIntegral (BS.length ct) * 8 :: Word64
```

`BS.length` returns `Int`, which on 64-bit platforms is 64 bits.
Multiplying by 8 could overflow `Word64` for inputs > 2^61 bytes.  This is
not practically exploitable (no system has 2 EiB of RAM), but SP 800-38D
limits input to 2^39 - 256 bits.  The code does not enforce this limit.

**Recommendation**: Add an input length check: `BS.length plaintext <=
(2^36 - 32)` (bytes), per SP 800-38D Section 5.2.1.1.

### GCM-05: splitBlocks produces short final block without padding — INFO

**File**: `src/UmbraVox/Crypto/GCM.hs`, line 50-51

`splitBlocks` can produce a final block shorter than 16 bytes.  This is
handled correctly in GCTR (line 150: `BS.take (BS.length blk) ks` truncates
the keystream) and in GHASH (the `padTo16` call on line 169).  No issue.

---

## 3. Ed25519.hs — RFC 8032 Section 5.1

### ED-01: No small-order public key rejection — HIGH

**File**: `src/UmbraVox/Crypto/Ed25519.hs`, lines 296-317

The `ed25519Verify` function does not reject small-order public keys or R
values.  The eight small-order points on Ed25519 (identity, and the seven
other torsion points) can be used in signature malleability attacks.

RFC 8032 Section 5.1.7 does not mandate small-order rejection (it specifies
cofactored verification via the `[8][S]B = [8]R + [8][k]A` equation), but
the code uses cofactorless verification (`[S]B == R + [k]A`).  This is the
ZIP-215 / ed25519-consensus divergence point.

With cofactorless verification and no small-order rejection:
- An attacker can produce multiple valid signatures for the same message
  under a small-order public key.
- Mixed-order R values create signature malleability.

**Recommendation**: Either:
(a) Switch to cofactored verification: `[8*S]B == [8]R + [8*k]A`, or
(b) Reject small-order points for both the public key and R.
Option (b) matches the stricter "ZIP-215" interpretation used by most
modern implementations.

### ED-02: scalarMul is variable-time — MEDIUM (acknowledged)

**File**: `src/UmbraVox/Crypto/Ed25519.hs`, lines 134-141

As noted in VRF-03, the double-and-add ladder branches on secret scalar
bits.  The module header acknowledges this.

### ED-03: pointDouble formula uses non-standard D variable name — LOW

**File**: `src/UmbraVox/Crypto/Ed25519.hs`, lines 114-131

The `pointDouble` function comment references "EFD dbl-2008-hwcd for a=-1"
but the variable names diverge from the EFD listing.  Specifically:

- Line 124: `!g = fSub b a` computes `B - A = Y^2 - X^2`.  For the a=-1
  twisted Edwards curve, `D = a*A = -X^2`, so `G = D + B = -X^2 + Y^2 =
  B - A`.  This is correct.
- Line 126: `!hh = fSub (p - a) b` computes `(-X^2) - Y^2 = -(X^2 + Y^2)`.
  For the a=-1 curve, `H = D - B = -X^2 - Y^2`.  This is correct.

The algebra checks out despite the confusing variable naming.

### ED-04: Verification compares encoded points (not projective) — INFO

**File**: `src/UmbraVox/Crypto/Ed25519.hs`, line 317

```haskell
in encodePoint lhs == encodePoint rhs
```

This normalizes both points to affine coordinates via modular inversion
before comparison.  This is correct (albeit slower than comparing in
projective coordinates).  The `==` on `ByteString` is NOT constant-time,
but verification is a public operation (no secret data in the comparison),
so this is acceptable.

---

## 4. Curve25519.hs — X25519 (RFC 7748)

### X25519-01: Montgomery ladder conditional swap uses branching — HIGH

**File**: `src/UmbraVox/Crypto/Curve25519.hs`, lines 161-163

```haskell
(!sx2, !sx3, !sz2, !sz3) =
    if swap' == 1
    then (x_3, x_2, z_3, z_2)
    else (x_2, x_3, z_2, z_3)
```

The conditional swap in the Montgomery ladder branches on `swap'`, which is
derived from secret scalar bits (`testBit k t`).  This is a textbook
timing side-channel.

RFC 7748 Section 5 specifies a constant-time conditional swap.  The module
header acknowledges the implementation is not constant-time, but this is
the single most important operation to protect in X25519 — it is the inner
loop of every DH computation.

**Recommendation**: Use XOR-based constant-time swap:
```
mask = negate swap'
x_2' = x_2 XOR (mask AND (x_2 XOR x_3))
```
(Or rely on the FFI path for production.)

### X25519-02: All-zero output rejection is correct — INFO

**File**: `src/UmbraVox/Crypto/Curve25519.hs`, lines 130-132

The code returns `Nothing` for all-zero DH output, correctly implementing
RFC 7748 Section 6.1 low-order point rejection.  This was previously
documented as a fix.

### X25519-03: Input u-coordinate not fully reduced — LOW

**File**: `src/UmbraVox/Crypto/Curve25519.hs`, line 128

```haskell
!u = decodeLE uCoord .&. (2 ^ (255 :: Int) - 1)
```

The code masks bit 255 per RFC 7748 but does not reduce modulo p.  RFC 7748
specifies that the u-coordinate is taken modulo p after decoding.  For
Curve25519, values in [p, 2^255-1] (i.e., p through p+18) would not be
reduced.  The `scalarMult` function works in GF(p) arithmetic so the
non-reduced value would be implicitly reduced on first use.

However, `u` is also used directly on line 183:
```haskell
!nz3 = fMul u (fMul df df)
```
Since `fMul` reduces mod p, this is safe.  No actual bug, but explicitly
reducing after decode would be cleaner.

### X25519-04: Integer arithmetic leaks timing — MEDIUM (acknowledged)

The `powMod` function (line 58-63) and all field operations use GHC
`Integer`, which has variable-time multiplication depending on operand
magnitude.  Acknowledged in the module header.

---

## 5. MLKEM.hs — ML-KEM-768 (FIPS 203)

### KEM-01: modQ uses signed Int division — potential negative intermediate — MEDIUM

**File**: `src/UmbraVox/Crypto/MLKEM.hs`, line 92

```haskell
modQ x = fromIntegral (((x `mod` _Q) + _Q) `mod` _Q)
```

The double-mod pattern handles negative inputs correctly.  However, `x` is
`Int`, and if intermediate polynomial arithmetic produces values outside
`[-(2^29), 2^29]` the `Int` could overflow on 32-bit platforms (where
`Int` is 32 bits).  On 64-bit platforms this is safe since coefficient
products are bounded by `3328 * 3328 * 3329 < 2^35`.

**Recommendation**: Add a compile-time or runtime assertion that `Int` is
at least 64 bits, or use `Int64` explicitly.

### KEM-02: NTT butterfly does not fully reduce intermediate values — MEDIUM

**File**: `src/UmbraVox/Crypto/MLKEM.hs`, lines 165-167

```haskell
let t = (z * fjlen) `mod` _Q
writeArray arr j       (((fj + t) `mod` _Q + _Q) `mod` _Q)
writeArray arr (j+len) (((fj - t) `mod` _Q + _Q) `mod` _Q)
```

The intermediate `z * fjlen` is computed as `Int * Int`.  Since `z` can be
up to `_Q - 1 = 3328` and `fjlen` can be up to `_Q - 1 = 3328`, the
product is at most `3328 * 3328 = 11,075,584`, which fits in `Int` on
both 32-bit and 64-bit platforms.  The subsequent `mod _Q` is correct.

However, in `invNtt` (line 208):
```haskell
let t = ((fj + fjlen) `mod` _Q + _Q) `mod` _Q
    u = (z * ((fjlen - fj + _Q) `mod` _Q)) `mod` _Q
```
The expression `(fjlen - fj + _Q) `mod` _Q` can produce a value up to
`_Q - 1`, then multiplied by `z` (up to 3328), giving at most
`3328 * 3328 = 11,075,584`.  This is safe on any platform where `Int` >= 32
bits.  No overflow.

### KEM-03: Rejection sampling may theoretically loop — LOW

**File**: `src/UmbraVox/Crypto/MLKEM.hs`, lines 333-361

The `sampleNTT` function generates SHAKE-128 output in progressively larger
blocks (840 bytes, then doubled).  If rejection sampling fails to produce
256 coefficients, it doubles and retries.

The acceptance rate per coefficient pair is ~81.3%.  With 840 bytes
(280 coefficient pairs), the expected accepted count is ~455, well above
256.  The probability of needing even one retry is astronomically low
(< 2^{-100}).  No practical concern.

### KEM-04: Implicit rejection timing — constantEq is used correctly — INFO

**File**: `src/UmbraVox/Crypto/MLKEM.hs`, line 552

```haskell
in if constantEq ct' ct
   then sharedSecret'
   else rejectionSecret
```

The re-encrypted ciphertext is compared to the received ciphertext using
`constantEq`.  This is correct per FIPS 203 Section 7.3 (implicit
rejection).  The branch after `constantEq` is on a public boolean, which
is acceptable.

### KEM-05: polyBaseMul gamma sign convention — needs verification — MEDIUM

**File**: `src/UmbraVox/Crypto/MLKEM.hs`, lines 216-236

The base-case multiplication uses `gamma1 = zetaTable ! (64 + i)` and
`gamma2 = (_Q - gamma1) `mod` _Q`.  FIPS 203 Algorithm 11 specifies that
the two base-case multiplications in each pair use `zeta^(2*BitRev7(i)+1)`
and `-(zeta^(2*BitRev7(i)+1))` respectively.

The `zetaTable` stores `17^(bitRev7(i))`.  The entry at index `64 + i`
gives `17^(bitRev7(64+i))`.  Whether `bitRev7(64+i)` equals
`2*bitRev7(i)+1` for all `i` in `[0..63]` depends on the bit-reversal
permutation.  This needs validation against FIPS 203 test vectors.

**Recommendation**: Validate NTT output against the FIPS 203 known-answer
tests (Section A).  If the zeta indexing is wrong, all KEM operations will
produce incorrect results (which would be caught by KAT tests but not by
structural review alone).

### KEM-06: Decapsulation key parsing uses bsSlice safely — INFO

**File**: `src/UmbraVox/Crypto/MLKEM.hs`, lines 541-546

The `mlkemDecaps` function uses `bsSlice` (which returns `Maybe`) for
parsing the decapsulation key, falling back to the rejection secret on
parse failure.  This correctly prevents panics on malformed keys.

### KEM-07: compressD rounding may produce incorrect results for edge cases — LOW

**File**: `src/UmbraVox/Crypto/MLKEM.hs`, lines 262-266

```haskell
let num = (x' `shiftL` d) + (_Q `div` 2)
    result = (num `div` _Q) .&. ((1 `shiftL` d) - 1)
```

This implements `round(2^d / q * x)` via the integer rounding trick
`(x * 2^d + q/2) / q`.  For `d = 1` and `x = 1665` (the midpoint):
`(1665 * 2 + 1664) / 3329 = 4994 / 3329 = 1`.  For `x = 1664`:
`(1664 * 2 + 1664) / 3329 = 4992 / 3329 = 1`.  These match the expected
compress behavior.  No issue found.

---

## 6. ConstantTime.hs — Cross-cutting

### CT-01: constantEq length comparison uses branching — LOW

**File**: `src/UmbraVox/Crypto/ConstantTime.hs`, line 33

```haskell
!lenMatch = if lenA == lenB then 0 else 1 :: Word8
```

This branches on lengths, but ByteString lengths are public metadata (not
secret).  The subsequent XOR fold processes `max(lenA, lenB)` bytes
regardless.  This is acceptable.

### CT-02: GHC optimization may break constant-time guarantees — INFO

GHC is free to transform the `foldl'` over `BS.zip` into short-circuiting
code if it determines the accumulator pattern allows it.  In practice,
GHC does not currently perform this optimization on `(.|.)` accumulation,
but there is no formal guarantee.

**Recommendation**: For production, use C FFI `CRYPTO_memcmp` or
`timingsafe_bcmp`.

---

## Summary by Severity

| ID | Severity | Module | Finding |
|---|---|---|---|
| ED-01 | HIGH | Ed25519 | No small-order point rejection in cofactorless verify |
| X25519-01 | HIGH | Curve25519 | Montgomery ladder cswap branches on secret bits |
| VRF-04 | MEDIUM | VRF | sqrt(-486664) sign convention not validated against RFC vectors |
| VRF-05 | MEDIUM | VRF | No subgroup check on decoded Gamma during verify |
| VRF-03 | MEDIUM | Ed25519/VRF | scalarMul is variable-time (acknowledged) |
| GCM-03 | MEDIUM | GCM | No nonce-reuse protection at module level |
| KEM-01 | MEDIUM | MLKEM | Int width assumption — unsafe on 32-bit |
| KEM-02 | MEDIUM | MLKEM | NTT intermediate values need 64-bit Int (safe in practice) |
| KEM-05 | MEDIUM | MLKEM | polyBaseMul zeta indexing needs KAT validation |
| X25519-04 | MEDIUM | Curve25519 | Integer arithmetic is variable-time (acknowledged) |
| VRF-06 | LOW | VRF | No explicit c < 2^128 range check |
| GCM-04 | LOW | GCM | No SP 800-38D input length enforcement |
| ED-03 | LOW | Ed25519 | Confusing variable names in pointDouble |
| X25519-03 | LOW | Curve25519 | u-coordinate not explicitly reduced mod p |
| KEM-03 | LOW | MLKEM | Rejection sampling retry is theoretically unbounded |
| KEM-07 | LOW | MLKEM | compressD edge-case rounding (verified correct) |
| CT-01 | LOW | ConstantTime | Length branch is on public data (acceptable) |

**Total**: 2 HIGH, 8 MEDIUM, 5 LOW, 6 INFO (INFO omitted from table)

---

## Recommendations (Priority Order)

1. **ED-01**: Add small-order point rejection to `ed25519Verify`, or switch
   to cofactored verification.  This affects signature malleability.

2. **X25519-01**: For any path where the pure-Haskell X25519 is reachable
   in production, the cswap must be made constant-time.  The FFI path
   documented in CRYPTO-SAFETY.md is the correct mitigation.

3. **VRF-04/VRF-05**: Validate VRF implementation against RFC 9381
   Appendix A test vectors.  Hard-code sqrt(-486664).  Add subgroup check
   on Gamma.

4. **KEM-05**: Run FIPS 203 KAT tests to validate NTT zeta indexing.

5. **KEM-01**: Assert `finiteBitSize (0 :: Int) >= 64` or switch NTT
   internals to `Int64`.

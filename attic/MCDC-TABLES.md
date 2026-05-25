# MC/DC Condition Tables — UmbraVox Crypto Modules

DO-178C DAL A compliance artifact.

MC/DC (Modified Condition/Decision Coverage) requires that each condition in a
compound decision independently affects the outcome.  Each table below lists one
MC/DC-adequate test set (the minimum set of rows where every condition is shown
to independently change the decision).

Coverage gaps note which table rows lacked a dedicated test before
`test/Test/Security/MCDC.hs` was added.

---

## How to read the tables

* **Decision column**: the overall Boolean result of the compound guard.
* **Independence column**: the condition that varies while all others are held
  fixed, demonstrating independent influence.
* A row pair that shares the same non-varying conditions but differs in exactly
  one condition and changes the decision constitutes an MC/DC pair.

---

## 1. `UmbraVox.Crypto.GCM` — `gcmEncrypt` (GCM.hs:161-162)

```haskell
gcmEncrypt key nonce aad plaintext
    | BS.length key   /= 32 = error "…"
    | BS.length nonce /= 12 = error "…"
```

These are sequential guards (short-circuit `||` semantics via Haskell pattern
matching on guards).  The compound decision is: **should an error be raised?**

Decision: `error` raised = `True`, function proceeds = `False`.

| Row | key_wrong (len≠32) | nonce_wrong (len≠12) | Decision  | Independent condition |
|-----|--------------------|----------------------|-----------|-----------------------|
| 1   | F                  | F                    | F (pass)  | —  (baseline)         |
| 2   | T                  | F                    | T (error) | key_wrong             |
| 3   | F                  | T                    | T (error) | nonce_wrong           |

MC/DC pairs: {1,2} for key_wrong; {1,3} for nonce_wrong.

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered | NIST TC16 encrypt, round-trip property |
| 2   | **Gap** | No test passes a wrong-length key to `gcmEncrypt` |
| 3   | **Gap** | No test passes a wrong-length nonce to `gcmEncrypt` |

---

## 2. `UmbraVox.Crypto.GCM` — `gcmDecrypt` (GCM.hs:183-185)

```haskell
gcmDecrypt key nonce aad ct tag
    | BS.length key   /= 32 = error "…"
    | BS.length nonce /= 12 = error "…"
    | BS.length tag   /= 16 = Nothing
```

Three independent guards; compound decision = {error / Nothing / proceed}.

| Row | key_wrong | nonce_wrong | tag_wrong | Decision           | Independent condition |
|-----|-----------|-------------|-----------|--------------------|-----------------------|
| 1   | F         | F           | F         | proceeds (Just/Nothing on auth) | — (baseline) |
| 2   | T         | F           | F         | error              | key_wrong             |
| 3   | F         | T           | F         | error              | nonce_wrong           |
| 4   | F         | F           | T         | Nothing            | tag_wrong             |

MC/DC pairs: {1,2}, {1,3}, {1,4}.

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered | NIST TC16 decrypt, tag tamper tests |
| 2   | **Gap** | No test passes a wrong-length key to `gcmDecrypt` |
| 3   | **Gap** | No test passes a wrong-length nonce to `gcmDecrypt` |
| 4   | Covered | `gcmDecrypt … tag` where `BS.length tag /= 16` tested implicitly by tag-tamper (length preserved); explicit short-tag path not tested → **Gap** |

---

## 3. `UmbraVox.Crypto.ChaChaPoly` — `chachaPolyEncrypt` (ChaChaPoly.hs:70-71)

```haskell
chachaPolyEncrypt key nonce aad plaintext
    | BS.length key   /= 32 = error "…"
    | BS.length nonce /= 12 = error "…"
```

Same structure as `gcmEncrypt`.

| Row | key_wrong | nonce_wrong | Decision  | Independent condition |
|-----|-----------|-------------|-----------|-----------------------|
| 1   | F         | F           | F (pass)  | —  (baseline)         |
| 2   | T         | F           | T (error) | key_wrong             |
| 3   | F         | T           | T (error) | nonce_wrong           |

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered | RFC 8439 round-trip tests |
| 2   | **Gap** | No dedicated test for wrong-length key |
| 3   | **Gap** | No dedicated test for wrong-length nonce |

---

## 4. `UmbraVox.Crypto.ChaChaPoly` — `chachaPolyDecrypt` (ChaChaPoly.hs:101-103)

```haskell
chachaPolyDecrypt key nonce aad ciphertext tag
    | BS.length key   /= 32 = error "…"
    | BS.length nonce /= 12 = error "…"
    | BS.length tag   /= 16 = Nothing
```

| Row | key_wrong | nonce_wrong | tag_wrong | Decision  | Independent condition |
|-----|-----------|-------------|-----------|-----------|----------------------|
| 1   | F         | F           | F         | proceed   | — (baseline)         |
| 2   | T         | F           | F         | error     | key_wrong            |
| 3   | F         | T           | F         | error     | nonce_wrong          |
| 4   | F         | F           | T         | Nothing   | tag_wrong            |

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered | Round-trip and tag verification tests |
| 2   | **Gap** | No dedicated bad-key-length test |
| 3   | **Gap** | No dedicated bad-nonce-length test |
| 4   | **Gap** | Short tag (len ≠ 16) path not explicitly tested |

---

## 5. `UmbraVox.Crypto.AES` — `aesEncrypt` (AES.hs:292-293)

```haskell
aesEncrypt key plaintext
    | BS.length key       /= 32 = error "…"
    | BS.length plaintext /= 16 = error "…"
```

| Row | key_wrong | pt_wrong | Decision  | Independent condition |
|-----|-----------|----------|-----------|-----------------------|
| 1   | F         | F        | F (pass)  | — (baseline)          |
| 2   | T         | F        | T (error) | key_wrong             |
| 3   | F         | T        | T (error) | pt_wrong              |

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered | FIPS-197 vectors, round-trip property |
| 2   | **Gap** | No test with wrong key length |
| 3   | **Gap** | No test with wrong plaintext length |

---

## 6. `UmbraVox.Crypto.AES` — `aesDecrypt` (AES.hs:303-304)

```haskell
aesDecrypt key ciphertext
    | BS.length key        /= 32 = error "…"
    | BS.length ciphertext /= 16 = error "…"
```

Identical structure to `aesEncrypt`.

| Row | key_wrong | ct_wrong | Decision  | Independent condition |
|-----|-----------|----------|-----------|-----------------------|
| 1   | F         | F        | F (pass)  | — (baseline)          |
| 2   | T         | F        | T (error) | key_wrong             |
| 3   | F         | T        | T (error) | ct_wrong              |

### Coverage gaps (pre-MCDC.hs)

All three gaps (rows 2, 3) were absent before `MCDC.hs`.

---

## 7. `UmbraVox.Crypto.AES` — `rcon` (AES.hs:120)

```haskell
rcon i
    | i >= 1 && i <= length rconTable = rconTable !! (i - 1)
    | otherwise                       = 0x00000000
```

Compound condition: `i >= 1 && i <= length rconTable` (length = 10).

| Row | i_ge_1 | i_le_10 | Decision  | Independent condition |
|-----|--------|---------|-----------|----------------------|
| 1   | T      | T       | T (valid) | — (baseline)         |
| 2   | F      | T       | F (zero)  | i_ge_1               |
| 3   | T      | F       | F (zero)  | i_le_10              |

MC/DC pairs: {1,2} for `i_ge_1`; {1,3} for `i_le_10`.

Note: `i = 0` makes `i_ge_1` false; `i = 11` makes `i_le_10` false;
`i = 5` satisfies both (valid path).

### Coverage gaps (pre-MCDC.hs)

The `rcon` function is internal to key expansion and is exercised indirectly
through `aesExpandKey` / `aesEncrypt`.  No test specifically targets the
boundary `i = 0`, `i = 10` (last valid), and `i = 11` (out-of-range).

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered (indirect) | Key expansion exercises valid i values |
| 2   | **Gap** | `i = 0` path (i_ge_1 = F) not explicitly tested |
| 3   | **Gap** | `i = 11` path (i_le_10 = F) not explicitly tested |

---

## 8. `UmbraVox.Crypto.Keccak` — `sponge` (Keccak.hs:239-241)

```haskell
sponge rate suffix outputLen msg
    | rate `mod` 8 /= 0         = error "…"
    | rate <= 0 || rate >= 200  = error "…"
```

The second guard is a disjunction: `rate <= 0 || rate >= 200`.

Decision (second guard fires) = `rate <= 0 || rate >= 200`.

| Row | rate_le_0 | rate_ge_200 | Decision (guard fires) | Independent condition |
|-----|-----------|-------------|------------------------|-----------------------|
| 1   | F         | F           | F (pass)               | — (baseline)          |
| 2   | T         | F           | T (error)              | rate_le_0             |
| 3   | F         | T           | T (error)              | rate_ge_200           |

Note: the first guard (`rate mod 8 /= 0`) must be satisfied (rate is a multiple
of 8) for the second guard to be reached.  The tables compose: we also need
rows where the first guard fires and the second is not reached.

First guard: `rate mod 8 /= 0`.

| Row | rate_mod8_ne_0 | Decision (guard 1) | Independent condition |
|-----|----------------|--------------------|-----------------------|
| A   | F              | F (falls through)  | — (baseline)          |
| B   | T              | T (error)          | rate_mod8_ne_0        |

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered | SHA3-256(""), SHAKE128 — valid rates exercised |
| 2   | **Gap** | `rate = -8` (multiple of 8, ≤ 0) — second guard, rate_le_0 = T path |
| 3   | **Gap** | `rate = 200` (multiple of 8, ≥ 200) — rate_ge_200 = T path |
| B   | **Gap** | `rate = 7` (not a multiple of 8) — first guard fires |

---

## 9. `UmbraVox.Crypto.MLKEM` — `rejection` sampling (MLKEM.hs:345,348)

```haskell
(n', acc') = if d1 < _Q && n < 256
             then (n + 1, fromIntegral d1 : acc)
             else (n, acc)
(n'', acc'') = if d2 < _Q && n' < 256
               then (n' + 1, fromIntegral d2 : acc')
               else (n', acc')
```

Two independent compound `&&` decisions, one for `d1` and one for `d2`.

**Decision d1**: accept sample `d1`.

| Row | d1_lt_Q | n_lt_256 | Decision (accept d1) | Independent condition |
|-----|---------|----------|---------------------|-----------------------|
| 1   | T       | T        | T (accept)          | — (baseline)          |
| 2   | F       | T        | F (reject)          | d1_lt_Q               |
| 3   | T       | F        | F (reject)          | n_lt_256              |

**Decision d2**: accept sample `d2` (uses updated `n'`).

| Row | d2_lt_Q | n_prime_lt_256 | Decision (accept d2) | Independent condition |
|-----|---------|----------------|---------------------|-----------------------|
| 1   | T       | T              | T (accept)          | — (baseline)          |
| 2   | F       | T              | F (reject)          | d2_lt_Q               |
| 3   | T       | F              | F (reject)          | n_prime_lt_256        |

`_Q = 3329`.

### Coverage gaps (pre-MCDC.hs)

These conditions are internal to `sampleNTT` and exercised only through
`mlkemKeyGen`/`mlkemEncaps`.  No test deliberately engineers the inputs to
hit `d >= _Q` or `n >= 256` on the first iteration.

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered (indirect) | Normal keygen exercises accept path |
| 2d1 | **Gap** | `d1 >= 3329` path not explicitly exercised |
| 3d1 | **Gap** | `n >= 256` path at d1-check not explicitly exercised |
| 2d2 | **Gap** | `d2 >= 3329` path not explicitly exercised |
| 3d2 | **Gap** | `n' >= 256` path at d2-check not explicitly exercised |

Note: `rejection` is not exported; the gaps are covered at the integration
level by constructing SHAKE-128 streams whose first 3-byte group produces
`d1 >= 3329` or an already-full accumulator.  The MCDC tests verify this
indirectly through `mlkemKeyGen`.

---

## 10. `UmbraVox.Crypto.Ed25519` — `recoverXForDecode` (Ed25519.hs:235)

```haskell
else if u `mod` p == 0 && not xSign
     then Just 0
     else Nothing
```

Compound condition: `u mod p == 0 && not xSign`.

| Row | u_mod_p_eq_0 | not_xSign | Decision (Just 0) | Independent condition |
|-----|--------------|-----------|-------------------|-----------------------|
| 1   | T            | T         | T (Just 0)        | — (baseline)          |
| 2   | F            | T         | F (Nothing)       | u_mod_p_eq_0          |
| 3   | T            | F         | F (Nothing)       | not_xSign             |

This condition is reached only when both `vx2 ≠ u mod p` and `vx2 ≠ (p - u) mod p`,
which only occurs when `u ≡ 0 (mod p)` (i.e. y = ±1 on the curve).

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | **Gap** | Decoding the identity point (y=1, positive x) |
| 2   | **Gap** | `u mod p != 0` path reaching this branch (unreachable in practice for valid field elements, but the guard exists) |
| 3   | **Gap** | Decoding y=1 with xSign=1 forces Nothing |

Note: Rows 1 and 3 are exercised by encoding and decoding the identity point
(y = 1). Row 2 is logically unreachable given valid field arithmetic.

---

## 11. `UmbraVox.Crypto.Ed25519` — `decodePoint` (Ed25519.hs:242-243)

```haskell
decodePoint bs
    | BS.length bs /= 32 = Nothing
```

Single condition (not compound), but included for completeness as a parameter
validation guard.

| Row | len_wrong | Decision  | Independent condition |
|-----|-----------|-----------|-----------------------|
| 1   | F         | proceed   | — (baseline)          |
| 2   | T         | Nothing   | len_wrong             |

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered | Verify/sign tests call `decodePoint` on 32-byte inputs |
| 2   | **Gap** | Wrong-length input to `decodePoint` not tested |

---

## 12. `UmbraVox.Crypto.KeyStore` — `loadIdentityKeyWithPassphrase` (KeyStore.hs:138)

```haskell
if BS.length blob /= blobLen
    then pure Nothing
```

Single condition.

| Row | blob_wrong_len | Decision  | Independent condition |
|-----|----------------|-----------|-----------------------|
| 1   | F              | proceed   | — (baseline)          |
| 2   | T              | Nothing   | blob_wrong_len        |

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered | KeyStore round-trip tests |
| 2   | **Gap** | Truncated-blob path (156 → short) not explicitly tested |

---

## 13. `UmbraVox.Crypto.Export` — `decryptExport` (Export.hs:102)

```haskell
decryptExport password blob
    | BS.length blob < headerLen + tagLen = Nothing
```

Single condition (`headerLen + tagLen = 60`).

| Row | blob_too_short | Decision  | Independent condition |
|-----|----------------|-----------|-----------------------|
| 1   | F              | proceed   | — (baseline)          |
| 2   | T              | Nothing   | blob_too_short        |

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered | Export round-trip tests |
| 2   | **Gap** | Short blob (< 60 bytes) path not tested |

---

## 14. `UmbraVox.Network.Noise.Handshake` — `decryptWithKey` (Handshake.hs:329)

```haskell
decryptWithKey k h cipherMac
    | BS.length cipherMac < hsHmacLen = Nothing
```

Single condition (`hsHmacLen = 32`).

| Row | too_short | Decision  | Independent condition |
|-----|-----------|-----------|----------------------|
| 1   | F         | proceed   | — (baseline)         |
| 2   | T         | Nothing   | too_short            |

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered | Noise handshake tests |
| 2   | **Gap** | `cipherMac` shorter than 32 bytes not explicitly tested |

---

## 15. `UmbraVox.Network.Noise.Handshake` — `recvFrame` (Handshake.hs:357-365)

```haskell
if BS.length lenBS < 4
    then pure Nothing
    else …
        if len >= maxFrameSize
            then pure Nothing
            else …
                if BS.length payload < fromIntegral len
                    then pure Nothing
                    else pure (Just payload)
```

Three sequential single conditions forming nested `if` structure.

| Row | hdr_short | len_too_big | payload_short | Decision  | Independent condition |
|-----|-----------|-------------|---------------|-----------|-----------------------|
| 1   | F         | F           | F             | Just payload | — (baseline)       |
| 2   | T         | —           | —             | Nothing   | hdr_short             |
| 3   | F         | T           | —             | Nothing   | len_too_big           |
| 4   | F         | F           | T             | Nothing   | payload_short         |

MC/DC pairs: {1,2}, {1,3}, {1,4}.

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered | Noise handshake round-trip |
| 2   | **Gap** | < 4 bytes for length prefix not tested |
| 3   | **Gap** | `len >= maxFrameSize` path not tested |
| 4   | **Gap** | Partial payload (less than declared length) not tested |

---

## 16. `UmbraVox.Network.Noise.Handshake` — `initHash` (Handshake.hs:277-279)

```haskell
initHash
    | BS.length protocolName <= 32 = protocolName <> BS.replicate …
    | otherwise = sha256 protocolName
```

Single condition; `protocolName` is a compile-time constant (32 bytes).

| Row | name_le_32 | Decision  | Independent condition |
|-----|------------|-----------|-----------------------|
| 1   | T          | pad path  | — (baseline)          |
| 2   | F          | hash path | name_le_32            |

### Coverage gaps (pre-MCDC.hs)

| Row | Status  | Notes |
|-----|---------|-------|
| 1   | Covered | Handshake tests exercise `initHash` via the constant-name path |
| 2   | **Gap** | The `sha256` branch is never reached at runtime because `protocolName` is 32 bytes; logic dead code; covered by structural check only |

---

## Summary of gaps addressed by `test/Test/Security/MCDC.hs`

| ID   | Module              | Condition                          | Row(s) added |
|------|---------------------|------------------------------------|-------------|
| GE1  | GCM                 | `gcmEncrypt` key len /= 32         | Row 2       |
| GE2  | GCM                 | `gcmEncrypt` nonce len /= 12       | Row 3       |
| GD1  | GCM                 | `gcmDecrypt` key len /= 32         | Row 2       |
| GD2  | GCM                 | `gcmDecrypt` nonce len /= 12       | Row 3       |
| GD3  | GCM                 | `gcmDecrypt` tag len /= 16         | Row 4       |
| CE1  | ChaChaPoly          | `chachaPolyEncrypt` key len /= 32  | Row 2       |
| CE2  | ChaChaPoly          | `chachaPolyEncrypt` nonce len /= 12 | Row 3      |
| CD1  | ChaChaPoly          | `chachaPolyDecrypt` key len /= 32  | Row 2       |
| CD2  | ChaChaPoly          | `chachaPolyDecrypt` nonce len /= 12 | Row 3      |
| CD3  | ChaChaPoly          | `chachaPolyDecrypt` tag len /= 16  | Row 4       |
| AE1  | AES                 | `aesEncrypt` key len /= 32         | Row 2       |
| AE2  | AES                 | `aesEncrypt` pt len /= 16          | Row 3       |
| AD1  | AES                 | `aesDecrypt` key len /= 32         | Row 2       |
| AD2  | AES                 | `aesDecrypt` ct len /= 16          | Row 3       |
| EX1  | Export              | `decryptExport` blob too short     | Row 2       |
| DP1  | Ed25519             | `decodePoint` len /= 32            | Row 2       |
| KS1  | KeyStore            | blob len /= blobLen                | Row 2       |
| KK1  | Keccak              | `sponge` rate mod 8 /= 0           | Row B       |
| KK2  | Keccak              | `sponge` rate <= 0                 | Row 2       |
| KK3  | Keccak              | `sponge` rate >= 200               | Row 3       |
| DW1  | Noise/Handshake     | `decryptWithKey` cipherMac < 32    | Row 2       |

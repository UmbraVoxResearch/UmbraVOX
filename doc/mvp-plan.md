# UmbraVOX MVP Plan: Codegen Pipeline → P2P Encrypted Messaging

## Status: COMPLETE (2026-04-23)

All three MVP phases are complete. Two clients can exchange post-quantum encrypted messages over TCP with a split-pane TUI, mDNS discovery, peer exchange, and Anthony DB persistence.

- 238 tests across 23 suites (all passing)
- 17 F* formal specifications (all verified)
- 10 .spec files generating 30 Haskell + C + FFI outputs
- Modular transport: TCP, Loopback (functional); UDP, Signal, XMPP, Discord, Matrix, Blockchain (stubs)
- Encrypted identity exports with BIP39 passphrases (full 2048-word wordlist)

## Context

All P0 security items are complete (CSPRNG, PQXDH, stealth addresses, nonce counter, legal notices). The crypto layer is solid. The codegen pipeline, P2P networking, and TUI/discovery layers are all functional.

---

## Phase A: Complete .spec → C Codegen Pipeline (DONE)

All 10 .spec files written. CryptoGen emitters (Haskell, C, FFI) and TestGen are functional. Pipeline generates 30 outputs (10 Haskell + 10 C + 10 FFI).

### A1. Complete CryptoGen — .spec → Haskell + C + FFI

**File**: `codegen/CryptoGen.hs` (204 lines, parser works, emitters are TODO)

Implement three emitters from the parsed .spec AST:

1. **Haskell emitter** (`processSpec` → `emitHaskell`)
   - Generate module header with REQ annotations
   - Emit function signatures from Params
   - Emit operation steps as Haskell expressions (assignments, conditionals, bitwise ops)
   - Handle: XOR, OR, AND, RotR, RotL, ShiftR, ShiftL, +mod, -mod, *mod, NOT, Index

2. **C emitter** (`processSpec` → `emitC`)
   - Generate C source with `#include <stdint.h>`
   - Emit constant-time operations: no branches on secret data
   - Use `uint32_t`/`uint64_t` types from params
   - Emit table lookups as constant arrays
   - Add `__attribute__((noinline))` for timing isolation

3. **FFI emitter** (`processSpec` → `emitFFI`)
   - Generate Haskell `foreign import ccall` declarations
   - Generate C wrapper functions matching Haskell calling convention
   - ByteString ↔ C pointer marshalling

### A2. Write .spec Files for All Primitives

Create spec files in `codegen/Specs/`:

| File | Standard | Priority |
|------|----------|----------|
| `SHA256.spec` | FIPS 180-4 | HIGH — simplest, validates pipeline |
| `SHA512.spec` | FIPS 180-4 | HIGH |
| `AES256.spec` | FIPS 197 | HIGH — S-box, KeyExpansion, rounds |
| `ChaCha20.spec` | RFC 8439 | MEDIUM |
| `Poly1305.spec` | RFC 8439 §2.5 | MEDIUM — needed for Noise_IK |
| `X25519.spec` | RFC 7748 | MEDIUM |
| `HMAC.spec` | RFC 2104 | LOW — wraps hash |
| `HKDF.spec` | RFC 5869 | LOW — wraps HMAC |
| `Keccak.spec` | FIPS 202 | MEDIUM |
| `MLKEM768.spec` | FIPS 203 | LOW — complex, defer |

### A3. Complete TestGen — Generate Test Harnesses

**File**: `codegen/TestGen.hs` (51 lines, skeleton only)

- Embed NIST CAVP / RFC KAT vectors per .spec
- Generate edge case tests
- Generate equivalence tests (pure Haskell == generated C via FFI)

### A4. Validate Pipeline End-to-End

1. `cabal run codegen` processes `codegen/Specs/SHA256.spec`
2. Produces `src/UmbraVox/Crypto/Generated/SHA256.hs` + `cbits/sha256.c` + `src/UmbraVox/Crypto/FFI/SHA256.hs`
3. `cabal build` compiles generated code
4. `cabal test` runs generated tests — NIST KAT vectors pass for both pure and FFI paths
5. Equivalence: 10,000 random inputs produce bitwise-identical output

---

## Phase B: P2P MVP — Two Clients, Encrypted TCP (DONE)

### B1. Poly1305 MAC (`src/UmbraVox/Crypto/Poly1305.hs`) — NEW
- RFC 8439 §2.5: one-time authenticator over GF(2^130-5)
- ~100 lines: clamp r, multiply, accumulate 16-byte blocks, finalize
- KAT vector from RFC 8439 §2.5.2

### B2. TCP Transport (`src/UmbraVox/Network/Transport.hs`)
- Simple TCP client/server using `network` package
- `listen`, `connect`, `send`, `recv`, `close`
- ~60 lines

### B3. Noise_IK Handshake (`src/UmbraVox/Network/Noise.hs`)
- Noise_IK pattern using X25519 + ChaChaPoly1305
- Prologue: `"UmbraVox_v1"`
- Output: two CipherState for encrypted transport
- ~150 lines

### B4. Simple Serialization (`src/UmbraVox/Protocol/CBOR.hs`)
- For MVP: length-prefixed binary encoding
- ~30 lines

### B5. Chat Session (`src/UmbraVox/Chat/Session.hs`)
- Wrapper: X3DH handshake → DoubleRatchet encrypt/decrypt
- ~50 lines

### B6. Chat Message (`src/UmbraVox/Chat/Message.hs`)
- Chunking (MVP: single chunk)
- ~20 lines

### B7. CLI Entry Point (`app/Main.hs`)
- `umbravox listen <port>` / `umbravox connect <host> <port>`
- Handshake → message loop (stdin → encrypt → send; recv → decrypt → stdout)
- ~80 lines

---

## Implementation Order

```
Phase A (codegen):
  A1. CryptoGen emitters (Haskell, C, FFI)     ← biggest piece
  A2. SHA256.spec (validate pipeline)
  A3. TestGen (KAT + equivalence)
  A4. Remaining .spec files
  
Phase B (P2P MVP):
  B1. Poly1305                    ← parallel with A
  B2. Transport (TCP)
  B3. Noise_IK (deps: B1, B2)
  B4. CBOR (simple)
  B5. Chat/Session
  B6. Chat/Message
  B7. Main.hs
```

## Verification (ALL GATES PASSED)

### Phase A gate: PASSED
- `cabal run codegen` generates Haskell + C + FFI for all 10 primitives
- Generated tests pass NIST KAT vectors
- Equivalence: pure == FFI for 10,000 random inputs

### Phase B gate: PASSED
- Two terminals exchange encrypted messages over TCP
- Noise_IK handshake + Signal Double Ratchet + ML-KEM-768 PQ wrapper

### Phase C gate: PASSED
- TUI with MC-style split pane, resize handling, 11% border padding
- mDNS/DNS-SD LAN discovery + peer exchange
- Anthony DB persistence for peers/settings/conversations
- BIP39 encrypted identity exports

## Files to Create/Modify

**Phase A:**
- `codegen/CryptoGen.hs` — implement emitters
- `codegen/TestGen.hs` — implement test generation
- `codegen/Specs/SHA256.spec` — first spec file
- `codegen/Specs/*.spec` — remaining primitives
- `UmbraVox.cabal` — add Generated/ and FFI/ modules, cbits

**Phase B:**
- NEW: `src/UmbraVox/Crypto/Poly1305.hs`
- MODIFY: `src/UmbraVox/Network/Transport.hs`
- MODIFY: `src/UmbraVox/Network/Noise.hs`
- MODIFY: `src/UmbraVox/Protocol/CBOR.hs`
- MODIFY: `src/UmbraVox/Chat/Session.hs`
- MODIFY: `src/UmbraVox/Chat/Message.hs`
- MODIFY: `app/Main.hs`
- MODIFY: `UmbraVox.cabal`

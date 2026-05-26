# csrc/fiat — Verified Field Arithmetic (fiat-crypto)

This directory contains vendored, formally verified, constant-time C from the
[fiat-crypto](https://github.com/mit-plv/fiat-crypto) project (MIT License).

## What fiat-crypto provides

fiat-crypto generates field arithmetic implementations from machine-checked
proofs in Coq. The extracted C output is standalone (no dependencies beyond
`<stdint.h>`) and has been formally verified to be:

- **Functionally correct** — field axioms proved in Coq
- **Constant-time** — no data-dependent branches or memory accesses
- **Portable** — pure C99, no intrinsics, no platform assumptions

The same C output is used by BoringSSL, the Go standard library (`crypto/internal/edwards25519/field`), and the Linux kernel (`lib/crypto/curve25519-hacl.c`).

## Files to vendor

| File | Description |
|------|-------------|
| `fiat_25519_64.c` | 64-bit GF(2^255-19) field arithmetic (Curve25519 / Ed25519 base field) |
| `fiat_25519_64.h` | Corresponding header |

Alternative name used in some fiat-crypto releases: `fiat_curve25519_64.c` /
`fiat_curve25519_64.h`. Check the upstream tag being vendored; both names refer
to the same field.

If targeting 32-bit platforms (illumos/SmartOS 32-bit, older BSDs), also vendor:

| File | Description |
|------|-------------|
| `fiat_25519_32.c` | 32-bit GF(2^255-19) field arithmetic |
| `fiat_25519_32.h` | Corresponding header |

## Operations provided

The vendored files expose the following field element operations over GF(2^255-19):

| C function | Description |
|------------|-------------|
| `fiat_25519_mul` | Field multiplication: `out = a * b mod p` |
| `fiat_25519_square` | Field squaring: `out = a^2 mod p` |
| `fiat_25519_add` | Field addition: `out = a + b mod p` |
| `fiat_25519_sub` | Field subtraction: `out = a - b mod p` |
| `fiat_25519_opp` | Field negation: `out = -a mod p` |
| `fiat_25519_to_bytes` | Canonical serialization to 32-byte little-endian |
| `fiat_25519_from_bytes` | Deserialization from 32-byte little-endian |
| `fiat_25519_carry` | Carry propagation (used internally after add/sub) |
| `fiat_25519_selectznz` | Constant-time conditional select |

These are sufficient to implement the full X25519 scalar multiplication and
Ed25519 sign/verify group law. The Haskell FFI bridge in `ffi_bridge.h` maps
these names to the `umbravox_fe_*` namespace expected by the rest of this
codebase.

## Upstream source

- Repository: https://github.com/mit-plv/fiat-crypto
- C output directory: `fiat-c/src/`
- License: MIT (see upstream `LICENSE` file; reproduce in this directory when vendoring)

See `VENDORING.md` for exact fetch instructions.

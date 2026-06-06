# csrc/fiat — fiat-crypto Verified Field Arithmetic (Third-Party, INTERIM Production)

## Status

fiat-crypto serves as the INTERIM production implementation for Ed25519 and X25519
field arithmetic, until UmbraVOX's own KaRaMeL-extracted C (from our F* Low* specs)
is ready.

**Intended final state**: Our own formally-extracted C in `csrc/extracted/` will replace
fiat-crypto in production. fiat-crypto will then move to `contrib/fiat-oracle/` and serve
as a differential oracle.

**Do not add production callers** to fiat-crypto directly. All callers go through
`src/UmbraVox/Crypto/Generated/FFI/<prim>.hs` which CryptoGen generates to call the
appropriate bridge function.

## Third-party provenance

fiat-crypto is developed by MIT PLV. It is NOT our code.
- Upstream: https://github.com/mit-plv/fiat-crypto
- C output directory: `fiat-c/src/`
- License: MIT
- Formal basis: field axioms proved in Coq, C extracted automatically

The same C output is used by BoringSSL, the Go standard library
(`crypto/internal/edwards25519/field`), and the Linux kernel
(`lib/crypto/curve25519-hacl.c`).

## Primitives covered (interim production)

The following primitives use fiat-crypto C as interim production:
- Ed25519 (field arithmetic over GF(2^255-19) for sign/verify)
- X25519 (scalar multiplication over Curve25519)

[SHA-256, SHA-512, ChaCha20, Poly1305, Keccak/SHA-3, HMAC, HKDF use csrc/hacl/ instead]

## Differential oracle role

fiat-crypto output is compared against:
1. CryptoGen-generated C (csrc/generated/<prim>.c)
2. Haskell reference implementation (src/UmbraVox/Crypto/<prim>.hs)

Any divergence between the three is a bug.

## Migration plan

See doc/IMPLEMENTATION-PLAN.md M36B for the KaRaMeL extraction plan. As each primitive
gets our own extracted C, its fiat-crypto files will move to contrib/fiat-oracle/.

## What fiat-crypto provides

fiat-crypto generates field arithmetic implementations from machine-checked
proofs in Coq. The extracted C output is standalone (no dependencies beyond
`<stdint.h>`) and has been formally verified to be:

- **Functionally correct** — field axioms proved in Coq
- **Constant-time** — no data-dependent branches or memory accesses
- **Portable** — pure C99, no intrinsics, no platform assumptions

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

See `VENDORING.md` for exact fetch instructions.

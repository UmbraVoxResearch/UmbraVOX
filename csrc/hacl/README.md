# csrc/hacl — HACL* Formally Verified C (Third-Party, INTERIM Production)

## Status

HACL* serves as the INTERIM production implementation for the 8 cryptographic primitives
listed below, until UmbraVOX's own KaRaMeL-extracted C (from our F* Low* specs) is ready.

**Intended final state**: Our own formally-extracted C in `csrc/extracted/` will replace
HACL* in production. HACL* will then move to `contrib/hacl-oracle/` and serve as a
differential oracle.

**Do not add production callers** to HACL* directly. All callers go through
`src/UmbraVox/Crypto/Generated/FFI/<prim>.hs` which CryptoGen generates to call the
appropriate bridge function.

## Third-party provenance

HACL* is developed by Microsoft Research and Inria. It is NOT our code.
- Upstream: https://github.com/hacl-star/hacl-star
- Commit: 504c298
- License: MIT / Apache-2.0
- Formal basis: F* + Low* specifications extracted via KaRaMeL by the HACL* team

## Primitives covered (interim production)

The following primitives use HACL* C as interim production, wired through CryptoGen FFI:
- SHA-256 (bridge_sha256.c → Hacl_Hash_SHA2_hash_256)
- SHA-512 (bridge_sha512.c → Hacl_Hash_SHA2_hash_512)
- ChaCha20 (bridge_chacha20.c → Hacl_Chacha20_*)
- Poly1305 (bridge_poly1305.c → Hacl_MAC_Poly1305_*)
- Keccak/SHA-3 (bridge_keccak.c → Hacl_Hash_SHA3_*)
- HMAC (bridge_hmac.c → Hacl_HMAC_*)
- HKDF (bridge_hkdf.c → Hacl_HKDF_*)

[Ed25519, X25519 use csrc/fiat/ bridge instead]

## Differential oracle role

HACL* output is compared against:
1. CryptoGen-generated C (csrc/generated/<prim>.c)
2. Haskell reference implementation (src/UmbraVox/Crypto/<prim>.hs)

Any divergence between the three is a bug.

## Migration plan

See doc/IMPLEMENTATION-PLAN.md M36B for the KaRaMeL extraction plan. As each primitive
gets our own extracted C, its HACL* files will move to contrib/hacl-oracle/.

## What HACL* Is

HACL* is written in Low* (a subset of F*) and extracted to portable C via the
KaRaMeL compiler. The extraction preserves machine-checked proofs of:

- Functional correctness (output matches the F* spec on all inputs)
- Memory safety (no buffer overflows, no use-after-free)
- Constant-time execution (no secret-dependent branches or memory accesses)

The extracted C files are ordinary C with no runtime dependency on F* or
KaRaMeL. They compile with any standard C compiler.

## Files to Vendor

The following files must be fetched from the upstream distribution and placed
here. See `VENDORING.md` for the exact fetch procedure.

### Algorithm sources

| File | Algorithm |
|---|---|
| `Hacl_SHA2_256.c` / `Hacl_SHA2_256.h` | SHA-256 |
| `Hacl_SHA2_512.c` / `Hacl_SHA2_512.h` | SHA-512 |
| `Hacl_Chacha20.c` / `Hacl_Chacha20.h` | ChaCha20 |
| `Hacl_Poly1305_32.c` / `Hacl_Poly1305_32.h` | Poly1305 (32-bit limbs) |
| `Hacl_SHA3.c` / `Hacl_SHA3.h` | Keccak / SHA-3 / SHAKE |
| `Hacl_HMAC.c` / `Hacl_HMAC.h` | HMAC (generic over hash) |
| `Hacl_HKDF.c` / `Hacl_HKDF.h` | HKDF (over HMAC) |

Coverage note: AES-256, AES-GCM, and ML-KEM-768 have no HACL* C coverage (see
`doc/hacl-evaluation.md` §2 for the structural reasons).

### Shared runtime headers

These headers are required by all algorithm sources above and must accompany
them:

| Path | Purpose |
|---|---|
| `Hacl_Krmllib.h` | KaRaMeL runtime (integer ops, endianness) |
| `krml/FStar_UInt128.h` | 128-bit integer support |
| `krml/FStar_UInt_8_16_32_64.h` | Fixed-width integer helpers |
| `krml/internal/target.h` | Compiler portability macros |
| `krml/lowstar_ignore.h` | `KRML_HOST_IGNORE` and related macros |
| `lib/memfunctions.h` | `memcpy`/`memset` wrappers |

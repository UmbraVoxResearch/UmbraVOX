# HACL* Vendored C Sources

This directory will contain verified C source files extracted from
[HACL*](https://github.com/cryspen/hacl-packages) (the Cryspen-maintained
distribution of HACL*), a formally verified cryptographic library.

## What HACL* Is

HACL* is written in Low* (a subset of F*) and extracted to portable C via the
KaRaMeL compiler. The extraction preserves machine-checked proofs of:

- Functional correctness (output matches the F* spec on all inputs)
- Memory safety (no buffer overflows, no use-after-free)
- Constant-time execution (no secret-dependent branches or memory accesses)

The extracted C files are ordinary C with no runtime dependency on F* or
KaRaMeL. They compile with any standard C compiler.

## Role in UmbraVOX

These sources serve as **comparison oracle** for differential testing (Option B
from `doc/hacl-evaluation.md`). They are NOT the active production
implementation — the codegen C path (`csrc/generated/`) remains primary.

The three-way comparison used in tests:
```
Haskell oracle  vs  codegen C (active)  vs  HACL* C (verified oracle)
```

Any disagreement among the three is reported as a test failure. Because HACL*
has a machine-checked F*-to-C proof chain, agreement with HACL* empirically
links the codegen C to a formally verified implementation.

## License

HACL* is MIT licensed. See `VENDORING.md` for the exact upstream commit and
file manifest. All files in this directory retain their original MIT license
headers.

## Bridge Files (Already Present)

The following thin C wrapper files are already present in this directory. They
adapt HACL* entry points to the function signatures expected by the Haskell
FFI layer. They will be compiled once the corresponding HACL* C sources are
vendored alongside them.

| File | Wraps | FFI module |
|---|---|---|
| `bridge_sha256.c` | `Hacl_SHA2_256_hash` | `UmbraVox.Crypto.Generated.FFI.SHA256` |
| `bridge_sha512.c` | `Hacl_SHA2_512_hash` | `UmbraVox.Crypto.Generated.FFI.SHA512` |
| `bridge_chacha20.c` | `Hacl_Chacha20_chacha20_{encrypt,decrypt}` | `UmbraVox.Crypto.Generated.FFI.ChaCha20` |

Each bridge also exports a `*_link_probe` symbol used by the Haskell FFI to
confirm that the compilation unit was linked in. See the file headers for
details.

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

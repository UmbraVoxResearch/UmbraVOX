# csrc/extracted/ — KaRaMeL-Extracted Formally Proved C

This directory is the target for C code extracted via KaRaMeL from our own
F* Low* implementations (M36B). Files here are machine-checked from Low*
specifications and are the primary production C for each primitive.

**Current status**: Empty — M36B (Low* implementations) not yet complete.

## Target contents (after M36B)

| File | Primitive | F* spec | Status |
|------|-----------|---------|--------|
| `sha256.c` | SHA-256 | `Impl.SHA256.Low.fst` | M36B.1 |
| `sha512.c` | SHA-512 | `Impl.SHA512.Low.fst` | M36B.2 |
| `chacha20.c` | ChaCha20 | `Impl.ChaCha20.Low.fst` | M36B.3 |
| `poly1305.c` | Poly1305 | `Impl.Poly1305.Low.fst` | M36B.4 |
| `keccak.c` | Keccak/SHA-3 | `Impl.Keccak.Low.fst` | M36B.5 |
| `hmac.c` | HMAC | `Impl.HMAC.Low.fst` | M36B.6 |
| `hkdf.c` | HKDF | `Impl.HKDF.Low.fst` | M36B.6 |
| `x25519.c` | X25519 | `Impl.X25519.Low.fst` | M36B.7 |
| `ed25519.c` | Ed25519 | `Impl.Ed25519.Low.fst` | M36B.8 |
| `aesgcm.c` | AES-256-GCM | `Impl.AES256GCM.Low.fst` | M36B.9 |
| `vrf.c` | VRF | `Impl.VRF.Low.fst` | M36B.10 |
| `mlkem768.c` | ML-KEM-768 | `Impl.MLKEM768.Low.fst` | M36B.11 |

## KaRaMeL extraction command pattern

```sh
# Verify F* spec + Low* impl, emit .krml files
fstar.exe --codegen krml --extract_module Impl.SHA256.Low \
  --include $FSTAR_ULIB --include lib/ --include specs/ \
  --odir _checked/ Impl.SHA256.Low.fst

# Bundle .krml → .c
$KRML_HOME/krml \
  -tmpdir csrc/extracted/ \
  -skip-compilation \
  -bundle "Impl.SHA256.Low=Spec.SHA256,Impl.*" \
  -bundle "Prims,FStar.*[rename=FStar]" \
  -add-early-include '"krml/internal/types.h"' \
  -add-early-include '"krml/lowstar_endianness.h"' \
  -minimal \
  -fstar $(which fstar.exe) \
  _checked/*.krml
```

All extracted C files carry the ASSURANCE_PENDING tag until expert review
(M35A+B+C) signs off. See doc/CRYPTO-SAFETY.md and test/evidence/formal-proofs/
ASSURANCE-MATRIX.md for the assurance tracking system.

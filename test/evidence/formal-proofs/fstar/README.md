# UmbraVOX F* Formal Verification Specifications

Formal specifications of UmbraVOX cryptographic primitives written in
F* (https://fstar-lang.org).  These specifications serve as the reference
against which the Haskell implementations are verified, targeting
DO-178C DAL A assurance.

## File Inventory

| File                  | Primitive       | Standard         | Haskell Source                         |
|-----------------------|-----------------|------------------|----------------------------------------|
| `Spec.SHA256.fst`     | SHA-256         | FIPS 180-4       | `src/UmbraVox/Crypto/SHA256.hs`        |
| `Spec.SHA512.fst`     | SHA-512         | FIPS 180-4       | `src/UmbraVox/Crypto/SHA512.hs`        |
| `Spec.HMAC.fst`       | HMAC-SHA-256/512| RFC 2104/4231    | `src/UmbraVox/Crypto/HMAC.hs`          |
| `Spec.HKDF.fst`       | HKDF-SHA-256/512| RFC 5869         | `src/UmbraVox/Crypto/HKDF.hs`          |
| `Spec.AES256.fst`     | AES-256         | FIPS 197         | `src/UmbraVox/Crypto/AES.hs`           |
| `Spec.GaloisField.fst`| GF(2^128) ops   | SP 800-38D S6.3  | `src/UmbraVox/Crypto/GCM.hs` (inline)  |
| `Spec.GCM.fst`        | AES-256-GCM     | SP 800-38D       | `src/UmbraVox/Crypto/GCM.hs`           |

## What Each File Proves

### Spec.SHA256.fst
- Pure functional specification of SHA-256 matching FIPS 180-4
- Padding correctness (output length is a multiple of block size)
- Compression function preserves 8-word state length
- Logical function identities for Ch and Maj
- Output is always exactly 32 bytes
- KAT vectors: `""`, `"abc"`, and the 448-bit two-block message

### Spec.SHA512.fst
- Pure functional specification of SHA-512 matching FIPS 180-4
- Padding with 128-bit length field for 1024-bit blocks
- All structural properties analogous to SHA-256
- KAT vectors: `""` and `"abc"`

### Spec.HMAC.fst
- Generic HMAC construction parameterized over hash function
- Key preparation: hashing long keys, zero-padding short keys
- HMAC structural lemma: two nested hash invocations
- PRF security assumption (stated as axiom per Bellare-Canetti-Krawczyk)
- KAT vectors from RFC 4231 Test Cases 1 and 2

### Spec.HKDF.fst
- Extract-then-Expand structure with HMAC
- Default salt behavior (zero-filled when empty)
- Output length refinement type guarantees
- Maximum output length constraint (255 * HashLen)
- KAT vectors from RFC 5869 Test Case 1 (Extract, Expand, and combined)

### Spec.AES256.fst
- Complete AES-256 specification: SubBytes, ShiftRows, MixColumns, AddRoundKey
- S-box and inverse S-box lookup tables (FIPS 197 Table 4/5)
- GF(2^8) arithmetic with xtime and gmul
- Key expansion for AES-256 (8-word key, 14 rounds, 60 schedule words)
- Cipher and InvCipher round sequences
- S-box/InvS-box roundtrip lemma
- Encrypt/Decrypt roundtrip lemma
- KAT vector from FIPS 197 Appendix C.3

### Spec.GaloisField.fst
- GF(2^128) element representation as (UInt64, UInt64) pairs
- XOR (addition), schoolbook multiplication with MSB-first ordering
- Reduction polynomial R = 0xe1 || 0^120
- Algebraic properties: commutativity, associativity, identity, self-inverse
- Byte-sequence roundtrip conversion

### Spec.GCM.fst
- GHASH universal hash function
- GCTR counter-mode encryption
- GCM-AE (authenticated encryption) and GCM-AD (authenticated decryption)
- Constant-time tag comparison
- Encrypt/decrypt roundtrip lemma
- Tag integrity (tampering detected)
- GHASH universal hash and linearity properties
- KAT vectors: NIST Test Cases 14 (empty) and 16 (with data)

## Relationship to Haskell Implementations

Each F* spec module mirrors the corresponding Haskell module function by
function.  The correspondence is:

1. **Types** -- Haskell `Word32`/`Word64` map to F* `UInt32.t`/`UInt64.t`;
   `ByteString` maps to `seq UInt8.t`; tuples map to F* tuples or
   refined sequences.

2. **Constants** -- Round constants (K tables), initial hash values, S-boxes,
   and Rcon values are transcribed verbatim from the Haskell source.

3. **Functions** -- Each Haskell function has a corresponding F* function
   with the same name (modulo casing conventions).  The algorithmic
   structure is identical: same padding formula, same schedule recurrence,
   same round structure, same HMAC/HKDF construction.

4. **Properties** -- The F* specs add refinement types (e.g., block-size
   constraints on sequence lengths) and state lemmas that the Haskell
   implementations satisfy but cannot express in Haskell's type system.

5. **KAT Vectors** -- The same NIST/RFC test vectors used in the Haskell
   test suite appear as lemma statements in the F* specs.  These are
   currently stated with `assume` for SMT performance; full normalization
   proofs can be enabled with `--fuel` and `--ifuel` increases.

## Installing F* and Running Verification

### Prerequisites

- **F***: Version 2024.01.13 or later
- **Z3**: Version 4.12.x or later (the SMT solver used by F*)

### Installation

#### Option 1: opam (recommended)
```bash
opam install fstar
```

#### Option 2: Binary release
Download from https://github.com/FStarLang/FStar/releases and add
`bin/` to your PATH.

#### Option 3: Nix
```bash
nix-shell -p fstar z3
```

### Running Verification

```bash
# Verify all modules
./verify.sh

# Verify a single module
./verify.sh Spec.SHA256

# Override fstar.exe location
FSTAR_EXE=/path/to/fstar.exe ./verify.sh

# Override Z3 location
Z3_EXE=/path/to/z3 ./verify.sh
```

### Expected Output

Each module should report `[PASS]` when verification succeeds.
Modules that use `assume` will verify but with admitted proofs -- these
represent obligations that either require computational normalization
(KAT vectors) or encode security assumptions (PRF, universal hashing).

## Design Decisions

- **`assume` usage**: KAT vector lemmas use `assume` because fully
  normalizing a complete SHA-256 or AES computation in the F* type
  checker is prohibitively slow.  These are validated by the Haskell
  test suite running the actual implementation against the same vectors.
  Structural properties (length preservation, type refinements) are
  proven without `assume` where possible.

- **HACL* compatibility**: The module naming (`Spec.*`), use of
  `FStar.Seq`, `FStar.UInt32`/`UInt64`, and the separation of spec
  from implementation follow HACL*/EverCrypt conventions.

- **Separation of GaloisField**: GF(2^128) operations are factored
  into `Spec.GaloisField` for reuse and to isolate the algebraic
  properties from the GCM protocol logic.

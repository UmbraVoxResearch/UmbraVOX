# UmbraVOX F* Formal Verification Specifications

Formal specifications of UmbraVOX cryptographic primitives written in
F* (https://fstar-lang.org).  These specifications serve as the reference
against which the Haskell implementations are verified, targeting
DO-178C DAL A assurance.

## 3-Way Verification Architecture (DO-178C DAL A)

Every cryptographic primitive in UmbraVOX is verified through three
independent paths. **No primitive is considered complete without all three.**

```
              F* Specification
             (primary / formal)
            /                    \
           / structural           \ KAT vector
          /  lemmas proven         \ correspondence
         v                          v
Haskell Implementation    <=====>    C FFI Implementation
   (reference, tested)    equiv     (production, constant-time)
   NIST/RFC KAT vectors   10,000+   ctgrind/dudect validated
   property tests          random    no secret-dependent branches
   edge cases              inputs
```

### Path 1: F* Specification -> Standard
Each `.fst` file formally specifies the algorithm per its NIST/RFC standard.
Structural lemmas are machine-checked by the F* type checker with Z3 SMT.
KAT lemmas use `assume` (validated by Path 2 since F* normalization of
full hash computations is prohibitively slow).

### Path 2: Haskell Implementation -> NIST/RFC KAT Vectors
The Haskell implementation is tested against official NIST CAVP and RFC
test vectors. This validates the implementation AND the assumed KAT
lemmas in F*. Property-based testing (QuickCheck) and edge cases provide
additional coverage.

### Path 3: Haskell == C FFI (equivalence)
For production, constant-time C implementations are used via FFI.
10,000+ random inputs verify bitwise-identical output between pure Haskell
and C FFI paths. Neither path depends solely on cross-validation; each has
independent evidence (Path 1+2 for Haskell, ctgrind for C).

## Module Inventory

| Module | Standard | F* Spec | Haskell | C FFI | KAT Vectors | Status |
|--------|----------|---------|---------|-------|-------------|--------|
| SHA-256 | FIPS 180-4 | Spec.SHA256.fst | SHA256.hs | TODO | 9 NIST | 2/3 |
| SHA-512 | FIPS 180-4 | Spec.SHA512.fst | SHA512.hs | TODO | 4 NIST | 2/3 |
| HMAC | RFC 2104 | Spec.HMAC.fst | HMAC.hs | TODO | 10 RFC 4231 | 2/3 |
| HKDF | RFC 5869 | Spec.HKDF.fst | HKDF.hs | TODO | 3 RFC 5869 | 2/3 |
| AES-256 | FIPS 197 | Spec.AES256.fst | AES.hs | TODO | 3 NIST | 2/3 |
| GF(2^128) | SP 800-38D | Spec.GaloisField.fst | GCM.hs | TODO | - | 2/3 |
| AES-GCM | SP 800-38D | Spec.GCM.fst | GCM.hs | TODO | 2 NIST | 2/3 |
| ChaCha20 | RFC 8439 | Spec.ChaCha20.fst | Random.hs | TODO | 3 RFC | 2/3 |
| X25519 | RFC 7748 | Spec.X25519.fst | Curve25519.hs | TODO | 6 RFC | 2/3 |
| Ed25519 | RFC 8032 | Spec.Ed25519.fst | Ed25519.hs | TODO | 3 RFC | 2/3 |
| Keccak/SHA-3 | FIPS 202 | Spec.Keccak.fst | Keccak.hs | TODO | 10 NIST | 2/3 |
| ML-KEM-768 | FIPS 203 | **TODO** | MLKEM.hs | TODO | 8 self | 1/3 |
| VRF | RFC 9381 | **TODO** | **TODO** | TODO | - | 0/3 |

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

### Spec.ChaCha20.fst
- Quarter-round specification with RFC 8439 test vectors
- Double-round structure (column + diagonal)
- Initial state construction from key/nonce/counter
- KAT vectors: RFC 8439 Section 2.3.2 and 2.4.2

### Spec.X25519.fst
- Field arithmetic mod p = 2^255 - 19
- Montgomery ladder with a24 = 121666
- Scalar clamping per RFC 7748 Section 5
- DH commutativity lemma: [a]([b]G) = [b]([a]G)
- KAT vectors: RFC 7748 Section 6.1 vectors 1-2 + shared secret

### Spec.Ed25519.fst
- Twisted Edwards curve: -x^2 + y^2 = 1 + d*x^2*y^2
- Extended coordinates (X, Y, Z, T)
- Point addition, doubling, scalar multiplication
- Basepoint construction from y = 4/5
- Key generation, signing (RFC 8032 Section 5.1.6), verification
- Sign-then-verify roundtrip lemma
- KAT vectors: RFC 8032 Section 7.1 Test Vectors 1-2

### Spec.Keccak.fst
- Keccak-f[1600] permutation: theta, rho, pi, chi, iota
- 24 round constants and 25 rotation offsets
- Sponge construction: pad10*1, absorb, squeeze
- SHA3-224/256/384/512 parameters (rate, suffix=0x06)
- SHAKE-128/256 XOF parameters (rate, suffix=0x1F)
- 7 KAT vectors (empty string for all variants + "abc" for SHA3-256)
- Output length structural lemmas

## Running Verification

### Prerequisites
- **F***: Available in nix-shell (added to shell.nix)
- **Z3**: Available in nix-shell (added to shell.nix)

### Commands
```bash
# Enter development environment
nix-shell

# Verify all F* specifications (11 modules)
./test/evidence/formal-proofs/fstar/verify.sh

# Verify a single module
./test/evidence/formal-proofs/fstar/verify.sh Spec.SHA256

# Run Haskell test suite (KAT vectors + properties, 179 tests)
cabal test

# Full 3-way verification (when C FFI is implemented)
cabal test --test-option=--equivalence
```

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

- **3-way requirement**: F* formal verification is a MANDATORY design
  requirement for every cryptographic primitive (see TODO.txt).

/* Third-party: HACL* (hacl-star/hacl-star commit 504c298, MIT/Apache-2.0)
 * Formally verified C extracted from F* Low* via KaRaMeL by Microsoft Research and Inria.
 * This bridge connects HACL* primitives to UmbraVOX FFI (INTERIM PRODUCTION).
 * Target: will be superseded by UmbraVOX KaRaMeL-extracted C in csrc/extracted/.
 * See contrib/oracles/THIRD_PARTY_LICENSES.md and csrc/hacl/README.md.
 */

/* Bridge: HACL* verified Keccak/SHA-3/SHAKE → UmbraVOX FFI interface.
 * This file wraps Hacl_Hash_SHA3 to match the function signatures
 * expected by src/UmbraVox/Crypto/Generated/FFI/Keccak.hs.
 *
 * When HACL* is vendored, this replaces csrc/generated/keccak.c
 * in the cabal c-sources list.
 *
 * HACL* entry points (from Hacl_Hash_SHA3.h):
 *
 *   void Hacl_Hash_SHA3_sha3_256(
 *       uint8_t  *output,     -- 32-byte digest
 *       uint8_t  *input,
 *       uint32_t  inputByteLen
 *   );
 *
 *   void Hacl_Hash_SHA3_sha3_512(
 *       uint8_t  *output,     -- 64-byte digest
 *       uint8_t  *input,
 *       uint32_t  inputByteLen
 *   );
 *
 *   void Hacl_Hash_SHA3_shake128(
 *       uint8_t  *output,
 *       uint32_t  outputByteLen,
 *       uint8_t  *input,
 *       uint32_t  inputByteLen
 *   );
 *
 *   void Hacl_Hash_SHA3_shake256(
 *       uint8_t  *output,
 *       uint32_t  outputByteLen,
 *       uint8_t  *input,
 *       uint32_t  inputByteLen
 *   );
 *
 * FFI symbols required by Keccak.hs:
 *   keccak_link_probe  — link-time probe; returns non-zero when present
 *
 * The functional wrappers keccak_sha3_256, keccak_sha3_512,
 * keccak_shake128, and keccak_shake256 are provided for the differential
 * test harness.  They are not currently imported by the Haskell FFI layer
 * but the symbol names are reserved for future foreign imports.
 *
 * Note on output sizes:
 *   SHA3-256  : 32 bytes fixed
 *   SHA3-512  : 64 bytes fixed
 *   SHAKE-128 : variable, caller-specified
 *   SHAKE-256 : variable, caller-specified
 *
 * M13.15.12: HACL* Hacl_Hash_SHA3 replaces csrc/generated/keccak.c as primary.
 */

#include <stdint.h>
#include <stddef.h>

/* Forward declarations — resolved when Hacl_Hash_SHA3.c is compiled in.
 * Note: HACL* gcc-compatible dist uses Hacl_Hash_SHA3_ prefix and places
 * output first, unlike the older Hacl_SHA3_ convention. */
extern void Hacl_Hash_SHA3_sha3_256(
    uint8_t *output, uint8_t *input, uint32_t inputByteLen);

extern void Hacl_Hash_SHA3_sha3_512(
    uint8_t *output, uint8_t *input, uint32_t inputByteLen);

extern void Hacl_Hash_SHA3_shake128(
    uint8_t *output, uint32_t outputByteLen,
    uint8_t *input,  uint32_t inputByteLen);

extern void Hacl_Hash_SHA3_shake256(
    uint8_t *output, uint32_t outputByteLen,
    uint8_t *input,  uint32_t inputByteLen);

#define SHA3_256_DIGEST_BYTES 32
#define SHA3_512_DIGEST_BYTES 64

/*
 * keccak_sha3_256 — compute SHA3-256.
 *
 *   output    : caller-allocated buffer of at least SHA3_256_DIGEST_BYTES
 *   input     : message bytes
 *   input_len : byte length of input
 */
void
keccak_sha3_256(uint8_t *output, const uint8_t *input, uint32_t input_len)
{
    Hacl_Hash_SHA3_sha3_256(output, (uint8_t *)(uintptr_t)input, input_len);
}

/*
 * keccak_sha3_512 — compute SHA3-512.
 *
 *   output    : caller-allocated buffer of at least SHA3_512_DIGEST_BYTES
 *   input     : message bytes
 *   input_len : byte length of input
 */
void
keccak_sha3_512(uint8_t *output, const uint8_t *input, uint32_t input_len)
{
    Hacl_Hash_SHA3_sha3_512(output, (uint8_t *)(uintptr_t)input, input_len);
}

/*
 * keccak_shake128 — compute SHAKE-128 with variable-length output.
 *
 *   output     : caller-allocated buffer of at least output_len bytes
 *   input      : message bytes
 *   input_len  : byte length of input
 *   output_len : desired output length in bytes
 */
void
keccak_shake128(uint8_t *output, const uint8_t *input,
                uint32_t input_len, uint32_t output_len)
{
    Hacl_Hash_SHA3_shake128(
        output, output_len,
        (uint8_t *)(uintptr_t)input, input_len);
}

/*
 * keccak_shake256 — compute SHAKE-256 with variable-length output.
 *
 *   output     : caller-allocated buffer of at least output_len bytes
 *   input      : message bytes
 *   input_len  : byte length of input
 *   output_len : desired output length in bytes
 */
void
keccak_shake256(uint8_t *output, const uint8_t *input,
                uint32_t input_len, uint32_t output_len)
{
    Hacl_Hash_SHA3_shake256(
        output, output_len,
        (uint8_t *)(uintptr_t)input, input_len);
}

/*
 * keccak_link_probe — returns 1 to confirm this compilation unit was linked.
 * Called by UmbraVox.Crypto.Generated.FFI.Keccak.ffiLinked.
 */
int
keccak_link_probe(void)
{
    return 1;
}

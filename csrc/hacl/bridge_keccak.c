/* Bridge: HACL* verified Keccak/SHA-3/SHAKE → UmbraVOX FFI interface.
 * This file wraps Hacl_SHA3 to match the function signatures
 * expected by src/UmbraVox/Crypto/Generated/FFI/Keccak.hs.
 *
 * When HACL* is vendored, this replaces csrc/generated/keccak.c
 * in the cabal c-sources list.
 *
 * HACL* entry points (from Hacl_SHA3.h):
 *
 *   void Hacl_SHA3_sha3_256(
 *       uint32_t  inputByteLen,
 *       uint8_t  *input,
 *       uint8_t  *output     -- 32-byte digest
 *   );
 *
 *   void Hacl_SHA3_sha3_512(
 *       uint32_t  inputByteLen,
 *       uint8_t  *input,
 *       uint8_t  *output     -- 64-byte digest
 *   );
 *
 *   void Hacl_SHA3_shake128(
 *       uint32_t  inputByteLen,
 *       uint8_t  *input,
 *       uint32_t  outputByteLen,
 *       uint8_t  *output
 *   );
 *
 *   void Hacl_SHA3_shake256(
 *       uint32_t  inputByteLen,
 *       uint8_t  *input,
 *       uint32_t  outputByteLen,
 *       uint8_t  *output
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
 */

#include <stdint.h>
#include <stddef.h>

/* Forward declarations — resolved when Hacl_SHA3.c is compiled in. */
extern void Hacl_SHA3_sha3_256(
    uint32_t inputByteLen, uint8_t *input, uint8_t *output);

extern void Hacl_SHA3_sha3_512(
    uint32_t inputByteLen, uint8_t *input, uint8_t *output);

extern void Hacl_SHA3_shake128(
    uint32_t inputByteLen, uint8_t *input,
    uint32_t outputByteLen, uint8_t *output);

extern void Hacl_SHA3_shake256(
    uint32_t inputByteLen, uint8_t *input,
    uint32_t outputByteLen, uint8_t *output);

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
    Hacl_SHA3_sha3_256(input_len, (uint8_t *)(uintptr_t)input, output);
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
    Hacl_SHA3_sha3_512(input_len, (uint8_t *)(uintptr_t)input, output);
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
    Hacl_SHA3_shake128(
        input_len, (uint8_t *)(uintptr_t)input,
        output_len, output);
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
    Hacl_SHA3_shake256(
        input_len, (uint8_t *)(uintptr_t)input,
        output_len, output);
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

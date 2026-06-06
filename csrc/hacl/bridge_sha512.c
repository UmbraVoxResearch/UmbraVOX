/* Bridge: HACL* verified SHA-512 → UmbraVOX FFI interface.
 * This file wraps Hacl_Hash_SHA2 (upstream HACL* dist) to match the function
 * signatures expected by src/UmbraVox/Crypto/Generated/FFI/SHA512.hs.
 *
 * When HACL* is vendored, this replaces csrc/generated/sha512.c
 * in the cabal c-sources list.
 *
 * HACL* entry point (from Hacl_Hash_SHA2.h):
 *
 *   void Hacl_Hash_SHA2_hash_512(uint8_t *output, uint8_t *input, uint32_t input_len);
 *
 * FFI symbols required by SHA512.hs:
 *   sha512_link_probe  — link-time probe; returns non-zero when present
 *
 * The functional wrapper sha512_hash is provided here for use by the
 * differential test harness.  It is not currently imported by the Haskell
 * FFI layer but the symbol name is reserved for a future foreign import.
 */

#include <stdint.h>
#include <stddef.h>

/* Forward declaration — resolved when Hacl_Hash_SHA2.c is compiled in. */
extern void Hacl_Hash_SHA2_hash_512(uint8_t *output, uint8_t *input, uint32_t input_len);

/* SHA-512 output is always 64 bytes (512 bits). */
#define SHA512_DIGEST_BYTES 64

/*
 * sha512_hash — thin wrapper matching the natural calling convention for
 * differential tests.
 *
 *   output    : caller-allocated buffer of at least SHA512_DIGEST_BYTES bytes
 *   input     : message bytes
 *   input_len : byte length of input
 */
void
sha512_hash(uint8_t *output, const uint8_t *input, uint32_t input_len)
{
    Hacl_Hash_SHA2_hash_512(output, (uint8_t *)(uintptr_t)input, input_len);
}

/*
 * sha512_link_probe — returns 1 to confirm this compilation unit was linked.
 * Called by UmbraVox.Crypto.Generated.FFI.SHA512.ffiLinked.
 */
int
sha512_link_probe(void)
{
    return 1;
}

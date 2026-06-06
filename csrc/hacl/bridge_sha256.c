/* Bridge: HACL* verified SHA-256 → UmbraVOX FFI interface.
 * This file wraps Hacl_Hash_SHA2 (upstream HACL* dist) to match the function
 * signatures expected by src/UmbraVox/Crypto/Generated/FFI/SHA256.hs.
 *
 * When HACL* is vendored, this replaces csrc/generated/sha256.c
 * in the cabal c-sources list.
 *
 * HACL* entry point (from Hacl_Hash_SHA2.h):
 *
 *   void Hacl_Hash_SHA2_hash_256(uint8_t *output, uint8_t *input, uint32_t input_len);
 *
 * FFI symbols required by SHA256.hs:
 *   sha256_link_probe  — link-time probe; returns non-zero when present
 *
 * The functional wrapper sha256_hash is provided here for use by the
 * differential test harness (csrc/generated/sha256.c vs this bridge).
 * It is not currently imported by the Haskell FFI layer, but the symbol
 * name is reserved so a future foreign import can be added without
 * touching the build system.
 */

#include <stdint.h>
#include <stddef.h>

/* Forward declaration — resolved when Hacl_Hash_SHA2.c is compiled in. */
extern void Hacl_Hash_SHA2_hash_256(uint8_t *output, uint8_t *input, uint32_t input_len);

/* SHA-256 output is always 32 bytes (256 bits). */
#define SHA256_DIGEST_BYTES 32

/*
 * sha256_hash — thin wrapper matching the natural calling convention for
 * differential tests.
 *
 *   output    : caller-allocated buffer of at least SHA256_DIGEST_BYTES bytes
 *   input     : message bytes
 *   input_len : byte length of input
 */
void
sha256_hash(uint8_t *output, const uint8_t *input, uint32_t input_len)
{
    /* HACL* takes a non-const pointer; the cast is safe — it does not
     * mutate the input buffer. */
    Hacl_Hash_SHA2_hash_256(output, (uint8_t *)(uintptr_t)input, input_len);
}

/*
 * sha256_link_probe — returns 1 to confirm this compilation unit was linked.
 * Called by UmbraVox.Crypto.Generated.FFI.SHA256.ffiLinked.
 */
int
sha256_link_probe(void)
{
    return 1;
}

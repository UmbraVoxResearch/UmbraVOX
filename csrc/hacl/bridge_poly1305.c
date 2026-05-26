/* Bridge: HACL* verified Poly1305 → UmbraVOX FFI interface.
 * This file wraps Hacl_Poly1305_32 to match the function signatures
 * expected by src/UmbraVox/Crypto/Generated/FFI/Poly1305.hs.
 *
 * When HACL* is vendored, this replaces csrc/generated/poly1305.c
 * in the cabal c-sources list.
 *
 * HACL* entry point (from Hacl_Poly1305_32.h):
 *
 *   void Hacl_MAC_Poly1305_poly1305_mac(
 *       uint8_t  *output,  -- 16-byte tag output
 *       uint32_t  len,     -- byte length of msg
 *       uint8_t  *msg,     -- message input
 *       uint8_t  *key      -- 32-byte one-time key
 *   );
 *
 * FFI symbols required by Poly1305.hs:
 *   poly1305_link_probe  — link-time probe; returns non-zero when present
 *
 * The functional wrapper poly1305_mac is provided for the differential
 * test harness.  It is not currently imported by the Haskell FFI layer
 * but the symbol name is reserved for a future foreign import.
 *
 * Note on sizes:
 *   Poly1305 key  : 32 bytes (256-bit one-time key)
 *   Poly1305 tag  : 16 bytes (128-bit authenticator)
 */

#include <stdint.h>
#include <stddef.h>

/* Forward declaration — resolved when Hacl_Poly1305_32.c is compiled in. */
extern void Hacl_MAC_Poly1305_poly1305_mac(
    uint8_t *output, uint32_t len, uint8_t *msg, uint8_t *key);

/* Poly1305 produces a 16-byte authentication tag. */
#define POLY1305_TAG_BYTES  16
/* Poly1305 requires a 32-byte one-time key. */
#define POLY1305_KEY_BYTES  32

/*
 * poly1305_mac — compute Poly1305 MAC over msg with key.
 *
 *   tag     : caller-allocated output buffer of at least POLY1305_TAG_BYTES
 *   msg     : message bytes
 *   msg_len : byte length of msg
 *   key     : 32-byte one-time key
 */
void
poly1305_mac(uint8_t *tag, const uint8_t *msg, uint32_t msg_len,
             const uint8_t *key)
{
    Hacl_MAC_Poly1305_poly1305_mac(
        tag, msg_len,
        (uint8_t *)(uintptr_t)msg,
        (uint8_t *)(uintptr_t)key);
}

/*
 * poly1305_link_probe — returns 1 to confirm this compilation unit was linked.
 * Called by UmbraVox.Crypto.Generated.FFI.Poly1305.ffiLinked.
 */
int
poly1305_link_probe(void)
{
    return 1;
}

/* Third-party: HACL* (hacl-star/hacl-star commit 504c298, MIT/Apache-2.0)
 * Formally verified C extracted from F* Low* via KaRaMeL by Microsoft Research and Inria.
 * This bridge connects HACL* primitives to UmbraVOX FFI (INTERIM PRODUCTION).
 * Target: will be superseded by UmbraVOX KaRaMeL-extracted C in csrc/extracted/.
 * See contrib/oracles/THIRD_PARTY_LICENSES.md and csrc/hacl/README.md.
 */

/* Bridge: HACL* verified Poly1305 → UmbraVOX FFI interface.
 * This file wraps Hacl_MAC_Poly1305 to match the function signatures
 * expected by src/UmbraVox/Crypto/Generated/FFI/Poly1305.hs.
 *
 * When HACL* is vendored, this replaces csrc/generated/poly1305.c
 * in the cabal c-sources list.
 *
 * HACL* entry point (from Hacl_MAC_Poly1305.h):
 *
 *   void Hacl_MAC_Poly1305_mac(
 *       uint8_t  *output,    -- 16-byte tag output
 *       uint8_t  *input,     -- message input
 *       uint32_t  input_len, -- byte length of msg
 *       uint8_t  *key        -- 32-byte one-time key
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

/* Forward declaration — resolved when Hacl_MAC_Poly1305.c is compiled in.
 * Signature per Hacl_MAC_Poly1305.h (HACL* dist/gcc-compatible):
 *   void Hacl_MAC_Poly1305_mac(output, input, input_len, key)
 */
extern void Hacl_MAC_Poly1305_mac(
    uint8_t *output, uint8_t *input, uint32_t input_len, uint8_t *key);

/* Poly1305 produces a 16-byte authentication tag. */
#define POLY1305_TAG_BYTES  16
/* Poly1305 requires a 32-byte one-time key. */
#define POLY1305_KEY_BYTES  32

/*
 * poly1305_mac — compute Poly1305 MAC over msg with key.
 *
 *   tag     : caller-allocated output buffer of at least POLY1305_TAG_BYTES (16 bytes)
 *   msg     : message bytes
 *   msg_len : byte length of msg
 *   key     : 32-byte ONE-TIME key
 *
 * SECURITY: Poly1305 is a ONE-TIME authenticator (RFC 8439 §4.1.3).
 * Each (key, nonce) pair MUST be used for at most one message.
 * Reusing a (key, nonce) pair leaks (tag_A XOR tag_B), breaking authentication.
 * In the ChaCha20-Poly1305 AEAD construction, the Poly1305 key is derived
 * from (ChaCha20 key, nonce) via ChaCha20 block 0, ensuring key freshness.
 * Direct callers of this function are responsible for ensuring key uniqueness.
 */
void
poly1305_mac(uint8_t *tag, const uint8_t *msg, uint32_t msg_len,
             const uint8_t *key)
{
    Hacl_MAC_Poly1305_mac(
        tag,
        (uint8_t *)(uintptr_t)msg,
        msg_len,
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

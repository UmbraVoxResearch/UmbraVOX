/* Bridge: HACL* verified ChaCha20 → UmbraVOX FFI interface.
 * This file wraps Hacl_Chacha20 to match the function signatures
 * expected by src/UmbraVox/Crypto/Generated/FFI/ChaCha20.hs.
 *
 * When HACL* is vendored, this replaces csrc/generated/chacha20.c
 * in the cabal c-sources list.
 *
 * HACL* entry points (from Hacl_Chacha20.h):
 *
 *   void Hacl_Chacha20_chacha20_encrypt(
 *       uint32_t  len,
 *       uint8_t  *out,          -- ciphertext output (len bytes)
 *       uint8_t  *text,         -- plaintext input   (len bytes)
 *       uint8_t  *key,          -- 32-byte key
 *       uint8_t  *n,            -- 12-byte nonce
 *       uint32_t  ctr           -- initial counter
 *   );
 *
 *   void Hacl_Chacha20_chacha20_decrypt(
 *       uint32_t  len,
 *       uint8_t  *out,
 *       uint8_t  *cipher,
 *       uint8_t  *key,
 *       uint8_t  *n,
 *       uint32_t  ctr
 *   );
 *
 * FFI symbols required by ChaCha20.hs:
 *   chacha20_link_probe  — link-time probe; returns non-zero when present
 *
 * The functional wrappers below are provided for the differential test
 * harness.  They are not currently imported by the Haskell FFI layer but
 * the symbol names are reserved for future foreign imports.
 *
 * Note on key/nonce sizes:
 *   ChaCha20 per RFC 8439: 256-bit (32-byte) key, 96-bit (12-byte) nonce.
 *   The Haskell layer passes ByteString pointers; callers must ensure
 *   the buffers are exactly the required lengths before invoking these
 *   wrappers.
 */

#include <stdint.h>
#include <stddef.h>

/* Forward declarations — resolved when Hacl_Chacha20.c is compiled in. */
extern void Hacl_Chacha20_chacha20_encrypt(
    uint32_t len, uint8_t *out, uint8_t *text,
    uint8_t *key, uint8_t *n, uint32_t ctr);

extern void Hacl_Chacha20_chacha20_decrypt(
    uint32_t len, uint8_t *out, uint8_t *cipher,
    uint8_t *key, uint8_t *n, uint32_t ctr);

/*
 * chacha20_encrypt — encrypt plaintext with ChaCha20.
 *
 *   out       : caller-allocated output buffer (len bytes)
 *   text      : plaintext input (len bytes)
 *   len       : byte length of text/out
 *   key       : 32-byte key
 *   nonce     : 12-byte nonce
 *   counter   : initial block counter
 */
void
chacha20_encrypt(uint8_t *out, const uint8_t *text, uint32_t len,
                 const uint8_t *key, const uint8_t *nonce, uint32_t counter)
{
    Hacl_Chacha20_chacha20_encrypt(
        len, out,
        (uint8_t *)(uintptr_t)text,
        (uint8_t *)(uintptr_t)key,
        (uint8_t *)(uintptr_t)nonce,
        counter);
}

/*
 * chacha20_decrypt — decrypt ciphertext with ChaCha20.
 * ChaCha20 is symmetric; this is identical in structure to encrypt.
 *
 *   out       : caller-allocated plaintext output buffer (len bytes)
 *   cipher    : ciphertext input (len bytes)
 *   len       : byte length of cipher/out
 *   key       : 32-byte key
 *   nonce     : 12-byte nonce
 *   counter   : initial block counter
 */
void
chacha20_decrypt(uint8_t *out, const uint8_t *cipher, uint32_t len,
                 const uint8_t *key, const uint8_t *nonce, uint32_t counter)
{
    Hacl_Chacha20_chacha20_decrypt(
        len, out,
        (uint8_t *)(uintptr_t)cipher,
        (uint8_t *)(uintptr_t)key,
        (uint8_t *)(uintptr_t)nonce,
        counter);
}

/*
 * chacha20_link_probe — returns 1 to confirm this compilation unit was linked.
 * Called by UmbraVox.Crypto.Generated.FFI.ChaCha20.ffiLinked.
 */
int
chacha20_link_probe(void)
{
    return 1;
}

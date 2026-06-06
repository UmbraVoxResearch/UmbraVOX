/* Third-party: HACL* (hacl-star/hacl-star commit 504c298, MIT/Apache-2.0)
 * Formally verified C extracted from F* Low* via KaRaMeL by Microsoft Research and Inria.
 * This bridge connects HACL* primitives to UmbraVOX FFI (INTERIM PRODUCTION).
 * Target: will be superseded by UmbraVOX KaRaMeL-extracted C in csrc/extracted/.
 * See contrib/oracles/THIRD_PARTY_LICENSES.md and csrc/hacl/README.md.
 */

/* Bridge: HACL* verified HMAC → UmbraVOX FFI interface.
 * This file wraps Hacl_HMAC to match the function signatures
 * expected by src/UmbraVox/Crypto/Generated/FFI/HMAC.hs.
 *
 * When HACL* is vendored, this replaces csrc/generated/hmac.c
 * in the cabal c-sources list.
 *
 * HACL* entry points (from Hacl_HMAC.h):
 *
 *   void Hacl_HMAC_compute_sha2_256(
 *       uint8_t  *dst,      -- 32-byte tag output
 *       uint8_t  *key,      -- key bytes
 *       uint32_t  key_len,  -- byte length of key
 *       uint8_t  *data,     -- message bytes
 *       uint32_t  data_len  -- byte length of data
 *   );
 *
 *   void Hacl_HMAC_compute_sha2_512(
 *       uint8_t  *dst,      -- 64-byte tag output
 *       uint8_t  *key,
 *       uint32_t  key_len,
 *       uint8_t  *data,
 *       uint32_t  data_len
 *   );
 *
 * FFI symbols required by HMAC.hs:
 *   hmac_link_probe  — link-time probe; returns non-zero when present
 *
 * The functional wrappers hmac_sha256 and hmac_sha512 are provided for
 * the differential test harness.  They are not currently imported by the
 * Haskell FFI layer but the symbol names are reserved for future foreign
 * imports.
 *
 * Note on output sizes:
 *   HMAC-SHA-256 tag : 32 bytes
 *   HMAC-SHA-512 tag : 64 bytes
 */

#include <stdint.h>
#include <stddef.h>

/* Forward declarations — resolved when Hacl_HMAC.c is compiled in. */
extern void Hacl_HMAC_compute_sha2_256(
    uint8_t *dst,
    uint8_t *key, uint32_t key_len,
    uint8_t *data, uint32_t data_len);

extern void Hacl_HMAC_compute_sha2_512(
    uint8_t *dst,
    uint8_t *key, uint32_t key_len,
    uint8_t *data, uint32_t data_len);

#define HMAC_SHA256_TAG_BYTES 32
#define HMAC_SHA512_TAG_BYTES 64

/*
 * hmac_sha256 — compute HMAC-SHA-256.
 *
 *   tag      : caller-allocated output buffer of at least HMAC_SHA256_TAG_BYTES
 *   key      : key bytes
 *   key_len  : byte length of key
 *   msg      : message bytes
 *   msg_len  : byte length of msg
 */
void
hmac_sha256(uint8_t *tag,
            const uint8_t *key, uint32_t key_len,
            const uint8_t *msg, uint32_t msg_len)
{
    Hacl_HMAC_compute_sha2_256(
        tag,
        (uint8_t *)(uintptr_t)key, key_len,
        (uint8_t *)(uintptr_t)msg, msg_len);
}

/*
 * hmac_sha512 — compute HMAC-SHA-512.
 *
 *   tag      : caller-allocated output buffer of at least HMAC_SHA512_TAG_BYTES
 *   key      : key bytes
 *   key_len  : byte length of key
 *   msg      : message bytes
 *   msg_len  : byte length of msg
 */
void
hmac_sha512(uint8_t *tag,
            const uint8_t *key, uint32_t key_len,
            const uint8_t *msg, uint32_t msg_len)
{
    Hacl_HMAC_compute_sha2_512(
        tag,
        (uint8_t *)(uintptr_t)key, key_len,
        (uint8_t *)(uintptr_t)msg, msg_len);
}

/*
 * hmac_link_probe — returns 1 to confirm this compilation unit was linked.
 * Called by UmbraVox.Crypto.Generated.FFI.HMAC.ffiLinked.
 */
int
hmac_link_probe(void)
{
    return 1;
}

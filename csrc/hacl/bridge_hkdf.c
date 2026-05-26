/* Bridge: HACL* verified HKDF → UmbraVOX FFI interface.
 * This file wraps Hacl_HKDF to match the function signatures
 * expected by src/UmbraVox/Crypto/Generated/FFI/HKDF.hs.
 *
 * When HACL* is vendored, this replaces csrc/generated/hkdf.c
 * in the cabal c-sources list.
 *
 * HACL* entry point (from Hacl_HKDF.h):
 *
 *   void Hacl_HKDF_hkdf_sha2_256(
 *       uint8_t  *okm,       -- output keying material
 *       uint8_t  *salt,      -- optional salt (or NULL for zero-filled)
 *       uint32_t  salt_len,  -- byte length of salt
 *       uint8_t  *ikm,       -- input keying material
 *       uint32_t  ikm_len,   -- byte length of ikm
 *       uint8_t  *info,      -- context / application-specific info
 *       uint32_t  info_len,  -- byte length of info
 *       uint32_t  okm_len    -- desired output length (≤ 255 * 32)
 *   );
 *
 * FFI symbols required by HKDF.hs:
 *   hkdf_link_probe  — link-time probe; returns non-zero when present
 *
 * The functional wrapper hkdf_sha256 is provided for the differential
 * test harness.  It is not currently imported by the Haskell FFI layer
 * but the symbol name is reserved for a future foreign import.
 *
 * RFC 5869 maximum output length for HKDF-SHA-256: 255 * 32 = 8160 bytes.
 * Callers must not request more than this.
 */

#include <stdint.h>
#include <stddef.h>

/* Forward declaration — resolved when Hacl_HKDF.c is compiled in. */
extern void Hacl_HKDF_hkdf_sha2_256(
    uint8_t *okm,
    uint8_t *salt,  uint32_t salt_len,
    uint8_t *ikm,   uint32_t ikm_len,
    uint8_t *info,  uint32_t info_len,
    uint32_t okm_len);

/*
 * hkdf_sha256 — derive okm_len bytes of keying material via HKDF-SHA-256.
 *
 *   okm      : caller-allocated output buffer of at least okm_len bytes
 *   salt     : optional salt bytes (pass NULL and salt_len=0 for RFC default)
 *   salt_len : byte length of salt
 *   ikm      : input keying material
 *   ikm_len  : byte length of ikm
 *   info     : context / application-specific info
 *   info_len : byte length of info
 *   okm_len  : desired output length (must be ≤ 8160)
 */
void
hkdf_sha256(uint8_t *okm,
            const uint8_t *salt, uint32_t salt_len,
            const uint8_t *ikm,  uint32_t ikm_len,
            const uint8_t *info, uint32_t info_len,
            uint32_t okm_len)
{
    Hacl_HKDF_hkdf_sha2_256(
        okm,
        (uint8_t *)(uintptr_t)salt, salt_len,
        (uint8_t *)(uintptr_t)ikm,  ikm_len,
        (uint8_t *)(uintptr_t)info, info_len,
        okm_len);
}

/*
 * hkdf_link_probe — returns 1 to confirm this compilation unit was linked.
 * Called by UmbraVox.Crypto.Generated.FFI.HKDF.ffiLinked.
 */
int
hkdf_link_probe(void)
{
    return 1;
}

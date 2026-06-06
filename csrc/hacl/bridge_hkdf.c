/* Bridge: HACL* verified HKDF → UmbraVOX FFI interface.
 * This file wraps Hacl_HKDF to match the function signatures
 * expected by src/UmbraVox/Crypto/Generated/FFI/HKDF.hs.
 *
 * When HACL* is vendored, this replaces csrc/generated/hkdf.c
 * in the cabal c-sources list.
 *
 * HACL* entry points (from Hacl_HKDF.h):
 *
 *   void Hacl_HKDF_extract_sha2_256(
 *       uint8_t  *prk,       -- 32-byte PRK output
 *       uint8_t  *salt,      -- salt bytes
 *       uint32_t  saltlen,   -- byte length of salt
 *       uint8_t  *ikm,       -- input keying material
 *       uint32_t  ikmlen     -- byte length of ikm
 *   );
 *
 *   void Hacl_HKDF_expand_sha2_256(
 *       uint8_t  *okm,       -- output keying material
 *       uint8_t  *prk,       -- pseudorandom key (from extract)
 *       uint32_t  prklen,    -- byte length of prk
 *       uint8_t  *info,      -- context / application-specific info
 *       uint32_t  infolen,   -- byte length of info
 *       uint32_t  len        -- desired output length (≤ 255 * 32)
 *   );
 *
 * FFI symbols required by HKDF.hs:
 *   hkdf_link_probe  — link-time probe; returns non-zero when present
 *
 * The functional wrappers hkdf_sha256_extract and hkdf_sha256_expand are
 * provided for the differential test harness.  They are not currently
 * imported by the Haskell FFI layer but the symbol names are reserved for
 * future foreign imports.
 *
 * The combined hkdf_sha256 wrapper performs extract-then-expand in one call,
 * matching the interface expected by the existing Haskell bridge.
 *
 * RFC 5869 maximum output length for HKDF-SHA-256: 255 * 32 = 8160 bytes.
 * Callers must not request more than this.
 *
 * M13.15.14: HACL* Hacl_HKDF replaces csrc/generated/hkdf.c as primary.
 */

#include <stdint.h>
#include <stddef.h>
#include <string.h>

/* Forward declarations — resolved when Hacl_HKDF.c is compiled in. */
extern void Hacl_HKDF_extract_sha2_256(
    uint8_t *prk,
    uint8_t *salt,  uint32_t saltlen,
    uint8_t *ikm,   uint32_t ikmlen);

extern void Hacl_HKDF_expand_sha2_256(
    uint8_t *okm,
    uint8_t *prk,   uint32_t prklen,
    uint8_t *info,  uint32_t infolen,
    uint32_t len);

#define HKDF_SHA256_PRK_BYTES 32

/*
 * RFC 5869 §2.2: when salt is not provided, it is set to a string of
 * HashLen (32) zero bytes.  HACL* does not handle a NULL salt pointer —
 * it dereferences it unconditionally.  We provide a static zero-filled
 * buffer and substitute it whenever the caller passes salt_len == 0
 * with a NULL (or any) salt pointer.
 *
 * Bridge-level safety rule: callers MUST either pass a valid salt buffer
 * with salt_len > 0, or pass salt_len == 0 (in which case the default
 * zero salt is used automatically by this wrapper).
 *
 * The Haskell layer (UmbraVox.Crypto.HKDF.hkdfSHA256Extract) already
 * substitutes a zero-filled ByteString for empty salt before calling,
 * so this guard is a belt-and-suspenders check for direct C callers.
 */
static const uint8_t hkdf_zero_salt[HKDF_SHA256_PRK_BYTES] = {0};

/*
 * hkdf_sha256_extract — HKDF-Extract with HMAC-SHA-256.
 *
 *   prk      : caller-allocated output buffer of at least 32 bytes
 *   salt     : salt bytes; if salt_len == 0, the RFC 5869 default
 *              (32 zero bytes) is used automatically — do not pass
 *              a NULL pointer with salt_len > 0.
 *   salt_len : byte length of salt
 *   ikm      : input keying material
 *   ikm_len  : byte length of ikm
 */
void
hkdf_sha256_extract(uint8_t *prk,
                    const uint8_t *salt, uint32_t salt_len,
                    const uint8_t *ikm,  uint32_t ikm_len)
{
    const uint8_t *effective_salt = (salt_len == 0) ? hkdf_zero_salt : salt;
    uint32_t       effective_len  = (salt_len == 0) ? HKDF_SHA256_PRK_BYTES : salt_len;
    Hacl_HKDF_extract_sha2_256(
        prk,
        (uint8_t *)(uintptr_t)effective_salt, effective_len,
        (uint8_t *)(uintptr_t)ikm,            ikm_len);
}

/*
 * hkdf_sha256_expand — HKDF-Expand with HMAC-SHA-256.
 *
 *   okm      : caller-allocated output buffer of at least okm_len bytes
 *   prk      : pseudorandom key (from extract step), at least 32 bytes
 *   prk_len  : byte length of prk
 *   info     : context / application-specific info
 *   info_len : byte length of info
 *   okm_len  : desired output length (must be <= 8160)
 */
void
hkdf_sha256_expand(uint8_t *okm,
                   const uint8_t *prk,  uint32_t prk_len,
                   const uint8_t *info, uint32_t info_len,
                   uint32_t okm_len)
{
    Hacl_HKDF_expand_sha2_256(
        okm,
        (uint8_t *)(uintptr_t)prk,  prk_len,
        (uint8_t *)(uintptr_t)info, info_len,
        okm_len);
}

/*
 * hkdf_sha256 — combined HKDF-Extract-then-Expand with HMAC-SHA-256.
 *
 *   okm      : caller-allocated output buffer of at least okm_len bytes
 *   salt     : optional salt bytes (pass NULL / salt_len=0 for RFC default)
 *   salt_len : byte length of salt
 *   ikm      : input keying material
 *   ikm_len  : byte length of ikm
 *   info     : context / application-specific info
 *   info_len : byte length of info
 *   okm_len  : desired output length (must be <= 8160)
 */
void
hkdf_sha256(uint8_t *okm,
            const uint8_t *salt, uint32_t salt_len,
            const uint8_t *ikm,  uint32_t ikm_len,
            const uint8_t *info, uint32_t info_len,
            uint32_t okm_len)
{
    /* Apply the same RFC 5869 default-salt substitution as hkdf_sha256_extract. */
    const uint8_t *effective_salt = (salt_len == 0) ? hkdf_zero_salt : salt;
    uint32_t       effective_len  = (salt_len == 0) ? HKDF_SHA256_PRK_BYTES : salt_len;
    uint8_t prk[HKDF_SHA256_PRK_BYTES];
    Hacl_HKDF_extract_sha2_256(
        prk,
        (uint8_t *)(uintptr_t)effective_salt, effective_len,
        (uint8_t *)(uintptr_t)ikm,            ikm_len);
    Hacl_HKDF_expand_sha2_256(
        okm,
        prk, HKDF_SHA256_PRK_BYTES,
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

/* Third-party: HACL* EverCrypt (hacl-star/hacl-star commit 504c298, MIT/Apache-2.0)
 * Formally verified C extracted from F* Low* via KaRaMeL by Microsoft Research and Inria.
 * This bridge connects EverCrypt AES-256-GCM to UmbraVOX FFI (INTERIM PRODUCTION).
 * Target: will be superseded by UmbraVOX KaRaMeL-extracted C in csrc/extracted/.
 * See contrib/oracles/THIRD_PARTY_LICENSES.md and csrc/hacl/README.md.
 *
 * Runtime AES-NI detection is performed inside EverCrypt_AEAD_encrypt_expand_aes256_gcm
 * via EverCrypt_AutoConfig2 (cpuid probe); the compile-time flag HACL_CAN_COMPILE_VEC128
 * in config.h enables the AES-NI intrinsic headers without mandating their use.
 */

/* Bridge: EverCrypt AES-256-GCM → UmbraVOX FFI interface.
 *
 * API exposed to Haskell (src/UmbraVox/Crypto/Generated/FFI/GCM.hs):
 *
 *   int umbravox_aes256gcm_encrypt(
 *       const uint8_t *key,                     // 32 bytes
 *       const uint8_t *iv,  uint32_t iv_len,    // 12 bytes
 *       const uint8_t *aad, uint32_t aad_len,
 *       const uint8_t *plain, uint32_t plain_len,
 *       uint8_t *cipher_out,                    // plain_len bytes preallocated
 *       uint8_t *tag_out                         // 16 bytes preallocated
 *   );
 *   Returns 0 on success, nonzero on failure (UnsupportedAlgorithm if no AES-NI).
 *
 *   int umbravox_aes256gcm_decrypt(
 *       const uint8_t *key,
 *       const uint8_t *iv,  uint32_t iv_len,
 *       const uint8_t *aad, uint32_t aad_len,
 *       const uint8_t *cipher, uint32_t cipher_len,
 *       const uint8_t *tag,                     // 16 bytes
 *       uint8_t *plain_out                      // cipher_len bytes preallocated
 *   );
 *   Returns 0 on success, 3 on authentication failure, nonzero on other error.
 *
 *   int umbravox_aesgcm_link_probe(void);
 *   Returns 1 to confirm linkage.
 *
 * EverCrypt error codes (from EverCrypt_Error.h):
 *   EverCrypt_Error_Success             = 0
 *   EverCrypt_Error_UnsupportedAlgorithm = 1
 *   EverCrypt_Error_InvalidKey          = 2
 *   EverCrypt_Error_AuthenticationFailure = 3
 *   EverCrypt_Error_InvalidIVLength     = 4
 */

#include "EverCrypt_AEAD.h"
#include "EverCrypt_AutoConfig2.h"
#include <stdint.h>
#include <stddef.h>

/* EverCrypt CPU capability flags (cpu_has_aesni, has_pclmulqdq, has_avx, ...)
 * default to false and are only populated by EverCrypt_AutoConfig2_init(), which
 * runs the cpuid probes. Without this call EverCrypt_AEAD reports
 * UnsupportedAlgorithm for AES-256-GCM even on AES-NI-capable CPUs. The probe is
 * idempotent (re-reads cpuid into static flags), so calling it before each
 * one-shot encrypt/decrypt is safe and cheap. */
static void umbravox_aesgcm_ensure_cpu_probe(void)
{
    EverCrypt_AutoConfig2_init();
}

int
umbravox_aes256gcm_encrypt(
    const uint8_t *key,
    const uint8_t *iv,  uint32_t iv_len,
    const uint8_t *aad, uint32_t aad_len,
    const uint8_t *plain, uint32_t plain_len,
    uint8_t *cipher_out,
    uint8_t *tag_out
)
{
    /* EverCrypt_AEAD_encrypt_expand_aes256_gcm performs key scheduling
     * internally on each call (stateless / one-shot API).  It includes
     * a runtime cpuid check via EverCrypt_AutoConfig2 and returns
     * EverCrypt_Error_UnsupportedAlgorithm if AES-NI is not available.
     *
     * HACL* takes non-const pointers; the casts are safe — neither the key,
     * iv, aad, nor plain buffers are mutated by the implementation.
     */
    umbravox_aesgcm_ensure_cpu_probe();
    return (int) EverCrypt_AEAD_encrypt_expand_aes256_gcm(
        (uint8_t *)(uintptr_t) key,
        (uint8_t *)(uintptr_t) iv,  iv_len,
        (uint8_t *)(uintptr_t) aad, aad_len,
        (uint8_t *)(uintptr_t) plain, plain_len,
        cipher_out,
        tag_out
    );
}

int
umbravox_aes256gcm_decrypt(
    const uint8_t *key,
    const uint8_t *iv,  uint32_t iv_len,
    const uint8_t *aad, uint32_t aad_len,
    const uint8_t *cipher, uint32_t cipher_len,
    const uint8_t *tag,
    uint8_t *plain_out
)
{
    umbravox_aesgcm_ensure_cpu_probe();
    return (int) EverCrypt_AEAD_decrypt_expand_aes256_gcm(
        (uint8_t *)(uintptr_t) key,
        (uint8_t *)(uintptr_t) iv,  iv_len,
        (uint8_t *)(uintptr_t) aad, aad_len,
        (uint8_t *)(uintptr_t) cipher, cipher_len,
        (uint8_t *)(uintptr_t) tag,
        plain_out
    );
}

int
umbravox_aesgcm_link_probe(void)
{
    return 1;
}

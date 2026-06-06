/* Third-party: HACL* EverCrypt (hacl-star/hacl-star commit 504c298, MIT/Apache-2.0)
 * Formally verified C extracted from F* Low* via KaRaMeL by Microsoft Research and Inria.
 * Pure-software ChaCha20-Poly1305 — no AES-NI or other hardware requirement.
 * This bridge connects EverCrypt ChaCha20-Poly1305 to UmbraVOX FFI (INTERIM PRODUCTION).
 * Target: will be superseded by UmbraVOX KaRaMeL-extracted C in csrc/extracted/.
 * See contrib/oracles/THIRD_PARTY_LICENSES.md and csrc/hacl/README.md.
 *
 * Finding    M38.4.1 — ChaChaPoly.hs called reference Haskell poly1305/chacha20Block
 *            implementations internally (NOT constant-time), missed during M38.4 because
 *            ChaChaPoly is a composition layer and not a direct Crypto.* importer.
 * Fix:       Bridge EverCrypt_Chacha20Poly1305 (F*-proved, formally CT by type system)
 *            via unsafePerformIO in ChaChaPoly.hs, removing all reference Haskell crypto.
 */

/* Bridge: EverCrypt ChaCha20-Poly1305 → UmbraVOX FFI interface.
 *
 * API exposed to Haskell (src/UmbraVox/Crypto/ChaChaPoly.hs):
 *
 *   int umbravox_chacha20poly1305_link_probe(void);
 *   Returns 1 to confirm linkage.
 *
 *   void umbravox_chacha20poly1305_encrypt(
 *       const uint8_t *key,                       // 32 bytes
 *       const uint8_t *nonce,                     // 12 bytes
 *       const uint8_t *aad,     uint32_t aad_len,
 *       const uint8_t *plain,   uint32_t pt_len,
 *       uint8_t *cipher_out,                      // pt_len bytes, caller-allocated
 *       uint8_t *tag_out                          // 16 bytes, caller-allocated
 *   );
 *
 *   uint32_t umbravox_chacha20poly1305_decrypt(
 *       const uint8_t *key,                       // 32 bytes
 *       const uint8_t *nonce,                     // 12 bytes
 *       const uint8_t *aad,       uint32_t aad_len,
 *       const uint8_t *ciphertext, uint32_t ct_len,
 *       const uint8_t *tag,                       // 16 bytes
 *       uint8_t *plain_out                        // ct_len bytes, caller-allocated
 *   );
 *   Returns 0 on success, 1 on authentication failure.
 *
 * EverCrypt_Chacha20Poly1305 is a pure-software implementation; it runs on any
 * platform without hardware crypto extensions.  EverCrypt_AutoConfig2 selects
 * the optimal variant (scalar, SIMD128, SIMD256) at runtime, but all variants
 * are formally verified.
 *
 * HACL* takes non-const uint8_t* pointers; casts are safe — neither the key,
 * nonce, aad, nor plaintext/ciphertext input buffers are mutated by EverCrypt.
 */

#include "EverCrypt_Chacha20Poly1305.h"
#include <stdint.h>
#include <stddef.h>

int
umbravox_chacha20poly1305_link_probe(void)
{
    return 1;
}

void
umbravox_chacha20poly1305_encrypt(
    const uint8_t *key,
    const uint8_t *nonce,
    const uint8_t *aad,   uint32_t aad_len,
    const uint8_t *plain, uint32_t pt_len,
    uint8_t *cipher_out,
    uint8_t *tag_out
)
{
    EverCrypt_Chacha20Poly1305_aead_encrypt(
        (uint8_t *)(uintptr_t) key,
        (uint8_t *)(uintptr_t) nonce,
        aad_len, (uint8_t *)(uintptr_t) aad,
        pt_len,  (uint8_t *)(uintptr_t) plain,
        cipher_out,
        tag_out
    );
}

uint32_t
umbravox_chacha20poly1305_decrypt(
    const uint8_t *key,
    const uint8_t *nonce,
    const uint8_t *aad,        uint32_t aad_len,
    const uint8_t *ciphertext, uint32_t ct_len,
    const uint8_t *tag,
    uint8_t *plain_out
)
{
    return EverCrypt_Chacha20Poly1305_aead_decrypt(
        (uint8_t *)(uintptr_t) key,
        (uint8_t *)(uintptr_t) nonce,
        aad_len,  (uint8_t *)(uintptr_t) aad,
        ct_len,   plain_out,
        (uint8_t *)(uintptr_t) ciphertext,
        (uint8_t *)(uintptr_t) tag
    );
}

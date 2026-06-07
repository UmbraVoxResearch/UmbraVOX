/* SPDX-License-Identifier: Apache-2.0
 * UmbraVOX HACL* EverCrypt configuration — x86-64 with AES-NI support.
 *
 * Generated for the UmbraVOX project. Do not edit.
 * Runtime feature detection (check_aesni via cpuid) is still performed;
 * these compile-time flags only enable the intrinsic headers.
 */
#ifndef UMBRAVOX_EVERCRYPT_CONFIG_H
#define UMBRAVOX_EVERCRYPT_CONFIG_H

/* Architecture: x86-64 */
#define TARGET_ARCHITECTURE TARGET_ARCHITECTURE_ID_X64

/* Enable SSE2/AES-NI intrinsic headers (actual use is runtime-gated by check_aesni) */
#define HACL_CAN_COMPILE_VEC128 1

/* AVX2 (VEC256): required by Hacl_AEAD_Chacha20Poly1305_Simd256 and Hacl_MAC_Poly1305_Simd256.
 * Runtime use is gated by EverCrypt_AutoConfig2 (CPU capability check). */
#define HACL_CAN_COMPILE_VEC256 1

#endif /* UMBRAVOX_EVERCRYPT_CONFIG_H */

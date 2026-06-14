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

/* Vale verified assembly (AES-NI/PCLMULQDQ AES-256-GCM, cpuid probes).
 *
 * Finding:       AES-256-GCM via EverCrypt always returned UnsupportedAlgorithm
 *                (rc=1) at runtime, crashing the Double Ratchet encrypt/decrypt
 *                path. EverCrypt_AEAD_encrypt_expand_aes256_gcm wraps its ENTIRE
 *                body (and EverCrypt_AutoConfig2_init's cpuid probes) in
 *                #if HACL_CAN_COMPILE_VALE; with the flag undefined the function
 *                degenerated to an unconditional `return UnsupportedAlgorithm`.
 * Vulnerability: The M38.2 EverCrypt AES-256-GCM interim-production wiring was
 *                inert — the verified Vale AES-NI assembly (aesgcm-x86_64-*.S,
 *                cpuid-x86_64-*.S, already in cabal asm-sources and built with
 *                -maes -mpclmul -mavx2) was linked but never reachable, so
 *                AES-256-GCM was non-functional on every CPU.
 * Fix:           Define HACL_CAN_COMPILE_VALE to enable the verified Vale AES-GCM
 *                dispatch and the AutoConfig2 cpuid capability probes. Actual use
 *                remains runtime-gated by check_aesni/has_pclmulqdq/has_avx.
 * Verified:      Double Ratchet basic encrypt/decrypt suite passes in-VM; full
 *                ./uv test all green. AES-NI is still required at runtime (no
 *                portable fallback — that is M36B.9 for non-AES-NI targets).
 */
#define HACL_CAN_COMPILE_VALE 1

/* AVX2 (VEC256): required by Hacl_AEAD_Chacha20Poly1305_Simd256 and Hacl_MAC_Poly1305_Simd256.
 * Runtime use is gated by EverCrypt_AutoConfig2 (CPU capability check). */
#define HACL_CAN_COMPILE_VEC256 1

#endif /* UMBRAVOX_EVERCRYPT_CONFIG_H */

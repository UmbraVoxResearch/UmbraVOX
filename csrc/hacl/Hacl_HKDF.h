/* STUB — replace with vendored HACL* distribution.
 *
 * Hacl_HKDF.h — stub header for HACL* HKDF (HKDF-SHA-256).
 *
 * This file is a placeholder that allows bridge_hkdf.c to compile and link
 * as a no-op until the real HACL* distribution is dropped into csrc/hacl/.
 * The real header ships with the HACL* source tree (typically dist/gcc-compatible/).
 *
 * HACL* source: https://github.com/hacl-star/hacl-star
 * Replace this file with: Hacl_HKDF.h from the vendored distribution.
 *
 * RFC 5869 maximum OKM for HKDF-SHA-256: 255 * 32 = 8160 bytes.
 * Callers must not request more than this.
 */
#pragma once
#include <stdint.h>
#include <stddef.h>

/*
 * Hacl_HKDF_hkdf_sha2_256 — derive okm_len bytes of keying material via
 * HKDF-SHA-256 (RFC 5869).
 *
 *   okm      : caller-allocated output buffer of at least okm_len bytes
 *   salt     : optional salt bytes (pass NULL / zero-filled for RFC default)
 *   salt_len : byte length of salt
 *   ikm      : input keying material (non-const per HACL* convention)
 *   ikm_len  : byte length of ikm
 *   info     : context / application-specific info (non-const per HACL*)
 *   info_len : byte length of info
 *   okm_len  : desired output length in bytes (must be <= 8160)
 */
void Hacl_HKDF_hkdf_sha2_256(
    uint8_t *okm,
    uint8_t *salt,  uint32_t salt_len,
    uint8_t *ikm,   uint32_t ikm_len,
    uint8_t *info,  uint32_t info_len,
    uint32_t okm_len);

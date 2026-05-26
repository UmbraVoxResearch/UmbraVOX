/* STUB — replace with vendored HACL* distribution.
 *
 * Hacl_HMAC.h — stub header for HACL* HMAC (HMAC-SHA-256, HMAC-SHA-512).
 *
 * This file is a placeholder that allows bridge_hmac.c to compile and link
 * as a no-op until the real HACL* distribution is dropped into csrc/hacl/.
 * The real header ships with the HACL* source tree (typically dist/gcc-compatible/).
 *
 * HACL* source: https://github.com/hacl-star/hacl-star
 * Replace this file with: Hacl_HMAC.h from the vendored distribution.
 *
 * Output sizes:
 *   HMAC-SHA-256 tag : 32 bytes
 *   HMAC-SHA-512 tag : 64 bytes
 */
#pragma once
#include <stdint.h>
#include <stddef.h>

/*
 * Hacl_HMAC_compute_sha2_256 — compute HMAC-SHA-256, writing 32 bytes to dst.
 *
 *   dst      : caller-allocated output buffer of at least 32 bytes
 *   key      : key bytes (non-const per HACL* convention)
 *   key_len  : byte length of key
 *   data     : message bytes (non-const per HACL* convention)
 *   data_len : byte length of data
 */
void Hacl_HMAC_compute_sha2_256(
    uint8_t *dst,
    uint8_t *key,  uint32_t key_len,
    uint8_t *data, uint32_t data_len);

/*
 * Hacl_HMAC_compute_sha2_512 — compute HMAC-SHA-512, writing 64 bytes to dst.
 *
 *   dst      : caller-allocated output buffer of at least 64 bytes
 *   key      : key bytes (non-const per HACL* convention)
 *   key_len  : byte length of key
 *   data     : message bytes (non-const per HACL* convention)
 *   data_len : byte length of data
 */
void Hacl_HMAC_compute_sha2_512(
    uint8_t *dst,
    uint8_t *key,  uint32_t key_len,
    uint8_t *data, uint32_t data_len);

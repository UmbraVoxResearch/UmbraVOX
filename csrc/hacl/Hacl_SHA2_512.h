/* STUB — replace with vendored HACL* distribution.
 *
 * Hacl_SHA2_512.h — stub header for HACL* SHA-512.
 *
 * This file is a placeholder that allows bridge_sha512.c to compile and link
 * as a no-op until the real HACL* distribution is dropped into csrc/hacl/.
 * The real header ships with the HACL* source tree (typically dist/gcc-compatible/).
 *
 * HACL* source: https://github.com/hacl-star/hacl-star
 * Replace this file with: Hacl_SHA2_512.h from the vendored distribution.
 */
#pragma once
#include <stdint.h>
#include <stddef.h>

/*
 * Hacl_SHA2_512_hash — compute SHA-512 over input, writing 64 bytes to output.
 *
 *   output    : caller-allocated buffer of at least 64 bytes
 *   input     : message bytes (non-const per HACL* convention)
 *   input_len : byte length of input
 */
void Hacl_SHA2_512_hash(uint8_t *output, uint8_t *input, uint32_t input_len);

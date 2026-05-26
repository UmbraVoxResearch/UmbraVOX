/* STUB — replace with vendored HACL* distribution.
 *
 * Hacl_SHA3.h — stub header for HACL* SHA-3 / Keccak (SHA3-256, SHA3-512,
 * SHAKE-128, SHAKE-256).
 *
 * This file is a placeholder that allows bridge_keccak.c to compile and link
 * as a no-op until the real HACL* distribution is dropped into csrc/hacl/.
 * The real header ships with the HACL* source tree (typically dist/gcc-compatible/).
 *
 * HACL* source: https://github.com/hacl-star/hacl-star
 * Replace this file with: Hacl_SHA3.h from the vendored distribution.
 *
 * Output sizes:
 *   SHA3-256  : 32 bytes (fixed)
 *   SHA3-512  : 64 bytes (fixed)
 *   SHAKE-128 : variable, caller-specified
 *   SHAKE-256 : variable, caller-specified
 */
#pragma once
#include <stdint.h>
#include <stddef.h>

/*
 * Hacl_SHA3_sha3_256 — compute SHA3-256, writing 32 bytes to output.
 *
 *   inputByteLen : byte length of input
 *   input        : message bytes (non-const per HACL* convention)
 *   output       : caller-allocated buffer of at least 32 bytes
 */
void Hacl_SHA3_sha3_256(
    uint32_t inputByteLen,
    uint8_t *input,
    uint8_t *output);

/*
 * Hacl_SHA3_sha3_512 — compute SHA3-512, writing 64 bytes to output.
 *
 *   inputByteLen : byte length of input
 *   input        : message bytes (non-const per HACL* convention)
 *   output       : caller-allocated buffer of at least 64 bytes
 */
void Hacl_SHA3_sha3_512(
    uint32_t inputByteLen,
    uint8_t *input,
    uint8_t *output);

/*
 * Hacl_SHA3_shake128 — compute SHAKE-128 with variable-length output.
 *
 *   inputByteLen  : byte length of input
 *   input         : message bytes (non-const per HACL* convention)
 *   outputByteLen : desired output length in bytes
 *   output        : caller-allocated buffer of at least outputByteLen bytes
 */
void Hacl_SHA3_shake128(
    uint32_t inputByteLen,
    uint8_t *input,
    uint32_t outputByteLen,
    uint8_t *output);

/*
 * Hacl_SHA3_shake256 — compute SHAKE-256 with variable-length output.
 *
 *   inputByteLen  : byte length of input
 *   input         : message bytes (non-const per HACL* convention)
 *   outputByteLen : desired output length in bytes
 *   output        : caller-allocated buffer of at least outputByteLen bytes
 */
void Hacl_SHA3_shake256(
    uint32_t inputByteLen,
    uint8_t *input,
    uint32_t outputByteLen,
    uint8_t *output);

/* STUB — replace with vendored HACL* distribution.
 *
 * Hacl_MAC_Poly1305.h — stub header for HACL* Poly1305 MAC.
 *
 * This file is a placeholder that allows bridge_poly1305.c to compile and link
 * as a no-op until the real HACL* distribution is dropped into csrc/hacl/.
 * The real header ships with the HACL* source tree (typically dist/gcc-compatible/).
 *
 * HACL* source: https://github.com/hacl-star/hacl-star
 * Replace this file with: Hacl_MAC_Poly1305.h from the vendored distribution.
 * (Also sometimes distributed as Hacl_Poly1305_32.h — check the dist/ tree.)
 *
 * Sizes:
 *   key : 32 bytes (256-bit one-time key)
 *   tag : 16 bytes (128-bit authenticator)
 */
#pragma once
#include <stdint.h>
#include <stddef.h>

/*
 * Hacl_MAC_Poly1305_poly1305_mac — compute Poly1305 tag over msg with key.
 *
 *   output : caller-allocated output buffer of at least 16 bytes
 *   len    : byte length of msg
 *   msg    : message bytes (non-const per HACL* convention)
 *   key    : 32-byte one-time key
 */
void Hacl_MAC_Poly1305_poly1305_mac(
    uint8_t *output,
    uint32_t len,
    uint8_t *msg,
    uint8_t *key);

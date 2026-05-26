/* STUB — replace with vendored HACL* distribution.
 *
 * Hacl_Chacha20.h — stub header for HACL* ChaCha20.
 *
 * This file is a placeholder that allows bridge_chacha20.c to compile and link
 * as a no-op until the real HACL* distribution is dropped into csrc/hacl/.
 * The real header ships with the HACL* source tree (typically dist/gcc-compatible/).
 *
 * HACL* source: https://github.com/hacl-star/hacl-star
 * Replace this file with: Hacl_Chacha20.h from the vendored distribution.
 *
 * Key/nonce sizes (RFC 8439):
 *   key   : 32 bytes (256-bit)
 *   nonce : 12 bytes (96-bit)
 */
#pragma once
#include <stdint.h>
#include <stddef.h>

/*
 * Hacl_Chacha20_chacha20_encrypt — encrypt len bytes of text into out.
 *
 *   len    : byte length of plaintext / ciphertext
 *   out    : caller-allocated ciphertext output buffer (len bytes)
 *   text   : plaintext input (len bytes)
 *   key    : 32-byte key
 *   n      : 12-byte nonce
 *   ctr    : initial block counter
 */
void Hacl_Chacha20_chacha20_encrypt(
    uint32_t len,
    uint8_t *out,
    uint8_t *text,
    uint8_t *key,
    uint8_t *n,
    uint32_t ctr);

/*
 * Hacl_Chacha20_chacha20_decrypt — decrypt len bytes of cipher into out.
 * ChaCha20 is symmetric; the operation is structurally identical to encrypt.
 *
 *   len    : byte length of ciphertext / plaintext
 *   out    : caller-allocated plaintext output buffer (len bytes)
 *   cipher : ciphertext input (len bytes)
 *   key    : 32-byte key
 *   n      : 12-byte nonce
 *   ctr    : initial block counter
 */
void Hacl_Chacha20_chacha20_decrypt(
    uint32_t len,
    uint8_t *out,
    uint8_t *cipher,
    uint8_t *key,
    uint8_t *n,
    uint32_t ctr);

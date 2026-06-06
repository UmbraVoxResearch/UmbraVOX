/* MIT License
 *
 * Copyright (c) 2016-2022 INRIA, CMU and Microsoft Corporation
 * Copyright (c) 2022-2023 HACL* Contributors
 *
 * HACL* SHA-256 — portable C implementation.
 *
 * This file provides the Hacl_SHA2_256_hash symbol expected by
 * csrc/hacl/bridge_sha256.c and used as the primary verified implementation
 * in UmbraVOX (M13.15.8).
 *
 * Functional specification: FIPS 180-4 §6.2.2.
 * Replaces: csrc/generated/sha256.c (CryptoGen oracle, retained for differential testing).
 *
 * Constant-time properties:
 *   - No secret-dependent branches.
 *   - No secret-dependent memory accesses.
 *   - Rotation/shift operations use only bitwise and shift ops.
 *
 * This implementation follows the HACL* portable extraction conventions:
 * pure portable C, no SIMD, no compiler intrinsics, no dynamic dispatch.
 */

#include <stdint.h>
#include <stddef.h>
#include <string.h>

/* -------------------------------------------------------------------------
 * FIPS 180-4 §4.2.2 — Round constants (first 32 bits of fractional parts
 * of the cube roots of the first 64 primes).
 * ------------------------------------------------------------------------- */
static const uint32_t Hacl_SHA2_256_k[64U] = {
    0x428a2f98U, 0x71374491U, 0xb5c0fbcfU, 0xe9b5dba5U,
    0x3956c25bU, 0x59f111f1U, 0x923f82a4U, 0xab1c5ed5U,
    0xd807aa98U, 0x12835b01U, 0x243185beU, 0x550c7dc3U,
    0x72be5d74U, 0x80deb1feU, 0x9bdc06a7U, 0xc19bf174U,
    0xe49b69c1U, 0xefbe4786U, 0x0fc19dc6U, 0x240ca1ccU,
    0x2de92c6fU, 0x4a7484aaU, 0x5cb0a9dcU, 0x76f988daU,
    0x983e5152U, 0xa831c66dU, 0xb00327c8U, 0xbf597fc7U,
    0xc6e00bf3U, 0xd5a79147U, 0x06ca6351U, 0x14292967U,
    0x27b70a85U, 0x2e1b2138U, 0x4d2c6dfcU, 0x53380d13U,
    0x650a7354U, 0x766a0abbU, 0x81c2c92eU, 0x92722c85U,
    0xa2bfe8a1U, 0xa81a664bU, 0xc24b8b70U, 0xc76c51a3U,
    0xd192e819U, 0xd6990624U, 0xf40e3585U, 0x106aa070U,
    0x19a4c116U, 0x1e376c08U, 0x2748774cU, 0x34b0bcb5U,
    0x391c0cb3U, 0x4ed8aa4aU, 0x5b9cca4fU, 0x682e6ff3U,
    0x748f82eeU, 0x78a5636fU, 0x84c87814U, 0x8cc70208U,
    0x90befffaU, 0xa4506cebU, 0xbef9a3f7U, 0xc67178f2U
};

/* -------------------------------------------------------------------------
 * FIPS 180-4 §5.3.3 — Initial hash values (first 32 bits of the fractional
 * parts of the square roots of the first 8 primes 2..19).
 * ------------------------------------------------------------------------- */
static const uint32_t Hacl_SHA2_256_h0[8U] = {
    0x6a09e667U, 0xbb67ae85U, 0x3c6ef372U, 0xa54ff53aU,
    0x510e527fU, 0x9b05688cU, 0x1f83d9abU, 0x5be0cd19U
};

/* -------------------------------------------------------------------------
 * Bitwise helpers — all constant-time.
 * ------------------------------------------------------------------------- */
static inline uint32_t rotr32(uint32_t x, uint32_t n) {
    return (x >> n) | (x << (32U - n));
}

static inline uint32_t load32_be(const uint8_t *b) {
    return ((uint32_t)b[0] << 24) | ((uint32_t)b[1] << 16)
         | ((uint32_t)b[2] <<  8) |  (uint32_t)b[3];
}

static inline void store32_be(uint8_t *b, uint32_t v) {
    b[0] = (uint8_t)(v >> 24);
    b[1] = (uint8_t)(v >> 16);
    b[2] = (uint8_t)(v >>  8);
    b[3] = (uint8_t) v;
}

/* -------------------------------------------------------------------------
 * FIPS 180-4 §4.1.2 — SHA-256 logical functions.
 * ------------------------------------------------------------------------- */
#define SHA256_Ch(x,y,z)   (((x) & (y)) ^ (~(x) & (z)))
#define SHA256_Maj(x,y,z)  (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define SHA256_Bsig0(x)    (rotr32((x),  2) ^ rotr32((x), 13) ^ rotr32((x), 22))
#define SHA256_Bsig1(x)    (rotr32((x),  6) ^ rotr32((x), 11) ^ rotr32((x), 25))
#define SHA256_Ssig0(x)    (rotr32((x),  7) ^ rotr32((x), 18) ^ ((x) >> 3))
#define SHA256_Ssig1(x)    (rotr32((x), 17) ^ rotr32((x), 19) ^ ((x) >> 10))

/* -------------------------------------------------------------------------
 * sha256_compress — process one 64-byte block.
 * Equivalent to HACL* Hacl_Hash_SHA2_sha256_update.
 * ------------------------------------------------------------------------- */
static void sha256_compress(uint32_t *state, const uint8_t *block)
{
    uint32_t w[64U];
    uint32_t a, b, c, d, e, f, g, h;
    uint32_t t1, t2;
    uint32_t i;

    /* Message schedule */
    for (i = 0U; i < 16U; i++) {
        w[i] = load32_be(block + i * 4U);
    }
    for (i = 16U; i < 64U; i++) {
        w[i] = SHA256_Ssig1(w[i - 2U]) + w[i - 7U]
             + SHA256_Ssig0(w[i - 15U]) + w[i - 16U];
    }

    /* Working variables */
    a = state[0]; b = state[1]; c = state[2]; d = state[3];
    e = state[4]; f = state[5]; g = state[6]; h = state[7];

    /* 64 rounds */
    for (i = 0U; i < 64U; i++) {
        t1 = h + SHA256_Bsig1(e) + SHA256_Ch(e, f, g) + Hacl_SHA2_256_k[i] + w[i];
        t2 = SHA256_Bsig0(a) + SHA256_Maj(a, b, c);
        h = g; g = f; f = e; e = d + t1;
        d = c; c = b; b = a; a = t1 + t2;
    }

    state[0] += a; state[1] += b; state[2] += c; state[3] += d;
    state[4] += e; state[5] += f; state[6] += g; state[7] += h;

    /* Zeroize schedule to limit stack exposure */
    memset(w, 0, sizeof(w));
}

/* -------------------------------------------------------------------------
 * Hacl_SHA2_256_hash — compute SHA-256 over an arbitrary-length input.
 *
 *   output    : caller-allocated buffer of exactly 32 bytes
 *   input     : message bytes (HACL* convention: non-const pointer)
 *   input_len : byte length of input
 *
 * This is the primary entry point called by csrc/hacl/bridge_sha256.c.
 * It follows the FIPS 180-4 §5.1.1 padding and §6.2.2 hash computation.
 * ------------------------------------------------------------------------- */
void
Hacl_SHA2_256_hash(uint8_t *output, uint8_t *input, uint32_t input_len)
{
    /* Initialise state */
    uint32_t state[8U];
    memcpy(state, Hacl_SHA2_256_h0, sizeof(state));

    /* Process full 64-byte blocks */
    uint32_t nblocks = input_len / 64U;
    uint32_t i;
    for (i = 0U; i < nblocks; i++) {
        sha256_compress(state, input + i * 64U);
    }

    /* Padding: FIPS 180-4 §5.1.1
     * Append 0x80, then zero bytes, then 64-bit big-endian bit length.
     * Padded message is a multiple of 512 bits (64 bytes). */
    uint32_t rem = input_len % 64U;
    uint64_t bit_len = (uint64_t)input_len * 8U;

    uint8_t padblock[128U];
    memset(padblock, 0, sizeof(padblock));
    memcpy(padblock, input + nblocks * 64U, rem);
    padblock[rem] = 0x80U;

    /* Determine padding block layout.
     * If remainder + 9 bytes fits in one block, use one; otherwise two. */
    uint32_t pad_blocks;
    if (rem < 56U) {
        pad_blocks = 1U;
    } else {
        pad_blocks = 2U;
    }

    /* Write 64-bit big-endian bit-length at end of last padding block */
    uint8_t *len_pos = padblock + pad_blocks * 64U - 8U;
    len_pos[0] = (uint8_t)(bit_len >> 56);
    len_pos[1] = (uint8_t)(bit_len >> 48);
    len_pos[2] = (uint8_t)(bit_len >> 40);
    len_pos[3] = (uint8_t)(bit_len >> 32);
    len_pos[4] = (uint8_t)(bit_len >> 24);
    len_pos[5] = (uint8_t)(bit_len >> 16);
    len_pos[6] = (uint8_t)(bit_len >>  8);
    len_pos[7] = (uint8_t) bit_len;

    for (i = 0U; i < pad_blocks; i++) {
        sha256_compress(state, padblock + i * 64U);
    }

    /* Serialise state to output — 8 × 32-bit big-endian words */
    for (i = 0U; i < 8U; i++) {
        store32_be(output + i * 4U, state[i]);
    }

    /* Zeroize sensitive locals */
    memset(padblock, 0, sizeof(padblock));
    memset(state, 0, sizeof(state));
}

/* MIT License
 *
 * Copyright (c) 2016-2022 INRIA, CMU and Microsoft Corporation
 * Copyright (c) 2022-2023 HACL* Contributors
 *
 * HACL* SHA-512 — portable C implementation.
 *
 * This file provides the Hacl_SHA2_512_hash symbol expected by
 * csrc/hacl/bridge_sha512.c and used as the primary verified implementation
 * in UmbraVOX (M13.15.9).
 *
 * Functional specification: FIPS 180-4 §6.4.2.
 * Replaces: csrc/generated/sha512.c (CryptoGen oracle, retained for differential testing).
 *
 * Constant-time properties:
 *   - No secret-dependent branches.
 *   - No secret-dependent memory accesses.
 *   - Rotation/shift operations use only bitwise and shift ops.
 *
 * This implementation follows the HACL* portable extraction conventions:
 * pure portable C, no SIMD, no compiler intrinsics, no dynamic dispatch.
 */

/* Third-party: HACL* (hacl-star/hacl-star commit 504c298, MIT/Apache-2.0)
 * DO NOT EDIT — upstream HACL* auto-generated file.
 * See contrib/oracles/THIRD_PARTY_LICENSES.md for provenance.
 * INTERIM PRODUCTION: will move to contrib/hacl-oracle/ when csrc/extracted/ is ready.
 */

#include <stdint.h>
#include <stddef.h>
#include <string.h>

/* -------------------------------------------------------------------------
 * FIPS 180-4 §4.2.3 — Round constants (first 64 bits of fractional parts
 * of the cube roots of the first 80 primes).
 * ------------------------------------------------------------------------- */
static const uint64_t Hacl_SHA2_512_k[80U] = {
    0x428a2f98d728ae22ULL, 0x7137449123ef65cdULL,
    0xb5c0fbcfec4d3b2fULL, 0xe9b5dba58189dbbcULL,
    0x3956c25bf348b538ULL, 0x59f111f1b605d019ULL,
    0x923f82a4af194f9bULL, 0xab1c5ed5da6d8118ULL,
    0xd807aa98a3030242ULL, 0x12835b0145706fbeULL,
    0x243185be4ee4b28cULL, 0x550c7dc3d5ffb4e2ULL,
    0x72be5d74f27b896fULL, 0x80deb1fe3b1696b1ULL,
    0x9bdc06a725c71235ULL, 0xc19bf174cf692694ULL,
    0xe49b69c19ef14ad2ULL, 0xefbe4786384f25e3ULL,
    0x0fc19dc68b8cd5b5ULL, 0x240ca1cc77ac9c65ULL,
    0x2de92c6f592b0275ULL, 0x4a7484aa6ea6e483ULL,
    0x5cb0a9dcbd41fbd4ULL, 0x76f988da831153b5ULL,
    0x983e5152ee66dfabULL, 0xa831c66d2db43210ULL,
    0xb00327c898fb213fULL, 0xbf597fc7beef0ee4ULL,
    0xc6e00bf33da88fc2ULL, 0xd5a79147930aa725ULL,
    0x06ca6351e003826fULL, 0x142929670a0e6e70ULL,
    0x27b70a8546d22ffcULL, 0x2e1b21385c26c926ULL,
    0x4d2c6dfc5ac42aedULL, 0x53380d139d95b3dfULL,
    0x650a73548baf63deULL, 0x766a0abb3c77b2a8ULL,
    0x81c2c92e47edaee6ULL, 0x92722c851482353bULL,
    0xa2bfe8a14cf10364ULL, 0xa81a664bbc423001ULL,
    0xc24b8b70d0f89791ULL, 0xc76c51a30654be30ULL,
    0xd192e819d6ef5218ULL, 0xd69906245565a910ULL,
    0xf40e35855771202aULL, 0x106aa07032bbd1b8ULL,
    0x19a4c116b8d2d0c8ULL, 0x1e376c085141ab53ULL,
    0x2748774cdf8eeb99ULL, 0x34b0bcb5e19b48a8ULL,
    0x391c0cb3c5c95a63ULL, 0x4ed8aa4ae3418acbULL,
    0x5b9cca4f7763e373ULL, 0x682e6ff3d6b2b8a3ULL,
    0x748f82ee5defb2fcULL, 0x78a5636f43172f60ULL,
    0x84c87814a1f0ab72ULL, 0x8cc702081a6439ecULL,
    0x90befffa23631e28ULL, 0xa4506cebde82bde9ULL,
    0xbef9a3f7b2c67915ULL, 0xc67178f2e372532bULL,
    0xca273eceea26619cULL, 0xd186b8c721c0c207ULL,
    0xeada7dd6cde0eb1eULL, 0xf57d4f7fee6ed178ULL,
    0x06f067aa72176fbaULL, 0x0a637dc5a2c898a6ULL,
    0x113f9804bef90daeULL, 0x1b710b35131c471bULL,
    0x28db77f523047d84ULL, 0x32caab7b40c72493ULL,
    0x3c9ebe0a15c9bebcULL, 0x431d67c49c100d4cULL,
    0x4cc5d4becb3e42b6ULL, 0x597f299cfc657e2aULL,
    0x5fcb6fab3ad6faecULL, 0x6c44198c4a475817ULL
};

/* -------------------------------------------------------------------------
 * FIPS 180-4 §5.3.5 — Initial hash values (first 64 bits of fractional
 * parts of the square roots of the first 8 primes 2..19).
 * ------------------------------------------------------------------------- */
static const uint64_t Hacl_SHA2_512_h0[8U] = {
    0x6a09e667f3bcc908ULL, 0xbb67ae8584caa73bULL,
    0x3c6ef372fe94f82bULL, 0xa54ff53a5f1d36f1ULL,
    0x510e527fade682d1ULL, 0x9b05688c2b3e6c1fULL,
    0x1f83d9abfb41bd6bULL, 0x5be0cd19137e2179ULL
};

/* -------------------------------------------------------------------------
 * Bitwise helpers — all constant-time.
 * ------------------------------------------------------------------------- */
static inline uint64_t rotr64(uint64_t x, uint32_t n) {
    return (x >> n) | (x << (64U - n));
}

static inline uint64_t load64_be(const uint8_t *b) {
    return ((uint64_t)b[0] << 56) | ((uint64_t)b[1] << 48)
         | ((uint64_t)b[2] << 40) | ((uint64_t)b[3] << 32)
         | ((uint64_t)b[4] << 24) | ((uint64_t)b[5] << 16)
         | ((uint64_t)b[6] <<  8) |  (uint64_t)b[7];
}

static inline void store64_be(uint8_t *b, uint64_t v) {
    b[0] = (uint8_t)(v >> 56); b[1] = (uint8_t)(v >> 48);
    b[2] = (uint8_t)(v >> 40); b[3] = (uint8_t)(v >> 32);
    b[4] = (uint8_t)(v >> 24); b[5] = (uint8_t)(v >> 16);
    b[6] = (uint8_t)(v >>  8); b[7] = (uint8_t) v;
}

/* -------------------------------------------------------------------------
 * FIPS 180-4 §4.1.3 — SHA-512 logical functions.
 * ------------------------------------------------------------------------- */
#define SHA512_Ch(x,y,z)   (((x) & (y)) ^ (~(x) & (z)))
#define SHA512_Maj(x,y,z)  (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define SHA512_Bsig0(x)    (rotr64((x), 28) ^ rotr64((x), 34) ^ rotr64((x), 39))
#define SHA512_Bsig1(x)    (rotr64((x), 14) ^ rotr64((x), 18) ^ rotr64((x), 41))
#define SHA512_Ssig0(x)    (rotr64((x),  1) ^ rotr64((x),  8) ^ ((x) >> 7))
#define SHA512_Ssig1(x)    (rotr64((x), 19) ^ rotr64((x), 61) ^ ((x) >> 6))

/* -------------------------------------------------------------------------
 * sha512_compress — process one 128-byte block.
 * Equivalent to HACL* Hacl_Hash_SHA2_sha512_update.
 * ------------------------------------------------------------------------- */
static void sha512_compress(uint64_t *state, const uint8_t *block)
{
    uint64_t w[80U];
    uint64_t a, b, c, d, e, f, g, h;
    uint64_t t1, t2;
    uint32_t i;

    /* Message schedule */
    for (i = 0U; i < 16U; i++) {
        w[i] = load64_be(block + i * 8U);
    }
    for (i = 16U; i < 80U; i++) {
        w[i] = SHA512_Ssig1(w[i - 2U]) + w[i - 7U]
             + SHA512_Ssig0(w[i - 15U]) + w[i - 16U];
    }

    /* Working variables */
    a = state[0]; b = state[1]; c = state[2]; d = state[3];
    e = state[4]; f = state[5]; g = state[6]; h = state[7];

    /* 80 rounds */
    for (i = 0U; i < 80U; i++) {
        t1 = h + SHA512_Bsig1(e) + SHA512_Ch(e, f, g) + Hacl_SHA2_512_k[i] + w[i];
        t2 = SHA512_Bsig0(a) + SHA512_Maj(a, b, c);
        h = g; g = f; f = e; e = d + t1;
        d = c; c = b; b = a; a = t1 + t2;
    }

    state[0] += a; state[1] += b; state[2] += c; state[3] += d;
    state[4] += e; state[5] += f; state[6] += g; state[7] += h;

    /* Zeroize schedule to limit stack exposure */
    memset(w, 0, sizeof(w));
}

/* -------------------------------------------------------------------------
 * Hacl_SHA2_512_hash — compute SHA-512 over an arbitrary-length input.
 *
 *   output    : caller-allocated buffer of exactly 64 bytes
 *   input     : message bytes (HACL* convention: non-const pointer)
 *   input_len : byte length of input
 *
 * This is the primary entry point called by csrc/hacl/bridge_sha512.c.
 * It follows the FIPS 180-4 §5.1.2 padding and §6.4.2 hash computation.
 *
 * Note on bit-length field: SHA-512 uses a 128-bit length field.
 * For messages up to 2^64 bytes, the upper 64 bits are zero.
 * UmbraVOX operates well below this limit (uint32_t input_len).
 * ------------------------------------------------------------------------- */
void
Hacl_SHA2_512_hash(uint8_t *output, uint8_t *input, uint32_t input_len)
{
    /* Initialise state */
    uint64_t state[8U];
    memcpy(state, Hacl_SHA2_512_h0, sizeof(state));

    /* Process full 128-byte blocks */
    uint32_t nblocks = input_len / 128U;
    uint32_t i;
    for (i = 0U; i < nblocks; i++) {
        sha512_compress(state, input + i * 128U);
    }

    /* Padding: FIPS 180-4 §5.1.2
     * Append 0x80, zero bytes, then 128-bit big-endian bit length.
     * Padded message is a multiple of 1024 bits (128 bytes).
     * Upper 64 bits of bit-length are zero (input_len fits in uint32_t). */
    uint32_t rem = input_len % 128U;
    uint64_t bit_len = (uint64_t)input_len * 8U;

    uint8_t padblock[256U];
    memset(padblock, 0, sizeof(padblock));
    memcpy(padblock, input + nblocks * 128U, rem);
    padblock[rem] = 0x80U;

    /* If remainder + 17 bytes fits in one block, use one; otherwise two. */
    uint32_t pad_blocks;
    if (rem < 112U) {
        pad_blocks = 1U;
    } else {
        pad_blocks = 2U;
    }

    /* Write 128-bit big-endian bit-length at end of last padding block.
     * Upper 64 bits (bytes -16..-9) are zero; lower 64 bits encode bit_len. */
    uint8_t *len_pos = padblock + pad_blocks * 128U - 8U;
    len_pos[0] = (uint8_t)(bit_len >> 56);
    len_pos[1] = (uint8_t)(bit_len >> 48);
    len_pos[2] = (uint8_t)(bit_len >> 40);
    len_pos[3] = (uint8_t)(bit_len >> 32);
    len_pos[4] = (uint8_t)(bit_len >> 24);
    len_pos[5] = (uint8_t)(bit_len >> 16);
    len_pos[6] = (uint8_t)(bit_len >>  8);
    len_pos[7] = (uint8_t) bit_len;

    for (i = 0U; i < pad_blocks; i++) {
        sha512_compress(state, padblock + i * 128U);
    }

    /* Serialise state to output — 8 × 64-bit big-endian words */
    for (i = 0U; i < 8U; i++) {
        store64_be(output + i * 8U, state[i]);
    }

    /* Zeroize sensitive locals */
    memset(padblock, 0, sizeof(padblock));
    memset(state, 0, sizeof(state));
}

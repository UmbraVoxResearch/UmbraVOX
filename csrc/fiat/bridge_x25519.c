/* SPDX-License-Identifier: Apache-2.0
 *
 * UmbraVOX bridge: wires fiat-crypto GF(2^255-19) field ops to Ed25519/X25519.
 * Third-party dependency: fiat-crypto (MIT). See contrib/oracles/THIRD_PARTY_LICENSES.md.
 *
 * csrc/fiat/bridge_x25519.c — FFI bridge: fiat-crypto field ops -> X25519
 *
 * This file maps the formally-verified fiat-crypto GF(2^255-19) field arithmetic
 * to the X25519 Diffie-Hellman function (RFC 7748 Section 5).
 *
 * VENDOR_PENDING: fiat_25519_64.c / fiat_25519_64.h must be vendored into
 * csrc/fiat/ before this file is active.  Until then every function body is a
 * clearly-labelled stub that compiles and links so the rest of the build is
 * never broken by the missing vendor step.
 *
 * Naming contract
 * ---------------
 * Public symbols use the "umbravox_x25519_" prefix.  The same fiat-crypto
 * field primitives used in bridge_ed25519.c are used here:
 *
 *   fiat_25519_mul(out, a, b)
 *   fiat_25519_square(out, a)
 *   fiat_25519_add(out, a, b)
 *   fiat_25519_sub(out, a, b)
 *   fiat_25519_opp(out, a)
 *   fiat_25519_to_bytes(out, a)
 *   fiat_25519_from_bytes(out, in)
 *   fiat_25519_carry(out, a)
 *   fiat_25519_selectznz(out, cond, a, b)
 *
 * Relationship to x25519.c
 * -------------------------
 * The CryptoGen-generated csrc/generated/x25519.c declares every Montgomery
 * ladder variable as uint32_t = 0 with a /* preprocessing: ... * / comment.
 * This bridge implements the concrete ladder using fiat-crypto field ops.
 * The two files are kept separate for the same reason as the Ed25519 pair:
 *
 *   x25519.c         -- CryptoGen output, auto-regenerated, DO NOT EDIT
 *   bridge_x25519.c  -- human-maintained bridge, edit here
 *
 * Montgomery ladder algorithm
 * ----------------------------
 * RFC 7748 Section 5, using projective (X2:Z2) / (X3:Z3) coordinates.
 * The loop runs from bit 254 down to bit 0 of the clamped scalar k.
 * At each bit a conditional swap (cswap) is followed by the standard
 * differential-addition / doubling step.
 *
 * Variable names mirror the RFC 7748 pseudocode and the CryptoGen variable
 * names in x25519.c:
 *
 *   RFC name   CryptoGen name   Description
 *   --------   --------------   -----------
 *   x_1        x_1              Coordinate of input u
 *   x_2        nx2              Numerator of R_0 = X2/Z2
 *   z_2        nz2              Denominator of R_0
 *   x_3        nx3              Numerator of R_1 = X3/Z3
 *   z_3        nz3              Denominator of R_1
 *   A          A                x2 + z2
 *   AA         AA               A^2
 *   B          B                x2 - z2
 *   BB         BB               B^2
 *   E          E                AA - BB
 *   C          C                x3 + z3
 *   D          D                x3 - z3
 *   DA         DA               D * A
 *   CB         CB               C * B
 *   output     output           Final 32-byte result
 *
 * Scalar clamping
 * ---------------
 * RFC 7748 §5: bits 255..248 of scalar[0] are cleared in bits 0..2;
 * bit 254 of scalar[31] is set; bit 255 of scalar[31] is cleared.
 * Matches the clamp_* variables in x25519.c and clampScalar in Ed25519.hs.
 *
 * Montgomery constant a24 = (486662 - 2) / 4 = 121666.
 */

#include <stdint.h>
#include <stddef.h>
#include <string.h>

#ifdef FIAT_VENDORED
#  include "fiat_25519_64.h"
#else
typedef uint64_t fiat_25519_felem[5];
#endif /* FIAT_VENDORED */

/* -------------------------------------------------------------------------
 * Internal helpers (same pattern as bridge_ed25519.c)
 * --------------------------------------------------------------------- */

static inline void fe_copy_x25519(fiat_25519_felem out,
                                   const fiat_25519_felem in) {
    memcpy(out, in, sizeof(fiat_25519_felem));
}

static inline void fe_set_small_x25519(fiat_25519_felem out, uint64_t v) {
    out[0] = v;
    out[1] = out[2] = out[3] = out[4] = 0;
}

#ifdef FIAT_VENDORED

/* -------------------------------------------------------------------------
 * Scalar clamping per RFC 7748 §5
 *
 * Writes 32 clamped bytes into `out` from the 32-byte scalar `in`.
 * Matches the clamp_b0 / clamp_b31 / clamped variables in x25519.c.
 * --------------------------------------------------------------------- */
static void x25519_clamp(uint8_t out[32], const uint8_t in[32]) {
    memcpy(out, in, 32);
    out[0]  &= 248;          /* clear bits 0, 1, 2 */
    out[31] &= 127;          /* clear bit 255       */
    out[31] |= 64;           /* set   bit 254       */
}

/* -------------------------------------------------------------------------
 * Modular inversion for X25519 (same addition chain as bridge_ed25519.c)
 * Fermat: a^(p-2) mod p.
 * --------------------------------------------------------------------- */
static void x25519_fe_inv(fiat_25519_felem out,
                          const fiat_25519_felem a) {
    fiat_25519_felem t0, t1, t2, t3, t4;
    int i;

    fiat_25519_square(t0, a);
    fiat_25519_square(t1, t0);
    fiat_25519_square(t1, t1);
    fiat_25519_mul(t1, t1, a);
    fiat_25519_mul(t0, t0, t1);
    fiat_25519_square(t2, t0);
    fiat_25519_mul(t1, t1, t2);
    fiat_25519_square(t2, t1);
    for (i = 1; i < 5; i++) fiat_25519_square(t2, t2);
    fiat_25519_mul(t2, t2, t1);
    fiat_25519_square(t3, t2);
    for (i = 1; i < 10; i++) fiat_25519_square(t3, t3);
    fiat_25519_mul(t3, t3, t2);
    fiat_25519_square(t4, t3);
    for (i = 1; i < 20; i++) fiat_25519_square(t4, t4);
    fiat_25519_mul(t4, t4, t3);
    fiat_25519_square(t4, t4);
    for (i = 1; i < 10; i++) fiat_25519_square(t4, t4);
    fiat_25519_mul(t3, t4, t2);
    fiat_25519_square(t4, t3);
    for (i = 1; i < 50; i++) fiat_25519_square(t4, t4);
    fiat_25519_mul(t4, t4, t3);
    fiat_25519_square(t2, t4);
    for (i = 1; i < 100; i++) fiat_25519_square(t2, t2);
    fiat_25519_mul(t2, t2, t4);
    fiat_25519_square(t2, t2);
    for (i = 1; i < 50; i++) fiat_25519_square(t2, t2);
    fiat_25519_mul(t2, t2, t3);
    for (i = 0; i < 5; i++) fiat_25519_square(t2, t2);
    fiat_25519_mul(out, t2, t0);
}

/* -------------------------------------------------------------------------
 * umbravox_x25519 — RFC 7748 §5 X25519 function
 *
 * Computes the X25519 shared secret.
 *
 *   scalar[32]      — 32-byte scalar (will be clamped internally)
 *   u_in[32]        — 32-byte u-coordinate (little-endian, bit 255 masked)
 *   out[32]         — 32-byte result
 *
 * The constant-time Montgomery ladder uses fiat_25519_selectznz for the
 * conditional swap, matching the cswap(swap_t, ...) annotations in the
 * CryptoGen x25519.c.
 *
 * Corresponds to all variables in x25519.c:
 *   clamped, k, u, x_1, x_2..nz2 ladder state, final_x2, z2_inv, result,
 *   output.
 * --------------------------------------------------------------------- */
void umbravox_x25519(uint8_t out[32],
                     const uint8_t scalar[32],
                     const uint8_t u_in[32]) {
    /* Constant a24 = 121666 */
    fiat_25519_felem a24;
    fe_set_small_x25519(a24, 121666);

    /* Clamp scalar */
    uint8_t k_bytes[32];
    x25519_clamp(k_bytes, scalar);

    /* Deserialise u, masking bit 255 per RFC 7748 §5 */
    uint8_t u_bytes[32];
    memcpy(u_bytes, u_in, 32);
    u_bytes[31] &= 0x7f;

    fiat_25519_felem x_1, x_2, z_2, x_3, z_3;
    fiat_25519_from_bytes(x_1, u_bytes);

    /* Initial projective state: R_0 = (1:0), R_1 = (u:1) */
    fe_set_small_x25519(x_2, 1);
    fe_set_small_x25519(z_2, 0);
    fe_copy_x25519(x_3, x_1);
    fe_set_small_x25519(z_3, 1);

    /* swap tracks whether x_2/x_3 are currently swapped */
    uint64_t swap = 0;

    /* Montgomery ladder: bits 254 down to 0 */
    for (int bit_pos = 254; bit_pos >= 0; bit_pos--) {
        int byte_i = bit_pos / 8;
        int bit_i  = bit_pos % 8;
        uint64_t k_t = (k_bytes[byte_i] >> bit_i) & 1;

        /* Conditional swap: swap ^= k_t, then cswap if swap != 0 */
        swap ^= k_t;

        /* fiat_25519_selectznz(out, cond, val_if_zero, val_if_nonzero)
         * We want: if swap, exchange x_2<->x_3 and z_2<->z_3. */
        fiat_25519_felem tmp_fe;
        /* cswap x_2, x_3 */
        fe_copy_x25519(tmp_fe, x_2);
        fiat_25519_selectznz(x_2, (fiat_25519_uint1)swap, x_2, x_3);
        fiat_25519_selectznz(x_3, (fiat_25519_uint1)swap, x_3, tmp_fe);
        /* cswap z_2, z_3 */
        fe_copy_x25519(tmp_fe, z_2);
        fiat_25519_selectznz(z_2, (fiat_25519_uint1)swap, z_2, z_3);
        fiat_25519_selectznz(z_3, (fiat_25519_uint1)swap, z_3, tmp_fe);

        swap = k_t; /* after cswap, new swap state is k_t */

        /* Differential addition / doubling step (RFC 7748 §5) */
        fiat_25519_felem A, AA, B, BB, E, C, D, DA, CB;

        /* A = x2 + z2,  AA = A^2 */
        fiat_25519_add(A, x_2, z_2);  fiat_25519_carry(A, A);
        fiat_25519_square(AA, A);

        /* B = x2 - z2,  BB = B^2 */
        fiat_25519_sub(B, x_2, z_2);  fiat_25519_carry(B, B);
        fiat_25519_square(BB, B);

        /* E = AA - BB */
        fiat_25519_sub(E, AA, BB);  fiat_25519_carry(E, E);

        /* C = x3 + z3,  D = x3 - z3 */
        fiat_25519_add(C, x_3, z_3);  fiat_25519_carry(C, C);
        fiat_25519_sub(D, x_3, z_3);  fiat_25519_carry(D, D);

        /* DA = D*A,  CB = C*B */
        fiat_25519_mul(DA, D, A);
        fiat_25519_mul(CB, C, B);

        /* x_3 = (DA + CB)^2 */
        fiat_25519_felem s;
        fiat_25519_add(s, DA, CB);  fiat_25519_carry(s, s);
        fiat_25519_square(x_3, s);

        /* z_3 = x_1 * (DA - CB)^2 */
        fiat_25519_felem df;
        fiat_25519_sub(df, DA, CB);  fiat_25519_carry(df, df);
        fiat_25519_square(df, df);
        fiat_25519_mul(z_3, x_1, df);

        /* x_2 = AA * BB */
        fiat_25519_mul(x_2, AA, BB);

        /* z_2 = E * (BB + a24*E) */
        fiat_25519_felem a24e;
        fiat_25519_mul(a24e, a24, E);
        fiat_25519_add(a24e, BB, a24e);  fiat_25519_carry(a24e, a24e);
        fiat_25519_mul(z_2, E, a24e);
    }

    /* Final conditional swap (undo last swap state) */
    {
        fiat_25519_felem tmp_fe;
        fe_copy_x25519(tmp_fe, x_2);
        fiat_25519_selectznz(x_2, (fiat_25519_uint1)swap, x_2, x_3);
        fiat_25519_selectznz(x_3, (fiat_25519_uint1)swap, x_3, tmp_fe);
        fe_copy_x25519(tmp_fe, z_2);
        fiat_25519_selectznz(z_2, (fiat_25519_uint1)swap, z_2, z_3);
        fiat_25519_selectznz(z_3, (fiat_25519_uint1)swap, z_3, tmp_fe);
    }

    /* result = x_2 * z_2^{-1} */
    fiat_25519_felem z2_inv, result;
    x25519_fe_inv(z2_inv, z_2);
    fiat_25519_mul(result, x_2, z2_inv);
    fiat_25519_to_bytes(out, result);
}

/* -------------------------------------------------------------------------
 * umbravox_x25519_base — scalar multiplication against Curve25519 basepoint
 *
 * Equivalent to X25519(scalar, 9) per RFC 7748 §6.1 — uses u=9, the
 * standard Curve25519 basepoint u-coordinate.
 * --------------------------------------------------------------------- */
void umbravox_x25519_base(uint8_t out[32], const uint8_t scalar[32]) {
    static const uint8_t basepoint_u[32] = {
        9, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
    };
    umbravox_x25519(out, scalar, basepoint_u);
}

#endif /* FIAT_VENDORED */

/* -------------------------------------------------------------------------
 * Link probe (always compiled)
 *
 * Returns:
 *   2 — fiat-crypto vendored and bridge fully functional
 *   1 — stub mode (VENDOR_PENDING)
 * --------------------------------------------------------------------- */
int bridge_x25519_link_probe(void) {
#ifdef FIAT_VENDORED
    return 2;
#else
    return 1; /* VENDOR_PENDING */
#endif
}

/* -------------------------------------------------------------------------
 * Stub symbols when FIAT_VENDORED is not defined.
 * --------------------------------------------------------------------- */
#ifndef FIAT_VENDORED

void umbravox_x25519(uint8_t out[32],
                     const uint8_t scalar[32],
                     const uint8_t u_in[32]) {
    (void)scalar; (void)u_in;
    memset(out, 0, 32);
}

void umbravox_x25519_base(uint8_t out[32], const uint8_t scalar[32]) {
    (void)scalar;
    memset(out, 0, 32);
}

#endif /* !FIAT_VENDORED */

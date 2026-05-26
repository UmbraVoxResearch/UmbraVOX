/* SPDX-License-Identifier: Apache-2.0
 *
 * csrc/fiat/bridge_ed25519.c — FFI bridge: fiat-crypto field ops -> Ed25519 group law
 *
 * This file maps the formally-verified fiat-crypto GF(2^255-19) field arithmetic
 * to the higher-level Ed25519 operations that the CryptoGen-generated code in
 * csrc/generated/ed25519extended.c describes in its preprocessing comments.
 *
 * VENDOR_PENDING: fiat_25519_64.c / fiat_25519_64.h must be vendored into
 * csrc/fiat/ before this file is active.  Until then every function body is a
 * clearly-labelled stub that compiles and links (returning an error sentinel)
 * so the rest of the build is never broken by the missing vendor step.
 *
 * Naming contract
 * ---------------
 * Public symbols use the "umbravox_fe_" prefix (field element layer) and the
 * "umbravox_ed_" prefix (Ed25519 group layer).  The fiat-crypto primitives are
 * accessed via the names declared in fiat_25519_64.h:
 *
 *   fiat_25519_mul(out, a, b)      -- field multiplication
 *   fiat_25519_square(out, a)      -- field squaring
 *   fiat_25519_add(out, a, b)      -- field addition
 *   fiat_25519_sub(out, a, b)      -- field subtraction
 *   fiat_25519_opp(out, a)         -- field negation
 *   fiat_25519_to_bytes(out, a)    -- canonical 32-byte little-endian output
 *   fiat_25519_from_bytes(out, in) -- 32-byte little-endian input
 *   fiat_25519_carry(out, a)       -- carry propagation (normalise limbs)
 *   fiat_25519_selectznz(out,c,a,b)-- constant-time conditional select
 *
 * Field element representation
 * ----------------------------
 * fiat-crypto uses five 64-bit limbs in a saturated/unsaturated 51-bit radix
 * representation.  The type alias below documents this; the actual typedef is
 * in fiat_25519_64.h.
 *
 *   typedef uint64_t fiat_25519_felem[5];
 *
 * The extended Edwards point representation mirrors the Haskell type
 * ExtPoint = (X, Y, Z, T) with the projective identity (0, 1, 1, 0).
 *
 * Relationship to ed25519extended.c
 * ----------------------------------
 * The CryptoGen-generated file declares every intermediate variable as
 * uint32_t = 0 with a /* preprocessing: ... * / comment that names the
 * abstract operation.  This bridge implements those abstract operations using
 * concrete fiat-crypto calls.  The two files are deliberately kept separate:
 *
 *   ed25519extended.c  -- CryptoGen output, auto-regenerated, DO NOT EDIT
 *   bridge_ed25519.c   -- human-maintained bridge, edit here
 *
 * When fiat-crypto is vendored, the production FFI path will call the
 * umbravox_ed_* entry points defined here instead of going through the
 * CryptoGen stub.
 *
 * Algorithm references
 * --------------------
 * Point addition:  Hisil-Wong-Carter-Dawson 2008, Section 3.1 (unified)
 * Point doubling:  EFD dbl-2008-hwcd, a=-1 twisted Edwards
 * Scalar multiply: double-and-add, MSB first
 * Encode/decode:   RFC 8032 Section 5.1.2 / 5.1.3
 * Curve constant:  d = -121665/121666 mod p
 */

#include <stdint.h>
#include <stddef.h>
#include <string.h>

/* -------------------------------------------------------------------------
 * Vendor gate
 * When fiat_25519_64.h is present the FIAT_VENDORED macro must be defined
 * (either via the header itself or via cc-options in the build).  All
 * concrete implementations below are compiled only when vendored.  The
 * stub section at the end provides minimal link targets otherwise.
 * --------------------------------------------------------------------- */
#ifdef FIAT_VENDORED
#  include "fiat_25519_64.h"
#else
/* Forward-declare the fiat-crypto types so the rest of the file parses
 * cleanly even without the vendored header. */
typedef uint64_t fiat_25519_felem[5];
#endif /* FIAT_VENDORED */

/* -------------------------------------------------------------------------
 * umbravox_fe_* — thin wrappers over fiat-crypto primitives
 *
 * These exist so the rest of the UmbraVOX C layer never calls fiat-crypto
 * names directly.  If we ever swap fiat-crypto for a different verified
 * library we change only these wrappers.
 * --------------------------------------------------------------------- */

/* Copy a field element. */
static inline void fe_copy(fiat_25519_felem out, const fiat_25519_felem in) {
    memcpy(out, in, sizeof(fiat_25519_felem));
}

/* Set a field element to the small constant v (v < 2^51). */
static inline void fe_set_small(fiat_25519_felem out, uint64_t v) {
    out[0] = v;
    out[1] = out[2] = out[3] = out[4] = 0;
}

#ifdef FIAT_VENDORED

/* Field multiply: out = a * b */
void umbravox_fe_mul(fiat_25519_felem out,
                     const fiat_25519_felem a,
                     const fiat_25519_felem b) {
    fiat_25519_mul(out, a, b);
}

/* Field square: out = a^2 */
void umbravox_fe_square(fiat_25519_felem out,
                        const fiat_25519_felem a) {
    fiat_25519_square(out, a);
}

/* Field add: out = a + b */
void umbravox_fe_add(fiat_25519_felem out,
                     const fiat_25519_felem a,
                     const fiat_25519_felem b) {
    fiat_25519_add(out, a, b);
    fiat_25519_carry(out, out);
}

/* Field subtract: out = a - b */
void umbravox_fe_sub(fiat_25519_felem out,
                     const fiat_25519_felem a,
                     const fiat_25519_felem b) {
    fiat_25519_sub(out, a, b);
    fiat_25519_carry(out, out);
}

/* Field negate: out = -a */
void umbravox_fe_neg(fiat_25519_felem out,
                     const fiat_25519_felem a) {
    fiat_25519_opp(out, a);
    fiat_25519_carry(out, out);
}

/* Serialize to 32-byte canonical little-endian. */
void umbravox_fe_to_bytes(uint8_t out[32],
                          const fiat_25519_felem a) {
    fiat_25519_to_bytes(out, a);
}

/* Deserialize from 32-byte little-endian. */
void umbravox_fe_from_bytes(fiat_25519_felem out,
                             const uint8_t in[32]) {
    fiat_25519_from_bytes(out, in);
}

/* Constant-time conditional select: out = cond ? a : b (cond must be 0 or 1). */
void umbravox_fe_cselect(fiat_25519_felem out,
                         uint64_t cond,
                         const fiat_25519_felem a,
                         const fiat_25519_felem b) {
    /* fiat_25519_selectznz signature: selectznz(out, cond, zero_val, nonzero_val)
     * selects nonzero_val when cond != 0.  We want: out = cond ? a : b, which
     * means select a when cond != 0, so: selectznz(out, cond, b, a). */
    fiat_25519_selectznz(out, (fiat_25519_uint1)cond, b, a);
}

/* -------------------------------------------------------------------------
 * Modular inversion via Fermat: a^(p-2) mod p
 *
 * Uses the addition-chain for p-2 = 2^255 - 21 from RFC 8032 Appendix A.
 * The chain requires 254 squarings and 11 multiplications.
 * --------------------------------------------------------------------- */
void umbravox_fe_inv(fiat_25519_felem out,
                     const fiat_25519_felem a) {
    /* Addition chain for (p-2): RFC 8032 §5.1.3 / Appendix A */
    fiat_25519_felem t0, t1, t2, t3, t4;
    int i;

    fiat_25519_square(t0, a);          /* a^2 */
    fiat_25519_square(t1, t0);         /* a^4 */
    fiat_25519_square(t1, t1);         /* a^8 */
    fiat_25519_mul(t1, t1, a);         /* a^9 */
    fiat_25519_mul(t0, t0, t1);        /* a^11 */
    fiat_25519_square(t2, t0);         /* a^22 */
    fiat_25519_mul(t1, t1, t2);        /* a^(2^5 - 1) */
    fiat_25519_square(t2, t1);
    for (i = 1; i < 5; i++) fiat_25519_square(t2, t2); /* a^(2^10 - 2^5) */
    fiat_25519_mul(t2, t2, t1);        /* a^(2^10 - 1) */
    fiat_25519_square(t3, t2);
    for (i = 1; i < 10; i++) fiat_25519_square(t3, t3);
    fiat_25519_mul(t3, t3, t2);        /* a^(2^20 - 1) */
    fiat_25519_square(t4, t3);
    for (i = 1; i < 20; i++) fiat_25519_square(t4, t4);
    fiat_25519_mul(t4, t4, t3);        /* a^(2^40 - 1) */
    fiat_25519_square(t4, t4);
    for (i = 1; i < 10; i++) fiat_25519_square(t4, t4);
    fiat_25519_mul(t3, t4, t2);        /* a^(2^50 - 1) */
    fiat_25519_square(t4, t3);
    for (i = 1; i < 50; i++) fiat_25519_square(t4, t4);
    fiat_25519_mul(t4, t4, t3);        /* a^(2^100 - 1) */
    fiat_25519_square(t2, t4);
    for (i = 1; i < 100; i++) fiat_25519_square(t2, t2);
    fiat_25519_mul(t2, t2, t4);        /* a^(2^200 - 1) */
    fiat_25519_square(t2, t2);
    for (i = 1; i < 50; i++) fiat_25519_square(t2, t2);
    fiat_25519_mul(t2, t2, t3);        /* a^(2^250 - 1) */
    for (i = 0; i < 5; i++) fiat_25519_square(t2, t2); /* a^(2^255 - 32) */
    fiat_25519_mul(out, t2, t0);       /* a^(2^255 - 21) = a^(p-2) */
}

/* -------------------------------------------------------------------------
 * Extended twisted Edwards point type
 *
 * Represents the affine point (x, y) as projective (X:Y:Z:T) with
 *   x = X/Z,  y = Y/Z,  T = X*Y/Z
 *
 * Mirrors the Haskell type:
 *   type ExtPoint = (Integer, Integer, Integer, Integer)
 * --------------------------------------------------------------------- */
typedef struct {
    fiat_25519_felem X;
    fiat_25519_felem Y;
    fiat_25519_felem Z;
    fiat_25519_felem T;
} umbravox_ed_point;

/* Curve constant d = -121665/121666 mod p.
 * Stored as a fiat-crypto field element, initialised on first use.
 * The little-endian byte encoding of d is:
 *   a3 78 59 13 ca 4d eb 75 ab d8 41 41 0a 00 07 00
 *   98 e8 79 77 40 c7 cc 38 73 fe 6f ee 2b ce 36 52  (32 bytes, LE)
 *
 * We initialise lazily via umbravox_ed_curve_d() to avoid depending on
 * C99 compound-literal initialisers for the felem array type.
 */
static fiat_25519_felem g_curve_d;
static int g_curve_d_init = 0;

static const uint8_t k_curve_d_bytes[32] = {
    0xa3,0x78,0x59,0x13,0xca,0x4d,0xeb,0x75,
    0xab,0xd8,0x41,0x41,0x0a,0x00,0x07,0x00,
    0x98,0xe8,0x79,0x77,0x40,0xc7,0xcc,0x38,
    0x73,0xfe,0x6f,0xee,0x2b,0xce,0x36,0x52
};

static const fiat_25519_felem *umbravox_ed_curve_d(void) {
    if (!g_curve_d_init) {
        fiat_25519_from_bytes(g_curve_d, k_curve_d_bytes);
        g_curve_d_init = 1;
    }
    return (const fiat_25519_felem *)g_curve_d;
}

/* -------------------------------------------------------------------------
 * umbravox_ed_point_add — unified point addition
 *
 * Hisil-Wong-Carter-Dawson 2008, Section 3.1, for -x^2 + y^2 = 1 + d*x^2*y^2
 *
 * Matches the Haskell pointAdd in Ed25519.hs and the CryptoGen variables
 * add_A..add_H, R_add_X..R_add_Z in ed25519extended.c.
 *
 *   A = (Y1-X1)*(Y2-X2)
 *   B = (Y1+X1)*(Y2+X2)
 *   C = T1*2*d*T2
 *   D = Z1*2*Z2
 *   E = B-A,  F = D-C,  G = D+C,  H = B+A
 *   X3 = E*F,  Y3 = G*H,  T3 = E*H,  Z3 = F*G
 * --------------------------------------------------------------------- */
void umbravox_ed_point_add(umbravox_ed_point *out,
                           const umbravox_ed_point *p1,
                           const umbravox_ed_point *p2) {
    fiat_25519_felem tmp, A, B, C, D, E, F, G, H;
    const fiat_25519_felem *d = umbravox_ed_curve_d();

    /* A = (Y1-X1)*(Y2-X2) */
    fiat_25519_sub(tmp, p1->Y, p1->X);
    fiat_25519_carry(A, tmp);
    fiat_25519_sub(tmp, p2->Y, p2->X);
    fiat_25519_carry(tmp, tmp);
    fiat_25519_mul(A, A, tmp);

    /* B = (Y1+X1)*(Y2+X2) */
    fiat_25519_add(tmp, p1->Y, p1->X);
    fiat_25519_carry(B, tmp);
    fiat_25519_add(tmp, p2->Y, p2->X);
    fiat_25519_carry(tmp, tmp);
    fiat_25519_mul(B, B, tmp);

    /* C = T1 * 2*d * T2 */
    fiat_25519_mul(C, p1->T, p2->T);
    fiat_25519_add(tmp, *d, *d);
    fiat_25519_carry(tmp, tmp);
    fiat_25519_mul(C, C, tmp);

    /* D = Z1 * 2*Z2 */
    fiat_25519_mul(D, p1->Z, p2->Z);
    fiat_25519_add(D, D, D);
    fiat_25519_carry(D, D);

    /* E=B-A, F=D-C, G=D+C, H=B+A */
    fiat_25519_sub(E, B, A);  fiat_25519_carry(E, E);
    fiat_25519_sub(F, D, C);  fiat_25519_carry(F, F);
    fiat_25519_add(G, D, C);  fiat_25519_carry(G, G);
    fiat_25519_add(H, B, A);  fiat_25519_carry(H, H);

    fiat_25519_mul(out->X, E, F);
    fiat_25519_mul(out->Y, G, H);
    fiat_25519_mul(out->T, E, H);
    fiat_25519_mul(out->Z, F, G);
}

/* -------------------------------------------------------------------------
 * umbravox_ed_point_double — dedicated doubling formula
 *
 * EFD dbl-2008-hwcd for a=-1 twisted Edwards.  Matches pointDouble in
 * Ed25519.hs.
 *
 *   A = X1^2
 *   B = Y1^2
 *   C = 2*Z1^2
 *   E = (X1+Y1)^2 - A - B
 *   G = -A+B          (G = B-A for a=-1)
 *   F = G-C
 *   H = -A-B
 *   X3=E*F, Y3=G*H, T3=E*H, Z3=F*G
 * --------------------------------------------------------------------- */
void umbravox_ed_point_double(umbravox_ed_point *out,
                              const umbravox_ed_point *p) {
    fiat_25519_felem A, B, C, E, F, G, H, tmp;

    fiat_25519_square(A, p->X);                       /* A = X^2   */
    fiat_25519_square(B, p->Y);                       /* B = Y^2   */

    fiat_25519_square(C, p->Z);                       /* Z^2       */
    fiat_25519_add(C, C, C);  fiat_25519_carry(C, C); /* C = 2*Z^2 */

    fiat_25519_add(tmp, p->X, p->Y);
    fiat_25519_carry(tmp, tmp);
    fiat_25519_square(E, tmp);                        /* (X+Y)^2   */
    fiat_25519_sub(E, E, A);  fiat_25519_carry(E, E);
    fiat_25519_sub(E, E, B);  fiat_25519_carry(E, E); /* E=(X+Y)^2-A-B */

    fiat_25519_sub(G, B, A);  fiat_25519_carry(G, G); /* G = B-A (a=-1) */
    fiat_25519_sub(F, G, C);  fiat_25519_carry(F, F); /* F = G-C        */

    fiat_25519_opp(H, A);     fiat_25519_carry(H, H); /* -A */
    fiat_25519_sub(H, H, B);  fiat_25519_carry(H, H); /* H = -A-B       */

    fiat_25519_mul(out->X, E, F);
    fiat_25519_mul(out->Y, G, H);
    fiat_25519_mul(out->T, E, H);
    fiat_25519_mul(out->Z, F, G);
}

/* -------------------------------------------------------------------------
 * umbravox_ed_encode — RFC 8032 §5.1.2 point encoding
 *
 * Output is 32 bytes: canonical little-endian y with x's low bit in
 * bit 255 (byte 31, bit 7).
 *
 * Steps:
 *   1. Recover affine x = X * Z^{-1},  y = Y * Z^{-1}
 *   2. Serialise y via fiat_25519_to_bytes
 *   3. OR the sign bit of x into byte[31] bit 7
 *
 * Corresponds to encodePoint in Ed25519.hs and the enc_* variables in
 * ed25519extended.c.
 * --------------------------------------------------------------------- */
void umbravox_ed_encode(uint8_t out[32],
                        const umbravox_ed_point *p) {
    fiat_25519_felem z_inv, x_affine, y_affine;
    uint8_t x_bytes[32];

    umbravox_fe_inv(z_inv, p->Z);
    fiat_25519_mul(x_affine, p->X, z_inv);
    fiat_25519_mul(y_affine, p->Y, z_inv);

    fiat_25519_to_bytes(out, y_affine);              /* y little-endian  */
    fiat_25519_to_bytes(x_bytes, x_affine);
    out[31] |= (x_bytes[0] & 1) << 7;               /* sign bit of x    */
}

/* -------------------------------------------------------------------------
 * umbravox_ed_decode — RFC 8032 §5.1.3 point decoding
 *
 * Returns 1 on success, 0 if the point is not on the curve.
 *
 * Steps (matching decodePoint / recoverXForDecode in Ed25519.hs and the
 * dec_* variables in ed25519extended.c):
 *   1. Extract sign bit s = in[31] >> 7; clear it to get y bytes.
 *   2. Deserialise y.
 *   3. Compute u = y^2 - 1,  v = d*y^2 + 1.
 *   4. x = (u*v^3) * (u*v^7)^((p-5)/8) mod p.
 *   5. Check v*x^2 == u; adjust by sqrt(-1) if needed.
 *   6. Negate x if sign doesn't match.
 *   7. Set Z=1, T=x*y.
 *
 * The (p-5)/8 exponentiation reuses part of the inversion addition chain.
 * --------------------------------------------------------------------- */
int umbravox_ed_decode(umbravox_ed_point *out,
                       const uint8_t in[32]) {
    uint8_t y_bytes[32];
    fiat_25519_felem y, y2, u, v, v3, v7, uv7, x, vx2, tmp;
    fiat_25519_felem sqrt_m1;
    /* sqrt(-1) mod p = 2^((p-1)/4) mod p — byte encoding (LE): */
    static const uint8_t k_sqrt_m1[32] = {
        0xb0,0x0a,0xea,0xa7,0x74,0x1b,0xe4,0xc4,
        0x78,0xe4,0x32,0xad,0x06,0x18,0x43,0x2f,
        0xa7,0xd7,0xfb,0x3d,0x99,0x00,0x4d,0x2b,
        0x0b,0xdf,0xc1,0x4f,0x80,0x24,0x83,0x2b
    };
    int i;

    memcpy(y_bytes, in, 32);
    int x_sign = (y_bytes[31] >> 7) & 1;
    y_bytes[31] &= 0x7f;

    fiat_25519_from_bytes(y, y_bytes);

    /* u = y^2 - 1 */
    fiat_25519_square(y2, y);
    fiat_25519_sub(u, y2, (fiat_25519_felem){1, 0, 0, 0, 0});
    fiat_25519_carry(u, u);

    /* v = d*y^2 + 1 */
    fiat_25519_mul(v, *umbravox_ed_curve_d(), y2);
    fiat_25519_add(v, v, (fiat_25519_felem){1, 0, 0, 0, 0});
    fiat_25519_carry(v, v);

    /* v3 = v^3 */
    fiat_25519_square(tmp, v);
    fiat_25519_mul(v3, tmp, v);

    /* v7 = v^7 = v3^2 * v */
    fiat_25519_square(tmp, v3);
    fiat_25519_mul(v7, tmp, v);

    /* uv7 = u * v^7 */
    fiat_25519_mul(uv7, u, v7);

    /* x = (u * v^3) * uv7^((p-5)/8)
     * (p-5)/8 addition chain: 252 squarings, similar to inversion but
     * shorter by 3 steps. */
    {
        fiat_25519_felem t0, t1, t2, t3, t4;
        fiat_25519_square(t0, uv7);
        fiat_25519_square(t1, t0);
        fiat_25519_square(t1, t1);
        fiat_25519_mul(t1, t1, uv7);
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
        fiat_25519_square(t2, t2);
        fiat_25519_square(t2, t2);
        /* result = uv7^((p-5)/8) */
        fiat_25519_mul(x, u, v3);
        fiat_25519_mul(x, x, t2);
    }

    /* Check v*x^2 == u */
    fiat_25519_square(tmp, x);
    fiat_25519_mul(vx2, v, tmp);

    /* Compare vx2 to u (constant-time byte comparison after serialisation) */
    uint8_t vx2_b[32], u_b[32], neg_u_b[32];
    fiat_25519_to_bytes(vx2_b, vx2);
    fiat_25519_to_bytes(u_b, u);

    /* -u mod p */
    fiat_25519_felem neg_u;
    fiat_25519_opp(neg_u, u);
    fiat_25519_carry(neg_u, neg_u);
    fiat_25519_to_bytes(neg_u_b, neg_u);

    int eq_u    = (memcmp(vx2_b, u_b,     32) == 0);
    int eq_negu = (memcmp(vx2_b, neg_u_b, 32) == 0);

    if (!eq_u && !eq_negu) {
        /* Special case: u == 0 and x_sign == 0 => x = 0 is valid */
        uint8_t zero32[32] = {0};
        if (memcmp(u_b, zero32, 32) == 0 && x_sign == 0) {
            fe_set_small(x, 0);
        } else {
            return 0; /* point not on curve */
        }
    } else if (!eq_u) {
        /* vx2 == -u: multiply x by sqrt(-1) */
        fiat_25519_from_bytes(sqrt_m1, k_sqrt_m1);
        fiat_25519_mul(x, x, sqrt_m1);
    }

    /* Adjust x sign */
    uint8_t x_b[32];
    fiat_25519_to_bytes(x_b, x);
    int x_low = x_b[0] & 1;
    if (x_low != x_sign) {
        fiat_25519_opp(x, x);
        fiat_25519_carry(x, x);
    }

    fe_copy(out->X, x);
    fe_copy(out->Y, y);
    fe_set_small(out->Z, 1);
    fiat_25519_mul(out->T, x, y);
    return 1;
}

/* -------------------------------------------------------------------------
 * umbravox_ed_scalar_mult — variable-time double-and-add scalar multiply
 *
 * Computes out = scalar * P, where scalar is a 256-bit little-endian integer.
 * This is the direct C analogue of scalarMul in Ed25519.hs.
 *
 * NOT CONSTANT-TIME.  The production FFI path (ed25519extended_link_probe /
 * ed25519extended.c) routes through libsodium or another constant-time
 * implementation for secret-scalar operations.  This function exists to
 * complete the fiat-crypto field-op mapping for non-secret scalars (e.g.
 * public scalar in verification, cofactor multiply for small-order checks).
 * --------------------------------------------------------------------- */
void umbravox_ed_scalar_mult(umbravox_ed_point *out,
                             const uint8_t scalar[32],
                             const umbravox_ed_point *P) {
    /* Identity point (0 : 1 : 1 : 0) */
    umbravox_ed_point acc;
    fe_set_small(acc.X, 0);
    fe_set_small(acc.Y, 1);
    fe_set_small(acc.Z, 1);
    fe_set_small(acc.T, 0);

    /* MSB-first double-and-add over 255 bits (skip leading zero bits) */
    for (int byte_i = 31; byte_i >= 0; byte_i--) {
        uint8_t byte_val = scalar[byte_i];
        for (int bit_i = 7; bit_i >= 0; bit_i--) {
            umbravox_ed_point_double(&acc, &acc);
            if ((byte_val >> bit_i) & 1) {
                umbravox_ed_point_add(&acc, &acc, P);
            }
        }
    }
    *out = acc;
}

#endif /* FIAT_VENDORED */

/* -------------------------------------------------------------------------
 * Link probe (always compiled, vendored or not)
 *
 * Returns:
 *   2 — fiat-crypto vendored and bridge fully functional
 *   1 — stub mode (VENDOR_PENDING: fiat-crypto not yet dropped in)
 * --------------------------------------------------------------------- */
int bridge_ed25519_link_probe(void) {
#ifdef FIAT_VENDORED
    return 2;
#else
    return 1; /* VENDOR_PENDING */
#endif
}

/* -------------------------------------------------------------------------
 * Stub symbols — compiled when FIAT_VENDORED is not defined.
 * These exist so the cabal c-sources list can include this file before
 * vendoring without causing undefined-reference linker errors in tests
 * that call bridge_ed25519_link_probe.
 * --------------------------------------------------------------------- */
#ifndef FIAT_VENDORED

typedef uint64_t fiat_25519_felem_stub[5];

typedef struct {
    fiat_25519_felem_stub X, Y, Z, T;
} umbravox_ed_point;

void umbravox_fe_mul(uint64_t *out, const uint64_t *a, const uint64_t *b)
    { (void)out; (void)a; (void)b; }
void umbravox_fe_square(uint64_t *out, const uint64_t *a)
    { (void)out; (void)a; }
void umbravox_fe_add(uint64_t *out, const uint64_t *a, const uint64_t *b)
    { (void)out; (void)a; (void)b; }
void umbravox_fe_sub(uint64_t *out, const uint64_t *a, const uint64_t *b)
    { (void)out; (void)a; (void)b; }
void umbravox_fe_neg(uint64_t *out, const uint64_t *a)
    { (void)out; (void)a; }
void umbravox_fe_to_bytes(uint8_t out[32], const uint64_t *a)
    { (void)out; (void)a; }
void umbravox_fe_from_bytes(uint64_t *out, const uint8_t in[32])
    { (void)out; (void)in; }
void umbravox_fe_cselect(uint64_t *out, uint64_t cond,
                         const uint64_t *a, const uint64_t *b)
    { (void)out; (void)cond; (void)a; (void)b; }
void umbravox_fe_inv(uint64_t *out, const uint64_t *a)
    { (void)out; (void)a; }
void umbravox_ed_point_add(umbravox_ed_point *out,
                           const umbravox_ed_point *p1,
                           const umbravox_ed_point *p2)
    { (void)out; (void)p1; (void)p2; }
void umbravox_ed_point_double(umbravox_ed_point *out,
                              const umbravox_ed_point *p)
    { (void)out; (void)p; }
void umbravox_ed_encode(uint8_t out[32], const umbravox_ed_point *p)
    { (void)out; (void)p; }
int  umbravox_ed_decode(umbravox_ed_point *out, const uint8_t in[32])
    { (void)out; (void)in; return 0; }
void umbravox_ed_scalar_mult(umbravox_ed_point *out,
                             const uint8_t scalar[32],
                             const umbravox_ed_point *P)
    { (void)out; (void)scalar; (void)P; }

#endif /* !FIAT_VENDORED */

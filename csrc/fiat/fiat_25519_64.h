/* STUB — replace with vendored fiat-crypto distribution.
 *
 * fiat_25519_64.h — stub header for fiat-crypto GF(2^255-19) field arithmetic,
 * 64-bit limb variant (5 limbs, 51-bit radix).
 *
 * This file is a placeholder that allows bridge_ed25519.c, bridge_x25519.c,
 * and ffi_bridge.h to compile and link as no-ops until the real fiat-crypto
 * distribution is dropped into csrc/fiat/.
 *
 * fiat-crypto source: https://github.com/mit-plv/fiat-crypto
 * Replace this file with: fiat_25519_64.c + fiat_25519_64.h from the vendored
 * distribution (typically under src/ExtractionOCaml/ or a pre-extracted copy).
 *
 * FIELD ELEMENT REPRESENTATION (64-bit)
 *   Limb count : 5
 *   Radix      : 2^51 (unsaturated / saturated depending on tightness)
 *   Tight      : each limb in [0, 2^51)   — invariant after carry
 *   Loose      : each limb may have one extra carry's worth of headroom
 *
 * All stubs are declared as extern (not defined here) so that an attempt to
 * link without the real implementation produces clear undefined-reference
 * errors rather than silently operating as no-ops.  The bridge .c files
 * already gate all concrete uses behind #ifdef FIAT_VENDORED; these
 * declarations exist solely to satisfy the compiler when that guard is absent.
 */
#pragma once
#include <stdint.h>
#include <stddef.h>

/* -------------------------------------------------------------------------
 * Scalar bit type: 0 or 1, carried as a full uint64_t word.
 * --------------------------------------------------------------------- */
typedef uint64_t fiat_25519_uint1;

/* -------------------------------------------------------------------------
 * Field element array types.
 *
 * Both tight and loose representations use 5 × uint64_t limbs.
 * The distinction is a semantic / precondition annotation, not a structural
 * one; both are compatible with plain uint64_t[5] at the C level.
 * --------------------------------------------------------------------- */
typedef uint64_t fiat_25519_tight_field_element[5];
typedef uint64_t fiat_25519_loose_field_element[5];

/* Convenience alias used by bridge_ed25519.c and bridge_x25519.c */
typedef uint64_t fiat_25519_limb_t;

/* Uniform field element alias — structurally identical to tight/loose variants.
 * Used throughout bridge_ed25519.c and bridge_x25519.c as a single array type. */
typedef uint64_t fiat_25519_felem[5];

/* -------------------------------------------------------------------------
 * Field operations — declared here, defined in the vendored .c file.
 *
 * Tight / Loose precondition notes (mirroring the real fiat-crypto header):
 *   mul, square  : tight <- loose, loose
 *   add, sub, opp: loose <- tight, tight  (opp is unary)
 *   carry        : tight <- loose
 *   to_bytes     : requires tight input
 *   from_bytes   : produces tight output
 *   selectznz    : cond must be 0 or 1; inputs and output are tight
 * --------------------------------------------------------------------- */

/*
 * fiat_25519_mul — field multiplication: out = a * b
 */
void fiat_25519_mul(
    fiat_25519_tight_field_element out,
    const fiat_25519_loose_field_element a,
    const fiat_25519_loose_field_element b);

/*
 * fiat_25519_square — field squaring: out = a^2
 */
void fiat_25519_square(
    fiat_25519_tight_field_element out,
    const fiat_25519_loose_field_element a);

/*
 * fiat_25519_add — field addition: out = a + b  (loose output)
 */
void fiat_25519_add(
    fiat_25519_loose_field_element out,
    const fiat_25519_tight_field_element a,
    const fiat_25519_tight_field_element b);

/*
 * fiat_25519_sub — field subtraction: out = a - b  (loose output)
 */
void fiat_25519_sub(
    fiat_25519_loose_field_element out,
    const fiat_25519_tight_field_element a,
    const fiat_25519_tight_field_element b);

/*
 * fiat_25519_opp — field negation: out = -a  (loose output)
 */
void fiat_25519_opp(
    fiat_25519_loose_field_element out,
    const fiat_25519_tight_field_element a);

/*
 * fiat_25519_carry — carry propagation / normalisation: out = carry(a)
 * Converts a loose element to a tight element.
 */
void fiat_25519_carry(
    fiat_25519_tight_field_element out,
    const fiat_25519_loose_field_element a);

/*
 * fiat_25519_to_bytes — canonical little-endian serialisation (32 bytes).
 * Input must be tight.
 */
void fiat_25519_to_bytes(
    uint8_t out[32],
    const fiat_25519_tight_field_element a);

/*
 * fiat_25519_from_bytes — deserialise 32 little-endian bytes into a tight
 * field element.  The top bit of in[31] is silently masked per RFC 7748.
 */
void fiat_25519_from_bytes(
    fiat_25519_tight_field_element out,
    const uint8_t in[32]);

/*
 * fiat_25519_selectznz — constant-time conditional select.
 *
 *   out  = (cond == 0) ? z : nz
 *   cond : must be exactly 0 or 1 (fiat_25519_uint1)
 */
void fiat_25519_selectznz(
    fiat_25519_tight_field_element out,
    fiat_25519_uint1 cond,
    const fiat_25519_tight_field_element z,
    const fiat_25519_tight_field_element nz);

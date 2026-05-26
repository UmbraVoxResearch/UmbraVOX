/* SPDX-License-Identifier: Apache-2.0 */
/*
 * ffi_bridge.h — Name-mapping shim from fiat-crypto to UmbraVox FFI
 *
 * fiat-crypto exports functions in the fiat_25519_* namespace.  Our Haskell
 * FFI layer (UmbraVox.Crypto.Field25519) uses the umbravox_fe_* namespace so
 * that the call sites remain stable across possible upstream renames and so
 * that grep for "umbravox_" finds all cross-language entry points in one pass.
 *
 * Every wrapper here is a trivially-inlined forwarder.  The compiler will
 * eliminate the indirection entirely; no ABI overhead is introduced.
 *
 * FIELD ELEMENT REPRESENTATION
 * fiat-crypto uses a limb array typedef:
 *   typedef uint64_t fiat_25519_uint1;   (single bit, 0 or 1)
 *   typedef uint64_t fiat_25519_loose_field_element[5];
 *   typedef uint64_t fiat_25519_tight_field_element[5];
 *
 * A "tight" element has each limb fully reduced; a "loose" element may have
 * one extra carry's worth of headroom.  Most operations consume loose inputs
 * and produce tight outputs; see the upstream header for the exact contract on
 * each function.
 *
 * For 32-bit targets (illumos/SmartOS 32-bit, older BSDs) include
 * fiat_25519_32.h instead and define FIAT_25519_32BIT before this header.
 *
 * USAGE
 *   #include "fiat_25519_64.h"   // (or 32-bit variant)
 *   #include "ffi_bridge.h"
 *
 * The Haskell module UmbraVox.Crypto.Field25519 imports:
 *   foreign import ccall "umbravox_fe_mul"        c_fe_mul        :: ...
 *   foreign import ccall "umbravox_fe_square"     c_fe_square     :: ...
 *   foreign import ccall "umbravox_fe_add"        c_fe_add        :: ...
 *   foreign import ccall "umbravox_fe_sub"        c_fe_sub        :: ...
 *   foreign import ccall "umbravox_fe_neg"        c_fe_neg        :: ...
 *   foreign import ccall "umbravox_fe_carry"      c_fe_carry      :: ...
 *   foreign import ccall "umbravox_fe_to_bytes"   c_fe_to_bytes   :: ...
 *   foreign import ccall "umbravox_fe_from_bytes" c_fe_from_bytes :: ...
 *   foreign import ccall "umbravox_fe_selectznz"  c_fe_selectznz  :: ...
 */

#ifndef UMBRAVOX_FFI_BRIDGE_H
#define UMBRAVOX_FFI_BRIDGE_H

#ifdef FIAT_25519_32BIT
#  include "fiat_25519_32.h"
#  define FIAT_FE_TYPE  fiat_25519_uint32
/* 32-bit limb arrays use 10 limbs */
#  define FIAT_TIGHT    fiat_25519_tight_field_element
#  define FIAT_LOOSE    fiat_25519_loose_field_element
#else
#  include "fiat_25519_64.h"
#  define FIAT_FE_TYPE  fiat_25519_uint1  /* underlying word type */
/* 64-bit limb arrays use 5 limbs */
#  define FIAT_TIGHT    fiat_25519_tight_field_element
#  define FIAT_LOOSE    fiat_25519_loose_field_element
#endif

#include <stdint.h>

/* ---------------------------------------------------------------------------
 * Field multiplication: out = a * b  (tight <- loose, loose)
 * fiat name: fiat_25519_mul
 * ------------------------------------------------------------------------- */
static inline void
umbravox_fe_mul(FIAT_TIGHT out,
                const FIAT_LOOSE a,
                const FIAT_LOOSE b)
{
    fiat_25519_mul(out, a, b);
}

/* ---------------------------------------------------------------------------
 * Field squaring: out = a^2  (tight <- loose)
 * fiat name: fiat_25519_square
 * ------------------------------------------------------------------------- */
static inline void
umbravox_fe_square(FIAT_TIGHT out,
                   const FIAT_LOOSE a)
{
    fiat_25519_square(out, a);
}

/* ---------------------------------------------------------------------------
 * Field addition: out = a + b  (loose <- tight, tight)
 * fiat name: fiat_25519_add
 * ------------------------------------------------------------------------- */
static inline void
umbravox_fe_add(FIAT_LOOSE out,
                const FIAT_TIGHT a,
                const FIAT_TIGHT b)
{
    fiat_25519_add(out, a, b);
}

/* ---------------------------------------------------------------------------
 * Field subtraction: out = a - b  (loose <- tight, tight)
 * fiat name: fiat_25519_sub
 * ------------------------------------------------------------------------- */
static inline void
umbravox_fe_sub(FIAT_LOOSE out,
                const FIAT_TIGHT a,
                const FIAT_TIGHT b)
{
    fiat_25519_sub(out, a, b);
}

/* ---------------------------------------------------------------------------
 * Field negation: out = -a  (loose <- tight)
 * fiat name: fiat_25519_opp
 * Note: fiat calls this "opp" (opposite); we expose it as "neg" for clarity.
 * ------------------------------------------------------------------------- */
static inline void
umbravox_fe_neg(FIAT_LOOSE out,
                const FIAT_TIGHT a)
{
    fiat_25519_opp(out, a);
}

/* ---------------------------------------------------------------------------
 * Carry propagation: out = carry-reduced(a)  (tight <- loose)
 * fiat name: fiat_25519_carry
 * Call after add/sub chains to maintain the tight invariant before mul/sqr.
 * ------------------------------------------------------------------------- */
static inline void
umbravox_fe_carry(FIAT_TIGHT out,
                  const FIAT_LOOSE a)
{
    fiat_25519_carry(out, a);
}

/* ---------------------------------------------------------------------------
 * Canonical serialization: encode a field element to 32 bytes (little-endian)
 * fiat name: fiat_25519_to_bytes
 * Input must be tight.
 * ------------------------------------------------------------------------- */
static inline void
umbravox_fe_to_bytes(uint8_t out[32],
                     const FIAT_TIGHT a)
{
    fiat_25519_to_bytes(out, a);
}

/* ---------------------------------------------------------------------------
 * Deserialization: decode 32 bytes (little-endian) into a field element
 * fiat name: fiat_25519_from_bytes
 * Output is tight; the top bit of the input is silently masked per RFC 7748.
 * ------------------------------------------------------------------------- */
static inline void
umbravox_fe_from_bytes(FIAT_TIGHT out,
                       const uint8_t in[32])
{
    fiat_25519_from_bytes(out, in);
}

/* ---------------------------------------------------------------------------
 * Constant-time conditional select: out = (cond ? nz : z)
 * fiat name: fiat_25519_selectznz
 * cond must be 0 or 1; no other values are defined.
 * Both tight and loose arrays share the same element count, so this works
 * on either representation — caller is responsible for consistency.
 * ------------------------------------------------------------------------- */
static inline void
umbravox_fe_selectznz(FIAT_TIGHT out,
                      fiat_25519_uint1 cond,
                      const FIAT_TIGHT z,
                      const FIAT_TIGHT nz)
{
    fiat_25519_selectznz(out, cond, z, nz);
}

/* ---------------------------------------------------------------------------
 * Undef internal macros to avoid polluting downstream translation units.
 * ------------------------------------------------------------------------- */
#undef FIAT_FE_TYPE
#undef FIAT_TIGHT
#undef FIAT_LOOSE

#endif /* UMBRAVOX_FFI_BRIDGE_H */

/* Copyright (c) INRIA and Microsoft Corporation. All rights reserved.
   Licensed under the Apache 2.0 and MIT Licenses. */

/* UmbraVOX vendored stub for HACL* krml/internal/fstar_uint128_gcc64.h
 *
 * This file provides the FStar.UInt128 inline implementations for compilers
 * that support native 128-bit integers via __uint128_t / unsigned __int128.
 * It is included from krml/internal/types.h when HAS_INT128 is defined
 * (GCC/Clang on x86_64, aarch64, etc.).
 *
 * At this point in the inclusion chain:
 *   - uint128_t == FStar_UInt128_uint128 == unsigned __int128  (from types.h)
 *   - load64_le / store64_le etc. are available  (from krml/lowstar_endianness.h)
 *
 * We define FStar_UInt64_eq_mask and FStar_UInt64_gte_mask locally (guarded)
 * because FStar_UInt_8_16_32_64.h may not have been included yet.
 */

#ifndef FSTAR_UINT128_GCC64
#define FSTAR_UINT128_GCC64

#include <stdint.h>
#include <stdbool.h>

/* --------------------------------------------------------------------------
 * FStar.UInt64 mask helpers (duplicated here in case the main header hasn't
 * been included yet; the definitions are identical to FStar_UInt_8_16_32_64.h)
 * -------------------------------------------------------------------------- */

#ifndef FSTAR_UINT64_EQ_MASK_DEFINED
#define FSTAR_UINT64_EQ_MASK_DEFINED
static inline uint64_t
FStar_UInt128_internal_u64_eq_mask(uint64_t a, uint64_t b)
{
  uint64_t x = a ^ b;
  uint64_t minus_x = ~x + 1ULL;
  uint64_t x_or_minus_x = x | minus_x;
  uint64_t xnx = x_or_minus_x >> 63U;
  return xnx - 1ULL;
}

static inline uint64_t
FStar_UInt128_internal_u64_gte_mask(uint64_t a, uint64_t b)
{
  uint64_t x = a;
  uint64_t y = b;
  uint64_t x_xor_y = x ^ y;
  uint64_t x_sub_y = x - y;
  uint64_t x_sub_y_xor_y = x_sub_y ^ y;
  uint64_t q = x_xor_y | x_sub_y_xor_y;
  uint64_t x_xor_q = x ^ q;
  uint64_t x_xor_q_ = x_xor_q >> 63U;
  return x_xor_q_ - 1ULL;
}
#endif /* FSTAR_UINT64_EQ_MASK_DEFINED */

/* --------------------------------------------------------------------------
 * 128-bit load / store  (using the native __int128 representation)
 * -------------------------------------------------------------------------- */

static inline uint128_t load128_le(uint8_t *b)
{
  uint128_t l = (uint128_t)load64_le(b);
  uint128_t h = (uint128_t)load64_le(b + 8);
  return (h << 64U) | l;
}

static inline void store128_le(uint8_t *b, uint128_t n)
{
  store64_le(b,     (uint64_t)n);
  store64_le(b + 8, (uint64_t)(n >> 64U));
}

static inline uint128_t load128_be(uint8_t *b)
{
  uint128_t h = (uint128_t)load64_be(b);
  uint128_t l = (uint128_t)load64_be(b + 8);
  return (h << 64U) | l;
}

static inline void store128_be(uint8_t *b, uint128_t n)
{
  store64_be(b,     (uint64_t)(n >> 64U));
  store64_be(b + 8, (uint64_t)n);
}

/* Aliases used by some HACL* files */
#define load128_le0  load128_le
#define store128_le0 store128_le
#define load128_be0  load128_be
#define store128_be0 store128_be

/* --------------------------------------------------------------------------
 * FStar.UInt128 arithmetic, implemented via native unsigned __int128
 * -------------------------------------------------------------------------- */

static inline uint128_t FStar_UInt128_add(uint128_t x, uint128_t y)
  { return x + y; }

static inline uint128_t FStar_UInt128_add_underspec(uint128_t x, uint128_t y)
  { return x + y; }

static inline uint128_t FStar_UInt128_add_mod(uint128_t x, uint128_t y)
  { return x + y; }

static inline uint128_t FStar_UInt128_sub(uint128_t x, uint128_t y)
  { return x - y; }

static inline uint128_t FStar_UInt128_sub_underspec(uint128_t x, uint128_t y)
  { return x - y; }

static inline uint128_t FStar_UInt128_sub_mod(uint128_t x, uint128_t y)
  { return x - y; }

static inline uint128_t FStar_UInt128_logand(uint128_t x, uint128_t y)
  { return x & y; }

static inline uint128_t FStar_UInt128_logor(uint128_t x, uint128_t y)
  { return x | y; }

static inline uint128_t FStar_UInt128_logxor(uint128_t x, uint128_t y)
  { return x ^ y; }

static inline uint128_t FStar_UInt128_lognot(uint128_t x)
  { return ~x; }

static inline uint128_t FStar_UInt128_shift_left(uint128_t x, uint32_t y)
  { return x << y; }

static inline uint128_t FStar_UInt128_shift_right(uint128_t x, uint32_t y)
  { return x >> y; }

static inline bool FStar_UInt128_eq(uint128_t x, uint128_t y)
  { return x == y; }

static inline bool FStar_UInt128_gt(uint128_t x, uint128_t y)
  { return x > y; }

static inline bool FStar_UInt128_lt(uint128_t x, uint128_t y)
  { return x < y; }

static inline bool FStar_UInt128_gte(uint128_t x, uint128_t y)
  { return x >= y; }

static inline bool FStar_UInt128_lte(uint128_t x, uint128_t y)
  { return x <= y; }

static inline uint128_t FStar_UInt128_eq_mask(uint128_t x, uint128_t y)
{
  uint64_t mask =
    FStar_UInt128_internal_u64_eq_mask((uint64_t)(x >> 64U), (uint64_t)(y >> 64U)) &
    FStar_UInt128_internal_u64_eq_mask((uint64_t)x,          (uint64_t)y);
  return ((uint128_t)mask) << 64U | mask;
}

static inline uint128_t FStar_UInt128_gte_mask(uint128_t x, uint128_t y)
{
  uint64_t mask =
    (FStar_UInt128_internal_u64_gte_mask(x >> 64U, y >> 64U) &
     ~FStar_UInt128_internal_u64_eq_mask(x >> 64U, y >> 64U)) |
    (FStar_UInt128_internal_u64_eq_mask(x >> 64U, y >> 64U) &
     FStar_UInt128_internal_u64_gte_mask((uint64_t)x, (uint64_t)y));
  return ((uint128_t)mask) << 64U | mask;
}

static inline uint128_t FStar_UInt128_uint64_to_uint128(uint64_t x)
  { return (uint128_t)x; }

static inline uint64_t FStar_UInt128_uint128_to_uint64(uint128_t x)
  { return (uint64_t)x; }

static inline uint128_t FStar_UInt128_mul32(uint64_t x, uint32_t y)
  { return (uint128_t)x * (uint128_t)y; }

static inline uint128_t FStar_UInt128_mul_wide(uint64_t x, uint64_t y)
  { return (uint128_t)x * (uint128_t)y; }

static inline uint128_t FStar_UInt128_mul(uint128_t x, uint128_t y)
  { return x * y; }

/* Struct accessors used by some older HACL* generated code */
static inline uint64_t
FStar_UInt128___proj__Mkuint128__item__low(uint128_t x)
  { return (uint64_t)x; }

static inline uint64_t
FStar_UInt128___proj__Mkuint128__item__high(uint128_t x)
  { return (uint64_t)(x >> 64U); }

#endif /* FSTAR_UINT128_GCC64 */

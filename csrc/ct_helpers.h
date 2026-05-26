/* SPDX-License-Identifier: Apache-2.0 */
/* Portable constant-time helper operations for generated C code.
 *
 * All functions use bitwise masking to avoid data-dependent branches,
 * ensuring execution time is independent of operand values.
 */
#ifndef CT_HELPERS_H
#define CT_HELPERS_H

#include <stdint.h>

/* Select a if cond is 1, b if cond is 0 (32-bit). */
static inline uint32_t ct_select32(uint32_t cond, uint32_t a, uint32_t b) {
    uint32_t mask = -(cond & 1);
    uint32_t result = (a & mask) | (b & ~mask);
    __asm__ __volatile__("" ::: "memory");
    return result;
}

/* Select a if cond is 1, b if cond is 0 (64-bit). */
static inline uint64_t ct_select64(uint64_t cond, uint64_t a, uint64_t b) {
    uint64_t mask = -(cond & 1);
    uint64_t result = (a & mask) | (b & ~mask);
    __asm__ __volatile__("" ::: "memory");
    return result;
}

/* Return 1 if a == b, 0 otherwise (32-bit). */
static inline uint32_t ct_eq32(uint32_t a, uint32_t b) {
    uint32_t x = a ^ b;
    uint32_t result = ((uint32_t)(-(int32_t)x) >> 31) ^ 1;
    __asm__ __volatile__("" ::: "memory");
    return result;
}

/* Return 1 if a < b, 0 otherwise (32-bit, unsigned).
   Widening to uint64_t before subtracting avoids any signed overflow or
   implementation-defined right-shift behaviour; bit 63 of the difference
   is set iff a < b. */
static inline uint32_t ct_lt32(uint32_t a, uint32_t b) {
    uint64_t diff = (uint64_t)a - (uint64_t)b;
    uint32_t result = (uint32_t)((diff >> 63) & 1);
    __asm__ __volatile__("" ::: "memory");
    return result;
}

/* Return 1 if a == 0, 0 otherwise (32-bit). */
static inline uint32_t ct_eq_zero32(uint32_t a) {
    return ct_eq32(a, 0);
}

/* Return 1 if a >= b, 0 otherwise (32-bit, unsigned). */
static inline uint32_t ct_gte32(uint32_t a, uint32_t b) {
    return ct_lt32(a, b) ^ 1;
}

/* Conditional swap: if cond is 1, swap *a and *b; otherwise no-op (32-bit). */
static inline void ct_cswap32(uint32_t cond, uint32_t *a, uint32_t *b) {
    uint32_t mask = -(cond & 1);
    uint32_t t = mask & (*a ^ *b);
    *a ^= t;
    *b ^= t;
}

/* Conditional swap (64-bit). */
static inline void ct_cswap64(uint64_t cond, uint64_t *a, uint64_t *b) {
    uint64_t mask = -(cond & 1);
    uint64_t t = mask & (*a ^ *b);
    *a ^= t;
    *b ^= t;
}

/* Conditional move: if cond is 1, set *dst = src; otherwise no-op (32-bit). */
static inline void ct_cmov32(uint32_t cond, uint32_t *dst, uint32_t src) {
    uint32_t mask = -(cond & 1);
    *dst ^= mask & (*dst ^ src);
}

/* Conditional move (64-bit). */
static inline void ct_cmov64(uint64_t cond, uint64_t *dst, uint64_t src) {
    uint64_t mask = -(cond & 1);
    *dst ^= mask & (*dst ^ src);
}

#endif /* CT_HELPERS_H */

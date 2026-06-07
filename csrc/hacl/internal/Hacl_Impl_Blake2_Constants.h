/* MIT License
 *
 * Copyright (c) 2016-2022 INRIA, CMU and Microsoft Corporation
 * Copyright (c) 2022-2023 HACL* Contributors
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/* Third-party: HACL* (hacl-star/hacl-star commit 504c298, MIT/Apache-2.0)
 * Reconstructed from upstream HACL* dist/gcc-compatible sources.
 * BLAKE2 constants: sigma permutation table, BLAKE2b IVs, BLAKE2s IVs.
 * See contrib/oracles/THIRD_PARTY_LICENSES.md for provenance.
 */

#ifndef internal_Hacl_Impl_Blake2_Constants_H
#define internal_Hacl_Impl_Blake2_Constants_H

#include "krml/internal/types.h"

/* BLAKE2 sigma permutation table — 10 rounds × 16 message indices (RFC 7693 §2.7).
 * Used by both BLAKE2b and BLAKE2s mixing functions.
 * Defined as static const so this header can be included by multiple translation units. */
static const uint32_t Hacl_Hash_Blake2b_sigmaTable[160U] = {
  /* round 0 */  0U,  1U,  2U,  3U,  4U,  5U,  6U,  7U,  8U,  9U, 10U, 11U, 12U, 13U, 14U, 15U,
  /* round 1 */ 14U, 10U,  4U,  8U,  9U, 15U, 13U,  6U,  1U, 12U,  0U,  2U, 11U,  7U,  5U,  3U,
  /* round 2 */ 11U,  8U, 12U,  0U,  5U,  2U, 15U, 13U, 10U, 14U,  3U,  6U,  7U,  1U,  9U,  4U,
  /* round 3 */  7U,  9U,  3U,  1U, 13U, 12U, 11U, 14U,  2U,  6U,  5U, 10U,  4U,  0U, 15U,  8U,
  /* round 4 */  9U,  0U,  5U,  7U,  2U,  4U, 10U, 15U, 14U,  1U, 11U, 12U,  6U,  8U,  3U, 13U,
  /* round 5 */  2U, 12U,  6U, 10U,  0U, 11U,  8U,  3U,  4U, 13U,  7U,  5U, 15U, 14U,  1U,  9U,
  /* round 6 */ 12U,  5U,  1U, 15U, 14U, 13U,  4U, 10U,  0U,  7U,  6U,  3U,  9U,  2U,  8U, 11U,
  /* round 7 */ 13U, 11U,  7U, 14U, 12U,  1U,  3U,  9U,  5U,  0U, 15U,  4U,  8U,  6U,  2U, 10U,
  /* round 8 */  6U, 15U, 14U,  9U, 11U,  3U,  0U,  8U, 12U,  2U, 13U,  7U,  1U,  4U, 10U,  5U,
  /* round 9 */ 10U,  2U,  8U,  4U,  7U,  6U,  1U,  5U, 15U, 11U,  9U, 14U,  3U, 12U, 13U,  0U
};

/* BLAKE2b IV table — fractional parts of sqrt of first 8 primes, 64-bit (SHA-512 IVs).
 * Used by Hacl_Hash_Blake2b_init. */
static const uint64_t Hacl_Hash_Blake2b_ivTable_B[8U] = {
  0x6A09E667F3BCC908ULL,  /* frac(sqrt(2))  */
  0xBB67AE8584CAA73BULL,  /* frac(sqrt(3))  */
  0x3C6EF372FE94F82BULL,  /* frac(sqrt(5))  */
  0xA54FF53A5F1D36F1ULL,  /* frac(sqrt(7))  */
  0x510E527FADE682D1ULL,  /* frac(sqrt(11)) */
  0x9B05688C2B3E6C1FULL,  /* frac(sqrt(13)) */
  0x1F83D9ABFB41BD6BULL,  /* frac(sqrt(17)) */
  0x5BE0CD19137E2179ULL   /* frac(sqrt(19)) */
};

/* BLAKE2s IV table — fractional parts of sqrt of first 8 primes, 32-bit (SHA-256 IVs).
 * Used by Hacl_Hash_Blake2s_init. */
static const uint32_t Hacl_Hash_Blake2b_ivTable_S[8U] = {
  0x6A09E667UL,  /* frac(sqrt(2))  */
  0xBB67AE85UL,  /* frac(sqrt(3))  */
  0x3C6EF372UL,  /* frac(sqrt(5))  */
  0xA54FF53AUL,  /* frac(sqrt(7))  */
  0x510E527FUL,  /* frac(sqrt(11)) */
  0x9B05688CUL,  /* frac(sqrt(13)) */
  0x1F83D9ABUL,  /* frac(sqrt(17)) */
  0x5BE0CD19UL   /* frac(sqrt(19)) */
};

#endif /* internal_Hacl_Impl_Blake2_Constants_H */

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

/* internal/Hacl_Spec.h — internal types for EverCrypt (HACL* dist/gcc-compatible)
 *
 * This file defines implementation-discriminant types used by EverCrypt_AEAD
 * and EverCrypt_AutoConfig2.  These types are separate from the public Hacl_Spec.h
 * to keep implementation details out of the public API surface.
 *
 * Reconstructed from HACL* commit 504c298 (hacl-star/hacl-star).
 * Third-party: MIT/Apache-2.0 — see contrib/oracles/THIRD_PARTY_LICENSES.md
 */

#ifndef internal_Hacl_Spec_H
#define internal_Hacl_Spec_H

#if defined(__cplusplus)
extern "C" {
#endif

#include <string.h>
#include "krml/internal/types.h"
#include "krml/lowstar_endianness.h"
#include "krml/internal/target.h"

/* Pull in the public Spec_Agile_AEAD_alg, Spec_Hash_Definitions etc. */
#include "Hacl_Spec.h"

/* Cipher expansion implementation discriminant — used internally by
 * EverCrypt_AEAD to select between HACL* ChaCha20-Poly1305 and Vale AES-GCM. */
typedef uint8_t Spec_Cipher_Expansion_impl;

#define Spec_Cipher_Expansion_Hacl_CHACHA20 ((uint8_t)0U)
#define Spec_Cipher_Expansion_Vale_AES128   ((uint8_t)1U)
#define Spec_Cipher_Expansion_Vale_AES256   ((uint8_t)2U)

#if defined(__cplusplus)
}
#endif

#endif /* internal_Hacl_Spec_H */

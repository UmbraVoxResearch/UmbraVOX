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


#ifndef internal_Hacl_Krmllib_H
#define internal_Hacl_Krmllib_H

#if defined(__cplusplus)
extern "C" {
#endif

#include <string.h>
#include "krml/internal/types.h"
#include "krml/lowstar_endianness.h"
#include "krml/internal/target.h"

#include "../Hacl_Krmllib.h"

/* FStar integer mask helpers — extern declarations; defined in hacl_fsmath.c.
 * The full declarations are already pulled in via ../Hacl_Krmllib.h above. */

/* FStar.UInt128 operations (add/logor/shift_left/mul_wide/store128_be/load128_be)
 * are NOT defined here.  They are supplied by the platform-selected uint128
 * implementation that "krml/internal/types.h" (included above) pulls in:
 *   - fstar_uint128_gcc64.h         when HAS_INT128 (native __uint128_t)
 *   - fstar_uint128_msvc.h          on MSVC x64
 *   - FStar_UInt128_Verified.h +
 *     fstar_uint128_struct_endianness.h   otherwise (portable struct impl)
 * Defining them here too produced a "redefinition of FStar_UInt128_add" error
 * (commit 8e6d3dc6 over-corrected a link fix that only concerned the extern
 * mask helpers above; the UInt128 helpers are header-inline and need no TU). */

#if defined(__cplusplus)
}
#endif

#define internal_Hacl_Krmllib_H_DEFINED
#endif /* internal_Hacl_Krmllib_H */

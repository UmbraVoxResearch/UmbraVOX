/* Compatibility shim: Hacl_SHA3.h → Hacl_Hash_SHA3.h
 *
 * The HACL* gcc-compatible distribution uses Hacl_Hash_SHA3.h with the
 * Hacl_Hash_SHA3_ prefix.  This shim provides the Hacl_SHA3_ names as
 * aliases so that code written against the older naming convention continues
 * to compile without modification.
 *
 * bridge_keccak.c (M13.15.12) uses extern declarations directly and does
 * not include this header, but it is retained for any other consumers that
 * may #include "Hacl_SHA3.h".
 *
 * M13.15.12: updated from stub to real shim after HACL* vendoring.
 */
#pragma once
#include "Hacl_Hash_SHA3.h"

/* Alias Hacl_SHA3_* → Hacl_Hash_SHA3_* for backward compatibility. */
#define Hacl_SHA3_sha3_256(inputByteLen, input, output) \
    Hacl_Hash_SHA3_sha3_256((output), (input), (inputByteLen))

#define Hacl_SHA3_sha3_512(inputByteLen, input, output) \
    Hacl_Hash_SHA3_sha3_512((output), (input), (inputByteLen))

#define Hacl_SHA3_shake128(inputByteLen, input, outputByteLen, output) \
    Hacl_Hash_SHA3_shake128((output), (outputByteLen), (input), (inputByteLen))

#define Hacl_SHA3_shake256(inputByteLen, input, outputByteLen, output) \
    Hacl_Hash_SHA3_shake256((output), (outputByteLen), (input), (inputByteLen))

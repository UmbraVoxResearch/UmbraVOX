/* SPDX-License-Identifier: Apache-2.0 */
/*
 * constant_time_eq — constant-time byte-array comparison via volatile
 * accumulator.  The volatile qualifier prevents the compiler from
 * short-circuiting the loop on an early mismatch.
 *
 * Returns 1 when the buffers are equal, 0 otherwise.
 */
#include <stdint.h>
#include <stddef.h>

int constant_time_eq(const uint8_t *a, const uint8_t *b, size_t len) {
    volatile uint8_t diff = 0;
    for (size_t i = 0; i < len; i++) {
        diff |= a[i] ^ b[i];
    }
    return diff == 0 ? 1 : 0;
}

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
    /* Branchless: widen to uint32_t so (diff - 1) wraps to 0xFFFFFFFF
     * when diff==0, then shift the sign-like bit down. When diff==0:
     * 0xFFFFFFFF >> 8 = 0x00FFFFFF, & 1 = 1. When diff!=0 (1..255):
     * (diff-1) is 0..254, >> 8 = 0, & 1 = 0. */
    return 1 & (((uint32_t)diff - 1) >> 8);
}

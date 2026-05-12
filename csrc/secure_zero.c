/* SPDX-License-Identifier: Apache-2.0 */
/*
 * umbravox_secure_zero — volatile write loop that resists dead-store
 * elimination by the C compiler.  Unlike memset(3), a compiler is not
 * permitted to remove writes through a volatile pointer even when it can
 * prove the memory is dead after the call.
 *
 * This provides a best-effort guarantee equivalent to explicit_bzero(3)
 * on platforms that do not expose that POSIX extension.
 */
#include <string.h>
void umbravox_secure_zero(void *ptr, size_t len) {
    volatile unsigned char *p = (volatile unsigned char *)ptr;
    while (len--) *p++ = 0;
}

/* SPDX-License-Identifier: Apache-2.0 */
/*
 * umbravox_secure_zero — platform-native secure memory erasure with a
 * portable volatile-write fallback.
 *
 * Priority order:
 *  1. memset_s  (C11 Annex K; also available on Apple platforms)
 *  2. explicit_bzero  (Linux, FreeBSD, OpenBSD, NetBSD, illumos)
 *  3. volatile write loop  (all other platforms)
 *
 * Unlike plain memset(3), none of these paths may be removed by a compiler
 * dead-store elimination pass even when the buffer is provably unreachable
 * after the call.
 */
#if defined(__APPLE__) && defined(__MACH__)
#  include <string.h>
#elif defined(__linux__) || defined(__FreeBSD__) || \
      defined(__OpenBSD__) || defined(__NetBSD__) || \
      defined(__sun)   /* illumos / Solaris */
#  include <strings.h>
#else
#  include <string.h>
#endif

void umbravox_secure_zero(void *ptr, size_t len) {
#if defined(__STDC_LIB_EXT1__) || (defined(__APPLE__) && defined(__MACH__))
    memset_s(ptr, len, 0, len);
#elif defined(__linux__) || defined(__FreeBSD__) || \
      defined(__OpenBSD__) || defined(__NetBSD__) || \
      defined(__sun)
    explicit_bzero(ptr, len);
#else
    volatile unsigned char *p = (volatile unsigned char *)ptr;
    while (len--) *p++ = 0;
#endif
}

/* SPDX-License-Identifier: Apache-2.0 */
/*
 * Platform-portable wrappers for mlock(2), munlock(2), and
 * madvise(MADV_DONTDUMP / MADV_NOCORE).
 *
 * These wrappers let the Haskell FFI call a single C function without
 * hardcoding platform-specific constant values.  The C compiler resolves
 * the correct constants from <sys/mman.h> per target OS.
 *
 * All functions are best-effort: callers should ignore non-zero returns.
 * mlock may fail with ENOMEM (RLIMIT_MEMLOCK) or EPERM on unprivileged
 * processes — this is expected and not fatal.
 */

#if defined(_WIN32) || defined(__wasi__) || defined(__EMSCRIPTEN__)

/* Windows / WASM: no POSIX mlock.  Stubs return 0 (success). */
#include <stddef.h>
int umbravox_mlock(void *ptr, size_t len)   { (void)ptr; (void)len; return 0; }
int umbravox_munlock(void *ptr, size_t len) { (void)ptr; (void)len; return 0; }
int umbravox_dontdump(void *ptr, size_t len){ (void)ptr; (void)len; return 0; }

#else

/* POSIX: Linux, BSD, illumos, macOS */
#include <sys/mman.h>
#include <stddef.h>

int umbravox_mlock(void *ptr, size_t len) {
    return mlock(ptr, len);
}

int umbravox_munlock(void *ptr, size_t len) {
    return munlock(ptr, len);
}

int umbravox_dontdump(void *ptr, size_t len) {
    /*
     * MADV_DONTDUMP (Linux ≥ 3.4, value 16): exclude pages from core dumps.
     * MADV_NOCORE (FreeBSD, OpenBSD, NetBSD): same semantics, different name.
     * illumos/Solaris: no equivalent; return 0 (no-op).
     * macOS: no public equivalent; return 0 (no-op).
     */
#if defined(MADV_DONTDUMP)
    return madvise(ptr, len, MADV_DONTDUMP);
#elif defined(MADV_NOCORE)
    return madvise(ptr, len, MADV_NOCORE);
#else
    (void)ptr; (void)len;
    return 0;
#endif
}

#endif

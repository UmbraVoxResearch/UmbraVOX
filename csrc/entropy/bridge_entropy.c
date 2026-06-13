/* SPDX-License-Identifier: Apache-2.0
 *
 * bridge_entropy.c — platform-aware OS entropy source for the UmbraVOX CSPRNG.
 *
 * Finding:     M40.7 / M40.35 / M40.36 — the CSPRNG previously read both the
 *              ChaCha20 key AND the 12-byte nonce directly from a non-blocking
 *              `/dev/urandom` handle, and detected fork() only lazily by
 *              comparing getProcessID() on the next randomBytes call.
 * Vulnerability:
 *              (1) `/dev/urandom` does not block before the kernel entropy pool
 *                  is initialised, so early-boot reads can return low-quality
 *                  bytes; HKDF-Extract debiases but cannot ADD missing entropy.
 *              (2) A fork()ed child shares the parent CSPRNG state until its
 *                  next draw; a same-microsecond PID-reuse collision could
 *                  reproduce (key,nonce), yielding keystream reuse and XOR
 *                  recovery of all CSPRNG-derived secrets.
 * Fix:         This bridge obtains entropy from the OS CSPRNG with pool-ready
 *              (blocking) semantics on every platform:
 *                - Linux: getrandom(2) flags=0 (blocks until pool init), with a
 *                  bounded ENOSYS/EINTR fallback to /dev/urandom.
 *                - BSD/illumos/macOS: getentropy(2) in <=256-byte chunks.
 *                - Windows: BCryptGenRandom(BCRYPT_USE_SYSTEM_PREFERRED_RNG).
 *                - Last resort: /dev/urandom read loop.
 *              A pthread_atfork() child handler sets a C11 atomic flag so the
 *              Haskell layer can force an immediate reseed in the child,
 *              narrowing (not eliminating) the fork-reuse window without
 *              relying solely on the lazy PID check.
 * Verified:    umbravox_entropy returns 0 only after exactly `len` bytes are
 *              written; partial reads and EINTR are retried; the atfork flag is
 *              read-and-cleared atomically by umbravox_entropy_forked.
 *
 * NOTE: no raw syscall(SYS_getrandom) is used — NetBSD and several BSDs lack
 * that syscall number, so the libc wrappers (getrandom/getentropy) are the
 * only portable entry points.
 */

/* Feature-test macros must precede all includes. */
#if defined(__linux__)
#  ifndef _GNU_SOURCE
#    define _GNU_SOURCE 1
#  endif
#endif
#ifndef _DEFAULT_SOURCE
#  define _DEFAULT_SOURCE 1
#endif
#if defined(__sun)
#  ifndef __EXTENSIONS__
#    define __EXTENSIONS__ 1
#  endif
#endif

#include <stdint.h>
#include <stddef.h>
#include <string.h>

#if defined(_WIN32)

/* ----------------------------- Windows ---------------------------------- */
#include <windows.h>
#include <bcrypt.h>
/* Link with bcrypt: see UmbraVox.cabal `if os(windows)` stanza. */

int
umbravox_entropy(uint8_t *buf, uint32_t len)
{
    NTSTATUS s = BCryptGenRandom(NULL, (PUCHAR)buf, (ULONG)len,
                                 BCRYPT_USE_SYSTEM_PREFERRED_RNG);
    return BCRYPT_SUCCESS(s) ? 0 : -1;
}

/* Windows processes do not fork(); the atfork machinery is inert. */
void umbravox_entropy_init(void) { }
int  umbravox_entropy_forked(void) { return 0; }

#else /* POSIX */

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <pthread.h>
#include <stdatomic.h>

#if defined(__linux__) || defined(__sun)
#  include <sys/random.h>   /* getrandom / getentropy declarations */
#endif

/* ----------------------- /dev/urandom fallback -------------------------- */
/* Bounded read loop. Returns 0 on success, -1 on any error or short EOF. */
static int
read_dev_urandom(uint8_t *buf, size_t len)
{
    int fd;
    size_t off = 0;

    do {
        fd = open("/dev/urandom", O_RDONLY);
    } while (fd < 0 && errno == EINTR);
    if (fd < 0) {
        return -1;
    }

    while (off < len) {
        ssize_t r = read(fd, buf + off, len - off);
        if (r < 0) {
            if (errno == EINTR) {
                continue;
            }
            (void)close(fd);
            return -1;
        }
        if (r == 0) {
            /* Unexpected EOF on /dev/urandom — treat as failure. */
            (void)close(fd);
            return -1;
        }
        off += (size_t)r;
    }
    (void)close(fd);
    return 0;
}

int
umbravox_entropy(uint8_t *buf, uint32_t len)
{
    size_t need = (size_t)len;
    size_t off = 0;

    if (len == 0) {
        return 0;
    }

#if defined(__linux__)
    /* Linux: getrandom(flags=0) blocks until the entropy pool is initialised,
     * then never blocks again. Loop to handle EINTR / short returns. */
    while (off < need) {
        ssize_t r = getrandom(buf + off, need - off, 0);
        if (r < 0) {
            if (errno == EINTR) {
                continue;
            }
            if (errno == ENOSYS) {
                /* Pre-3.17 kernel without getrandom: fall back. */
                return read_dev_urandom(buf, need);
            }
            return -1;
        }
        off += (size_t)r;
    }
    return 0;

#elif defined(__APPLE__) || defined(__OpenBSD__) || defined(__FreeBSD__) || \
      defined(__NetBSD__) || defined(__DragonFly__) || defined(__sun)
    /* getentropy fills at most 256 bytes per call and is all-or-nothing: for a
     * buffer <=256 bytes it either writes the whole buffer and returns 0, or
     * returns -1 without writing. Per POSIX/BSD it does NOT fail with EINTR for
     * such buffers, so the chunk loop never observes a partial write. The EINTR
     * retry below is therefore not expected to fire; it is retained only as
     * defence-in-depth against libc variants on the BSD/illumos targets whose
     * EINTR contract is not formally guaranteed. It cannot loop forever because
     * getentropy makes no progress claim under EINTR. */
    while (off < need) {
        size_t chunk = need - off;
        if (chunk > 256) {
            chunk = 256;
        }
        if (getentropy(buf + off, chunk) != 0) {
            if (errno == EINTR) {
                continue;
            }
            if (errno == ENOSYS) {
                return read_dev_urandom(buf, need);
            }
            return -1;
        }
        off += chunk;
    }
    return 0;

#else
    /* Unknown POSIX / WASM: /dev/urandom is the portable floor. */
    return read_dev_urandom(buf, need);
#endif
}

/* --------------------------- fork handling ------------------------------ */
/* C11 atomic flag set by the pthread_atfork child handler. Read-and-cleared
 * by umbravox_entropy_forked so the Haskell CSPRNG can reseed in the child. */
static atomic_int g_entropy_forked = 0;

static void
entropy_atfork_child(void)
{
    atomic_store(&g_entropy_forked, 1);
}

static pthread_once_t g_atfork_once = PTHREAD_ONCE_INIT;

static void
entropy_register_atfork(void)
{
    /* prepare/parent handlers unused — only the child must reseed. */
    (void)pthread_atfork(NULL, NULL, entropy_atfork_child);
}

void
umbravox_entropy_init(void)
{
    (void)pthread_once(&g_atfork_once, entropy_register_atfork);
}

int
umbravox_entropy_forked(void)
{
    /* Atomically read the flag and clear it; the next call sees 0 unless a
     * subsequent fork re-sets it. */
    return atomic_exchange(&g_entropy_forked, 0);
}

#endif /* POSIX */

/* entropy_link_probe — returns 1 to confirm this unit was linked.
 * Called by UmbraVox.Crypto.Generated.FFI.Entropy.ffiLinked. */
int
entropy_link_probe(void)
{
    return 1;
}

# Platform Compatibility Audit — UmbraVOX Haskell Codebase

**Audit scope:** `src/` and `app/` directories, all `.hs` files  
**Audit date:** 2026-05-10  
**Milestone:** M14.8.1-8.7

---

## Summary

UmbraVOX targets Linux-only deployment today. The codebase contains a
moderate number of platform-specific dependencies, concentrated in five
areas: POSIX file-permission APIs, a hardcoded Linux socket constant,
`/dev/urandom` entropy, `stty`-based terminal handling, and the VM/KVM
release tooling. The cryptography core and consensus layers are
fully portable. The TUI, mDNS, and release-bridge components require the
most work to broaden platform support.

---

## 1. Linux-Specific Imports — `System.Posix.*`

### 1.1 `src/UmbraVox/App/RuntimeLog.hs` lines 28–34

```
import System.Posix.Files        (ownerReadMode, ownerWriteMode,
                                   setFileMode, unionFileModes)
import System.Posix.Process      (getProcessID)
```

**What it does:** Sets log-file permissions to `0600` (owner read/write
only); reads the process PID for log-line tagging.

**Won't work on:** Windows (no POSIX file-mode API). Compiles on BSD,
illumos, and macOS because `unix` is available there.

**Suggested fix:**  
- Replace `setFileMode` with `System.Directory`-based workaround or the
  `directory` package's `setPermissions` (cross-platform).  
- Replace `getProcessID` with `System.Posix.Process.getProcessID` guarded
  by `#if !defined(mingw32_HOST_OS)` / `#else System.Win32.Process.getCurrentProcessId`.

---

### 1.2 `src/UmbraVox/App/Startup.hs` line 31

```
import System.Posix.Files (ownerReadMode, ownerWriteMode,
                            setFileMode, unionFileModes)
```

**What it does:** Sets `0600` permissions on the data directory and key
files after creation.

**Won't work on:** Windows.

**Suggested fix:** Abstract into a `setPrivatePermissions :: FilePath -> IO ()`
helper in `UmbraVox.App.Platform` and use CPP or a typeclass to dispatch
between `System.Posix.Files.setFileMode` and `System.Win32.File.setFileAttributes`.

---

### 1.3 `src/UmbraVox/Crypto/KeyStore.hs` line 32

```
import System.Posix.Files (ownerReadMode, ownerWriteMode,
                            setFileMode, unionFileModes)
```

**What it does:** Sets `0600` on the encrypted key-store file.

**Won't work on:** Windows.

**Suggested fix:** Same shared `setPrivatePermissions` helper as 1.2.

---

### 1.4 `src/UmbraVox/Storage/Anthony.hs` lines 37–38

```
import System.Posix.Files (setFileMode, ownerReadMode, ownerWriteMode,
                            unionFileModes)
import System.Posix.Types ()
```

**What it does:** Sets `0600` on the SQLite database file.

**Won't work on:** Windows.

**Suggested fix:** Same `setPrivatePermissions` helper.

---

### 1.5 `src/UmbraVox/Storage/Encryption.hs` line 31

```
import System.Posix.Files (setFileMode, ownerReadMode, ownerWriteMode,
                            unionFileModes)
```

**What it does:** Sets `0600` on the encrypted storage file.

**Won't work on:** Windows.

**Suggested fix:** Same `setPrivatePermissions` helper.

---

### 1.6 `src/UmbraVox/Crypto/Random.hs` line 20

```
import System.Posix.Process (getProcessID)
```

**What it does:** Captures the PID at CSPRNG seed time to detect
`fork(2)` — if the PID changes between calls the CSPRNG is reseeded to
prevent key reuse after a fork.

**Won't work on:** Windows (no `fork`, no `getProcessID` in the `unix`
package).

**Suggested fix:** Guard with CPP:
```haskell
#if defined(mingw32_HOST_OS)
import System.Win32.Process (getCurrentProcessId)
getPID = fromIntegral <$> getCurrentProcessId
#else
import System.Posix.Process (getProcessID)
getPID = fromIntegral <$> getProcessID
#endif
```
Fork detection is meaningful only on POSIX; a Windows stub returning a
constant is acceptable since Windows does not `fork`.

---

## 2. Linux-Specific Constants

### 2.1 `src/UmbraVox/Network/MDNS.hs` lines 88–89

```haskell
ipAddMembership :: CInt
ipAddMembership = 35
```

**What it does:** Raw numeric constant for `IP_ADD_MEMBERSHIP` passed to
a `c_setsockopt` FFI call to join the mDNS multicast group.

**Platform values:**

| Platform | `IP_ADD_MEMBERSHIP` |
|---|---|
| Linux | 35 |
| macOS / BSD | 12 |
| illumos / Solaris | 19 |
| Windows (`ws2tcpip.h`) | 12 |

Hardcoding 35 will silently fail (the socket call will return an error,
which is `void`-discarded) on every non-Linux platform.

**Won't work correctly on:** macOS, FreeBSD, OpenBSD, NetBSD, illumos,
Windows.

**Suggested fix — preferred:** Use the `network` package's
`Network.Socket.setSocketOption` with `AddMembership` if that option is
exposed, or include a small C shim that exports the correct constant:

```c
// cbits/multicast.c
#include <netinet/in.h>
const int umbravox_ip_add_membership = IP_ADD_MEMBERSHIP;
```

```haskell
foreign import ccall "umbravox_ip_add_membership"
    ipAddMembership :: CInt
```

---

### 2.2 `src/UmbraVox/Network/MDNS.hs` line 165

```haskell
void (c_setsockopt fd ipprotoIP 34 (castPtr ptr) 4)
```

**What it does:** Raw constant 34 for `IP_MULTICAST_LOOP` on Linux.

**Platform values:**

| Platform | `IP_MULTICAST_LOOP` |
|---|---|
| Linux | 34 |
| macOS / BSD | 11 |
| illumos | 18 |
| Windows | 11 |

**Won't work correctly on:** macOS, FreeBSD, OpenBSD, NetBSD, illumos,
Windows.

**Suggested fix:** Same C-shim pattern as 2.1:

```c
const int umbravox_ip_multicast_loop = IP_MULTICAST_LOOP;
```

---

### 2.3 `app/Main.hs` lines 68–69

```haskell
-- SIGWINCH = 28 on Linux/macOS (not exported by all versions of
-- System.Posix.Signals)
sigWINCH :: CInt
sigWINCH = 28
```

**What it does:** Signal number for `SIGWINCH` (terminal resize), used
with `installHandler` to update the stored terminal size on resize.

**Platform values:**

| Platform | `SIGWINCH` |
|---|---|
| Linux | 28 |
| macOS | 28 |
| FreeBSD / OpenBSD | 28 |
| NetBSD | 28 |
| illumos / Solaris | 20 |
| Windows | N/A (no signals in this sense) |

The comment says "Linux/macOS" but the value happens to be 28 across
most BSDs as well. illumos is the dangerous edge case.

**Won't work correctly on:** illumos (SIGWINCH = 20 there, so handler
will silently attach to the wrong signal). Windows has no SIGWINCH at
all.

**Suggested fix:** Use the already-exported `sigWINCH` from
`System.Posix.Signals` (available since `unix-2.7`). If a version guard
is still needed, import it conditionally rather than hardcoding:

```haskell
import System.Posix.Signals (sigWINCH, installHandler, Handler(Catch))
```

---

## 3. Process/Filesystem — Hardcoded Paths

### 3.1 `src/UmbraVox/Crypto/Random.hs` line 191

```haskell
withBinaryFile "/dev/urandom" ReadMode ...
```

**What it does:** Primary entropy source for the CSPRNG. Reads raw bytes
from the OS CSPRNG device.

**Won't work on:** Windows (no `/dev/urandom`). Compiles but will throw
a runtime exception on Windows.

**Suggested fix:** Use the `entropy` package (`System.Entropy.getEntropy`)
or `crypton`/`cryptonite`'s `getRandomBytes`, both of which dispatch to
`CryptGenRandom` on Windows and `/dev/urandom` on POSIX. Alternatively,
guard with CPP using `RtlGenRandom` via FFI on Windows.

---

### 3.2 `src/UmbraVox/TUI/Terminal.hs` lines 40, 62, 70  
### `src/UmbraVox/TUI/Actions.hs` line 117

```haskell
readProcess "stty" ["-F", "/dev/tty", ...] ""
```

**What it does:** Uses `stty` and `/dev/tty` to:
- Enter raw mode (disable echo, XON/XOFF, enable no-echo).
- Restore the terminal on exit.
- Query terminal dimensions (`stty size`).

**Won't work on:** Windows (no `stty`, no `/dev/tty`). The `-F` flag is
also Linux-specific; macOS `stty` uses `-f` instead.

**Suggested fix:**  
- Replace `stty -F /dev/tty` with `stty -f /dev/tty` on macOS, and use
  CPP or runtime `System.Info.os` detection.  
- For terminal dimensions, prefer `System.Posix.Terminal.queryTerminal`
  / `TIOCGWINSZ` ioctl wrapped in a C shim, or use the `terminal-size`
  Hackage package which handles Linux, macOS, and Windows.  
- For raw mode, use `System.Posix.Terminal` (`getTerminalAttributes`,
  `setTerminalAttributes`) directly, avoiding the `stty` subprocess.

---

### 3.3 `src/UmbraVox/Tools/ReleaseBridge.hs` lines 203, 431, 552, 555, 573, 577

```haskell
hasKVM <- doesFileExist "/dev/kvm"
```

**What it does:** Checks for KVM hardware virtualisation support before
launching QEMU with `-machine q35,accel=kvm`.

**Won't work on:** macOS (uses Hypervisor.framework; no `/dev/kvm`),
FreeBSD (bhyve; no `/dev/kvm`), Windows (Hyper-V or WHPX; no `/dev/kvm`),
illumos (bhyve/KVM port may expose `/dev/kvm` on SmartOS — edge case).

**Note:** This is release-tooling infrastructure, not production runtime
code. It is intentionally Linux-specific. The finding is informational
for CI portability.

**Suggested fix:** Encapsulate KVM detection in a helper that also
checks macOS-equivalent paths (`/dev/vmm` for bhyve on FreeBSD, or
simply falls through to `accel=hvf` on macOS):

```haskell
detectAccel :: IO String
detectAccel = do
    kvm <- doesFileExist "/dev/kvm"
    if kvm then pure "kvm"
    else do
        hvf <- doesFileExist "/dev/apm"  -- rough macOS proxy
        pure (if hvf then "hvf" else "tcg")
```

---

### 3.4 `src/UmbraVox/Tools/ReleaseBridge.hs` lines 771, 783, 796

```haskell
overlay = "/tmp/umbravox-integration-agent-" ++ show agentId ++ ".qcow2"
"-serial", "file:/tmp/umbravox-integration-agent-" ++ show agentId ++ ".log"
let logFile = "/tmp/umbravox-integration-agent-" ++ show i ++ ".log"
```

**What it does:** Hard-wires `/tmp` as the scratch directory for
ephemeral VM overlay images and serial logs during integration tests.

**Won't work on:** Windows (no `/tmp`). On macOS, `/tmp` is a symlink to
`/private/tmp` and works fine. On NixOS sandboxed builds the `/tmp` path
may be restricted.

**Suggested fix:** Use `System.IO.Temp.getCanonicalTemporaryDirectory`
(from the `temporary` package) to get a portable temp root:

```haskell
import System.IO.Temp (getCanonicalTemporaryDirectory)
tmpRoot <- getCanonicalTemporaryDirectory
let overlay = tmpRoot </> "umbravox-integration-agent-" ++ show agentId ++ ".qcow2"
```

---

## 4. Network Stack

### 4.1 `src/UmbraVox/Network/MDNS.hs` line 136

```haskell
NS.setSocketOption sock NS.ReusePort 1
```

**What it does:** Sets `SO_REUSEPORT` so multiple processes can bind the
same mDNS port (5353).

**Won't work on:** Windows (no `SO_REUSEPORT`; the `network` package
exposes it only on POSIX). `ReusePort` will throw an exception or be
silently ignored depending on the GHC network version.

**Suggested fix:** Guard with CPP or wrap in a helper that catches the
`SocketError` and continues without it on Windows:

```haskell
setReusePort :: NS.Socket -> IO ()
#if defined(mingw32_HOST_OS)
setReusePort _ = pure ()
#else
setReusePort sock = NS.setSocketOption sock NS.ReusePort 1
#endif
```

---

### 4.2 `src/UmbraVox/Network/MDNS.hs` lines 81–82 (FFI `setsockopt`)

```haskell
foreign import ccall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr () -> CInt -> IO CInt
```

**What it does:** Direct FFI import of POSIX `setsockopt` for multicast
membership. The function signature is POSIX-standard, but the calling
convention and the constants passed differ by platform (see §2.1/2.2).

**Won't work on:** Windows (the equivalent is `setsockopt` in `Ws2_32.dll`
with different calling convention under MinGW). The FFI import itself
will link fine under MinGW, but the numeric constants will be wrong.

**Suggested fix:** Replace with a C shim that exports a single portable
`umbravox_join_multicast(fd, mcast_addr, local_addr)` function that
uses the correct OS headers.

---

### 4.3 `src/UmbraVox/App/Startup.hs` line 394  
### `src/UmbraVox/Network/Transport.hs` lines 140, 252  
### `src/UmbraVox/Network/Transport/UDP.hs` line 88

```haskell
NS.setSocketOption sock NS.ReuseAddr 1
```

**What it does:** `SO_REUSEADDR` for listening sockets.

**Platform note:** `SO_REUSEADDR` semantics differ between Linux and
Windows. On Windows it also implies `SO_REUSEPORT` (allows port
hijacking), which is a security concern in multi-user environments.

**Won't break compilation on:** Any platform — the `network` package
exposes `ReuseAddr` on Windows.

**Suggested fix:** Document the Windows behaviour difference in each
call site. No code change required for compilation portability, but
operators on Windows should be aware of the semantic difference.

---

## 5. Terminal Handling

### 5.1 `src/UmbraVox/TUI/Terminal.hs` lines 22, 27, 45, 57, 59

```haskell
esc code = "\ESC[" ++ code
hideCursor = putStr "\ESC[?25l"
showCursor = putStr "\ESC[?25h"
putStr "\ESC[?1000h\ESC[?1006h"   -- xterm mouse SGR mode
putStr "\ESC[?1006l\ESC[?1000l"
```

**What it does:** Sends ANSI/VT100 and xterm-specific escape sequences
for cursor control and mouse reporting.

**Won't work on:** Windows `conhost.exe` before Windows 10 version 1511.
WASM (no terminal). Dumb terminals or terminals that don't support
xterm extensions (e.g., old `xterm`, `rxvt` without mouse support).

**Note:** Windows 10+ `cmd.exe` and Windows Terminal support VT sequences
when `ENABLE_VIRTUAL_TERMINAL_PROCESSING` is set via `SetConsoleMode`.
The Haskell `ansi-terminal` package handles this automatically.

**Suggested fix:** Replace bare ANSI string construction with the
`ansi-terminal` package, which sets `ENABLE_VIRTUAL_TERMINAL_PROCESSING`
on Windows and provides `hHideCursor`/`hShowCursor` helpers. Mouse
reporting (1000h/1006h) has no portable abstraction and should remain
guarded by a runtime check of the `$TERM` environment variable.

---

### 5.2 `src/UmbraVox/TUI/Terminal.hs` line 47–48  
### `src/UmbraVox/TUI/Actions.hs` lines 115–116

```haskell
hSetBuffering stdin NoBuffering
hSetEcho stdin False
```

**What it does:** Sets stdin to unbuffered mode and disables echo for raw
keyboard input.

**Platform note:** These are `System.IO` calls, present on all GHC
platforms including Windows. However, `hSetEcho` is a no-op on Windows
prior to GHC 8.4 (it silently succeeds). On WASM there is no stdin.

**Won't break on:** Linux, macOS, BSD, Windows (with modern GHC/base).

**Suggested fix:** No change required for the target platforms. Document
that WASM is unsupported.

---

### 5.3 `app/Main.hs` line 211

```haskell
_ <- installHandler sigWINCH (Catch $ getTermSize >>= writeIORef ...) Nothing
```

**What it does:** Installs a `SIGWINCH` handler via `System.Posix.Signals`
to update the stored terminal dimensions on resize.

**Won't work on:** Windows (no `System.Posix.Signals`; the entire
`unix` package is unavailable on `mingw32`). The program will fail to
link on Windows.

**Suggested fix:** Guard with CPP. On Windows, terminal resize can be
detected by calling `GetConsoleScreenBufferInfo` on a timer, or by
handling `WINDOW_BUFFER_SIZE_EVENT` from `ReadConsoleInput`.

```haskell
#if !defined(mingw32_HOST_OS)
    _ <- installHandler sigWINCH (Catch $ getTermSize >>= writeIORef (asTermSize st)) Nothing
#endif
```

---

## 6. External Tool Dependencies

### 6.1 `stty` — `src/UmbraVox/TUI/Terminal.hs`, `src/UmbraVox/TUI/Actions.hs`

The `-F /dev/tty` flag is **Linux-specific**. macOS `stty` uses `-f`.
FreeBSD/NetBSD/OpenBSD use `-f`. illumos uses `-T /dev/term/x`.

| Platform | Flag |
|---|---|
| Linux | `-F /dev/tty` |
| macOS | `-f /dev/tty` |
| FreeBSD/NetBSD/OpenBSD | `-f /dev/tty` |
| illumos | `-T /dev/term/...` |
| Windows | N/A |

**Suggested fix:** Replace all `stty` subprocess calls with direct
`System.Posix.Terminal` (`getTerminalAttributes`/`setTerminalAttributes`)
which is portable across all POSIX systems and eliminates the subprocess.

---

### 6.2 `sha256sum` — `src/UmbraVox/Tools/Provenance.hs` line 168

```haskell
readProcessWithExitCode "sha256sum" [path] ""
```

**What it does:** Computes SHA-256 checksums for release artefacts.

**Won't work on:** macOS (uses `shasum -a 256`), FreeBSD/OpenBSD/NetBSD
(uses `sha256`), Windows (no `sha256sum` without extra tooling).

**Suggested fix:** Since UmbraVOX already has a pure Haskell SHA-256
implementation in `UmbraVox.Crypto.SHA256`, use it directly instead of
shelling out:

```haskell
import qualified UmbraVox.Crypto.SHA256 as S
import qualified Data.ByteString as BS
sha256File path = do
    bs <- BS.readFile path
    pure (showHex (S.sha256 bs))
```

---

### 6.3 `date -u` — `src/UmbraVox/Tools/Provenance.hs` line 136

```haskell
readProcessWithExitCode "date" ["-u", "+%Y-%m-%dT%H:%M:%SZ"] ""
```

**What it does:** Obtains the current UTC timestamp for provenance records.

**Won't work on:** Windows (no GNU `date`; MSYS2 `date` uses different
flags). Even on macOS/BSD, `date -u +fmt` works but is slightly fragile.

**Suggested fix:** Use `Data.Time.Clock.getCurrentTime` and
`Data.Time.Format.formatTime`:

```haskell
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime
```

---

### 6.4 `tcpdump` — `src/UmbraVox/Tools/PcapVerify.hs`

```haskell
readProcessWithExitCode "tcpdump" ["-r", path, ...] ""
```

**What it does:** Reads `.pcap` files during protocol verification.
Gracefully skips if `tcpdump` is unavailable.

**Won't work on:** Windows without WinPcap/Npcap. The code already
handles absence gracefully (returns `True` / skips), so this is a
**low-severity** finding.

**Suggested fix:** Consider using the `pcap` Hackage package for
cross-platform pcap reading, or the pure `network-packet` library.
No change required to maintain current skip behaviour.

---

### 6.5 `qemu-system-x86_64` / `accel=kvm` — `src/UmbraVox/Tools/ReleaseBridge.hs`

Multiple call sites invoke `qemu-system-x86_64` with `-machine q35,accel=kvm`.

**Won't work on:** macOS (use `qemu-system-x86_64 -machine q35,accel=hvf`
or `accel=tcg`), FreeBSD (bhyve or QEMU with `-accel tcg`), Windows
(QEMU/Windows with WHPX or TCG). The binary name also differs on ARM
hosts (`qemu-system-aarch64`).

**Note:** This is release-tooling infrastructure only. The finding is
informational for CI matrix expansion.

**Suggested fix:** Parameterise the QEMU accelerator in a helper:

```haskell
qemuAccel :: IO String
qemuAccel = do
    kvm <- doesFileExist "/dev/kvm"
    pure (if kvm then "accel=kvm" else "accel=tcg")
```

---

## Severity Matrix

| # | File(s) | Finding | Severity | Platforms Affected |
|---|---|---|---|---|
| 2.1 | `Network/MDNS.hs:89` | `IP_ADD_MEMBERSHIP = 35` hardcoded | **High** | macOS, BSD, illumos, Windows |
| 2.2 | `Network/MDNS.hs:165` | `IP_MULTICAST_LOOP = 34` hardcoded | **High** | macOS, BSD, illumos, Windows |
| 3.1 | `Crypto/Random.hs:191` | `/dev/urandom` hardcoded | **High** | Windows |
| 5.3 | `app/Main.hs:211` | `installHandler sigWINCH` (unix package) | **High** | Windows |
| 2.3 | `app/Main.hs:69` | `sigWINCH = 28` hardcoded | **Medium** | illumos (SIGWINCH=20) |
| 1.1–1.6 | Multiple | `System.Posix.Files` / `System.Posix.Process` | **Medium** | Windows |
| 3.2 | `TUI/Terminal.hs` | `stty -F /dev/tty` subprocess | **Medium** | macOS, Windows, illumos |
| 4.1 | `Network/MDNS.hs:136` | `SO_REUSEPORT` | **Medium** | Windows |
| 4.2 | `Network/MDNS.hs:81` | FFI `setsockopt` with Linux constants | **Medium** | Windows (wrong constants) |
| 6.2 | `Tools/Provenance.hs:168` | `sha256sum` CLI | **Low** | macOS, BSD, Windows |
| 6.3 | `Tools/Provenance.hs:136` | `date -u` CLI | **Low** | Windows |
| 3.3 | `Tools/ReleaseBridge.hs` | `/dev/kvm` existence check | **Low** | macOS, BSD, Windows (tooling only) |
| 3.4 | `Tools/ReleaseBridge.hs` | `/tmp` hardcoded | **Low** | Windows (tooling only) |
| 6.4 | `Tools/PcapVerify.hs` | `tcpdump` subprocess (graceful skip) | **Low** | Windows |
| 6.5 | `Tools/ReleaseBridge.hs` | `qemu-system-x86_64 accel=kvm` | **Low** | macOS, BSD, Windows (tooling only) |
| 4.3 | Multiple | `SO_REUSEADDR` semantics differ | **Info** | Windows (semantic, not compile) |
| 5.1 | `TUI/Terminal.hs` | ANSI/xterm escape sequences | **Info** | Old Windows consoles, WASM |
| 5.2 | `TUI/Terminal.hs` | `hSetEcho`/`hSetBuffering` | **Info** | WASM only |

---

## Recommended Action Plan

**Immediate (unblock macOS/BSD builds):**
1. Replace `IP_ADD_MEMBERSHIP = 35` and `IP_MULTICAST_LOOP = 34` with
   a C shim that reads the correct system header values.
2. Replace `/dev/urandom` with the `entropy` package.
3. Guard `installHandler sigWINCH` and `System.Posix.Signals` import
   under `#if !defined(mingw32_HOST_OS)`.

**Short-term (clean up POSIX file-permission calls):**
4. Create `src/UmbraVox/App/Platform.hs` exporting
   `setPrivatePermissions :: FilePath -> IO ()` and
   `getCurrentPID :: IO Int`, with CPP dispatch for Windows stubs.
   Replace all `System.Posix.Files.setFileMode` / `System.Posix.Process.getProcessID`
   call sites with the platform helper.

**Medium-term (TUI portability):**
5. Replace `stty -F` subprocess calls with `System.Posix.Terminal`
   directly, and add a runtime `System.Info.os` guard for the flag
   difference on macOS.
6. Replace `sha256sum` and `date -u` shell-outs in `Tools/Provenance.hs`
   with pure Haskell equivalents.

**Long-term / CI matrix:**
7. Parameterise QEMU accelerator detection in `Tools/ReleaseBridge.hs`.
8. Replace `/tmp` literals with `getCanonicalTemporaryDirectory`.

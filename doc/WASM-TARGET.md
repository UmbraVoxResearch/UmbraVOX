# UmbraVox WASM Target

This document describes the WebAssembly (WASM) build target for UmbraVox,
its scope, constraints, and how to invoke it once the toolchain is available.

---

## 1. GHC WASM Backend Status

The GHC WASM backend (`wasm32-wasi` target) has been available since GHC 9.6
via the [`ghc-wasm-meta`](https://gitlab.haskell.org/ghc/ghc-wasm-meta) project
maintained by Cheng Shao at Well-Typed / Tweag.

Key facts:

- GHC compiles Haskell to WASM via the standard code generator; no interpreter
  or transpiler is involved.
- The WASI (WebAssembly System Interface) target is suitable for server-side or
  embedded WASM runtimes (Wasmtime, WasmEdge, Node.js, browser via WASI shim).
- `ghc-wasm-meta` provides a Nix overlay that exposes `wasm32-wasi-ghc` and
  `wasm32-wasi-cabal` wrappers.
- GHC 9.6 and 9.8 are the most tested versions; GHC 9.10+ support is in
  progress upstream.
- The RTS can be compiled with `-optc-msimd128` for SIMD acceleration on
  runtimes that support it.

The UmbraVox nix-shell (see `shell.nix`) does **not** currently include
`ghc-wasm-meta`. The scaffold documented here is ready to use once that
toolchain is added to the shell.

---

## 2. Module Eligibility

WASM compilation requires pure Haskell code with no POSIX/network/FFI
dependencies that are unavailable under WASI.

### 2.1 Eligible: pure crypto core

These modules depend only on `base`, `bytestring`, `containers`, `binary`,
`array`, and `text`, and contain no calls to `network`, `unix`, `process`,
`directory`, or C FFI beyond what WASI provides:

| Module | Notes |
|--------|-------|
| `UmbraVox.Crypto.SHA256` | Pure Haskell |
| `UmbraVox.Crypto.SHA512` | Pure Haskell |
| `UmbraVox.Crypto.HMAC` | Pure Haskell |
| `UmbraVox.Crypto.HKDF` | Pure Haskell |
| `UmbraVox.Crypto.AES` | Pure Haskell |
| `UmbraVox.Crypto.GCM` | Pure Haskell |
| `UmbraVox.Crypto.ChaChaPoly` | Pure Haskell |
| `UmbraVox.Crypto.Poly1305` | Pure Haskell |
| `UmbraVox.Crypto.Curve25519` | Pure Haskell |
| `UmbraVox.Crypto.Ed25519` | Pure Haskell |
| `UmbraVox.Crypto.MLKEM` | Pure Haskell |
| `UmbraVox.Crypto.Keccak` | Pure Haskell |
| `UmbraVox.Crypto.ConstantTime` | Pure Haskell |
| `UmbraVox.Crypto.BIP39` | Pure Haskell |
| `UmbraVox.Crypto.SecureBytes` | Uses `Foreign.Concurrent`; WASI-compatible with wasm32-wasi-ghc |
| `UmbraVox.Crypto.Signal.DoubleRatchet` | Pure transforms; no IO persistence |
| `UmbraVox.Crypto.Signal.X3DH` | Pure transforms |
| `UmbraVox.Crypto.Signal.PQXDH` | Pure transforms |
| `UmbraVox.Protocol.Encoding` | Pure serialization |
| `UmbraVox.Protocol.CBOR` | Pure serialization |
| `UmbraVox.Version` | Metadata only |

### 2.2 Not eligible: IO, network, TUI, storage

These modules depend on POSIX sockets, filesystem I/O, or terminal libraries
that are outside the WASI capability model:

- `UmbraVox.Network.*` — POSIX sockets, DNS
- `UmbraVox.TUI.*` — terminal I/O
- `UmbraVox.Storage.*` — filesystem, SQLite
- `UmbraVox.App.*` — process lifecycle, config files
- `UmbraVox.Runtime.*` — threads, STM-backed state machines
- `UmbraVox.Tools.*` — shell subprocess calls
- `UmbraVox.Crypto.Random` — `getEntropy`; replace with host-provided CSPRNG
- `UmbraVox.Crypto.Generated.FFI.*` — C FFI; not available under WASM
- `UmbraVox.Crypto.Generated.*` — reference C implementations

---

## 3. Build Approach

A `wasm` flag is defined in `UmbraVox.cabal`:

```
flag wasm
  description: Build only pure crypto core (no network/TUI/storage).
               For use with wasm32-wasi-ghc via ghc-wasm-meta.
  default:     False
  manual:      True
```

When `-f wasm` is passed to cabal:

- Only the WASM-eligible modules listed in §2.1 are compiled.
- Network, TUI, storage, app, tools, and headless-runtime modules are excluded.
- C source files (`csrc/generated/*.c`, `csrc/secure_zero.c`) are excluded.
- Platform dependencies (`network`, `unix`, `process`, `directory`, `filepath`,
  `time`) are excluded from `build-depends`.
- The preprocessor symbol `WASM_BUILD` is defined (`-DWASM_BUILD`).

The resulting library is the `umbravox-core-wasm` surface intended for
embedding in browser or WASI host environments.

### 3.1 Size budget

Target: **< 2 MB** for the compiled WASM module (after `wasm-opt -Oz`).

The pure crypto core at GHC's `-O2` typically produces 1–3 MB of WASM before
optimization. Aggressive dead-code elimination via the WASM linker and
`wasm-opt` is expected to bring the stripped module well under budget. If
budget overrun is observed, the `Double Ratchet` session-state machinery
(large data type, many continuations) should be profiled first.

---

## 4. Build Instructions

See `scripts/build-wasm.sh` for the full invocation. Summary:

```bash
# Prerequisites
#   1. ghc-wasm-meta in PATH (wasm32-wasi-ghc, wasm32-wasi-cabal)
#   2. wasm-opt from binaryen (optional, for size optimization)

wasm32-wasi-cabal build UmbraVox -f wasm

# Post-process (optional)
wasm-opt -Oz dist-newstyle/.../UmbraVox.wasm -o umbravox-core.wasm
```

The flag defaults to `False`, so the normal `cabal build umbravox` invocation
(used in CI and the nix-shell) is unaffected.

---

## 5. Future Work

- Add `ghc-wasm-meta` to `shell.nix` once GHC 9.10 WASM support stabilizes.
- Add a WASM smoke-test lane to `scripts/release-lane-readiness-linux-x86_64.sh`
  that runs `wasmtime umbravox-core.wasm` against a KAT fixture.
- Expose a `wasm_api` entry-point module that wraps the crypto core with
  `foreign export javascript` stubs for the browser target.
- Investigate `UmbraVox.Crypto.Random` replacement: accept entropy as a
  function argument rather than calling `getEntropy` directly, making the
  module host-agnostic.

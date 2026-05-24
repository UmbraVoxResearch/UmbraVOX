# Static Musl Feasibility Evaluation

## Context

UmbraVOX currently ships a dynamically-linked Linux x86_64 release bundle
with a bundled loader and shared library closure. M2.3 tracks whether a
static musl-linked single-file binary is feasible as an alternative.

## Feasibility Assessment

### What Works

- GHC supports musl-based static linking via `pkgsStatic` in Nix or
  Alpine-based toolchains.
- The project has only one external Haskell dependency (`network`), which
  builds cleanly against musl.
- All boot libraries (base, bytestring, containers, etc.) support static
  linking.
- A static binary would eliminate the loader-wrapper and bundled lib
  directory, producing a single portable executable.

### What Requires Investigation

- **Network library and musl DNS**: musl's DNS resolver behaves
  differently from glibc's (no nsswitch, no mDNS via libnss). The mDNS
  discovery feature would need testing or a fallback path.
- **Constant-time concerns**: musl's crypto-adjacent functions (memcmp,
  etc.) may have different timing properties than glibc. Since UmbraVOX
  uses pure Haskell crypto (not libc crypto), this is low-risk but should
  be verified.
- **FFI C sources**: The 10 generated C files use standard C only and
  should compile against musl without changes.
- **Binary size**: A fully static Haskell binary is typically 20-40 MB
  (vs ~15 MB dynamic + libs). Acceptable for a single-file target.
- **Thread support**: musl supports pthreads. GHC's RTS should work.

### Licensing Implications

- musl is MIT-licensed. No copyleft obligations.
- Static linking with musl does not introduce GPL or LGPL constraints.
- glibc is LGPL-2.1+, which is why dynamic linking is used today. A musl
  static build would actually simplify license compliance.

## Recommendation

Static musl linking is **feasible** and would simplify the release
artifact. However, it should remain **experimental** (M2.3.3) until:

1. The mDNS/DNS resolution behavior is verified under musl.
2. A CI lane produces and smoke-tests a musl-static binary.
3. The single-file artifact passes the same VM smoke pipeline as the
   dynamic bundle.

## Current Status

The AppImage scaffold (M2.3.1) exists as a layout-only placeholder. It
does not yet produce a working single-file artifact. The musl track would
replace the AppImage approach if proven viable.

## Single-File Track Policy (M2.3.3)

The single-file track remains **experimental** and outside the
authoritative release posture until:

- A working single-file artifact exists (musl static or AppImage).
- The artifact passes the VM smoke pipeline (`./uv vm smoke`).
- Support and compatibility policies are documented.
- Parity with the dynamic bundle is demonstrated through differential
  testing.

Until these conditions are met, `./uv release linux` (dynamic bundle)
remains the only supported Linux release artifact.

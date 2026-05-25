# Agent Development Guide

Instructions for AI agents working on UmbraVOX. All build, test, and
verification MUST happen inside NixOS VMs. The host system's nix store
should never be touched by compilation.

## MANDATORY: Use the Build System

**All development, testing, and verification by agents MUST use `./uv`.**
Agents must NOT run `cabal`, `ghc`, `nix-build`, `nix-shell`, or any
compiler/build tool directly. The build system exists to catch issues
early — bypassing it means problems are discovered late or not at all.

After ANY code change, verify it works:
```bash
./uv build                # Compiles in VM — catches build errors
./uv test                 # Runs tests in VM — catches regressions
./uv check                # Runs lint/format/license/complexity/generated-headers/constant-time-branches gates
```

After infrastructure/nix changes, verify the full pipeline:
```bash
./uv vm build-image       # Builds dev VM image — catches nix/disk issues
./uv dev                  # Boots VM — catches boot/service issues
```

**Never assume a change works without testing it through `./uv`.** The
build system enforces VM isolation, correct toolchain versions, network
policy, and disk space checks. Bypassing it leads to broken builds that
only surface when users try to build.

## Host Prerequisites

The **only** tools needed on the host:
- **qemu** (`qemu-system-x86_64` with KVM support)
- **nix** or **bash** (either works; `./uv` is a bash script)
- **curl** (for downloading the seed VM image on first run)

No GHC, cabal, GCC, or other toolchain required. Everything compiles
inside NixOS VMs. Nothing is written outside the project directory.

## Host Filesystem Isolation

**Rule: Nothing writes outside the project directory.**

Zero writes outside the project directory. No exceptions -- not even
during bootstrap. The seed VM image is downloaded (not built on host),
and all nix compilation happens inside VMs.

- Build artifacts: `dist-newstyle/`, `build/`
- Cabal store: `dist-newstyle/cabal-store/` (NOT `~/.cabal/`)
- Go cache: `build/go/mod/`, `build/go/cache/` (NOT `~/go/`)
- Test temp files: `build/test-tmp/` (NOT `/tmp/`)
- VM scratch: `build/vm/` (NOT `/nix/store/`)
- App data (dev): `.umbravox-data/` (NOT `~/.umbravox/`)

Set `UMBRAVOX_DATA=.umbravox-data` to keep runtime data project-local.
The nix shells set this automatically.

## Bootstrap Flow

On first `./uv vm build-image`:
1. Downloads pre-built builder VM image from GitHub release (~300MB)
   + SHA-256 sidecar verification
2. If download fails: falls back to local nix-build (~3GB host writes)
3. Boots the builder VM, which downloads packages from cache.nixos.org
   and builds the full dev VM image natively (mke2fs -d, no cptofs/LKL)
4. Extracts compressed result via 9p share, decompresses on host
5. Caches the builder image at build/vm/builder-image/

The scratch disk (build/vm/nix-cache.qcow2, 100GB thin-provisioned)
stores downloaded packages across builds. Use `./uv vm clean-image`
to remove all cached images and free disk space.

## VM-First Development

**Rule: Never compile on the host.** Use VMs for all:
- Haskell builds (`cabal build`, `cabal test`)
- C compilation (generated crypto code)
- F* verification (`fstar.exe`)
- Coq proofs (`coqc`)
- Signal-Server (Maven/Java)

### CRITICAL: Always Use `./uv` Commands

**Never run `cabal build`, `cabal test`, or `nix-shell --run` directly.**
Always use `./uv` commands, which route through `vm_or_local` to ensure
builds happen inside the VM.

```bash
# CORRECT — uses VM via ./uv
./uv build               # Build in VM
./uv test                # Test in VM
./uv verify              # F* verification in VM

# WRONG — bypasses VM isolation
nix-shell shell.nix --pure --run "cabal build all"   # ← NEVER DO THIS
cabal build                                           # ← NEVER DO THIS
```

This applies to AI agents as well. When verifying that code compiles,
agents MUST use `./uv build` (or the VM exec path), never direct
`cabal build` invocations. Direct builds on the host:
- Touch the host nix store (policy violation)
- May use different toolchain versions than the VM
- Bypass VM network isolation
- Can leave stale `dist-newstyle/` state that breaks subsequent builds

### CRITICAL: Never Run nix-build on the Host

**All nix-build operations happen inside the builder VM, not on the host.**

This includes building VM images, runtime images, and any nix derivation.
The only exception is `--on-host` which is an explicit opt-in for developers
who understand the tradeoff.

```bash
# CORRECT — builds inside the builder VM
./uv vm build-image              # Dev VM image
./uv vm build-runtime-image      # Firecracker + QEMU runtime images

# WRONG — touches the host nix store
nix-build nix/vm-image.nix       # ← NEVER DO THIS
nix-build nix/vm-runtime.nix     # ← NEVER DO THIS
nix-shell --run "..."            # ← NEVER DO THIS (except nix-shell shell.nix for Go tools)
```

The builder VM has nix daemon + network + scratch disk. It downloads from
cache.nixos.org and builds images natively. Results come back via the 9p
output share. The host nix store is never written to.

**Agents: when iterating on nix configs, do NOT test with `nix-build` on
the host. Use `./uv vm build-image` or `./uv vm build-runtime-image`.
If the build fails, debug inside `./uv dev` or by reading the VM console
output — do not fall back to host nix commands.**

### VM Architecture

Three VM tiers, each built by the builder VM:

| VM | Size | Built by | Used by |
|----|------|----------|---------|
| **Builder** | ~3GB | Downloaded or `nix-build nix/vm-builder.nix` (host, one-time) | `./uv vm build-image`, `./uv vm build-runtime-image` |
| **Dev** | ~26GB | Builder VM | `./uv build`, `./uv test`, `./uv verify`, `./uv dev` |
| **Runtime** | ~1.3GB | Builder VM | `./uv run` (Firecracker TUI/headless, QEMU GUI) |

The builder VM is the only image that may be built on the host (bootstrap).
All other images are built inside VMs.

### Debugging with Serial Console

All VMs expose a serial console for interactive debugging:

```bash
./uv dev                 # Interactive serial console (login as root)
./uv dev --gui           # GUI window with QEMU GTK console
```

Inside the VM:
- Workspace at `/work/umbravox`
- Run `cabal build all`, `cabal test`, `fstar.exe` directly
- Check `/output/vm-exec-status` for last exec command result
- Check `/mnt/src/.vm-exec-cmd` for the command that was sent
- Systemd journal: `journalctl -u umbravox-smoke` for boot logs

For exec mode debugging:
```bash
# Run a specific command in the VM and get its exit code
./uv exec -- cabal build all
echo $?  # exit code from the VM command
```

### Quick Reference

```bash
./uv build               # Build in VM (host needs only qemu + bash)
./uv test                # Run tests in VM
./uv verify              # F* verification in VM
./uv dev                 # Interactive dev shell in VM (serial)
./uv dev --gui           # Interactive dev shell (QEMU GTK window)
./uv vm signal build-jar   # Build Signal-Server JAR in VM
./uv vm signal update      # Update Signal-Server version (interactive)
./uv vm signal run         # Boot Signal-Server runtime VM
./uv vm signal test        # Signal-Server integration test suite (Go, 687 lines)
./uv vm signal health      # Health-check Signal-Server
./uv check               # Run lint/format/license/complexity/generated-headers/constant-time-branches gates
```

### VM-local Nix Config (Image Builds)

`./uv vm build-image` and `./uv vm signal build-jar` use local `nix`
configuration from `nix/vm-build.env` and fail closed.
Environment variables override file values.

```bash
UMBRAVOX_NIX_BUILD_DIR=build/vm/tmp
UMBRAVOX_NIX_SANDBOX_BUILD_DIR=/build
UMBRAVOX_NIX_LOCAL_ONLY=1
UMBRAVOX_NIX_REQUIRE_CONFIG=1
```

Hard guard: remote-builder variables are rejected in local-only mode.

## Languages (strict)

- **Haskell**: all application code, crypto, protocol, TUI, bridge plugins
- **C**: generated crypto + FFI (csrc/) — **generated by codegen, never hand-edited**
- **Go**: build orchestration (tools/cmd/, the `./uv` binary)
- **Shell**: scripts/, uv bootstrap wrapper
- **Nix**: VM images, environments
- **F***: formal specs (test/evidence/formal-proofs/fstar/)
- **Coq**: external proofs (test/evidence/formal-proofs/coq/)

No Rust or Python in production. Go is build tooling only. Python for cert-gen scripts only.

## Codegen Pipeline (CRITICAL)

**Rule: never fix generated C files directly. Fix the generator.**

The codegen pipeline produces C, Haskell, and FFI wrappers from
`.spec` files:

```
app/codegen/Specs/*.spec  →  app/codegen/CryptoGen.hs  →  csrc/generated/*.c
                                                    src/UmbraVox/Generated/*.hs
                                                    csrc/generated/*_ffi.c
```

- `./uv build` always runs codegen before compilation (assurance)
- Generated C is the **primary** crypto implementation (constant-time)
- Differential testing compares generated C output against libsodium/HACL*
- If a differential test fails, **fix CryptoGen.hs** (the generator),
  not the generated `.c` file — the fix must flow from spec to output
- The F* verification specs and codegen specs share the same algorithm
  definitions, providing a verified-to-compiled assurance chain
- Generated C branch detection is a **hard fail** gate: `./uv check`
  rejects any generated C file containing conditional branches that
  could leak secret-dependent timing information

## Project Structure

```
src/UmbraVox/Crypto/           Pure crypto (SHA, AES, X25519, Ed25519, ML-KEM)
src/UmbraVox/Crypto/Signal/    Signal protocol (X3DH, PQXDH, DoubleRatchet)
src/UmbraVox/Crypto/Signal/Compat.hs  Signal-compat params (WhisperText)
src/UmbraVox/Protocol/         Wire formats (CBOR, SignalWire, Handshake)
src/UmbraVox/Network/          Transport (TCP, UDP, SOCKS5, IPC, Providers)
src/UmbraVox/Bridge/Signal/    Signal bridge plugin (IPC subprocess)
src/UmbraVox/TUI/              Terminal UI (Render, Input, Menu, Dialog)
src/UmbraVox/App/              Core app (Config, Types, State, Defaults)
test/evidence/formal-proofs/coq/   14 Coq files, 475 Qed, 0 Admitted
test/evidence/formal-proofs/fstar/ 24 F* specs, 0 admit(), 25 assume val
nix/                           VM images + build configs
plugins/                       Bridge plugin manifests + templates
scripts/                       VM orchestration + test scripts
```

## Building

```bash
./uv build                            # VM (preferred)
```

## Testing

```bash
./uv test                             # full suite in VM
./uv test core                        # deterministic core suite
./uv test tcp                         # real localhost TCP end-to-end
./uv test soak                        # longer stress suite
```

## Coq Proofs

```bash
# Build all 14 files:
nix-shell --run "cd test/evidence/formal-proofs/coq && make"  # Coq's own Makefile, not the project build system

# Coqprime files need -native-compiler no:
coqc -native-compiler no -R . UmbraVox Ed25519GroupUniversal.v

# Count Qed:
grep -ch 'Qed\.' test/evidence/formal-proofs/coq/*.v | paste -sd+ | bc
```

## Signal Bridge

IPC subprocess protocol:
```
Host → Plugin: AUTH, SEND, RECV, CONTACTS, STATUS, PING, CLOSE
Plugin → Host: AUTH_OK, OK, DATA, CONTACTS, STATUS, PONG, ERR
```

Uses own crypto with Signal params — no libsignal dependency.

## Shell-to-Go Migration Status

6 of 7 major shell scripts now have Go replacements:

| Script | Go replacement | Status |
|--------|---------------|--------|
| vm-signal-test.sh (1157 lines) | tools/cmd/signal-test (687 lines) | Done (v0.5.15) |
| fstar-eval-vectors.sh | tools/cmd/fstar-eval (498 lines) | Done (v0.5.15) |
| vm-smoke-run.sh (239 lines) | tools/cmd/vm-smoke | Done (v0.5.14) |
| test-coqprime-vm.sh | tools/cmd/test-coqprime | Done (v0.5.14) |
| release-package.sh + lib-release.sh | tools/cmd/release | Done (v0.5.14) |
| lib-vm.sh (301 lines) | partially replaced by Go pkg/ | In progress |
| Platform setup scripts | keep as shell (platform-specific) | Not planned |

## Conventions

- **Zero warnings**: `cabal build all` clean
- **Zero Admitted**: all Coq files
- **No Co-Authored-By**: never add attribution to commits
- **VM isolation**: never compile on host
- **Commit prefix**: feat/fix/proof/test/docs/infra/chore
- **Security fixes**: require Finding/Vulnerability/Fix/Verified documentation

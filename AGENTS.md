# Agent Development Guide

Instructions for AI agents working on UmbraVOX. All build, test, and
verification MUST happen inside NixOS VMs. The host system's nix store
should never be touched by compilation.

## Host Prerequisites

The **only** tools needed on the host:
- **qemu** (`qemu-system-x86_64` with KVM support)
- **nix** or **make** (either works)
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

On first `make vm-image-build`:
1. Downloads a minimal seed VM image (~300MB) + SHA-256 verification
2. Boots the seed VM, which builds the full builder VM image inside itself
3. Caches the builder image at build/vm/builder-image/
4. All subsequent builds use the cached builder -- no downloads needed

Alternative: users with nix installed can build the seed locally instead
of downloading (option B at the prompt). This is faster but touches
/nix/store.

The persistent nix cache disk (build/vm/nix-cache.qcow2) stores
downloaded packages across builds, enabling fully offline rebuilds
after the first successful build.

## VM-First Development

**Rule: Never compile on the host.** Use VMs for all:
- Haskell builds (`cabal build`, `cabal test`)
- C compilation (generated crypto code)
- F* verification (`fstar.exe`)
- Coq proofs (`coqc`)
- Signal-Server (Maven/Java)

### CRITICAL: Always Use `make` Targets

**Never run `cabal build`, `cabal test`, or `nix-shell --run` directly.**
Always use `make` targets, which route through `vm_or_local` to ensure
builds happen inside the VM.

```bash
# CORRECT — uses VM via make system
make build               # Build in VM
make test                # Test in VM
make verify              # F* verification in VM

# WRONG — bypasses VM isolation
nix-shell shell.nix --pure --run "cabal build all"   # ← NEVER DO THIS
cabal build                                           # ← NEVER DO THIS
```

This applies to AI agents as well. When verifying that code compiles,
agents MUST use `make build` (or the VM exec path), never direct
`cabal build` invocations. Direct builds on the host:
- Touch the host nix store (policy violation)
- May use different toolchain versions than the VM
- Bypass VM network isolation
- Can leave stale `dist-newstyle/` state that breaks subsequent builds

### Quick Reference

```bash
make build               # Build in VM (host needs only qemu + make)
make test                # Run tests in VM
make verify              # F* verification in VM
make vm-dev              # Interactive dev shell in VM (serial)
make vm-run-gui          # Interactive dev shell (QEMU GTK window)
make vm-signal-server-build-jar  # Build Signal-Server JAR in VM
make vm-signal-server    # Boot Signal-Server runtime VM
make check-isolation     # Verify host nix store is clean
```

### VM-local Nix Config (Image Builds)

`make vm-image-build` and `make vm-signal-server-build` use local `nix`
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
- **C**: generated crypto + FFI (csrc/)
- **Shell**: scripts/, Makefile
- **Nix**: VM images, environments
- **F***: formal specs (test/evidence/formal-proofs/fstar/)
- **Coq**: external proofs (test/evidence/formal-proofs/coq/)

No Rust, Go, or Python in production. Python for cert-gen scripts only.

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
make vm-build-only                    # VM (preferred)
```

## Testing

```bash
make vm-test                          # full suite in VM
make test-signal-bridge-ipc           # bridge IPC smoke test
make test-signal-compat               # wire-compat (needs Signal-Server VM)
```

## Coq Proofs

```bash
# Build all 14 files:
nix-shell --run "cd test/evidence/formal-proofs/coq && make"

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

## Conventions

- **Zero warnings**: `cabal build all` clean
- **Zero Admitted**: all Coq files
- **No Co-Authored-By**: never add attribution to commits
- **VM isolation**: never compile on host; `make check-isolation` to verify
- **Commit prefix**: feat/fix/proof/test/docs/infra/chore
- **Security fixes**: require Finding/Vulnerability/Fix/Verified documentation

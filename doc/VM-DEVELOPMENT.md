# VM-Based Development Migration (M13.13)

## Overview

UmbraVOX development is migrating from a local toolchain (via `shell.nix`)
to a NixOS VM-based workflow.  The full development toolchain (GHC 9.6,
Cabal, F\*, Z3, Coq, AFL++, valgrind, gcc, etc.) runs inside an isolated
NixOS QEMU VM.  The host machine only needs orchestration tools: QEMU,
git, make, and basic POSIX utilities.

## Motivation

- **Reproducibility**: The VM image is built from `nix/vm-image.nix` with
  a pinned nixpkgs.  Every developer gets the exact same toolchain.
- **Isolation**: Build artifacts, test side-effects, and fuzzing output
  stay inside the VM's tmpfs.  The host filesystem is never modified.
- **Platform parity**: The same VM image used for CI smoke testing is now
  the primary development environment.
- **Security**: The VM has no network access.  All packages are baked into
  the image at build time.

## Quick Start

```bash
# Enter the minimal orchestration shell
nix-shell shell-minimal.nix

# Build the VM image (first time only; cached afterwards)
make vm-image-build

# Interactive development inside the VM
make vm-dev

# Or run specific tasks non-interactively
make vm-build     # cabal build all
make vm-test      # cabal test umbravox-test --test-options="required"
make vm-verify    # F* formal verification
```

## Architecture

```
Host (shell-minimal.nix)          NixOS VM (nix/vm-image.nix)
========================          ============================
QEMU, git, make                   GHC 9.6, Cabal, F*, Z3, Coq
genext2fs, e2fsprogs               gcc, gdb, valgrind, AFL++
                                   graphviz, jq, patchelf
     |                                  |
     |  source tree (ext2 disk)         |
     +-- /dev/vdb (read-only) -------> /mnt/src
                                        |
                                   cp -> /work/umbravox (tmpfs, writable)
                                        |
                                   cabal build / test / verify
```

### Disk Layout

| Device     | Purpose                    | Format | Access    |
|------------|----------------------------|--------|-----------|
| `/dev/vda` | NixOS root (COW overlay)   | qcow2  | read-write|
| `/dev/vdb` | Source tree                | ext2   | read-only |
| `/dev/vdc` | F\* cache output (optional)| ext2   | read-write|

## Makefile Targets

### `make vm-dev`

Boots the VM with `-serial stdio` and auto-login.  You get a root shell
inside the NixOS VM with the full toolchain available.  Source is on
`/dev/vdb`; copy it to `/work/umbravox` to work with it.

Shut down with `poweroff` or Ctrl-A then X (QEMU monitor escape).

### `make vm-build`

Non-interactive.  Boots the VM, runs `cabal build all --enable-tests`,
then powers off.  Exit code reflects build success/failure.

### `make vm-test`

Non-interactive.  Boots the VM, builds, then runs the required test gate
(`cabal test umbravox-test --test-options="required"`), then powers off.

### `make vm-verify`

Non-interactive.  Boots the VM, builds, then runs F\* formal verification
(`cabal run fstar-verify`), then powers off.

## Migration Plan

### Phase 1: Parallel Operation (current)

Both `shell.nix` (full local toolchain) and `shell-minimal.nix`
(orchestration only) coexist.  Developers can use either workflow.

- `nix-shell` or `nix-shell shell.nix` gives the full local toolchain.
- `nix-shell shell-minimal.nix` gives the VM orchestration shell.
- All existing Makefile targets (`make build`, `make test`, etc.) continue
  to work unchanged with the local toolchain.
- New `vm-*` targets use the VM.

### Phase 2: VM-Primary

Once the team validates the VM workflow:

1. Rename `shell.nix` to `shell-full.nix` (preserved for escape-hatch use).
2. Rename `shell-minimal.nix` to `shell.nix` (becomes the default).
3. Update CI to use the VM workflow.

### Phase 3: VM-Only

Remove the full local shell entirely.  All development uses the VM.
The host only has QEMU and git.

## Prerequisites

- Linux host with KVM support (`/dev/kvm` must exist)
- Nix package manager (for `nix-shell` and `nix build`)
- First-time VM image build requires network access (for Nix to fetch packages)

## Troubleshooting

### "VM image not found"

Run `make vm-image-build` to build and cache the NixOS VM image.
This is a one-time operation unless `flake.nix` or `flake.lock` change.

### "/dev/kvm not found"

Ensure KVM is enabled in your kernel and your user has access:

```bash
ls -la /dev/kvm
# If permission denied:
sudo usermod -aG kvm $USER
```

### "genext2fs not found"

Enter the orchestration shell first: `nix-shell shell-minimal.nix`

### Changes inside VM are lost

By design.  The VM uses a tmpfs workspace and a COW overlay on the root
disk.  Nothing persists after shutdown.  Edit source on the host, then
use `make vm-build` / `make vm-test` to validate inside the VM.

### Slow first build

The first `make vm-image-build` downloads and installs the full NixOS
toolchain.  Subsequent runs use the cached image and are fast.  The
two-stage build also pre-caches F\* verification results in the image.

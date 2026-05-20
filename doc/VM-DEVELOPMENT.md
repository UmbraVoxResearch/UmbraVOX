# VM-First Development (M13.13)

## Overview

UmbraVOX uses a VM-first development model.  All `make build`, `make test`,
`make verify`, and other standard Makefile targets route through an isolated
NixOS QEMU VM by default.  The full development toolchain (GHC 9.6, Cabal,
F\*, Z3, Coq, AFL++, valgrind, gcc, etc.) runs inside the VM.  The host
machine only needs orchestration tools: QEMU, git, make, and basic POSIX
utilities (provided by `shell-minimal.nix`).

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
# Enter the orchestration shell (commands route to VM automatically)
nix-shell shell-minimal.nix

# Build the VM image (first time only; cached afterwards)
make vm-image-build

# Standard commands — these all run inside the VM by default
make build        # cabal build all (in VM)
make test         # cabal test umbravox-test --test-options="required" (in VM)
make verify       # F* formal verification (in VM)

# Interactive development inside the VM
make vm-dev
make vm-run-gui

# Bypass the VM for local execution (requires full nix-shell toolchain)
UMBRAVOX_LOCAL=1 make build
UMBRAVOX_LOCAL=1 make test
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
     |                                  |
     |  build cache (qcow2 disk)        |
     +-- /dev/vdc (read-write) ------> /cache (dist-newstyle + cabal store)
                                        |
                                   cp -> /work/umbravox (tmpfs, writable)
                                   ln -s /cache/dist-newstyle
                                        |
                                   cabal build / test / verify
```

### Disk Layout

| Device     | Purpose                        | Format | Access    | Persistent |
|------------|--------------------------------|--------|-----------|------------|
| `/dev/vda` | NixOS root (COW overlay)       | qcow2  | read-write| no (temp)  |
| `/dev/vdb` | Source tree                    | ext2   | read-only | no (temp)  |
| `/dev/vdc` | Build cache (dist-newstyle)    | qcow2  | read-write| **yes**    |

The build cache disk (`build/vm/build-cache.qcow2`, 4GB) persists across VM
sessions.  `dist-newstyle` and `.cabal/store` are symlinked to `/cache/` in
the guest, so subsequent builds reuse compiled artifacts.
The host does not mount this filesystem directly; it only stores the qcow2
backing file.

### VM Resources

VM resources auto-scale to 50% of the host, with a 25% minimum floor:

| Resource | Value | Rationale |
|----------|-------|-----------|
| RAM | 50% of host (min 25%, floor 2 GB) | GHC parallel compilation benefits from large heap |
| CPU cores | 50% of host (min 25%, floor 2) | Parallel `cabal build -j` |
| Root disk | COW overlay | Disposable per session |
| Cache disk | 4 GB qcow2 | Persists build artifacts across sessions |

## Makefile Targets

All standard `make` targets (`build`, `test`, `verify`, `quality`, etc.)
now route through the VM by default.  Set `UMBRAVOX_LOCAL=1` to bypass the
VM and run locally (requires the full `nix-shell` toolchain).

### `make build` / `make test` / `make verify`

These are the primary development commands.  By default they boot the VM,
execute the corresponding operation, and power off.  Exit code reflects
success/failure.

### `make vm-dev`

Boots the VM with `-serial stdio` and auto-login.  You get a root shell
inside the NixOS VM with the full toolchain available.  Source is on
`/dev/vdb`; copy it to `/work/umbravox` to work with it.

Shut down with `poweroff` or Ctrl-A then X (QEMU monitor escape).

### `make vm-image-build`

Builds and caches the NixOS VM image using `nix build` directly.  Does not
require cabal or any Haskell toolchain on the host.  Only needs QEMU, git,
make, and nix (all provided by `shell-minimal.nix`).

### `make vm-cache-clean`

Removes the persistent build cache disk (`build/vm/build-cache.qcow2`).
The next VM build will start from scratch.  Use when the cache becomes
corrupted or you want a clean build.

### Accessing Build Output from the Host

The VM shares a directory with the host via virtio-9p:

| Location | Description |
|----------|-------------|
| Guest: `/output/` | Shared output directory (writable) |
| Host: `build/vm-output/` | Same directory, visible immediately |

Inside `make vm-dev`, copy any files to `/output/` and they appear on the
host at `build/vm-output/` in real time — no extraction step needed.

```bash
# Inside the VM:
cp /work/umbravox/dist-newstyle/build/.../umbravox /output/
cp /work/umbravox/build/releases/*.tar.gz /output/

# On the host (immediately available):
ls build/vm-output/
```

Use `make vm-extract` to check what's in the output directory.

### TUI Screenshot / Recording / Visual Regression

```bash
make vm-screenshot          # Capture 8 TUI scenario frames (ANSI + HTML)
make vm-record              # Record an asciinema session of the TUI scenario
make vm-visual-regression   # Compare current TUI against reference baselines
```

These targets build the TUI binary inside the VM, then run `vm-tui-scenario.sh`
which drives tmux through 8 key states (initial screen, help overlay, prefs
dialog, identity panel, etc.).  Results are copied to the host via the 9p
shared output directory (`build/vm-output/screenshots/`).

To update reference baselines after intentional UI changes:

```bash
make visual-reference-update
```

### Network Policy

The VM has **no network access by default** (`-nic none`).  Network access
is controlled by `vm-network-policy.conf` in the repository root, which is
read by `scripts/vm-network-policy.sh` before QEMU launches.

```
# vm-network-policy.conf — deny-all by default
# Uncomment ALLOW rules to permit specific outbound connections:
# ALLOW tcp 93.184.216.34 443
```

The policy file is host-side only — the VM guest cannot modify it.  See
`vm-network-policy.conf` for the full syntax and per-VM-type sections.

### SOCKS5 Transport Test

```bash
make vm-socks5-test         # Test SOCKS5 proxy transport in VM
```

### Explicit `vm-*` Targets

The `vm-build`, `vm-test`, and `vm-verify` targets still exist as explicit
aliases but are now equivalent to the standard targets (which also route
through the VM).

## Migration Plan

### Phase 1: Parallel Operation (completed)

Both `shell.nix` and `shell-minimal.nix` coexisted.  Developers could use
either workflow.  Standard `make` targets used the local toolchain while
`vm-*` targets used the VM.

### Phase 2: VM-Primary (current)

All standard Makefile targets (`make build`, `make test`, `make verify`,
`make quality`, etc.) now route through the VM by default.

- `nix-shell` (i.e. `shell.nix`) provides the full local toolchain but
  its banner documents that commands run in the VM by default.
- `nix-shell shell-minimal.nix` provides an orchestration-only shell
  (QEMU, git, make) for developers who do not need the full local
  toolchain.
- `UMBRAVOX_LOCAL=1` bypasses the VM and runs commands locally using the
  host toolchain.  This requires the full `nix-shell` (not minimal).
- `make run` is guarded in VM-first mode; use `make vm-run-gui` for VM UI
  or `UMBRAVOX_LOCAL=1 make run-local` for explicit host compile+run.
- `make vm-image-build` works without cabal — it uses `nix build` directly.
- `make vm-dev` provides an interactive development shell inside the VM.

### Phase 3: VM-Only (future)

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

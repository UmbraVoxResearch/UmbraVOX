# VM-First Development (M13.13)

## Overview

UmbraVOX uses a VM-first development model.  All `./uv build`, `./uv test`,
`./uv verify`, and other standard build commands route through an isolated
NixOS QEMU VM by default.  The full development toolchain (GHC 9.14, Cabal,
F\*, Z3, Coq, AFL++, valgrind, gcc, etc.) runs inside the VM.  The host
machine only needs orchestration tools: QEMU, git, `./uv` (Go binary), and
basic POSIX utilities (provided by `shell-minimal.nix`).

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
./uv vm build-image

# Standard commands — these all run inside the VM by default
./uv build        # cabal build all (in VM)
./uv test         # cabal test umbravox-test --test-options="required" (in VM)
./uv verify       # F* formal verification (in VM)

# Interactive development inside the VM
./uv dev
./uv dev --gui
```

## Architecture

```
Host (shell-minimal.nix)          NixOS VM (nix/vm-image.nix)
========================          ============================
QEMU, git, ./uv                   GHC 9.14, Cabal, F*, Z3, Coq
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

## Build Commands

All standard `./uv` commands (`build`, `test`, `verify`, `quality`, etc.)
route through the VM by default. Host-local compile bypass is disabled.

### `./uv build` / `./uv test` / `./uv verify`

These are the primary development commands.  By default they boot the VM,
execute the corresponding operation, and power off.  Exit code reflects
success/failure.

### `./uv dev`

Boots the VM with `-serial stdio` and auto-login.  You get a root shell
inside the NixOS VM with the full toolchain available.  Source is on
`/dev/vdb`; copy it to `/work/umbravox` to work with it.

Shut down with `poweroff` or Ctrl-A then X (QEMU monitor escape).

### `./uv vm build-image`

Builds and caches the NixOS VM image inside a builder VM (M20.5.8).  The
builder VM has its own Nix daemon and a 60GB scratch disk for `/nix/store`,
so the host's `/nix/store` is never touched.  Only needs QEMU, git,
`./uv`, and nix on the host (nix is only used to build the lightweight
builder image itself, not the full dev-toolchain image).

To build directly on the host instead (legacy approach), use
`./uv vm build-image --on-host`.  This uses ~30GB of host disk and writes
to the host `/nix/store`.

### `./uv vm build-image --on-host`

Legacy target that builds the VM image directly on the host via `nix build`.
Touches the host `/nix/store` and consumes ~30GB.  Prefer `./uv vm build-image`.

### `./uv clean`

Removes the persistent build cache disk (`build/vm/build-cache.qcow2`).
The next VM build will start from scratch.  Use when the cache becomes
corrupted or you want a clean build.

### Accessing Build Output from the Host

The VM shares a directory with the host via virtio-9p:

| Location | Description |
|----------|-------------|
| Guest: `/output/` | Shared output directory (writable) |
| Host: `build/vm-output/` | Same directory, visible immediately |

Inside `./uv dev`, copy any files to `/output/` and they appear on the
host at `build/vm-output/` in real time — no extraction step needed.

```bash
# Inside the VM:
cp /work/umbravox/dist-newstyle/build/.../umbravox /output/
cp /work/umbravox/build/releases/*.tar.gz /output/

# On the host (immediately available):
ls build/vm-output/
```

For `./uv build`, `./uv test`, and `./uv verify`, the guest also writes
`/output/vm-exec-status` (host: `build/vm-output/vm-exec-status`) so the
host command exits with the same pass/fail status as the in-guest command.

### TUI Screenshot / Recording / Visual Regression

These capabilities exist as shell scripts but are not yet wired into `./uv vm`
subcommands. Run them directly:

```bash
scripts/vm-screenshot-capture.sh     # Capture TUI scenario frames
scripts/vm-tui-scenario.sh           # Drive tmux through 8 key states
```

The scripts build the TUI binary inside the VM, then run `vm-tui-scenario.sh`
which drives tmux through 8 key states (initial screen, help overlay, prefs
dialog, identity panel, etc.).  Results are copied to the host via the 9p
shared output directory (`build/vm-output/screenshots/`).

### Network Policy

The VM has **no network access by default** (`-nic none`).  Network access
is controlled by `conf/vm-network-policy.conf` in the repository root, which is
enforced by `./uv` (Go code in `tools/pkg/netpol/`) before QEMU launches.

The default smoke/development boot path is offline-first and does not wait for
`network.target` before starting UmbraVOX guest init/smoke units.

```
# conf/vm-network-policy.conf — deny-all by default
# Uncomment ALLOW rules to permit specific outbound connections:
# ALLOW tcp 93.184.216.34 443
```

The policy file is host-side only — the VM guest cannot modify it.  See
`conf/vm-network-policy.conf` for the full syntax and per-VM-type sections.

### SOCKS5 Transport Test

```bash
./uv vm socks5-test         # Test SOCKS5 proxy transport in VM
```

### VM Routing

All standard commands (`./uv build`, `./uv test`, `./uv verify`) route
through the VM automatically. There are no separate `./uv vm build` or
`./uv vm test` aliases -- use the standard commands directly.

## Direct Execution (--direct)

For CI environments without KVM access (GitHub Actions, GitLab CI),
commands can run directly on the host via nix-shell:

    ./uv build --direct     # Build on host via nix-shell
    ./uv test --direct      # Test on host via nix-shell
    ./uv check              # Already runs on host (no --direct needed)

This is the secondary path — the primary development flow uses VMs.
The --direct flag is intended for:
- CI runners without KVM (GitHub-hosted runners)
- Quick local iteration when VM boot time matters
- Portability testing of the host-build path

Note: --direct requires nix-shell and shell.nix on the host.

## Migration Plan

### Phase 1: Parallel Operation (completed)

Both `shell.nix` and `shell-minimal.nix` coexisted.  Developers could use
either workflow.  Standard build commands used the local toolchain while
`vm` subcommands used the VM.

### Phase 2: VM-Primary (current)

All standard build commands (`./uv build`, `./uv test`, `./uv verify`,
`./uv check`, etc.) now route through the VM by default.

- `nix-shell` (i.e. `shell.nix`) provides the full local toolchain but
  its banner documents that commands run in the VM by default.
- `nix-shell shell-minimal.nix` provides an orchestration-only shell
  (QEMU, git, `./uv`) for developers who do not need the full local
  toolchain.
- `./uv dev --gui` provides an interactive QEMU GTK window for GUI development.
- `./uv vm build-image` works without cabal -- it uses `nix build` directly.
- `./uv dev` provides an interactive development shell inside the VM.

### Phase 3: VM-Only (future)

Remove the full local shell entirely.  All development uses the VM.
The host only has QEMU and git.

## Prerequisites

- Linux host with KVM support (`/dev/kvm` must exist)
- Nix package manager (for `nix-shell` and `nix build`)
- First-time VM image build requires network access (for Nix to fetch packages)

## Seed Bootstrap

The two-stage bootstrap lets `./uv vm build-image` work without any nix
toolchain on the host. Instead of building on the host, a minimal "seed"
NixOS VM is downloaded, booted, and used to build the full builder VM
image entirely inside QEMU.

### What the seed image is

The seed image is a minimal NixOS qcow2 (~300MB) containing just enough
to run `nix-build`: the Nix daemon, basic coreutils, and network tools
for fetching nixpkgs. It has no GHC, cabal, or project-specific
toolchain -- those are built by the seed VM when it produces the full
builder image.

### Building the seed locally (for maintainers)

Maintainers who need to produce a new seed image should use the VM image
builder directly:

```bash
./uv vm build-image
```

This builds the VM image from `nix/vm-image.nix`. The output
lands in the build cache directory.

### Publishing (for CI)

After building a new seed locally:

1. Compute the SHA-256 checksum:

   ```bash
   sha256sum build/vm/seed/nixos-seed.qcow2
   ```

2. Upload the seed image to a GitHub release (or other hosting).

3. Update the pinned hash in `./uv vm build-image` (Go code in
   `tools/cmd/umbravox-vm/cmd_image.go`): find the `SEED_SHA256`
   constant and replace its value with the new checksum. Update the
   `SEED_URL` constant if the download location changed.

4. Commit both changes together so the URL and hash stay in sync.

### Customizing the seed location

Set `UMBRAVOX_SEED_URL` to override the default download URL:

```bash
UMBRAVOX_SEED_URL=https://internal-mirror.example.com/nixos-seed.qcow2 \
  ./uv vm build-image
```

This is useful for air-gapped networks, corporate mirrors, or local
caches. The SHA-256 verification still applies regardless of the URL.

## VM Image Tiers

All VM images derive from a 4-tier NixOS module hierarchy. Each tier
imports the one below it — strictly additive.

### Tier 1: Base (`nix/tiers/base.nix`)
Boots, has a shell, can mount disks, can run a binary.
No network, no nix daemon, no toolchain.

Packages: bashInteractive, coreutils, util-linux, ncurses, sqlite

Used by:
- Lightweight QEMU runtime (`./uv run tui`, `./uv run gui`, `./uv run headless`)
- Smoke test guests

### Tier 2: Network (`nix/tiers/network.nix`)
Base + DHCP, DNS, outbound HTTPS. No nix daemon.

Additional packages: curl, cacert, jq

Used by:
- CI test runner (QEMU with network)
- Signal Server runtime VM (PostgreSQL, Redis, etc.)

### Tier 3: Builder (`nix/tiers/builder.nix`)
Network + nix daemon. Can build nix derivations and fetch
packages from cache.nixos.org.

Additional: nix, git, nix.enable, sandbox=false, 4 build users

Used by:
- VM image builder (`./uv vm build-image`)
- Signal Server build VM (`./uv vm signal build-jar`)

### Tier 4: Dev (`nix/tiers/dev.nix`)
Builder + full development toolchain.

Additional: GHC 9.14.1, Cabal, GCC, GDB, Valgrind, Coq, F*, Z3, TLA+,
Go, SQLite, AFL++, graphviz, patchelf, genext2fs, tmux, asciinema, etc.

Used by:
- Dev VM (`./uv dev`, `./uv build`, `./uv test`, `./uv verify`)

### Creating a New VM

1. Choose the appropriate tier based on what the VM needs
2. Create `nix/vm-<name>.nix` importing the tier
3. Add VM-specific config (services, boot params, extra packages)
4. Add a YAML definition in `vm-defs/` (Phase 3)
5. Wire into `./uv` CLI

## Runtime VM (lightweight QEMU)

The project uses two VM tiers:

1. **Dev VM** (QEMU, ~26GB) — full toolchain (GHC 9.14, Cabal, F*, Z3, Coq)
   Used by: `./uv build`, `./uv test`, `./uv verify`, `./uv dev`
   NixOS tier: Tier 4 (Dev)

2. **Runtime VM** (lightweight QEMU, ~1.6GB) — minimal (glibc, sqlite, ncurses)
   Used by: `./uv run`, `./uv run tui`, `./uv run gui`, `./uv run headless`
   NixOS tier: Tier 1 (Base)

All `./uv run` modes use a lightweight QEMU image with a qcow2 COW overlay.
QEMU is the only supported runtime hypervisor.

### Building Runtime Images

    ./uv vm build-runtime-image

### Running

    ./uv run              # Default: lightweight QEMU TUI (serial console)
    ./uv run tui          # Same as default
    ./uv run gui          # Lightweight QEMU with VGA display
    ./uv run headless     # Lightweight QEMU daemon mode

The runtime VM boots the pre-built binary from `build/runtime/`. If no
binary exists, run `./uv build` first.

## Troubleshooting

### "VM image not found"

Run `./uv vm build-image` to build and cache the NixOS VM image.
This is a one-time operation unless `nix/vm-image.nix` changes.

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
use `./uv build` / `./uv test` to validate inside the VM.

### Slow first build

The first `./uv vm build-image` downloads and installs the full NixOS
toolchain.  Subsequent runs use the cached image and are fast.  The
two-stage build also pre-caches F\* verification results in the image.

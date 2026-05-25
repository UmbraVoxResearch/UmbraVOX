# UmbraVOX Build Path Map

Complete map of all build, test, and run paths. All compilation happens
inside VMs unless `--direct` is explicitly used.

## Host (bootstrap only)

```
./uv                              Bootstrap wrapper
  └── go build → build/tools/umbravox-vm  (one-time, -trimpath -s -w)
```

## VM Image Building

```
./uv vm build-image               Build dev VM (26GB, tiers/dev.nix)
  ├── Downloads builder image from GitHub release
  │   └── build/vm/builder-image/nixos.img (~3GB)
  ├── Boots builder VM (QEMU, tiers/builder.nix — nix daemon + network)
  │   ├── nix-cache.qcow2 (persistent nix store across builds)
  │   ├── Downloads packages from cache.nixos.org
  │   └── nix-build nix/vm-image.nix → nixos.img.zst
  ├── Extracts via 9p share → build/vm-output/
  └── Decompresses → build/vm/image/nixos.img (26GB)

./uv vm build-runtime-image       Build runtime VMs (~1.3GB, tiers/base.nix)
  ├── Same builder VM as above
  └── nix-build nix/vm-runtime.nix
      ├── -A firecracker → build/vm/runtime-image/
      │   ├── rootfs.ext4.zst (compressed)
      │   ├── vmlinux (kernel)
      │   ├── initrd
      │   └── init-path (NixOS closure path)
      └── -A qemu → build/vm/runtime-qemu-image/nixos.img
```

## Development (Dev VM — QEMU, tiers/dev.nix)

All commands share the same disk setup:
- Source disk (ext2, readonly, /dev/vdb → /mnt/src)
- Build cache disk (qcow2, /dev/vdc → /cache) — persists dist-newstyle
- Output share (9p, /output → build/vm-output/)
- Working copy at /work/umbravox/ (tmpfs, writable)

```
./uv build                        Compile in dev VM
  ├── cabal run codegen            (regenerates C + Haskell from .spec)
  ├── cabal build all --enable-tests
  └── Extract runtime bundle → build/runtime/
      ├── bin/umbravox (stripped)
      └── lib/*.so (ldd closure)

./uv test [SUITE]                 Test in dev VM
  ├── cabal build all --enable-tests  (cached from build → instant)
  └── cabal test umbravox-test --test-options='required'

./uv verify                       F* verification in dev VM
  ├── cabal build all (cached)
  └── cabal run fstar-verify

./uv dev [--gui]                  Interactive dev shell
  ├── Boots to bash --login (serial) or VGA console (--gui)
  └── User runs commands interactively

./uv exec -- <cmd>                Arbitrary command in dev VM
  └── Runs <cmd>, writes exit status to /output/vm-exec-status
```

## Running (Runtime VM — Firecracker or QEMU, tiers/base.nix)

Requires `./uv build` first (populates build/runtime/).

```
./uv run                          Firecracker TUI (default)
./uv run tui                      Firecracker, serial console
  ├── Creates app disk (ext2) from build/runtime/
  ├── Decompresses rootfs.ext4.zst → writable copy
  ├── Reads init-path for NixOS init= kernel arg
  └── firecracker --no-api --config-file (sub-second boot)

./uv run gui                      QEMU lightweight runtime
  ├── Same app disk
  ├── COW overlay on runtime-qemu-image
  └── QEMU with -display gtk -vga std, app on tty1

./uv run headless                 Firecracker daemon mode
  └── Same as tui, no console= kernel arg
```

## Quality Gates (host-side)

```
./uv check                        All gates
  ├── lint                         Tabs, line length, trailing newlines
  ├── format                       Code formatting
  ├── license                      License headers
  ├── complexity                   Cyclomatic complexity (needs VM for binary)
  ├── generated-headers            CryptoGen marker in csrc/generated/
  ├── assurance                    doc/assurance-matrix.md freshness
  └── pre-release                  10 gates (F* admits, assume inventory, etc.)
```

## Signal Server (tiers/builder.nix build, tiers/network.nix runtime)

```
./uv vm signal build-jar          Build 333MB fat JAR in signal build VM
  ├── nix-build vm-signal-server.nix -A buildVm (if needed)
  ├── Boots QEMU with network (Maven needs deps)
  ├── git clone, mvn package -P exclude-spam-filter -Djib.skip=true
  └── 333MB shaded JAR → build/signal-server-jar/signal-server.jar

./uv vm signal health             Host-side HTTP health check
  └── Polls localhost:8081/healthcheck (30 retries, 2s apart)
```

## End-to-End Test

```
./uv test e2e                     Full pipeline from clean state
  1. Build dev VM image            (./uv vm build-image)
  2. Build + codegen               (./uv build)
  3. Test suite                    (./uv test)
  4. Quality gates                 (./uv check)
  5. Build runtime images          (./uv vm build-runtime-image)
```

## Direct Mode (CI without KVM)

```
./uv build --direct               WARNING + [y/N] confirmation
  └── nix-shell shell.nix --pure --run "cabal run codegen && cabal build all"

./uv test --direct                WARNING + [y/N] confirmation
  └── nix-shell shell.nix --pure --run "cabal build && cabal test"

--direct-bypass-interactive        Skip prompt, still show warning (CI scripts)
```

## Persistent Artifacts

| Path | Purpose | Persists across |
|------|---------|----------------|
| `build/vm/image/` | Dev VM image (26GB) | Until `./uv clean --all` |
| `build/vm/builder-image/` | Builder VM (symlink to nix store) | Until `./uv vm clean-image` |
| `build/vm/build-cache.qcow2` | cabal dist-newstyle + store | All dev VM boots |
| `build/vm/nix-cache.qcow2` | Builder's nix store scratch | All builder VM boots |
| `build/vm/runtime-image/` | Firecracker rootfs + kernel | Until rebuild |
| `build/vm/runtime-qemu-image/` | QEMU runtime image | Until rebuild |
| `build/runtime/` | Extracted binary + libs | Each `./uv build` |
| `build/signal-server-jar/` | Signal Server fat JAR | Each `./uv vm signal build-jar` |
| `build/tools/` | Compiled Go binaries | Until `./uv clean` |

## NixOS Image Tiers

All VM images derive from a 4-tier hierarchy (see doc/VM-DEVELOPMENT.md):

```
tiers/base.nix      → shell, mount, run binary
tiers/network.nix   → base + DHCP, DNS, curl
tiers/builder.nix   → network + nix daemon
tiers/dev.nix       → builder + GHC, Cabal, Go, F*, Z3, Coq, AFL
```

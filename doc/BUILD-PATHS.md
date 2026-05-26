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
./uv vm build-image               Build dev VM (26GB, nix/tiers/dev.nix)
  ├── Downloads builder image from GitHub release
  │   └── build/vm/builder-image/nixos.img (~3GB)
  ├── Boots builder VM (QEMU, nix/tiers/builder.nix — nix daemon + network)
  │   ├── nix-cache.qcow2 (persistent nix store across builds)
  │   ├── Downloads packages from cache.nixos.org
  │   └── nix-build nix/vm-image.nix → nixos.img.zst
  ├── Extracts via 9p share → build/vm-output/
  └── Decompresses → build/vm/image/nixos.img (26GB)

./uv vm build-runtime-image       Build runtime VM (~1.6GB, nix/tiers/base.nix)
  ├── Same builder VM as above
  └── nix-build nix/vm-runtime.nix
      └── -A qemu → build/vm/runtime-qemu-image/nixos.img
```

## Development (Dev VM — QEMU, nix/tiers/dev.nix)

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

./uv test all                     Run all named suites in one VM session
  ├── cabal build all --enable-tests  (cached)
  └── cabal test umbravox-test --test-options='<all 15 named suites>'

./uv verify                       F* verification in dev VM
  ├── cabal build all (cached)
  └── cabal run fstar-verify

./uv dev [--gui]                  Interactive dev shell
  ├── Boots to bash --login (serial) or VGA console (--gui)
  └── User runs commands interactively

./uv exec -- <cmd>                Arbitrary command in dev VM
  └── Runs <cmd>, writes exit status to /output/vm-exec-status
```

## Running (Runtime VM — lightweight QEMU, nix/tiers/base.nix)

Requires `./uv build` first (populates build/runtime/).

```
./uv run                          Lightweight QEMU TUI (default)
./uv run tui                      Lightweight QEMU, serial console
  ├── Creates app disk (ext2) from build/runtime/
  ├── COW overlay on runtime-qemu-image (~1.6GB)
  └── QEMU with -nographic, app on serial console

./uv run gui                      Lightweight QEMU with VGA display
  ├── Same app disk
  ├── COW overlay on runtime-qemu-image
  └── QEMU with -display gtk -vga std, app on tty1

./uv run headless                 Lightweight QEMU daemon mode
  └── Same as tui, no console output
```

QEMU is the only supported runtime hypervisor.

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

## Signal Server (nix/tiers/builder.nix build, nix/tiers/network.nix runtime)

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
./uv test e2e                     Warm pipeline (build → test → check → runtime → SBOM)
  1. Build dev VM image            (./uv vm build-image)
  2. Build + codegen               (./uv build)
  3. Test suite                    (./uv test)
  4. Quality gates                 (./uv check)
  5. Build runtime images          (./uv vm build-runtime-image)

./uv test e2e --bootstrap         Cold start (clean → build image → full e2e)
  1. Remove existing VM image      (./uv vm clean-image)
  2. Enter nix-shell               (nix-shell shell.nix)
  3. Build dev VM image from scratch  (./uv vm build-image)
  4. Run full e2e pipeline above
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
| `build/vm/runtime-image/` | (unused, legacy) | Until rebuild |
| `build/vm/runtime-qemu-image/` | QEMU runtime image (~1.6GB) | Until rebuild |
| `build/runtime/` | Extracted binary + libs | Each `./uv build` |
| `build/signal-server-jar/` | Signal Server fat JAR | Each `./uv vm signal build-jar` |
| `build/tools/` | Compiled Go binaries | Until `./uv clean` |

## NixOS Image Tiers

All VM images derive from a 4-tier hierarchy (see doc/VM-DEVELOPMENT.md):

```
nix/tiers/base.nix      → shell, mount, run binary
nix/tiers/network.nix   → base + DHCP, DNS, curl
nix/tiers/builder.nix   → network + nix daemon
nix/tiers/dev.nix       → builder + GHC, Cabal, Go, F*, Z3, Coq, TLA+, AFL++, Valgrind
```

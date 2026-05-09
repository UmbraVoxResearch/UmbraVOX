# Runner Classes and Isolation Model

## Runner Classes

UmbraVOX defines three classes of build/test runners, ordered by
isolation strength:

### Class 1: Host Runner (Current Default)

- **Environment**: Developer machine inside `nix-shell`
- **Isolation**: Nix provides hermetic inputs; execution trusts the host
- **Speed**: Fastest (no VM boot overhead)
- **Use case**: Development iteration, local testing
- **Command**: `make build && make test && make verify`
- **Trust level**: Non-authoritative — results depend on host state

### Class 2: VM Runner (Authoritative for Linux)

- **Environment**: NixOS QEMU guest with KVM acceleration
- **Isolation**: Full kernel-level isolation; no shared host state
- **Speed**: ~2-5 min overhead for VM boot + build
- **Use case**: Release qualification, evidence bundles
- **Command**: `make vm-smoke`
- **Trust level**: Authoritative — clean environment, reproducible

### Class 3: Container Runner (Artifact Smoke Only)

- **Environment**: Minimal container (podman/docker) with release bundle
- **Isolation**: Filesystem-level; shares host kernel
- **Speed**: Fast (no boot, just exec)
- **Use case**: Quick artifact sanity check
- **Command**: `make release-smoke-linux`
- **Trust level**: Partial — validates artifact structure, not build

## Isolation Model

```
┌─────────────────────────────────────────────────────────┐
│  Host (nix-shell)                          Class 1     │
│  ├── make build/test/verify                            │
│  ├── make release-linux                                │
│  │                                                     │
│  ├── make vm-smoke                         Class 2     │
│  │   └── QEMU/KVM Guest (NixOS)                       │
│  │       ├── Full toolchain pre-installed              │
│  │       ├── Source mounted as ext2 disk               │
│  │       ├── Build + test + verify + release           │
│  │       └── Exit code propagated to host              │
│  │                                                     │
│  └── make release-smoke-linux              Class 3     │
│      └── Container (Ubuntu 24.04)                      │
│          └── Release bundle mounted read-only          │
│              └── Binary execution check only           │
└─────────────────────────────────────────────────────────┘
```

### TAP+Bridge Dual-LAN Setup (M5.3.3)

For full L2 isolation testing with pcap capture, the dual-LAN mode
uses Linux bridges with TAP devices:

```
Host
├── br-umbravox-a (10.0.42.0/24)
│   ├── tap-a0 → Agent 0
│   ├── tap-a1 → Agent 1
│   └── tap-a2 → Agent 2
│
├── br-umbravox-b (10.0.43.0/24)
│   ├── tap-b0 → Agent 3
│   ├── tap-b1 → Agent 4
│   └── tap-b2 → Agent 5
│
└── IP forwarding between bridges
```

Setup requires `CAP_NET_ADMIN` (sudo):
```bash
sudo scripts/vm-network-setup.sh setup 6
make vm-integration-test-dual-lan
sudo scripts/vm-network-setup.sh teardown
```

Traffic capture:
```bash
tcpdump -i br-umbravox-a -w build/evidence/lan-a.pcap &
tcpdump -i br-umbravox-b -w build/evidence/lan-b.pcap &
```

## Artifact Handoff

### VM Runner (Class 2) Artifact Flow

1. Host creates ext2 source disk from repository via `genext2fs`
2. QEMU mounts source disk as `/dev/vdb` (read-only)
3. Guest copies source to writable tmpfs at `/work`
4. Guest builds and tests from the tmpfs copy
5. Guest produces release artifact in `/work/umbravox/build/releases/`
6. Guest reports SMOKE_RESULT=PASS/FAIL via serial console
7. Host parses serial output for result

Note: The release artifact produced inside the VM is currently
**not extracted back to the host**. The VM smoke pipeline verifies
that the artifact CAN be built in isolation. Extracting the
in-guest artifact for distribution is tracked as future work.

### Container Runner (Class 3) Artifact Flow

1. Host builds release artifact via `make release-linux`
2. Host mounts `build/releases/` into container
3. Container extracts and smoke-checks the artifact
4. Container reports exit code

## QEMU vs Firecracker

| Property | QEMU | Firecracker |
|----------|------|-------------|
| Available | Yes (via nix-shell) | Yes (via nix-shell) |
| KVM acceleration | Yes | Yes |
| NixOS image support | Yes (bootable raw disk) | Needs vmlinux + rootfs |
| Current status | Functional (`make vm-smoke`) | Scaffold only |
| Use case | Full pipeline isolation | Lightweight, fast-boot isolation |

Firecracker support (M2.4.2) remains scaffold-only. It requires:
- A vmlinux kernel (not bzImage) — needs a separate Nix derivation
- A minimal rootfs (not a full NixOS disk image)
- A Firecracker JSON config template

The QEMU lane is the current authoritative isolated runner.

## Multi-Architecture Emulation (M5.5)

### arm64 via QEMU System Emulation

UmbraVOX can be tested on arm64 via QEMU system emulation on x86_64
hosts. This is significantly slower than KVM-accelerated VMs (~10-50x)
because QEMU must emulate the ARM instruction set in software.

| Property | Value |
|----------|-------|
| Machine | `virt` |
| CPU | `cortex-a72` |
| Serial console | `ttyAMA0` |
| Bootloader | Direct kernel boot (no GRUB) |
| KVM acceleration | Not available (cross-arch) |
| Nix build | Requires aarch64 builder or `binfmt_misc` |
| Expected build time | ~30-60 min (emulated compilation) |
| Expected test time | ~10-30 min (emulated execution) |

**Current status**: Stub image definition exists at `nix/vm-image-aarch64.nix`.
Actual building requires either:
1. A native aarch64 builder (physical ARM64 machine or cloud instance)
2. QEMU user-mode emulation via `binfmt_misc` registered on the host
   (`boot.binfmt.emulatedSystems = [ "aarch64-linux" ];` on NixOS)

### Cross-Architecture Test Matrix (M5.5.2)

| Target | Builder | Runner | Status |
|--------|---------|--------|--------|
| x86_64-linux | x86_64 (KVM) | QEMU q35 (KVM) | Functional |
| aarch64-linux | aarch64 or binfmt | QEMU virt (emulated) | Stub |
| x86_64-linux (Firecracker) | x86_64 (KVM) | Firecracker | Scaffold |

Native ARM64 testing provides the strongest assurance. Emulated testing
catches architecture-specific issues (endianness, word size, alignment)
but at significant performance cost.

## Cross-Build Parity Checks (M3.2.2)

When cross-built artifacts are introduced (e.g., building arm64 on
x86_64), parity must be verified by comparing:

1. **Binary content**: SHA-256 of the stripped binary
2. **Library closure**: Set of shared libraries and their versions
3. **Test results**: Same test suites must pass on both builders
4. **Manifest fields**: ABI target, kernel minimum, glibc minimum

Cross-built artifacts are treated as **non-authoritative** (M3.2.1)
until a native builder for the target architecture produces the same
results. The support matrix (M3.2.3) documents which targets have
native vs. cross-built evidence.

Currently, no cross-built artifacts exist. All release targets are
either native (Linux x86_64) or source-only (Windows, macOS, BSD,
FreeDOS).

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

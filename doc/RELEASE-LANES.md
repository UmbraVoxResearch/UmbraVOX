# UmbraVOX Release Lanes

## Purpose

Define the execution model for deterministic release builds while keeping
native host builds available.

## Runner Classes

- `linux-hosted-fast`: regular CI/host runner, no KVM requirement.
- `linux-kvm-integration`: Linux runner with KVM for QEMU/KVM validation.
- `linux-kvm-release`: isolated Linux KVM runner for authoritative release jobs.
- `native-macos`: macOS native runner for source/native validation.
- `native-windows`: Windows native runner for source/native validation.

## Lane Model

1. `fast` lane
- build/test/verify via `nix-shell` + `make`.
- no VM requirement.

2. `integration` lane (QEMU/KVM)
- validates release scripts and bundle behavior in an isolated Linux VM.
- debug-first lane for VM/runtime issues.

3. `authoritative-release` lane (Firecracker preferred, QEMU fallback)
- builds release artifacts from pinned inputs.
- publishes manifests/checksums/SBOM/license artifacts.
- runs isolated smoke checks against produced bundles.

## Current Scope

- Linux release artifacts are executable bundles.
- non-Linux targets remain source releases until native artifact lanes are fully operational.

## Local Commands

```sh
nix-shell
make release-linux
make release-smoke-linux
make release-lane-qemu
make release-lane-firecracker
```

The `release-lane-*` commands are scaffolding entrypoints that currently check
host capability and print required environment/setup constraints.

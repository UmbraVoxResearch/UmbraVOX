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
- current scaffold validates host prerequisites for the planned QEMU/KVM lane.
- intended next step is a reproducible guest image running the release workflow
  in-guest.
- debug-first lane for VM/runtime issues once guest boot is implemented.

3. `authoritative-release` lane (Firecracker preferred, QEMU fallback)
- current scaffold validates host prerequisites for the planned Firecracker
  authoritative lane.
- intended next step is booting a pinned builder microVM and executing the
  release graph inside the guest.
- artifact publication and in-guest smoke execution are target behavior, not
  current behavior.

4. `microvm-smoke` lane (QEMU or Firecracker)
- current scaffold checks for a built Linux release artifact plus host
  prerequisites for the selected VMM.
- intended next step is booting a smoke guest and running bundle
  launch/manifest checks in-guest.
- no full VM or microVM boot is implemented yet.

## Current Scope

- Linux release artifacts are executable bundles.
- `make release-smoke-linux` performs the current isolated smoke check with
  `podman` or `docker`; it is not the microVM smoke lane.
- QEMU, Firecracker, and microVM smoke entrypoints are scaffolds with host
  capability checks and next-step messaging only.
- non-Linux targets remain source releases until native artifact lanes are fully operational.

## Local Commands

```sh
nix-shell
make release-linux
make release-smoke-linux
make release-lane-qemu
make release-lane-firecracker
./scripts/release-smoke-microvm.sh qemu
./scripts/release-smoke-microvm.sh firecracker
```

Current command behavior:

- `make release-smoke-linux` extracts the latest Linux bundle in a container
  and checks basic launch/linkage files.
- `make release-lane-qemu` and `make release-lane-firecracker` only verify
  host prerequisites and print the next implementation step.
- `scripts/release-smoke-microvm.sh <qemu|firecracker>` only verifies a Linux
  artifact exists, checks host prerequisites, and prints the next
  implementation step.

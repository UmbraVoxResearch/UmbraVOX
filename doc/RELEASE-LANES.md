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
- current entrypoint checks for a built Linux release artifact plus host
  prerequisites for the selected VMM.
- runner-hook execution is available now for both QEMU and Firecracker via
  `UMBRAVOX_QEMU_SMOKE_RUNNER` / `UMBRAVOX_FIRECRACKER_SMOKE_RUNNER`.
- direct pinned-input boot paths are also wired:
  QEMU via `UMBRAVOX_QEMU_*` inputs, Firecracker via
  `UMBRAVOX_FIRECRACKER_*` inputs.
- only the QEMU path currently has a documented deterministic command-line
  profile helper for in-guest smoke intent.
- this is not yet the authoritative release lane, and this document does not
  claim a maintained guest image or proven end-to-end in-guest boot workflow.

## Current Scope

- Linux release artifacts are executable bundles.
- `make release-smoke-linux` performs the current isolated smoke check with
  `podman` or `docker`; it is not the microVM smoke lane.
- QEMU and Firecracker release-lane entrypoints remain host-prerequisite
  scaffolds only.
- The microVM smoke entrypoint is partially wired beyond scaffold status:
  it can dispatch host runner hooks for both VMMs, QEMU can invoke a pinned
  boot command line directly, and Firecracker can invoke a pinned config file
  directly when all required inputs are supplied.
- None of that is a claim that UmbraVOX currently ships a maintained guest
  image, performs artifact handoff into a guest automatically, or proves the
  booted guest executes bundle checks end-to-end by default.
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
- `scripts/release-smoke-microvm.sh <qemu|firecracker>` verifies artifact +
  host prerequisites and can then either:
  run a host-provided runner hook via `UMBRAVOX_QEMU_SMOKE_RUNNER` or
  `UMBRAVOX_FIRECRACKER_SMOKE_RUNNER`, or
  invoke a direct pinned-input boot path for the selected VMM.
- QEMU mode also supports a pinned-boot path via
  `UMBRAVOX_QEMU_KERNEL` + `UMBRAVOX_QEMU_INITRD` +
  `UMBRAVOX_QEMU_ROOTFS` + `UMBRAVOX_QEMU_APPEND`.
- For deterministic command-line profiles, use
  `scripts/release-smoke-qemu-profile.sh bundle-basic` and pass the output as
  `UMBRAVOX_QEMU_APPEND`, or set `UMBRAVOX_QEMU_PROFILE=bundle-basic`.
- Firecracker mode accepts pinned inputs via
  `UMBRAVOX_FIRECRACKER_KERNEL` + `UMBRAVOX_FIRECRACKER_ROOTFS` +
  `UMBRAVOX_FIRECRACKER_CONFIG`.
- The default path for both VMMs is still messaging plus prerequisite checks
  unless one of those execution hooks/inputs is explicitly provided.

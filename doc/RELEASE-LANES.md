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

Current implementation note:

- `linux-hosted-fast` is exercised by the active `nix-shell` + `make` workflow.
- `linux-kvm-integration` and `linux-kvm-release` have repo-owned scaffold
  entrypoints and microVM smoke hooks, but not yet a maintained guest image or
  authoritative in-guest release graph.
- `native-macos` and `native-windows` are runner-class placeholders only today;
  this repository does not yet ship repo-owned native CI/release lanes for them.

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
- `make release-lane-readiness` now aggregates the repo-owned native runner
  readiness scripts for Linux x86_64, Linux arm64, macOS, Windows, and BSD.
- intended next step is booting a pinned builder microVM and executing the
  release graph inside the guest.
- artifact publication and in-guest smoke execution are target behavior, not
  current behavior.

4. `microvm-smoke` lane (QEMU or Firecracker)
- current entrypoint checks for a built Linux release artifact plus host
  prerequisites for the selected VMM.
- runner-hook execution is available now for both QEMU and Firecracker via
  `UMBRAVOX_QEMU_SMOKE_RUNNER` / `UMBRAVOX_FIRECRACKER_SMOKE_RUNNER`.
- direct pinned-input boot paths are also wired and invocable:
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
- `make release-smoke-qemu` and `make release-smoke-firecracker` are the
  microVM smoke entrypoints.
- `make release-smoke-qemu-profile` adds the deterministic QEMU profile
  wrapper around `make release-smoke-qemu`.
- `make release-smoke-firecracker-pinned` adds the pinned-input wrapper around
  `make release-smoke-firecracker`.
- `make release-lane-qemu` and `make release-lane-firecracker` remain
  host-prerequisite scaffolds only.
- `make release-lane-readiness` runs the aggregate native runner readiness
  scripts and reports the Linux x86_64 lane as required while the Linux arm64,
  macOS, Windows, and BSD lanes remain informational.
- `make platform-sanity` and `make sanity` only verify that the related lane
  scripts/helpers are wired into the tree.
- None of that is a claim that UmbraVOX currently ships a maintained guest
  image, performs artifact handoff into a guest automatically, or proves the
  booted guest executes bundle checks end-to-end by default.
- non-Linux targets remain source releases until native artifact lanes are fully operational.

## Aggregate Readiness Checks

Current repo-owned readiness checks across the release lanes are:

- `fast`: `make build`, `make test`, and `make verify`, with `make quality`
  aggregating the active host-side release gate.
- Linux artifact packaging: `make release-linux` plus the manifest/digest
  outputs recorded in each staged release artifact.
- Linux smoke validation: `make release-smoke-linux` for the current
  container-based isolated bundle launch/linkage check.
- microVM readiness baseline: `make release-smoke-qemu` and
  `make release-smoke-firecracker` verify artifact presence, selected VMM
  availability, and `/dev/kvm` before any runner hook or pinned boot path.
- microVM direct invocation readiness: QEMU and Firecracker can both execute
  caller-supplied runner hooks or pinned boot inputs when those inputs are
  provided explicitly.

Remaining gaps before these lanes become authoritative release evidence:

- no maintained repo-owned guest image/rootfs currently performs bundle
  verification in-guest by default
- no maintained Firecracker guest-image/config pair currently defines the
  in-guest verification command for M2.4.4.c.4
- no authoritative Firecracker/QEMU in-guest release graph currently produces
  the final Linux artifact inside the guest
- Linux x86_64 still lacks dedicated repo-owned native runner evidence beyond
  the host packaging workflow, including runner logs for build/test/verify
- macOS, Windows, BSD, and Linux arm64 do not yet have repo-owned native lanes
  with recorded build/test/release evidence
- cross-target parity evidence is still missing for any future non-native or
  cross-built artifact claims

## Platform Lane Posture

| Target | Intended lane / builder class | Current repo-owned state | Current artifact posture |
| --- | --- | --- | --- |
| Linux x86_64 | `linux-hosted-fast` now; `linux-kvm-integration` / `linux-kvm-release` later | Native host packaging works now; QEMU/Firecracker lane entrypoints are scaffolded only | Native Linux terminal bundle is the only current prebuilt artifact |
| Linux arm64 | future native Linux arm64 runner | no repo-owned lane yet | no current release artifact |
| macOS terminal | `native-macos` | runner class reserved only; no repo-owned native lane yet | source release with native build instructions |
| Windows CLI | `native-windows` | runner class reserved only; no repo-owned native lane yet | source release with native build instructions |
| BSD terminal | future BSD-native runner | no repo-owned lane yet | source release with native build instructions |
| FreeDOS research | no supported native runtime lane planned in current MVP | no native lane | research/source release only; unsupported runtime |

## Cross-Target Policy

- A target is authoritative only when the artifact is produced and validated on
  its intended native or isolated release lane.
- The current repository does not claim cross-built Windows, macOS, BSD, or
  FreeDOS binaries as release-quality native artifacts.
- Until native parity evidence exists, any future cross-built artifacts remain
  non-authoritative and must not be presented as equivalent to native-built
  outputs.
- Current non-Linux release targets therefore stay explicit source releases
  instead of shipping unverified cross-built binaries.
- The future authoritative Linux lane is the in-guest Firecracker/QEMU release
  path described above; that execution model is not complete yet.

## Local Commands

```sh
nix-shell
make release-linux
make release-smoke-linux
make release-smoke-qemu
make release-smoke-qemu-profile
make release-smoke-firecracker
make release-smoke-firecracker-pinned
make release-lane-qemu
make release-lane-firecracker
make release-lane-readiness
make platform-sanity
make sanity
```

Current command behavior:

- `make release-smoke-linux` extracts the latest Linux bundle in a container
  and checks basic launch/linkage files.
- `make release-lane-qemu` and `make release-lane-firecracker` only verify
  host prerequisites and print the next implementation step.
- `make release-lane-readiness` runs the aggregate native runner readiness
  checks and exits non-zero if the Linux x86_64 lane is blocked.
- `make platform-sanity` and `make sanity` verify Makefile wiring for the
  helper scripts and current lane targets.
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
- Firecracker currently relies on the caller to supply a maintained kernel,
  rootfs, and config that actually perform in-guest bundle verification; the
  repo does not yet define or ship those inputs.
- The default path for both VMMs is still messaging plus prerequisite checks
  unless one of those execution hooks/inputs is explicitly provided.

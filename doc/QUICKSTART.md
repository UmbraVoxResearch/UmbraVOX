# UmbraVOX Quickstart

## Prerequisites

- [Nix](https://nixos.org/download.html)

All build and verification tooling is provided through `nix-shell`.
Flake commands are also supported through `scripts/nix-flake.sh`.

The repository preserves 30 generated artifacts from 10 `.spec` files. In the
active build/test graph, generated coverage is currently limited to the parser
path and generated SHA-256 parity; the remaining generated outputs are
preserved artifacts pending broader wiring.

## Common Commands

```bash
nix-shell
make build
make build-haskell
make run
make test
make test-haskell
make test-core
make test-core-crypto
make test-core-network
make test-core-chat
make test-core-tui
make test-core-tools
make test-tcp
make test-fault
make test-recovery
make test-tui-sim
make test-integrity
make test-mdns
make test-deferred
make soak
make verify
make verify-haskell
make quality
make evidence
make release-linux
make release-appimage
make release-windows-cli
make release-macos-terminal
make release-bsd-terminal
make release-freedos
make release
make release-smoke-linux
make release-smoke-appimage
make release-smoke-qemu
make release-smoke-qemu-profile
make release-smoke-firecracker
make release-smoke-firecracker-pinned
make release-lane-qemu
make release-lane-firecracker
make release-lane-readiness
make release-sbom-generate
make release-license-bundle-generate
make release-license-check
make release-linking
make release-manifest
make release-checksums
make release-gate-assurance
make vm-smoke
make vm-image-build
make firecracker-smoke
make firecracker-image-build
make vm-image-clean
make platform-sanity
make sanity
scripts/nix-flake.sh flake show --no-write-lock-file
```

## What Each Test Target Does

- `make test` runs the required messaging-MVP gate.
- It streams the full live suite output and writes a per-run log under `build/test-artifacts/`.
- `make test-core` runs the full deterministic core suite.
- `make test-core-crypto` runs deterministic crypto/unit coverage only.
- `make test-core-network` runs deterministic network/discovery coverage only.
- `make test-core-chat` runs deterministic chat/protocol coverage only.
- `make test-core-tui` runs deterministic non-simulated TUI coverage only.
- `make test-core-tools` runs deterministic codegen/tools/fuzz coverage only.
- `make test-tcp` runs real localhost TCP messaging scenarios.
- `make test-fault` runs adversarial and malformed-input scenarios.
- `make test-recovery` runs persistence and restart coverage.
- `make test-tui-sim` runs TUI simulation coverage outside the fast gate.
- `make test-integrity` runs explicit wire-format and cryptographic integrity coverage.
- `make test-mdns` runs only the exact mDNS/discovery suite.
- `make test-deferred` runs preserved blockchain, storage, consensus, and economics suites outside the MVP gate.
- Exact runner names such as `mdns`, `sha256`, or `tui-sim-dialogs` are also accepted via `cabal test umbravox-test --test-options='<suite>'`.
- `make soak` runs the longer stress suite and writes artifacts under `build/test-artifacts/`.
- `make verify` runs the F* verification pass.
- `make build-haskell`, `make test-haskell`, and `make verify-haskell` are opt-in bridge wrappers. They use the legacy `Makefile` path by default and only switch to the Haskell orchestration path when `UMBRAVOX_USE_HASKELL_ORCH=1` is set.
- `make quality` runs the full build pipeline and is equivalent to `make` (`build + test + verify + complexity + lint + license + format-check`).
- `make evidence` runs `make quality` and writes a timestamped publication evidence bundle under `build/evidence/` with logs, git metadata, and copied test artifacts.
- `make release-linux` builds a portable Linux x86_64 terminal bundle with a patched local loader/lib set.
- `make release-appimage` builds an experimental AppDir-style scaffold derived from the Linux bundle and does not yet claim a supported single-file artifact.
- `make release-windows-cli` writes a Windows CLI source-release zip with native build instructions.
- `make release-macos-terminal` writes a macOS terminal source-release tarball with native build instructions.
- `make release-bsd-terminal` writes a BSD terminal source-release tarball with native build instructions.
- `make release-freedos` writes a FreeDOS research/source release zip with an explicit unsupported-runtime note.
- `make release` builds every currently defined release artifact under `build/releases/`.
- `make release-smoke-linux` runs the working container-based Linux bundle smoke check.
- `make release-smoke-appimage` runs the non-authoritative AppImage scaffold smoke placeholder and only checks scaffold layout.
- `make release-smoke-qemu` runs the QEMU microVM smoke entrypoint:
  it checks artifact + host prerequisites, then either executes a runner hook
  (`UMBRAVOX_QEMU_SMOKE_RUNNER`) or invokes a direct pinned-input boot path
  from `UMBRAVOX_QEMU_*` inputs.
- `make release-smoke-qemu-profile` is the deterministic wrapper for the QEMU
  microVM smoke entrypoint and sets `QEMU_SMOKE_PROFILE=bundle-basic` unless
  overridden.
- `make release-smoke-firecracker` runs the Firecracker microVM smoke entrypoint:
  it checks artifact + host prerequisites, then either executes a runner hook
  (`UMBRAVOX_FIRECRACKER_SMOKE_RUNNER`) or invokes a direct pinned-input boot
  path from `UMBRAVOX_FIRECRACKER_*` inputs.
- `make release-smoke-firecracker-pinned` is the pinned-input wrapper for the
  Firecracker microVM smoke entrypoint and passes the `Makefile` variables
  through to the script.
- `make release-lane-qemu` and `make release-lane-firecracker` are still
  host-prerequisite scaffolds for future in-guest release execution; they do
  not yet run the release graph inside a guest.
- `make release-lane-readiness` aggregates the current native runner readiness
  checks and treats Linux x86_64 as required while Linux arm64, macOS,
  Windows, and BSD remain informational.
- `make platform-sanity` and `make sanity` check that the helper scripts and
  lane wiring are still present in the tree.
- Release/readiness orchestration now routes through Haskell entrypoints
  via `cabal run umbravox -- <command>`. Shell scripts in `scripts/` are
  preserved for reference but are no longer the primary execution path.
- `make release-lane-readiness-haskell` is the current opt-in Haskell bridge
  for readiness checks; it shells out to the existing aggregate script.
- `make build-haskell`, `make test-haskell`, and `make verify-haskell` are
  bridge wrappers for the corresponding build/test/verify commands. They stay
  on the legacy `Makefile` path unless `UMBRAVOX_USE_HASKELL_ORCH=1` is set.
- `make release-sbom-generate` generates a Software Bill of Materials
  listing all Haskell dependencies, C sources, and build tools with their
  licenses.
- `make release-license-bundle-generate` generates aggregated third-party
  license text for all dependencies.
- `make release-license-check` enforces the license allow-list policy
  against all known dependencies.
- `make release-linking` analyzes static vs dynamic linking obligations
  for the release bundle.
- `make release-manifest` generates a release provenance manifest with
  git commit, tag, builder info, and SHA-256 digests for all artifacts
  under `build/releases/`.
- `make release-checksums` emits SHA-256 checksums for all release
  artifacts to stdout and `build/releases/SHA256SUMS.txt`.
- `make release-gate-assurance` checks that the assurance matrix
  (`doc/assurance-matrix.md`) is present, contains the required sections,
  and was updated at least as recently as any material crypto source change.
- Only the QEMU microVM path currently has a documented deterministic
  command-line smoke profile helper:
  `scripts/release-smoke-qemu-profile.sh bundle-basic`.
- The Firecracker microVM path can boot from pinned caller-supplied inputs, but
  the repo still does not define a maintained Firecracker guest image/config
  that performs bundle verification in-guest by default.
- `make vm-smoke` boots a NixOS QEMU VM with the full development
  toolchain pre-installed and runs the complete pipeline (build, test,
  verify, complexity, license, format-check, release-linux) inside the
  isolated guest. No network access is needed in-guest. The VM image is
  cached at `build/vm/image` and only rebuilt when `flake.nix` or
  `flake.lock` change.
- To capture VM smoke evidence for release qualification, redirect the
  output to `build/evidence/`:
  ```bash
  make vm-smoke 2>&1 | tee build/evidence/vm-smoke-linux-x86_64.log
  ```
  The log includes kernel version, hostname, git commit, artifact
  SHA-256, and per-step pass/fail results. This serves as the
  dedicated Linux x86_64 runner evidence (M3.1.1.b) once all 8 steps
  pass.
- `make vm-image-build` builds and caches the NixOS VM image without
  running the smoke pipeline.
- `make vm-image-clean` removes the cached VM image.
- `make firecracker-smoke` boots a Firecracker microVM with the NixOS
  guest image and runs the full isolated pipeline. Requires KVM and
  the `firecracker` binary (both provided by nix-shell).
- `make firecracker-image-build` builds and caches the Firecracker guest
  image (vmlinux kernel + ext4 rootfs) without running the pipeline.
- Flake parity commands are available through `flake.nix` apps/checks/packages.
  Use `scripts/nix-flake.sh ...` to avoid repeating feature flags.
- Release packaging now fails by default on dirty or untagged commits.
  CI/local overrides are explicit: `UMBRAVOX_ALLOW_DIRTY_RELEASE=1` and
  `UMBRAVOX_ALLOW_UNTAGGED_RELEASE=1`.
- `license` is blocking; `lint` and `format-check` are advisory/non-blocking in the current pipeline.

## Aggregate Readiness Check

- Use `make quality` as the current aggregate host-side readiness gate
  (`build + test + verify + complexity + lint + license + format-check`).
- Use `make release-linux` to stage the only current prebuilt native artifact.
- Use `make release-smoke-linux` to run the current isolated Linux bundle smoke
  check.
- Use `make release-smoke-qemu` or `make release-smoke-firecracker` to confirm
  microVM prerequisites and then exercise either runner-hook or pinned-boot
  smoke inputs when those are supplied.
- Remaining readiness gaps are unchanged: no maintained repo-owned guest image
  performs in-guest bundle verification by default, no authoritative in-guest
  release graph exists yet, and non-Linux targets remain source releases until
  native lanes and parity evidence exist.
- The AppImage track is intentionally experimental and scaffold-only until
  support policy and parity evidence are proven.
- The shell-to-Haskell migration is complete for all orchestration targets.
  All Makefile targets now route through Haskell entrypoints. Shell scripts
  are preserved in `scripts/` for reference only.
- The readiness bridge is currently Haskell-backed only for the aggregate
  readiness command; all other orchestration remains shell-driven.

## Release Target Posture

- Linux x86_64 is the only current prebuilt native artifact lane.
- Windows CLI, macOS terminal, and BSD terminal currently remain explicit
  source releases until repo-owned native builders and parity evidence exist.
- FreeDOS remains a research/source release with an unsupported-runtime note.
- QEMU and Firecracker entrypoints are release-lane scaffolding plus smoke-path
  hooks; they are not yet authoritative in-guest release execution.
- If cross-built artifacts are introduced later, they should be treated as
  non-authoritative until native parity evidence exists.

## First Run

1. Enter `nix-shell`.
2. Run `make build`.
3. Launch the app with `make run` or `cabal run umbravox`.
4. Open **Contacts** with `F2` and create a new single-peer connection.
5. Enter the remote `host:port` and let the Noise_IK handshake complete.

## Scope Note

The active MVP is direct peer-to-peer messaging. Deferred blockchain and
economics material remains preserved in `attic/doc-legacy-2026-04-28/`.

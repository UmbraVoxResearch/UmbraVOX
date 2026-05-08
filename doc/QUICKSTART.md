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
make run
make test
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
make quality
make evidence
make release-linux
make release-windows-cli
make release-macos-terminal
make release-bsd-terminal
make release-freedos
make release
make release-smoke-linux
make release-smoke-qemu
make release-smoke-firecracker
make release-lane-qemu
make release-lane-firecracker
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
- `make quality` runs the full build pipeline and is equivalent to `make` (`build + test + verify + complexity + lint + license + format-check`).
- `make evidence` runs `make quality` and writes a timestamped publication evidence bundle under `build/evidence/` with logs, git metadata, and copied test artifacts.
- `make release-linux` builds a portable Linux x86_64 terminal bundle with a patched local loader/lib set.
- `make release-windows-cli` writes a Windows CLI source-release zip with native build instructions.
- `make release-macos-terminal` writes a macOS terminal source-release tarball with native build instructions.
- `make release-bsd-terminal` writes a BSD terminal source-release tarball with native build instructions.
- `make release-freedos` writes a FreeDOS research/source release zip with an explicit unsupported-runtime note.
- `make release` builds every currently defined release artifact under `build/releases/`.
- `make release-smoke-linux` runs the working container-based Linux bundle smoke check.
- `make release-smoke-qemu` runs the QEMU microVM smoke entrypoint:
  it checks artifact + host prerequisites, then either executes a runner hook
  (`UMBRAVOX_QEMU_SMOKE_RUNNER`) or invokes a direct pinned-input boot path
  from `UMBRAVOX_QEMU_*` inputs.
- `make release-smoke-firecracker` runs the Firecracker microVM smoke entrypoint:
  it checks artifact + host prerequisites, then either executes a runner hook
  (`UMBRAVOX_FIRECRACKER_SMOKE_RUNNER`) or invokes a direct pinned-input boot
  path from `UMBRAVOX_FIRECRACKER_*` inputs.
- `make release-lane-qemu` and `make release-lane-firecracker` are still
  host-prerequisite scaffolds for future in-guest release execution; they do
  not yet run the release graph inside a guest.
- Only the QEMU microVM path currently has a documented deterministic
  command-line smoke profile helper:
  `scripts/release-smoke-qemu-profile.sh bundle-basic`.
- Flake parity commands are available through `flake.nix` apps/checks/packages.
  Use `scripts/nix-flake.sh ...` to avoid repeating feature flags.
- Release packaging now fails by default on dirty or untagged commits.
  CI/local overrides are explicit: `UMBRAVOX_ALLOW_DIRTY_RELEASE=1` and
  `UMBRAVOX_ALLOW_UNTAGGED_RELEASE=1`.
- `license` is blocking; `lint` and `format-check` are advisory/non-blocking in the current pipeline.

## First Run

1. Enter `nix-shell`.
2. Run `make build`.
3. Launch the app with `make run` or `cabal run umbravox`.
4. Open **Contacts** with `F2` and create a new single-peer connection.
5. Enter the remote `host:port` and let the Noise_IK handshake complete.

## Scope Note

The active MVP is direct peer-to-peer messaging. Deferred blockchain and
economics material remains preserved in `attic/doc-legacy-2026-04-28/`.

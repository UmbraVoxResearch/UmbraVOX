# UmbraVOX Quickstart

## Prerequisites

- [Nix](https://nixos.org/download.html)
- Linux host with KVM support (`/dev/kvm` must exist)

All build, test, and verification commands route through an isolated NixOS VM
by default.  The host only needs QEMU and git (provided by nix-shell).
No local GHC, cabal, or F* installation is required for standard operations.

The `./uv` driver auto-compiles on first use -- no manual build step is needed.

Flake commands are also supported through `scripts/nix-flake.sh`.

The repository preserves 54 generated artifacts from 18 `.spec` files. In the
active build/test graph, generated coverage is currently limited to the parser
path and generated SHA-256 parity; the remaining generated outputs are
preserved artifacts pending broader wiring.

## Common Commands (VM-First)

All `./uv` commands run inside the NixOS VM by default when the VM image
is present.  If no VM image is found, VM-routed commands fail with an explicit
instruction to run `./uv vm build-image` first. Host-local compilation is
disabled.

```bash
nix-shell shell-minimal.nix
./uv vm build-image    # Build the NixOS dev VM (once)
./uv build             # Build (routes to VM by default)
./uv test              # Run tests
./uv test soak         # Longer stress suite
./uv verify            # F* verification pass
./uv                   # Fast gate (build + test + check)
./uv dev               # Interactive development shell inside the NixOS VM
./uv release linux     # Build portable Linux x86_64 bundle
./uv clean             # Clean build artifacts
scripts/nix-flake.sh flake show --no-write-lock-file
```

## What Each Test Target Does

- `./uv test` runs the required messaging-MVP gate.
- It streams the full live suite output and writes a per-run log under `build/test-artifacts/`.
- `./uv test all` runs every named suite sequentially in a single VM session (~30 min).
- `./uv test soak` runs the longer stress suite and writes artifacts under `build/test-artifacts/`.
- `./uv verify` runs the F* verification pass.
- `./uv` (no args) runs the fast gate (`build + test + check`).
- Full gate: `./uv build && ./uv test && ./uv verify && ./uv check`.
- `./uv check` runs lint + format + license + complexity + generated-headers + constant-time-branches gates.
- `./uv build` builds library + executables (routes to VM by default).
- `./uv release linux` builds a portable Linux x86_64 terminal bundle with a patched local loader/lib set.
- `./uv dev` opens an interactive development shell inside the NixOS VM with
  the full toolchain (GHC 9.14, Cabal, F*, Z3, Coq, AFL++, valgrind).
- `./uv vm build-image` builds and caches the NixOS VM image without
  running the smoke pipeline.
- `./uv clean` removes build artifacts including the cached VM image.
- Exact runner names such as `mdns`, `sha256`, or `tui-sim-dialogs` are also accepted via `cabal test umbravox-test --test-options='<suite>'`.
- Flake parity commands are available through `flake.nix` apps/checks/packages.
  Use `scripts/nix-flake.sh ...` to avoid repeating feature flags.
- Release packaging now fails by default on dirty or untagged commits.
  CI/local overrides are explicit: `UMBRAVOX_ALLOW_DIRTY_RELEASE=1` and
  `UMBRAVOX_ALLOW_UNTAGGED_RELEASE=1`.
- `license` is blocking; `lint` and `format-check` are advisory/non-blocking in the current pipeline.

## VM-First Development Model

All standard `./uv` commands (`build`, `test`, `verify`, etc.) route through
an isolated NixOS VM by default.  The host only needs QEMU and git.

| Command | Description |
|---------|-------------|
| `nix-shell shell-minimal.nix` | Orchestration-only shell (QEMU, git -- no cabal/ghc) |
| `./uv vm build-image` | Build the NixOS dev VM (once) |
| `./uv build` | Build library + executables (routes to VM by default) |
| `./uv test` | Run `required` test suite (routes to VM by default) |
| `./uv test all` | Run all 15 named suites in a single VM session (~30 min) |
| `./uv verify` | Run F* formal verification (routes to VM by default) |
| `./uv` | Fast gate (build + test + check) |
| `./uv check` | Lint + format + license + complexity + generated-headers + constant-time-branches |
| `./uv vm signal test` | Signal-Server integration test suite (Go) |
| `./uv dev` | Interactive development shell inside the NixOS VM |
| `./uv release linux` | Build portable Linux x86_64 bundle |
| `./uv clean` | Clean build artifacts |
| `nix-shell` | Full local shell (commands still route to VM by default) |

Inside `./uv dev`, you have the full toolchain (GHC 9.14, Cabal, F*, Z3,
Coq, AFL++, valgrind) and can run any command interactively. The source is
mounted read-only at `/mnt/src` and copied to `/work/umbravox` on boot.

See `doc/VM-DEVELOPMENT.md` for the full VM development guide and troubleshooting.

## Aggregate Readiness Check

- Use `./uv` (no args) as the fast aggregate gate (`build + test + check`).
- For the full readiness gate (includes formal verification), run:
  `./uv build && ./uv test && ./uv verify && ./uv check`.
- Use `./uv release linux` to stage the only current prebuilt native artifact.
- Remaining readiness gaps are unchanged: no maintained repo-owned guest image
  performs in-guest bundle verification by default, no authoritative in-guest
  release graph exists yet, and non-Linux targets remain source releases until
  native lanes and parity evidence exist.
- The AppImage track is intentionally experimental and scaffold-only until
  support policy and parity evidence are proven.
- The `./uv` driver consolidates the build orchestration layer. Lane checks,
  SBOM, compliance, VM orchestration, and traffic verification targets route
  through Haskell entrypoints. Release packaging, smoke, and platform-lane
  targets still invoke shell scripts directly.

## Release Target Posture

- Linux x86_64 is the only current prebuilt native artifact lane.
- Windows CLI, macOS terminal, and BSD terminal currently remain explicit
  source releases until repo-owned native builders and parity evidence exist.
- FreeDOS remains a research/source release with an unsupported-runtime note.
- QEMU entrypoints are release-lane scaffolding plus smoke-path hooks; they
  are not yet authoritative in-guest release execution.
- If cross-built artifacts are introduced later, they should be treated as
  non-authoritative until native parity evidence exists.

## First Run

1. Enter `nix-shell shell-minimal.nix`.
2. Run `./uv vm build-image` (first time only -- builds and caches the NixOS VM image; `./uv` auto-compiles on first use).
3. Run `./uv build` (builds everything inside the VM).
4. Run `./uv dev` to enter the interactive VM shell, then launch the app.
5. Click `[ New ]` in the contacts toolbar (below the contact list) to start a new connection.
6. Enter the remote `host:port` and let the Noise_IK handshake complete.

## Scope Note

The active MVP is direct peer-to-peer messaging. Deferred blockchain and
economics material remains preserved in `attic/doc-legacy-2026-04-28/`.

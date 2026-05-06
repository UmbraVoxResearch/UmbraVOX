# UmbraVOX Quickstart

## Prerequisites

- [Nix](https://nixos.org/download.html)

All build and verification tooling is provided through `nix-shell`.

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
make test-tcp
make test-fault
make test-recovery
make test-tui-sim
make test-integrity
make test-deferred
make soak
make verify
make quality
```

## What Each Test Target Does

- `make test` runs the required messaging-MVP gate.
- It streams the full live suite output and writes a per-run log under `build/test-artifacts/`.
- `make test-core` runs deterministic crypto, protocol, TUI, and codegen coverage.
- `make test-tcp` runs real localhost TCP messaging scenarios.
- `make test-fault` runs adversarial and malformed-input scenarios.
- `make test-recovery` runs persistence and restart coverage.
- `make test-tui-sim` runs TUI simulation coverage outside the fast gate.
- `make test-integrity` runs explicit wire-format and cryptographic integrity coverage.
- `make test-deferred` runs preserved blockchain, storage, consensus, and economics suites outside the MVP gate.
- `make soak` runs the longer stress suite and writes artifacts under `build/test-artifacts/`.
- `make verify` runs the F* verification pass.
- `make quality` runs the full build pipeline and is equivalent to `make` (`build + test + verify + complexity + lint + license + format-check`).
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

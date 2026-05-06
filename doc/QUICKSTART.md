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

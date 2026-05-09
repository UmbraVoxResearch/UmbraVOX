# UmbraVOX Active Architecture

## Runtime Stack

```text
TUI
  -> Chat session and contact flows
  -> Signal protocol state
  -> PQ wrapper
  -> Noise_IK transport
  -> TCP sockets
  -> Temporary sqlite3-backed persistence
```

## Release Orchestration

- Build and release orchestration currently uses `Makefile` targets plus shell
  scripts as a bridge layer.
- That bridge exists to keep release and readiness commands runnable while the
  long-term Haskell entrypoints are introduced.
- The migration target is phased: first mirror the current shell behavior in
  Haskell, then route wrappers through Haskell entrypoints, and only then
  retire shell-specific logic after logs, exit codes, and coverage match.
- Current docs and TODOs should be read as bridge mode, not as a completed
  Haskell orchestration migration.

## Active Modules

- `Crypto/` and `Crypto/Signal/` implement the messaging cryptography.
- `Network/` implements transport, Noise, mDNS, and peer exchange.
- `Chat/` implements session and application messaging behavior.
- `Storage/Anthony` currently wraps a temporary sqlite3-backed persistence shim for the MVP.
- `TUI/` provides the terminal application surface.
- `Tools/` contains quality-gate helpers including F* verification.

## Test Architecture

- `test/Main.hs` exposes tiered suite entrypoints.
- `required` is the fast messaging gate.
- `deferred` keeps preserved blockchain/economics/storage suites explicit but separate.
- `soak` is intentionally outside the fast gate.

## Legacy Design Archive

The broader long-term design and hardening documents now live in
`attic/doc-legacy-2026-04-28/`.

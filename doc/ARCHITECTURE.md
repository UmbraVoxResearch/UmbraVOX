# UmbraVOX Active Architecture

## Runtime Stack

```text
TUI (interactive surface)
  -> Runtime.Headless (terminal-independent runtime core)
  -> Chat session and contact flows
  -> Signal protocol state
  -> PQ wrapper
  -> Noise_IK transport
  -> TCP sockets
  -> Temporary sqlite3-backed persistence
```

## UI/Runtime Separation

- `UmbraVox.Runtime.Headless` owns runtime bootstrap and non-interactive
  execution paths.
- `UmbraVox.TUI.*` owns rendering, input handling, and menu/dialog interaction.
- Shared behavior (network, chat flow, identity/session plumbing) remains below
  the UI boundary so integration tests and VM orchestration can run without a
  terminal UI.

This boundary is intentional: feature behavior should be implemented in shared
runtime/application layers, while TUI code stays focused on presentation.

## Responsive Grid Architecture (TUI)

The active TUI uses a responsive grid layout strategy:

- Wide viewports present multi-pane composition for concurrent context (contacts,
  conversation, and status).
- Reduced viewports rebalance panes into compact/stacked layouts while
  preserving keyboard navigation.
- Modal overlays and menu flows remain consistent across breakpoints.

Responsive behavior is UI-only. Runtime, wire behavior, and assurance boundaries
are unchanged by layout mode.

## VM-First Development Model

All standard `make` targets (`build`, `test`, `verify`, `quality`, etc.)
route through an isolated NixOS QEMU VM by default.  The host only needs
orchestration tools (QEMU, git, make) provided by `shell-minimal.nix`.
The full development toolchain (GHC 9.6, Cabal, F*, Z3, Coq, AFL++, etc.)
lives inside the VM image built from `nix/vm-image.nix`.

Host-local compile bypass is disabled. See `doc/VM-DEVELOPMENT.md` for
details.

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

- `App/` — application configuration, startup, and runtime support:
  - `App.Config` — `AppConfig` with plugin registry, storage handles, ephemeral mode.
  - `App.ConfigFile` — config file parsing (`~/.umbravox/config`) with SHA-256 hash pinning.
  - `App.Startup` — identity resolution, persistence preference, disk-write guards.
  - `App.SwapCheck` — Linux swap detection for ephemeral mode warnings.
  - `App.RuntimeLog` — redacted runtime logging (15 sensitive fields filtered).
- `Crypto/` and `Crypto/Signal/` implement the messaging cryptography.
  - `Crypto.SecureBytes` — pinned, zeroed, mlock'd key buffers with C FFI (`csrc/secure_zero.c`, `csrc/secure_mlock.c`).
- `Network/` implements transport (TCP, UDP, SOCKS5, IPC), Noise, mDNS, and peer exchange.
- `Chat/` implements session and application messaging behavior.
- `Plugin/` — ephemeral-by-default plugin framework (M17):
  - `Plugin.Types` — `PluginDef`, `PluginType` (Function/Transaction/Meta), `PluginRegistry`.
  - `Plugin.Registry` — dependency resolution, enable/disable API, 8 persistence plugins.
- `Storage/` — pluggable persistence backends:
  - `Storage.Class` — abstract `StorageHandle` interface (15 function fields).
  - `Storage.InMemory` — IORef-based in-memory backend (zero disk writes, used in ephemeral mode).
  - `Storage.Anthony` — SQLite-backed persistence (via external `anthony` CLI tool).
  - `Storage.Encryption` — app-layer AEAD encryption for at-rest fields.
- `TUI/` provides the terminal application surface (rich text editor, markdown rendering, emoji picker).
- `Runtime/Headless` — terminal-independent runtime for VM/integration testing.
- `Tools/` contains quality-gate helpers including F* verification, release orchestration, and VM smoke testing.

## Differential Testing

Multi-oracle differential testing validates that the active Haskell crypto
implementations produce correct output for all supported primitives:

- 10 primitive suites test against RFC/NIST vectors (SHA-256, SHA-512, SHA-3,
  HMAC, HKDF, X25519, Ed25519, AES-GCM, ChaCha20-Poly1305, Poly1305).
- 4 negative suites verify fail-closed rejection (wrong key, bitflip,
  truncated ciphertext, Ed25519 malleability).
- 8 metamorphic suites check self-consistency properties (AEAD roundtrip,
  sign-verify, commutativity, determinism, avalanche).

All 22 suites are part of `make test-differential` (Tier 1, <90s) and are
wired into the required test gate via `coreCryptoSuites`.

## Test Architecture

- `test/Main.hs` exposes tiered suite entrypoints.
- `required` is the fast messaging gate.
- `deferred` keeps preserved blockchain/economics/storage suites explicit but separate.
- `soak` is intentionally outside the fast gate.

## Legacy Design Archive

The broader long-term design and hardening documents now live in
`attic/doc-legacy-2026-04-28/`.

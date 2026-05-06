# UmbraVOX MVP Plan

## Status: Messaging MVP Hardening In Progress (2026-04-28)

The direct messaging MVP remains the active implementation target.

Current in-scope MVP surface:

- codegen-backed crypto and generated coverage
- PQXDH + Signal Double Ratchet messaging
- Noise_IK over TCP
- TUI workflows for direct chat
- mDNS + peer exchange
- Anthony-backed messaging persistence
- local identity persistence across restart

Deferred but preserved in the codebase and documentation:

- consensus and blockchain transport
- chain/state/checkpoint storage
- chat-to-blockchain transaction packaging
- token economics implementation
- advanced network overlays such as gossip, Dandelion++, and chain sync

## Verification Strategy

The MVP verification flow is now tiered instead of one flat aggregate run.

- `make test`
  Runs the required messaging-MVP gate: core + TCP + fault + recovery.
- `make test-core`
  Runs deterministic crypto, protocol, TUI simulation, codegen, and loopback messaging coverage.
- `make test-tcp`
  Runs real localhost TCP end-to-end scenarios.
- `make test-fault`
  Runs adversarial malformed-input, replay, corruption, truncation, reordering, and disconnect scenarios.
- `make test-recovery`
  Runs persistence and restart-oriented scenarios.
- `make test-deferred`
  Runs the preserved blockchain, storage, consensus, and economics suites outside the messaging-MVP fast gate.
- `make soak`
  Runs the longer stress suite and writes `build/test-artifacts/soak-report.txt`.

## Current Implementation Focus

### 1. Preserve codegen as a first-class source of truth

- Keep the `.spec -> Haskell/C/FFI` pipeline intact.
- Keep generated-code validation in the required gate.
- Prefer extending tests around generated behavior rather than bypassing the generator.

### 2. Harden the direct messaging stack

- Verify the chat pipeline end-to-end over both loopback and real TCP.
- Exercise failure modes at the transport and parser boundaries.
- Validate persistence/reopen behavior for conversations, trusted keys, and local identity.

### 3. Keep deferred subsystems visible

- Do not remove unfinished blockchain-facing design or stub modules from the documentation.
- Keep deferred modules compiling.
- Keep deferred scope clearly marked as not required for the messaging-MVP hardening gate.
- Keep the deferred test surface runnable through `make test-deferred`.

## Acceptance Criteria

The messaging MVP is considered hardened when all of the following hold:

- `make test` passes
- `make soak` passes when explicitly run
- direct TCP chat scenarios pass with real sockets
- fault-injection scenarios fail gracefully without crashes or hangs
- persistence and restart-oriented scenarios preserve local messaging state as intended
- codegen and generated-coverage checks remain part of the required path

## Notes

This document tracks the active messaging MVP only. The broader blockchain design remains documented in the consensus, truncation, economics, and future-planning documents and is intentionally not removed.

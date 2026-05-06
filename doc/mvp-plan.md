# UmbraVOX MVP Plan

## Status

Messaging MVP hardening and completion are the active priorities on `main`.

## In Scope

- codegen-backed crypto and generated coverage
- PQXDH, Signal Double Ratchet, and messaging workflows
- Noise_IK over real TCP
- TUI workflows for connecting and chatting
- temporary sqlite3-backed persistence used by messaging
- identity persistence and restart behavior
- F* verification and hardening tests

## Out of Scope for the Active MVP Gate

- blockchain transport
- chat-to-chain transaction packaging
- consensus and truncation
- chain/state/checkpoint expansion
- token economics implementation

Those areas remain preserved in the codebase and in
`attic/doc-legacy-2026-04-28/`.

## Verification Workflow

- `make test` runs the required messaging gate.
- `make test-deferred` runs preserved non-MVP suites explicitly.
- `make soak` runs longer stress coverage.
- `make verify` runs the F* verification pass.

## Current Priorities

1. Keep the messaging hardening harness green and expanding.
2. Deepen restart and persistence assertions.
3. Preserve codegen as a first-class source of truth.
4. Keep deferred subsystems compiling and visible without pulling them into the fast MVP gate.

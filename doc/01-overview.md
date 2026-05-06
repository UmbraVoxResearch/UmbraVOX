# UmbraVOX Overview

UmbraVOX is a peer-to-peer encrypted messaging system. The active MVP is the
direct messaging stack: local identity management, peer discovery, Noise_IK
transport over TCP, PQXDH session setup, Signal Double Ratchet messaging, TUI
workflows, and a temporary sqlite3-backed persistence shim.

## Current Scope

- direct peer-to-peer encrypted chat
- local identity persistence across restart
- mDNS and peer exchange discovery
- real TCP transport hardening
- codegen-backed crypto coverage
- F* verification as a required quality gate

## Deferred but Preserved

These areas are intentionally not part of the active MVP gate, but they remain
preserved in code and in the attic archive:

- blockchain transport and transaction packaging
- chain/state/checkpoint storage expansion
- consensus and truncation subsystems
- token economics implementation
- future hardening and research documents

## Verification Model

The active hardening harness is tiered:

- `core` for deterministic crypto, protocol, TUI, and codegen coverage
- `tcp` for real localhost TCP scenarios
- `fault` for adversarial and malformed-input coverage
- `recovery` for persistence and restart behavior
- `deferred` for preserved non-MVP suites
- `soak` for longer-running stress coverage

The required MVP gate is `make test`, which runs `core + tcp + fault + recovery`.

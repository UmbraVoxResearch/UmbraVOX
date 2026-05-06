# UmbraVOX

Post-quantum encrypted peer-to-peer messaging.

[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)
[![Tests](https://img.shields.io/badge/tests-make%20test-brightgreen.svg)](#test-workflow)
[![Haskell](https://img.shields.io/badge/language-Haskell-purple.svg)](UmbraVox.cabal)

UmbraVOX is a decentralized communications research project, protocol
design, writing project, and reference implementation effort focused on
censorship resistance, privacy-preserving communication, and durable freedom
of thought and expression in the digital age.

> Security notice: UmbraVOX is research software. It has not been
> independently audited and should not be trusted for protecting sensitive
> communications until a serious independent review has been completed.

## Quickstart

### Prerequisites

- [Nix](https://nixos.org/) package manager

### Build & Run

```sh
nix-shell              # Enter development environment
cabal build            # Build everything
cabal run umbravox     # Launch the TUI
make test              # Fast messaging-MVP hardening gate
make test-core-network # Deterministic network/discovery suite
make test-tui-sim      # TUI simulation suite
make test-integrity    # Wire/integrity suite
make test-mdns         # Exact mDNS/discovery suite
make test-deferred     # Preserved deferred blockchain/economics suites
make soak              # Longer soak/stress run with artifact report
make                   # Full pipeline: build + test + verify + complexity + lint + license + format-check
make quality           # Same full pipeline as make; lint/format-check are advisory, license is blocking
````

### First Launch

1. UmbraVOX starts with a Welcome dialog. Press `Esc` to dismiss.
2. The node auto-listens on port `7853`, or the next available port, at startup.
3. Press `F2`, the Contacts menu, then select `New` to start a conversation.
4. Choose `Private`, `Single`, or `Group`.
5. To connect to a peer, type the other peer's `host:port` and press `Enter`.
6. For runtime troubleshooting, enable debug logging in `F4 -> Settings -> a`
   or set `UMBRAVOX_DEBUG_LOG=1`. Logs write to the configured path without
   including plaintext payloads, but they still remain sensitive operational
   metadata and are not production-safe telemetry.

See [`doc/QUICKSTART.md`](doc/QUICKSTART.md) for the active command set and
[`doc/README.md`](doc/README.md) for the fresh documentation index.

For the current cryptographic assurance boundary and the planned path toward
a stronger model, see [`doc/assurance-roadmap.md`](doc/assurance-roadmap.md).

## Current MVP Security Model

The active confidentiality and integrity path is the layered Haskell runtime
path:

```text
Plaintext
  |-> Signal Double Ratchet encrypt
  |-> PQ Outer Wrapper encrypt
  |-> Noise_IK transport encryption
  |-> TCP transmission
```

That layered boundary is the intended MVP protection model.

This should be read as a defense-in-depth design, not as a claim that
"triple encryption" automatically makes the system secure. The security
claim depends on correct composition, independent keys, explicit domain
separation, downgrade resistance, replay handling, and correct binding
between the message identity context, the post-quantum wrapping context,
and the transport session context.

The intended role of each layer is:

| Layer                 | Purpose                                                                                   |
| --------------------- | ----------------------------------------------------------------------------------------- |
| Signal Double Ratchet | End-to-end message secrecy, authentication, forward secrecy, and post-compromise recovery |
| PQ Outer Wrapper      | Post-quantum hedge for payload confidentiality, especially harvest-now-decrypt-later risk |
| Noise_IK              | Authenticated transport security for live peer communication                              |
| TCP                   | Network carriage only, not a security boundary                                            |

## Threat Model Summary

UmbraVOX currently treats the Haskell messaging MVP as the active
security-bearing path. Other preserved code, generated artifacts, deferred
blockchain-facing modules, economics stubs, manuscript material, and legacy
design documents are not automatically part of the active confidentiality
and integrity boundary unless explicitly wired into the MVP path and tested.

### In Scope

The MVP protection model is intended to address:

| Threat                           | Intended Protection                                                                       |
| -------------------------------- | ----------------------------------------------------------------------------------------- |
| Passive network observer         | Noise transport encryption plus encrypted message payloads                                |
| Active network man-in-the-middle | Noise authentication plus message-layer identity and session authentication               |
| Malicious or curious relay       | Message-layer end-to-end encryption should protect message contents                       |
| Transport compromise             | Signal and post-quantum payload layers should still protect message contents              |
| Harvest-now-decrypt-later        | Post-quantum outer wrapping is intended to hedge future quantum decryption risk           |
| Message tampering                | AEAD and message-layer authentication should reject modified ciphertexts                  |
| Replay or reordering             | Message and transport layers should reject or safely handle invalid sequence behavior     |
| Later key exposure               | Double Ratchet design limits exposure according to ratchet state and compromise timing    |
| Parser abuse                     | Malformed, truncated, oversized, reordered, or adversarial wire inputs should fail closed |

### Out of Scope

UmbraVOX does not currently claim to solve:

| Threat                                                  | Current Status                                                                     |
| ------------------------------------------------------- | ---------------------------------------------------------------------------------- |
| Endpoint compromise                                     | If the local device is compromised, plaintext and live key material may be exposed |
| Malware, keyloggers, screen capture, or memory scraping | Out of scope for the current MVP                                                   |
| Traffic analysis and metadata correlation               | Not fully addressed by this cryptographic stack                                    |
| Global passive adversary resistance                     | Not claimed                                                                        |
| Anonymous routing                                       | Not yet part of the active MVP security boundary                                   |
| Secure deletion from managed Haskell memory             | Not currently guaranteed                                                           |
| Production-grade side-channel resistance                | Not currently guaranteed                                                           |
| Independent third-party assurance                       | No independent audit has been completed                                            |

## Connection Trust Modes

| Mode        | Accept       | mDNS |    PEX |  DB | Behavior                                         |
| ----------- | ------------ | ---: | -----: | --: | ------------------------------------------------ |
| Swing       | All          |   On |   Auto |  On | Most open. Shares peer lists automatically       |
| Promiscuous | All          |   On | Manual |  On | Accept anyone silently                           |
| Selective   | Confirm      |   On |    Off |  On | Shows fingerprint, user decides. Default mode    |
| Chaste      | Trusted only |  Off |    Off |  On | Silent reject indistinguishable from MAC failure |
| Chastity    | Trusted only |  Off |    Off | Off | Chaste plus no persistence. Fully ephemeral      |

Change mode in Preferences:

```text
F4 -> Settings
```

## Features

* PQXDH plus ML-KEM-768 post-quantum key exchange
* Signal Double Ratchet with AES-256-GCM
* Noise_IK transport encryption with key separation
* Post-quantum outer wrapping for message payloads
* Five connection trust modes, from Swing to Chastity
* TUI with F1-F4 dropdown menus plus a top-level Quit menu
* mDNS LAN peer discovery
* Default port sequence beginning at 7853
* Identity persistence across restart for the local node
* Tiered hardening harness: core, TCP, fault, recovery, deferred, soak
* 17 F* formal verification specs
* Explicit distinction between active MVP code and deferred research code

## Keyboard

| Key     | Action                                  |
| ------- | --------------------------------------- |
| F1-F4   | Open menus: Help, Contacts, Chat, Prefs |
| Tab     | Switch pane: Contacts or Chat           |
| Ctrl+N  | Quick new connection                    |
| Ctrl+Q  | Quit                                    |
| Up/Down | Navigate or scroll                      |
| Enter   | Send message or select                  |
| Esc     | Close dialog or menu                    |

## Architecture

```text
src/UmbraVox/
  Crypto/          Primitives:
                   SHA-256/512, AES-GCM, X25519, Ed25519,
                   ML-KEM-768, Keccak/SHA-3, Poly1305, HKDF,
                   VRF, ConstantTime, BIP39, Export,
                   StealthAddress, KeyStore

  Crypto/Signal/   Signal protocol:
                   X3DH, PQXDH, Double Ratchet, SenderKeys

  Network/         Transport:
                   TCP, Loopback, Noise_IK, mDNS, PeerExchange

  Chat/            Session management:
                   Wire format, Contacts, API

  Storage/         Temporary sqlite3-backed persistence shim:
                   Schema

  Protocol/        Encoding:
                   CBOR framing, QRCode, MessageFormat, WireFormat

  TUI/             Terminal UI:
                   Types, Render, Dialog, Menu, Input, Actions

  Economics/       Token model stubs

  Consensus/       Ouroboros Praos adaptation stubs

  Tools/           Complexity checker, F* verifier

  Version.hs       Build version information
```

## Encryption Pipeline

```text
Plaintext
  |-> Signal Double Ratchet encrypt
      AES-256-GCM plus HMAC-SHA256

  |-> PQ Outer Wrapper encrypt
      ML-KEM-768-derived key material into AES-256-GCM

  |-> Noise_IK transport encryption
      X25519 plus ChaChaPoly1305

  |-> TCP transmission
```

## Cryptographic Composition Principles

The layered encryption model depends on correct composition. Each layer must
have a clear purpose and must not accidentally weaken another layer.

The project aims to maintain the following rules:

1. **No cross-layer key reuse**

   Signal message keys, post-quantum wrapper keys, and Noise transport keys
   must remain separate.

2. **Explicit domain separation**

   Key derivation, associated data, transcript material, and protocol labels
   should identify which layer and purpose they belong to.

3. **Transcript and identity binding**

   The post-quantum wrapper and Noise transport session should be bound to
   the same authenticated peer and session context as the message layer.

4. **Downgrade resistance**

   Peers must not silently fall back from the intended MVP protection model
   to a weaker mode without explicit policy and visible user intent.

5. **Replay resistance**

   Message numbers, ratchet state, transport nonces, and protocol framing
   must reject or safely handle duplicate, stale, or reordered ciphertexts.

6. **Parser hardening**

   Malformed, truncated, oversized, reordered, or adversarial wire inputs
   must fail closed.

7. **Clear failure behavior**

   Authentication failure, decode failure, trust-policy rejection, and
   transport corruption must not leak sensitive state.

## Assurance Model

This section is the fast way to understand what is security-critical today,
what is formally backed today, and what remains planned work rather than an
active assurance claim.

* The active confidentiality and integrity path is the layered Haskell
  runtime path shown above: Signal Double Ratchet, PQ outer wrapping, and
  Noise transport. That layered encryption boundary remains the intended MVP
  protection model.
* The generated Haskell namespace is active, but it is currently wrapper-style
  code that delegates to the audited handwritten/reference Haskell
  implementations.
* The generated C artifacts are currently build/link participants only. They
  are `*_link_probe` stubs, not active cryptographic implementations.
* The generated FFI modules are currently bridge wrappers that call the C link
  probe and then delegate back to the handwritten/reference Haskell crypto.
  They do not currently execute cryptographic semantics in C.
* The F* layer is handwritten, not generated from NIST or RFC text. Its role
  is to formalize and verify algorithm properties against the intended
  standards.
* 17 F* specifications are present, and the current full `make verify` run is
  green under the active toolchain. That means the handwritten formal model
  suite is internally consistent again. It does not by itself prove that the
  active Haskell implementation refines those models.

What this means in practice:

* The current security story relies on the active Haskell cryptographic path,
  the runtime tests, and the handwritten F* suite as a green formal model
  layer.
* The current generated C/FFI surface provides build-graph, linkage, and
  namespace coverage, not an independently assured or constant-time production
  crypto path.
* Any stronger claim about generated C, generated FFI, independent
  cross-language equivalence, or life-critical publication assurance requires
  additional evidence and implementation work that is still tracked in
  [`TODO.txt`](TODO.txt).

## Formal Verification and Proof Direction

UmbraVOX includes formal verification work and generated-source research.
For the hash core work, the current proof direction is to replace recursive
assume-based builders with explicit invariants over the fixed SHA-256 and
SHA-512 recurrences.

This matters for assurance, not because it changes SHA-256 or SHA-512
security, but because it strengthens the argument that the implementation
actually computes the intended compression functions.

The desired proof posture is:

```text
1. Bounds and memory safety
2. No verifier-only assumptions in security-critical paths
3. Correct word-level operations
4. Correct rotations, shifts, and modular additions
5. SHA-256 and SHA-512 schedule equivalence
6. SHA-256 and SHA-512 round-state equivalence
7. Compression output equivalence
8. Whole-hash equivalence including padding and length encoding
9. Known-answer agreement with standard test vectors
```

For fixed-size SHA cores, the recurrence lengths are constant:

```text
SHA-256 schedule and rounds: 64
SHA-512 schedule and rounds: 80
```

Encoding those shapes with explicit invariants rather than recursive
assumptions improves auditability and makes the proof obligations easier to
inspect.

## Documentation

| Document                                               | Description                                                                      |
| ------------------------------------------------------ | -------------------------------------------------------------------------------- |
| [`doc/README.md`](doc/README.md)                       | Fresh active documentation set                                                   |
| [`doc/QUICKSTART.md`](doc/QUICKSTART.md)               | Build, run, and test workflow                                                    |
| [`doc/01-overview.md`](doc/01-overview.md)             | Current MVP scope and verification model                                         |
| [`doc/ARCHITECTURE.md`](doc/ARCHITECTURE.md)           | Active runtime and test architecture                                             |
| [`doc/mvp-plan.md`](doc/mvp-plan.md)                   | Current hardening and completion priorities                                      |
| [`doc/assurance-roadmap.md`](doc/assurance-roadmap.md) | Current assurance boundary and stronger target model                             |
| [`doc/assurance-matrix.md`](doc/assurance-matrix.md)   | Evidence ledger for standards targets, tests, generated surfaces, and trust gaps |
| [`LEGAL-NOTICE.md`](LEGAL-NOTICE.md)                   | Legal and compliance notice                                                      |
| [`PUBLISHING-NOTE.md`](PUBLISHING-NOTE.md)             | Expressive, research, and publication framing                                    |
| [`TODO.txt`](TODO.txt)                                 | Current development notes                                                        |

Legacy long-form design, hardening, and future-planning documents are
preserved under:

```text
attic/doc-legacy-2026-04-28/
```

## Known Limitations

UmbraVOX is not production security software.

Current known limitations include:

* Pure Haskell cryptographic implementations are not constant-time because of
  GHC lazy evaluation, allocation, garbage collection, and runtime effects.
  C FFI backends are planned for security-critical primitive implementations.
* Key material zeroing is not currently guaranteed in Haskell-managed memory.
  `SecureByteString` with FFI-backed `explicit_bzero` or equivalent memory
  handling is planned.
* Consensus, blockchain transport, and token economics modules are stubs or
  design-preservation surfaces, not active MVP security boundaries.
* The CSPRNG, `globalCSPRNG`, is process-global and synchronized with `MVar`,
  but it has not been independently audited.
* No independent security audit has been performed.
* Traffic analysis and metadata protection are not solved by the current MVP.
* Endpoint compromise remains out of scope.
* Formal verification coverage is partial and should not be read as a full
  proof of system security.
* Some generated-source surfaces are preserved for codegen research rather
  than treated as active runtime coverage.
* The generated C artifacts currently prove build and linkage participation,
  not independent generated C cryptographic semantics.

## Test Workflow

The active messaging-MVP gate is split by intent rather than kept as one
large flat suite.

* `make test` runs the required messaging gate: deterministic core, real TCP,
  fault injection, and recovery. It streams the full live output and writes a
  per-run log under `build/test-artifacts/`.
* `make test-core` runs the full deterministic core suite.
* `make test-core-crypto` runs only deterministic crypto/unit coverage.
* `make test-core-network` runs only deterministic network/discovery coverage.
* `make test-core-chat` runs only deterministic chat/protocol coverage.
* `make test-core-tui` runs only deterministic non-simulated TUI coverage.
* `make test-core-tools` runs only deterministic codegen/tools/fuzz coverage.
* `make test-tcp` runs real localhost TCP end-to-end scenarios.
* `make test-fault` runs adversarial transport and parser hardening scenarios.
* `make test-recovery` runs persistence and restart-oriented scenarios.
* `make test-tui-sim` runs the TUI simulation coverage that is kept out of the
  fast gate.
* `make test-integrity` runs the explicit wire-format and cryptographic
  integrity coverage.
* `make test-mdns` runs only the exact mDNS/discovery suite.
* `make test-deferred` runs the preserved blockchain, economics, and storage
  stub suites outside the MVP fast gate.
* Exact suite names such as `mdns`, `sha256`, or `tui-sim-dialogs` are also
  accepted directly through `cabal test umbravox-test --test-options='<suite>'`.
* `make soak` runs the longer stress suite and writes:

```text
build/test-artifacts/soak-report.txt
```

Deferred blockchain-facing modules remain in the codebase and documentation,
but they are not part of the messaging-MVP hardening gate. Use
`make test-deferred` when you want explicit coverage of that preserved
surface.

The broader preserved generated-source tree also remains in the repository,
but only the explicitly wired generated surfaces and tests are treated as
active build coverage today. In practice, the active harness now exercises
the generated Haskell wrapper namespace and the linked C/FFI bridge namespace
through parity and integration checks.

Those bridge checks prove build/link participation and wrapper parity against
the active Haskell implementations. They do not yet prove independent
generated C crypto semantics.

## Security Review Checklist

Before making stronger security claims, the following should be completed:

* Independent cryptographic design review
* Independent implementation audit
* Review of Signal/PQ/Noise composition
* Review of key derivation and domain separation
* Review of downgrade resistance
* Review of replay handling
* Review of trust-mode transitions
* Review of malformed-input behavior
* Review of memory handling and key lifetime
* Review of random number generation
* Review of generated C and FFI boundaries
* Reproducible test-vector coverage
* Cross-implementation compatibility tests
* Side-channel assessment for security-critical primitive paths
* Documentation of all security assumptions and non-goals

## Security Notice

THIS SOFTWARE HAS NOT BEEN INDEPENDENTLY PEER-REVIEWED OR AUDITED.

The cryptographic implementations, while tested against NIST/RFC vectors and
formally specified in F*, have not been reviewed by an independent third
party. This software SHOULD NOT BE TRUSTED for protecting sensitive
communications until an independent security audit has been completed.

Use at your own risk. See [`LEGAL-NOTICE.md`](LEGAL-NOTICE.md) for full
terms.

## Expression and Legal Notice

This repository, and any related UmbraVox manuscripts, drafts,
specifications, commentary, examples, proofs, or source text, are intended
as expressive works. They may include political, philosophical, scientific,
mathematical, literary, and technical speech.

Source code, algorithms, formal descriptions, pseudocode, protocol
specifications, and implementation notes may also constitute protected
expression in some jurisdictions. However, laws regulating cryptographic
software, privacy technologies, export controls, publication, compilation,
possession, distribution, importation, and operational deployment differ
across countries and legal regimes.

Accordingly:

* this repository is provided for informational, research, educational, and
  expressive purposes
* nothing in this repository constitutes legal advice
* any person who compiles, ports, modifies, distributes, exports, deploys, or
  operates software derived from this work is solely responsible for
  compliance with applicable law
* the authors, contributors, and publishers make no representation that any
  implementation or use of this work is lawful in every jurisdiction

If you reduce any portion of this work to practice in executable form,
distribute binaries, provide hosted services, or operate derived systems in
the real world, you are responsible for determining and satisfying all legal,
regulatory, licensing, and compliance obligations that apply where you live
or operate.

## Compliance Advisory

This project implements strong cryptography and decentralized communications
technology. Operators deploying this software must independently assess
compliance with applicable laws in their jurisdiction, including but not
limited to cryptography export controls, telecommunications regulations,
lawful intercept requirements, and data retention obligations.

See [`LEGAL-NOTICE.md`](LEGAL-NOTICE.md) for detailed legal information
including export controls, telecommunications, data retention, and token
economics notices. See [`PUBLISHING-NOTE.md`](PUBLISHING-NOTE.md) for the
expressive and research framing of this work.

## Project Status

UmbraVOX is an active research and reference implementation project.

The immediate focus is:

```text
1. hardening the active Haskell messaging MVP
2. preserving but separating deferred blockchain and economics surfaces
3. improving formal proof coverage for fixed cryptographic cores
4. replacing assume-heavy proof shapes with explicit invariants
5. clarifying the exact active security boundary
6. preparing the codebase for future independent review
```

## License

See [`LICENSE`](LICENSE).

```
::contentReference[oaicite:1]{index=1}
```

[1]: https://github.com/UmbraVoxResearch/UmbraVOX "GitHub - UmbraVoxResearch/UmbraVOX: A decentralized communications research project and technical manuscript.  UmbraVox is a protocol design, writing project, and reference implementation effort focused on censorship resistance, privacy-preserving communication, and durable freedom of thought and expression in the digital age. · GitHub"

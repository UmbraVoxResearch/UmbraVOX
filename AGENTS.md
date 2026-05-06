# Agents

This file is a reference index only. All authoritative project information lives in the documents listed below.

## Project Status

MVP is functional. The fast messaging gate is green. 17 F* formal
specifications are present, and the current full `make verify` run is green.
10 `.spec` files generate 30 Haskell + C + FFI outputs.

Current assurance model:

- The active cryptographic protection path is the layered runtime path in
  `README.md`: Signal Double Ratchet, PQ outer wrapping, and Noise transport.
- Generated Haskell modules are active as wrappers over the handwritten Haskell
  crypto implementations.
- Generated C artifacts are currently link-probe stubs, not active crypto
  implementations.
- Generated FFI modules currently call the C link probes and then delegate back
  to handwritten Haskell crypto; they are not yet an independently assured C
  execution path.
- The F* specifications are handwritten formal models, not generated from NIST
  or RFC text. They remain assurance-critical and the current full verifier run
  is green, but that does not constitute a machine-checked refinement proof
  from the handwritten models to the active Haskell runtime.

## Source of Truth

### Active Documentation

- [README.md](README.md) — project overview and active workflow
- [TODO.txt](TODO.txt) — implementation tracking and current priorities
- [doc/README.md](doc/README.md) — active documentation index
- [doc/QUICKSTART.md](doc/QUICKSTART.md) — active build, run, and test commands
- [doc/01-overview.md](doc/01-overview.md) — current MVP scope and verification model
- [doc/ARCHITECTURE.md](doc/ARCHITECTURE.md) — active runtime and test architecture
- [doc/mvp-plan.md](doc/mvp-plan.md) — current messaging MVP hardening plan
- [doc/assurance-roadmap.md](doc/assurance-roadmap.md) — current assurance boundary and stronger target model
- [doc/assurance-matrix.md](doc/assurance-matrix.md) — current evidence ledger for standards, implementations, tests, and trust gaps

### Legacy Documentation Archive

- `attic/doc-legacy-2026-04-28/` — preserved prior design, hardening, future-planning, and reference material

### Legal

- [LEGAL-NOTICE.md](LEGAL-NOTICE.md) — Export controls, telecommunications, data retention, token economics
- [PUBLISHING-NOTE.md](PUBLISHING-NOTE.md) — Expressive and research framing
- [LICENSE](LICENSE) — Project license

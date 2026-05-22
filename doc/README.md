# UmbraVOX Active Documentation

This `doc/` tree is the fresh active documentation set for the current
messaging MVP.

## Active Docs

- [QUICKSTART.md](QUICKSTART.md) — build, run, and test commands
- [ARCHITECTURE.md](ARCHITECTURE.md) — active runtime, module layout, and test architecture
- [PROJECT-CHARTER.md](PROJECT-CHARTER.md) — mission, scope, assurance layers, non-claims
- [CRYPTO-AUDIT-v0.3.0.md](CRYPTO-AUDIT-v0.3.0.md) — cryptographic audit report (v0.3.0)
- [PROTOCOL-AUDIT-v0.3.0.md](PROTOCOL-AUDIT-v0.3.0.md) — protocol audit report (v0.3.0)
- [CRYPTO-SAFETY.md](CRYPTO-SAFETY.md) — nonce constructions and HKDF domain-separation strings
- [DHT-NETWORK-PLAN.md](DHT-NETWORK-PLAN.md) — DHT network design and implementation plan
- [ENCRYPTED-ENVELOPE-DESIGN.md](ENCRYPTED-ENVELOPE-DESIGN.md) — encrypted envelope and stealth addressing design
- [VM-DEVELOPMENT.md](VM-DEVELOPMENT.md) — VM-first development guide and troubleshooting
- [VM-ISOLATION.md](VM-ISOLATION.md) — VM isolation audit
- [RELEASES.md](RELEASES.md) — release artifact types, portability constraints, and smoke-check status
- [RELEASE-LANES.md](RELEASE-LANES.md) — host, QEMU/KVM, Firecracker, and microVM smoke lane model
- [transport-providers.md](transport-providers.md) — pluggable transport provider architecture
- [platform-compatibility.md](platform-compatibility.md) — platform compatibility audit (illumos, BSDs, macOS, Windows)
- [runner-classes.md](runner-classes.md) — runner classes and isolation model
- [hacl-evaluation.md](hacl-evaluation.md) — HACL* adoption evaluation
- [musl-evaluation.md](musl-evaluation.md) — static musl feasibility evaluation

## Archived Docs

Stale documents moved during doc audits:

- `doc/attic/` — DO-178C coverage, F* axiom registry, MC/DC tables, WASM target

The previous documentation tree was preserved without deletion at:

- `attic/doc-legacy-2026-04-28/`

That archive contains the older long-form design, future-planning, hardening,
and reference material that is not part of the fresh active doc set.

# UmbraVOX Active Documentation

This `doc/` tree is the fresh active documentation set for the current
messaging MVP.

## Active Docs

- [QUICKSTART.md](QUICKSTART.md) — build, run, and test commands
- [ARCHITECTURE.md](ARCHITECTURE.md) — active runtime, module layout, and test architecture
- [PROJECT-CHARTER.md](PROJECT-CHARTER.md) — mission, scope, assurance layers, non-claims
- [CRYPTO-AUDIT-v0.3.0.md](CRYPTO-AUDIT-v0.3.0.md) — cryptographic audit report (v0.3.0)
- [PROTOCOL-AUDIT-v0.3.0.md](PROTOCOL-AUDIT-v0.3.0.md) — protocol audit report (v0.3.0)
- [CRYPTO-SAFETY.md](CRYPTO-SAFETY.md) — nonce constructions, HKDF domain-separation strings, CT gap, and differential testing limitations
- [VERIFIED-C.md](VERIFIED-C.md) — verified C architecture: interim vs target production, oracle coverage, linkage gap, assurance grades
- [DHT-NETWORK-PLAN.md](DHT-NETWORK-PLAN.md) — DHT network design and implementation plan
- [IMPLEMENTATION-PLAN.md](IMPLEMENTATION-PLAN.md) — current execution plan for the next implementation slice
- [ENCRYPTED-ENVELOPE-DESIGN.md](ENCRYPTED-ENVELOPE-DESIGN.md) — encrypted envelope and stealth addressing design
- [VM-DEVELOPMENT.md](VM-DEVELOPMENT.md) — VM-first development guide and troubleshooting
- [VM-ISOLATION.md](VM-ISOLATION.md) — VM isolation audit
- [NIX-CACHE-POLICY.md](NIX-CACHE-POLICY.md) — soft-cap policy for host and builder Nix stores
- [RELEASES.md](RELEASES.md) — release artifact types, portability constraints, and smoke-check status
- [RELEASE-LANES.md](RELEASE-LANES.md) — host, QEMU/KVM, Firecracker, and microVM smoke lane model
- [transport-providers.md](transport-providers.md) — pluggable transport provider architecture
- [platform-compatibility.md](platform-compatibility.md) — platform compatibility audit (illumos, BSDs, macOS, Windows)
- [runner-classes.md](runner-classes.md) — runner classes and isolation model
- [hacl-evaluation.md](hacl-evaluation.md) — HACL* adoption evaluation
- [musl-evaluation.md](musl-evaluation.md) — static musl feasibility evaluation
- [MILESTONE-DOCS.md](MILESTONE-DOCS.md) — per-milestone documentation index (M20-M28)

## contrib/

The `contrib/` tree contains third-party source trees used exclusively to build
hermetic NixOS oracle VMs for differential testing.  These sources are **not**
part of UmbraVOX and carry their own licenses — see
[contrib/oracles/THIRD_PARTY_LICENSES.md](../contrib/oracles/THIRD_PARTY_LICENSES.md).
The main project is Apache-2.0.

## Archived Docs

Stale documents moved during doc audits:

- `doc/attic/` — DO-178C coverage, F* axiom registry, MC/DC tables, WASM target

The previous documentation tree was preserved without deletion at:

- `attic/doc-legacy-2026-04-28/`

That archive contains the older long-form design, future-planning, hardening,
and reference material that is not part of the fresh active doc set.

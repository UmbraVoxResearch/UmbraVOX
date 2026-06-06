# CI Testing Strategy

3-tier CI pipeline. All tiers run in VMs on self-hosted runners with KVM.

## Tier 1: PR/Push (~10 min)

Runs on every push and pull request. Fast feedback gate.

| Command | Level | What it catches |
|---------|-------|-----------------|
| `./uv check` | L7 | Lint, format, license, complexity, headers, assurance |
| `./uv test` | L0 | Core logic, crypto, protocol, chat (required gate) |
| `./uv test integrity` | L15 | SHA-256 test vector validation |
| `./uv test differential` | L2 | Generated C vs libsodium/HACL* oracle |
| `./uv check pre-release` | L13 | F* admit count, assume inventory, assurance matrix |

## Tier 2: Nightly (~4 hr)

Runs daily via cron. Full regression net.

| Command | Level | What it catches |
|---------|-------|-----------------|
| All PR/Push tests | — | Baseline |
| `./uv test all` | L1 | Every named suite (core-crypto, core-network, core-chat, core-tui, core-tools, tui-sim, mdns, deferred, etc.) |
| `./uv test e2e` | L10 | Full pipeline from clean state |
| `./uv test tcp` | L1 | TCP hardening |
| `./uv test fault` | L1 | Fault injection |
| `./uv test recovery` | L1 | State recovery |
| `./uv test soak` | L4 | 2hr stress test |
| `./uv fuzz differential` | L5 | Fuzz testing |
| `./uv verify` | L6 | F* formal verification (32 specs) |
| `./uv coverage --check` | L8 | HPC coverage targets |
| `./uv vm smoke release` | L9 | Release bundle smoke |
| `./uv vm smoke freebsd` | L9 | FreeBSD platform |
| `./uv vm smoke openbsd` | L9 | OpenBSD platform |
| `./uv vm integration` | L12 | Multi-VM integration (3 agents) |
| `./uv vm signal test` | L14 | Signal Server wire-compat |

## Tier 3: Pre-release (manual, ~8 hr)

Triggered manually before tagging a release. Exhaustive validation.

| Command | Level | What it catches |
|---------|-------|-----------------|
| `./uv test e2e --bootstrap` | L10 | Cold-start bootstrap chain (clean → nix-shell → build image → full pipeline) |
| All Nightly tests | — | Baseline |
| `./uv coverage --mcdc` | L8 | MC/DC coverage |
| `./uv test ephemeral` | L11 | Fresh image build + test |
| `./uv vm smoke netbsd` | L9 | NetBSD platform |
| `./uv vm smoke illumos` | L9 | illumos platform |
| `./uv vm smoke dragonfly` | L9 | DragonFlyBSD platform |
| `./uv vm smoke arm64` | L9 | ARM64 platform |
| `./uv test --direct` | — | Host build path regression |
| `./uv release all` | — | Build all 12 platform artifacts |

## Test Levels

See [TEST-LEVELS.md](TEST-LEVELS.md) for the complete test level reference
(L0-L15), named suite descriptions, and command details.

## Coverage Matrix

Every test path is covered by at least one tier:

| Test | PR/Push | Nightly | Pre-release |
|------|---------|---------|-------------|
| `./uv check` | ✓ | ✓ | ✓ |
| `./uv test` | ✓ | ✓ | ✓ |
| `./uv test integrity` | ✓ | ✓ | ✓ |
| `./uv test differential` | ✓ | ✓ | ✓ |
| `./uv check pre-release` | ✓ | ✓ | ✓ |
| `./uv test all` | — | ✓ | ✓ |
| `./uv test e2e` | — | ✓ | ✓ |
| `./uv test e2e --bootstrap` | — | — | ✓ |
| `./uv test tcp` | — | ✓ | ✓ |
| `./uv test fault` | — | ✓ | ✓ |
| `./uv test recovery` | — | ✓ | ✓ |
| `./uv test soak` | — | ✓ | ✓ |
| `./uv fuzz differential` | — | ✓ | ✓ |
| `./uv verify` | — | ✓ | ✓ |
| `./uv coverage --check` | — | ✓ | ✓ |
| `./uv vm smoke release` | — | ✓ | ✓ |
| `./uv vm smoke freebsd` | — | ✓ | ✓ |
| `./uv vm smoke openbsd` | — | ✓ | ✓ |
| `./uv vm integration` | — | ✓ | ✓ |
| `./uv vm signal test` | — | ✓ | ✓ |
| `./uv coverage --mcdc` | — | — | ✓ |
| `./uv test ephemeral` | — | — | ✓ |
| `./uv vm smoke netbsd` | — | — | ✓ |
| `./uv vm smoke illumos` | — | — | ✓ |
| `./uv vm smoke dragonfly` | — | — | ✓ |
| `./uv vm smoke arm64` | — | — | ✓ |
| `./uv test --direct` | — | — | ✓ |
| `./uv release all` | — | — | ✓ |

## Requirements

All tiers require a self-hosted runner with:
- Linux x86_64 with KVM (`/dev/kvm`)
- Nix package manager
- QEMU
- 16GB+ RAM, 100GB+ disk
- Network access (for nix cache downloads)

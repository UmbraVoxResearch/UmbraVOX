# UmbraVOX Test Levels

16 test levels covering unit tests through exhaustive pre-release validation.
All tests run inside VMs unless `--direct` is explicitly used.

## Test Level Table

| Level | Name | Command | Description | Time | VM? |
|-------|------|---------|-------------|------|-----|
| L0 | Unit (fast gate) | `./uv test` | Required gate: core crypto, protocol, chat, storage. Runs on every build. | ~5 min | Yes |
| L1 | Named suites | `./uv test <suite>` | Individual module group (15 suites: core-crypto, core-network, core-chat, core-tui, core-tools, tcp, fault, recovery, tui-sim, integrity, mdns, deferred, differential, soak). | 2-10 min | Yes |
| L1 | All suites | `./uv test all` | Every named suite sequentially in a single VM session. Complete unit coverage. | ~30 min | Yes |
| L2 | Differential | `./uv test differential` | Generated constant-time C vs libsodium/HACL* reference oracle. Catches codegen regressions. | ~5 min | Yes |
| L4 | Soak | `./uv test soak` | Long-running stress test. Message throughput, memory stability, key rotation under load. | ~2 hr | Yes |
| L5 | Fuzz | `./uv fuzz [differential\|afl]` | Fuzzing harnesses. Differential fuzzing against oracle. AFL++ coverage-guided fuzzing. | Unbounded | Yes |
| L6 | Formal verification | `./uv verify` | F* verification of 24 spec modules. Checks type safety, protocol correctness, crypto properties. | ~1 hr | Yes |
| L7 | Quality gates | `./uv check` | Static checks: lint, format, license, cyclomatic complexity, generated headers, assurance matrix freshness. | ~30 sec | Host |
| L8 | Coverage | `./uv coverage --check` | HPC expression coverage against per-module targets (50-100% by module tier). | ~15 min | Yes |
| L8 | MC/DC coverage | `./uv coverage --mcdc` | Modified Condition/Decision Coverage for safety-critical paths. | ~15 min | Yes |
| L9 | Platform smoke | `./uv vm smoke <target>` | Boot a platform VM, verify binary executes. Targets: freebsd, openbsd, netbsd, illumos, dragonfly, arm64, release. | ~5 min | Yes |
| L10 | End-to-end | `./uv test e2e` | Full pipeline from clean state: build image → compile → test → check → runtime images. | ~45 min | Yes |
| L10 | End-to-end (bootstrap) | `./uv test e2e --bootstrap` | Cold-start path: clean → nix-shell → build image → full pipeline. Tests the bootstrap chain from scratch. | ~60 min | Yes |
| L11 | Ephemeral | `./uv test ephemeral` | Build a fresh VM image, run tests inside it, discard. Catches image staleness. | ~30 min | Yes |
| L12 | Integration | `./uv vm integration [--dual-lan]` | Multi-VM test with 3-6 agents communicating across virtual networks. | ~15 min | Yes |
| L13 | Pre-release | `./uv check pre-release` | 10 assurance gates: F* admit count, assume val inventory, ledger consistency, proof hygiene, Coq build, infrastructure tests, differential oracle, assurance matrix, GPG signing, reproducibility. | ~2 min | Host |
| L14 | Signal compat | `./uv vm signal test` | Signal Server wire-compatibility testing. Builds JAR, boots backing services, runs protocol tests. | ~10 min | Yes |
| L15 | Vector integrity | `./uv test integrity` | SHA-256 validation of test vector files. Catches vector corruption or tampering. | ~1 min | Yes |

## Named Test Suites (L1)

| Suite | Focus | Modules |
|-------|-------|---------|
| `core` | Core library | Basic types, utilities, version |
| `core-crypto` | Cryptographic primitives | SHA, AES, ChaCha20, Ed25519, X25519, Poly1305, HKDF, HMAC, Keccak, ML-KEM |
| `core-network` | Network layer | Noise handshake, relay, discovery, DHT, rate limiting |
| `core-chat` | Chat protocol | Session, message, contacts, outbound queue, wire format |
| `core-tui` | Terminal UI | Render, input, layout, dialogs, actions |
| `core-tools` | Build tools | Complexity checker, F* verifier, codegen |
| `tcp` | TCP hardening | Connection handling, timeout, backpressure |
| `fault` | Fault injection | Network partition, message loss, reordering |
| `recovery` | State recovery | Ratchet recovery, session resumption, key exhaustion |
| `tui-sim` | TUI simulation | Full TUI interaction simulation (contacts, chat, settings, menus) |
| `integrity` | Test vectors | SHA-256 integrity validation of all test vector files |
| `mdns` | mDNS discovery | Peer discovery, announcement, conflict resolution |
| `deferred` | Deferred features | Consensus stubs, blockchain stubs (behind `-fstubs` flag) |
| `differential` | Differential oracle | Generated C output vs libsodium/HACL* reference |
| `soak` | Stress/endurance | Long-running message exchange, key rotation, memory stability |

## CI Tier Mapping

See [CI-TESTING.md](CI-TESTING.md) for which levels run in each CI tier:
- **PR/Push**: L0, L2, L7, L13, L15
- **Nightly**: All above + L1, L4, L5, L6, L8, L9, L10, L12, L14
- **Pre-release**: All above + L8 (MC/DC), L11, remaining L9 platforms, `--direct`

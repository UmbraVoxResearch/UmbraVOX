# Runner Classes and Isolation Model

## Runner Classes

UmbraVOX defines three classes of build/test runners, ordered by
isolation strength:

### Class 1: Host Runner (Local Override)

- **Environment**: Developer machine in VM-first shell (`nix-shell shell-minimal.nix`)
- **Isolation**: Nix provides hermetic inputs; execution trusts the host
- **Speed**: Fastest (no VM boot overhead)
- **Use case**: Quick local iteration when VM overhead is undesirable
- **Command**: `./uv build && ./uv test`
- **Trust level**: Non-authoritative — results depend on host state
- **Note**: No longer the default.  Standard `./uv` commands now route
  through the VM (Class 2).  The host runner requires the full `nix-shell`
  toolchain.

### Class 2: VM Runner (Current Default)

- **Environment**: NixOS QEMU guest with KVM acceleration
- **Isolation**: Full kernel-level isolation; no shared host state
- **Speed**: ~2-5 min overhead for VM boot + build
- **Use case**: Default for all development (build, test, verify) and release qualification
- **Command**: `./uv build`, `./uv test`, `./uv verify`, `./uv vm smoke`
- **Trust level**: Authoritative — clean environment, reproducible
- **Note**: All standard `./uv` targets now route through the VM by default.
  `./uv vm smoke` runs the full release pipeline.  `./uv dev` provides
  an interactive shell inside the VM.

### Class 3: Container Runner (Artifact Smoke Only)

- **Environment**: Minimal container (podman/docker) with release bundle
- **Isolation**: Filesystem-level; shares host kernel
- **Speed**: Fast (no boot, just exec)
- **Use case**: Quick artifact sanity check
- **Command**: `./uv release smoke-linux`
- **Trust level**: Partial — validates artifact structure, not build

## Isolation Model

```
┌─────────────────────────────────────────────────────────┐
│  Host (nix-shell)                                      │
│  │                                                     │
│  ├── ./uv build/test/verify (default)      Class 2     │
│  │   └── QEMU/KVM Guest (NixOS)                       │
│  │       ├── Full toolchain pre-installed              │
│  │       ├── Source mounted as ext2 disk               │
│  │       ├── Build + test + verify                     │
│  │       └── Exit code propagated to host              │
│  │                                                     │
│  ├── ./uv vm smoke                         Class 2     │
│  │   └── Same VM, full release pipeline                │
│  │                                                     │
│  ├── ./uv build                            Class 1     │
│  │   └── Local execution (requires full toolchain)     │
│  │                                                     │
│  └── ./uv release smoke-linux              Class 3     │
│      └── Container (Ubuntu 24.04)                      │
│          └── Release bundle mounted read-only          │
│              └── Binary execution check only           │
└─────────────────────────────────────────────────────────┘
```

### VM Functional Test Runners

In addition to release smoke testing, the VM infrastructure supports
specialized functional tests:

| Runner | Image | Purpose |
|--------|-------|---------|
| `vm-socks5-test` | vm-test-image | SOCKS5 proxy transport via microsocks |
| `vm-screenshot` | vm-image | TUI frame capture via tmux capture-pane |
| `vm-record` | vm-image | TUI session recording via asciinema |
| `vm-visual-regression` | vm-image | Diff frames against reference captures |

These runners use the same QEMU + virtio-disk + serial-console
architecture as the smoke runner but execute specialized test scripts
instead of the full build pipeline.

### TAP+Bridge Dual-LAN Setup (M5.3.3)

For full L2 isolation testing with pcap capture, the dual-LAN mode
uses Linux bridges with TAP devices:

```
Host
├── br-umbravox-a (10.0.42.0/24)
│   ├── tap-a0 → Agent 0
│   ├── tap-a1 → Agent 1
│   └── tap-a2 → Agent 2
│
├── br-umbravox-b (10.0.43.0/24)
│   ├── tap-b0 → Agent 3
│   ├── tap-b1 → Agent 4
│   └── tap-b2 → Agent 5
│
└── IP forwarding between bridges
```

Setup requires `CAP_NET_ADMIN` (sudo):
```bash
sudo ./uv vm netsetup setup 6
./uv vm integration --dual-lan
sudo ./uv vm netsetup teardown
```

Traffic capture:
```bash
tcpdump -i br-umbravox-a -w build/evidence/lan-a.pcap &
tcpdump -i br-umbravox-b -w build/evidence/lan-b.pcap &
```

## Integration Test Scenarios (M5.4)

### M5.4.4: mDNS Peer Discovery

Tests that UmbraVOX agents discover each other via mDNS multicast
announcements on a virtual LAN.

**Prerequisites**: Agents on the same dgram multicast group or TAP bridge.
mDNS uses UDP 224.0.0.251:5353 which works on L2 broadcast domains.

**Flow**:
1. Agent 0 starts, binds port 7853, announces via mDNS
2. Agent 1 starts, binds port 7853, announces via mDNS
3. Agent 2 starts, binds port 7853, announces via mDNS
4. After 10-15s, each agent should have discovered the other 2
5. Verify: mDNS peer list contains correct IPs and ports

**Verification**:
- Agent logs contain `mdns.discovered` events
- Peer count matches (N-1 peers per agent)
- No self-discovery (filtered by pubkey)

**Current status**: mDNS code exists in `src/UmbraVox/Network/MDNS.hs`.
Integration testing requires the headless binary to run the mDNS
subsystem inside the VM. Currently verified at the unit test level
(`./uv test core-network`).

### M5.4.5: PEX Cross-LAN Discovery

Tests that agents on separate LANs discover each other via the Peer
Exchange (PEX) protocol after a seed connection bridges the two LANs.

**Prerequisites**: Dual-LAN setup (TAP+bridge, M5.3.3). Agent 0 on
LAN A seeds a connection to Agent 3 on LAN B.

**Flow**:
1. LAN A: Agents 0, 1, 2 discover each other via mDNS
2. LAN B: Agents 3, 4, 5 discover each other via mDNS
3. Agent 0 connects to Agent 3 via seed peer (cross-LAN TCP)
4. PEX exchange: Agent 0 sends [1, 2] to Agent 3
5. PEX exchange: Agent 3 sends [4, 5] to Agent 0
6. Agent 0 learns about Agents 4, 5 (marked indirect, 1-hop)
7. Agent 3 learns about Agents 1, 2 (marked indirect, 1-hop)

**1-hop rule limitation**:
- Agent 1 does NOT learn about Agent 4 (would require 2 hops)
- Only the seed agents (0 and 3) gain cross-LAN visibility
- Full mesh requires explicit connections or a relay mechanism

**Verification**:
- Agent 0 logs `pex.exchange` with sent=2, received=2
- Agent 3 logs `pex.exchange` with sent=2, received=2
- Agent 0 logs `pex.peer_received` for Agents 4 and 5
- Agent 3 logs `pex.peer_received` for Agents 1 and 2
- Agents 1, 2, 4, 5 have NO cross-LAN peers (1-hop rule)

**Current status**: PEX exchange is wired into the handshake path
(M5.2.1). Received peers are logged with IP:port. Auto-connect is
best-effort (M5.2.2). Full cross-LAN testing requires dual-LAN
infrastructure (M5.3.3) and the headless binary in the release bundle.

### M5.4.6: Disconnect/Reconnect Resilience

Tests that agents handle peer disconnection gracefully and can
reconnect after network interruption.

**Flow**:
1. Agents 0 and 1 establish connection and exchange messages
2. Agent 1's network is interrupted (kill QEMU network device)
3. Agent 0 detects disconnection within 5 seconds
4. Agent 0's contact list shows Agent 1 as offline
5. Agent 1's network is restored
6. Agent 1 reconnects to Agent 0
7. Message exchange resumes

**Verification**:
- Agent 0 logs `transport.peer_disconnected` for Agent 1
- Agent 0's session status shows `Offline`
- After reconnect: new handshake completes
- Post-reconnect messages are delivered correctly
- No message from the disconnected period is lost or duplicated

**Current status**: Disconnect detection exists in the TCP transport
layer. Reconnect requires the headless binary. Currently tested at
the unit level (`./uv test tcp`, `./uv test recovery`).

## Artifact Handoff

### VM Runner (Class 2) Artifact Flow

1. Host creates ext2 source disk from repository via `genext2fs`
2. QEMU mounts source disk as `/dev/vdb` (read-only)
3. Guest copies source to writable tmpfs at `/work`
4. Guest builds and tests from the tmpfs copy
5. Guest produces release artifact in `/work/umbravox/build/releases/`
6. Guest reports SMOKE_RESULT=PASS/FAIL via serial console
7. Host parses serial output for result

Note: The release artifact produced inside the VM is currently
**not extracted back to the host**. The VM smoke pipeline verifies
that the artifact CAN be built in isolation. Extracting the
in-guest artifact for distribution is tracked as future work.

### Container Runner (Class 3) Artifact Flow

1. Host builds release artifact via `./uv release linux`
2. Host mounts `build/releases/` into container
3. Container extracts and smoke-checks the artifact
4. Container reports exit code

## Hypervisor

QEMU is the sole VM hypervisor. Firecracker was evaluated and
permanently removed as a runtime target.

| Property | QEMU |
|----------|------|
| Available | Yes (via nix-shell) |
| KVM acceleration | Yes |
| NixOS image support | Yes (bootable raw disk) |
| Current status | Functional (`./uv vm smoke`) |
| Use case | Full pipeline isolation |

## Multi-Architecture Emulation (M5.5)

### arm64 via QEMU System Emulation

UmbraVOX can be tested on arm64 via QEMU system emulation on x86_64
hosts. This is significantly slower than KVM-accelerated VMs (~10-50x)
because QEMU must emulate the ARM instruction set in software.

| Property | Value |
|----------|-------|
| Machine | `virt` |
| CPU | `cortex-a72` |
| Serial console | `ttyAMA0` |
| Bootloader | Direct kernel boot (no GRUB) |
| KVM acceleration | Not available (cross-arch) |
| Nix build | Requires aarch64 builder or `binfmt_misc` |
| Expected build time | ~30-60 min (emulated compilation) |
| Expected test time | ~10-30 min (emulated execution) |

**Current status**: Stub image definition exists at `nix/vm-image-aarch64.nix`.
Actual building requires either:
1. A native aarch64 builder (physical ARM64 machine or cloud instance)
2. QEMU user-mode emulation via `binfmt_misc` registered on the host
   (`boot.binfmt.emulatedSystems = [ "aarch64-linux" ];` on NixOS)

### Cross-Architecture Test Matrix (M5.5.2)

| Target | Builder | Runner | Status |
|--------|---------|--------|--------|
| x86_64-linux | x86_64 (KVM) | QEMU q35 (KVM) | Functional |
| aarch64-linux | aarch64 or binfmt | QEMU virt (emulated) | Stub |

Native ARM64 testing provides the strongest assurance. Emulated testing
catches architecture-specific issues (endianness, word size, alignment)
but at significant performance cost.

## Cross-Build Parity Checks (M3.2.2)

When cross-built artifacts are introduced (e.g., building arm64 on
x86_64), parity must be verified by comparing:

1. **Binary content**: SHA-256 of the stripped binary
2. **Library closure**: Set of shared libraries and their versions
3. **Test results**: Same test suites must pass on both builders
4. **Manifest fields**: ABI target, kernel minimum, glibc minimum

Cross-built artifacts are treated as **non-authoritative** (M3.2.1)
until a native builder for the target architecture produces the same
results. The support matrix (M3.2.3) documents which targets have
native vs. cross-built evidence.

Currently, no cross-built artifacts exist. All release targets are
either native (Linux x86_64) or source-only (Windows, macOS, BSD,
FreeDOS).

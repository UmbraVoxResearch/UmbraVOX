<!-- SPDX-License-Identifier: Apache-2.0 -->

# Module-Level MC/DC Coverage Targets (M16.2-4)

**Standard:** DO-178C DAL A
**Tool:** GHC HPC (Haskell Program Coverage)
**Metric:** Expression coverage (finest-grained HPC metric)
**Date:** 2026-05-22

---

## 1. Coverage Target Tiers

Modules are grouped into four tiers based on criticality.  The targets
below apply to HPC expression coverage reported by `hpc report --per-module`.

| Tier     | Target | Rationale                                                    |
|----------|--------|--------------------------------------------------------------|
| Crypto   | 100%   | Core cryptographic primitives; any uncovered path is a risk  |
| Protocol | 95%    | Wire format and handshake paths; high assurance boundary     |
| Network  | 90%    | Distributed networking; complex but not security-critical    |
| TUI/App  | 80%    | User interface; lower risk, higher churn                     |

---

## 2. Per-Module Targets

### 2.1 Crypto Modules (100% decision coverage target)

| Module                              | Target |
|-------------------------------------|--------|
| UmbraVox.Crypto.SHA256              | 100%   |
| UmbraVox.Crypto.SHA512              | 100%   |
| UmbraVox.Crypto.AES                 | 100%   |
| UmbraVox.Crypto.GCM                 | 100%   |
| UmbraVox.Crypto.Ed25519             | 100%   |
| UmbraVox.Crypto.Curve25519          | 100%   |
| UmbraVox.Crypto.MLKEM              | 100%   |
| UmbraVox.Crypto.HMAC               | 100%   |
| UmbraVox.Crypto.HKDF               | 100%   |
| UmbraVox.Crypto.Keccak             | 100%   |
| UmbraVox.Crypto.Poly1305           | 100%   |
| UmbraVox.Crypto.ChaChaPoly         | 100%   |
| UmbraVox.Crypto.ConstantTime       | 100%   |
| UmbraVox.Crypto.BIP39              | 100%   |
| UmbraVox.Crypto.Export             | 100%   |
| UmbraVox.Crypto.VRF               | 100%   |
| UmbraVox.Crypto.Random            | 100%   |
| UmbraVox.Crypto.PQWrapper         | 100%   |
| UmbraVox.Crypto.KeyStore          | 100%   |
| UmbraVox.Crypto.SecureBytes       | 100%   |
| UmbraVox.Crypto.StealthAddress    | 100%   |
| UmbraVox.Crypto.RatchetPersist    | 100%   |
| UmbraVox.Crypto.Signal.X3DH       | 100%   |
| UmbraVox.Crypto.Signal.PQXDH      | 100%   |
| UmbraVox.Crypto.Signal.DoubleRatchet | 100% |
| UmbraVox.Crypto.Signal.SenderKeys | 100%   |
| UmbraVox.Crypto.Signal.Session    | 100%   |

### 2.2 Protocol Modules (95% decision coverage target)

| Module                              | Target |
|-------------------------------------|--------|
| UmbraVox.Protocol.WireFormat        | 95%    |
| UmbraVox.Protocol.Handshake         | 95%    |
| UmbraVox.Protocol.RouteToken        | 95%    |
| UmbraVox.Protocol.MessageFormat     | 95%    |
| UmbraVox.Protocol.SignalWire        | 95%    |
| UmbraVox.Protocol.CBOR             | 95%    |
| UmbraVox.Protocol.Encoding         | 95%    |
| UmbraVox.Protocol.UTF8             | 95%    |
| UmbraVox.Protocol.QRCode           | 95%    |
| UmbraVox.Protocol.ProofOfWork      | 95%    |
| UmbraVox.Network.Noise             | 95%    |
| UmbraVox.Network.Noise.State       | 95%    |
| UmbraVox.Network.Noise.Handshake   | 95%    |

### 2.3 Network Modules (90% decision coverage target)

| Module                              | Target |
|-------------------------------------|--------|
| UmbraVox.Network.Dandelion         | 90%    |
| UmbraVox.Network.PeerManager       | 90%    |
| UmbraVox.Network.DHT               | 90%    |
| UmbraVox.Network.DHT.Types         | 90%    |
| UmbraVox.Network.DHT.RoutingTable  | 90%    |
| UmbraVox.Network.DHT.RPC           | 90%    |
| UmbraVox.Network.DHT.Lookup        | 90%    |
| UmbraVox.Network.DHT.Store         | 90%    |
| UmbraVox.Network.Gossip            | 90%    |
| UmbraVox.Network.Transport         | 90%    |
| UmbraVox.Network.Protocol          | 90%    |
| UmbraVox.Network.Sync              | 90%    |
| UmbraVox.Network.MDNS              | 90%    |
| UmbraVox.Network.PeerExchange      | 90%    |
| UmbraVox.Network.Presence          | 90%    |
| UmbraVox.Network.Discovery         | 90%    |
| UmbraVox.Network.RateLimit         | 90%    |
| UmbraVox.Network.Relay             | 90%    |
| UmbraVox.Network.Listener          | 90%    |

### 2.4 TUI/App Modules (80% decision coverage target)

| Module                              | Target |
|-------------------------------------|--------|
| UmbraVox.TUI.*                     | 80%    |
| UmbraVox.App.*                     | 80%    |
| UmbraVox.Chat.*                    | 80%    |
| UmbraVox.Storage.*                 | 80%    |
| UmbraVox.Tools.*                   | 80%    |
| UmbraVox.Bridge.*                  | 80%    |

---

## 3. Enforcement

The `./uv coverage-check` target parses HPC output and compares each
module against its tier target.  It exits non-zero if any module falls
below its target.

The `./uv coverage-report` target generates the HPC HTML report and a
summary file under `build/coverage/`.

---

## 4. Exceptions and Waivers

Modules with structurally unreachable branches (documented in
`test/evidence/condition-tables/crypto-compound-guards.md`) are exempt
from 100% alternative coverage but must still meet expression coverage
targets.

Generated FFI bridge modules (`UmbraVox.Crypto.Generated.*`) are thin
wrappers and are expected to reach 100% expression coverage trivially.

Types-only modules (`UmbraVox.Crypto.Warning`) contain no executable
code and are excluded from coverage analysis.

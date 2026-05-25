# Security Review v0.4.1

Date: 2026-05-22
Scope: Full adversarial review — privacy, cryptography, security architecture, build, supply chain.
Reviewers: 5 specialized agents (privacy, crypto, security arch, build, supply chain)

## Summary

| Category | CRITICAL | HIGH | MEDIUM | LOW | Total |
|----------|----------|------|--------|-----|-------|
| Privacy  | 1 | 5 | 7 | 2 | 15 |
| Crypto   | 1 | 4 | 9 | 4 | 18 |
| Security | 0 | 4 | 6 | 4 | 14 |
| Build    | 1 | 4 | 5 | 2 | 13 |
| Supply Chain | 0 | 3 | 5 | 3 | 12 |
| **Total** | **3** | **20** | **32** | **15** | **72** |

Note: Some findings overlap across categories (e.g., constant-time crypto appears in both Crypto and Build). Deduplicated actionable items: ~50.

------------------------------------------------------------------------

## CRITICAL Findings (3)

### CR-001: mDNS broadcasts pubkey fingerprint + display name in cleartext
- **Category:** Privacy
- **File:** Network/MDNS.hs:227-230
- **Risk:** Passive LAN observer collects all UmbraVOX identities + IP mappings
- **Fix:** Ephemeral rotating IDs via HKDF(key + per-boot nonce). Remove display name from announcements. Require explicit user opt-in for mDNS.

### CR-002: Variable-time Haskell crypto ships by default
- **Category:** Crypto + Build + Supply Chain
- **Files:** Crypto/AES.hs, Ed25519.hs, Curve25519.hs, MLKEM.hs; UmbraVox.cabal:20-23
- **Risk:** AES S-box, X25519 scalar mul, ML-KEM NTT all variable-time. Practical cache timing attacks on co-tenant systems.
- **Fix:** Default pure-haskell-crypto to False. Add compile-time #error for production builds without FFI.

### CR-003: Shell injection in VM exec heredoc
- **Category:** Build
- **File:** scripts/vm-dev-run.sh:236-265
- **Risk:** Unquoted heredoc expands $cmd on host before VM boot.
- **Fix:** Quote heredoc delimiter ('EXECEOF'), pass cmd via file not variable expansion.

------------------------------------------------------------------------

## HIGH Findings (20)

### HI-001: No message padding — length correlation defeats Dandelion
- **Category:** Privacy
- **File:** Protocol/WireFormat.hs:97-110, 193-215
- **Fix:** Pad payloads to fixed size buckets (256/1024/4096/16384) before encryption.

### HI-002: DHT NodeId derived directly from identity key — linkable
- **Category:** Privacy
- **File:** Network/DHT/Types.hs:55-57
- **Fix:** Use HKDF with per-boot ephemeral salt for NodeId derivation.

### HI-003: PEX shares pubkey fingerprint + IP address
- **Category:** Privacy
- **File:** Network/PeerExchange.hs:103-112
- **Fix:** Exchange only opaque session-specific tokens.

### HI-004: PQXDH message 1 leaks initiator identity key
- **Category:** Privacy
- **File:** Protocol/Handshake.hs:153-155
- **Fix:** Wrap PQXDH initial message inside Noise IK encryption.

### HI-005: Listener logs peer fingerprints in cleartext
- **Category:** Privacy
- **File:** Network/Listener.hs:152-159
- **Fix:** Truncated 4-byte fingerprint with explicit user confirmation.

### HI-006: GCM counter overflow at 2^32 blocks — no length check
- **Category:** Crypto
- **File:** Crypto/GCM.hs:41
- **Fix:** Reject inputs longer than (2^32 - 2) * 16 bytes per NIST SP 800-38D.

### HI-007: CSPRNG Word32 counter overflow wraps silently
- **Category:** Crypto
- **File:** Crypto/Random.hs:346
- **Fix:** Force reseed near maxBound or switch to Word64 counter.

### HI-008: Stealth address lacks prime-order subgroup check on spend key
- **Category:** Crypto
- **File:** Crypto/StealthAddress.hs:147-153
- **Fix:** Verify scalarMul(groupL, bPoint) == identity after decoding spend pubkey.

### HI-009: WireFormat AEAD nonce only 32-bit — reuse after 2^32 msgs
- **Category:** Crypto
- **File:** Protocol/WireFormat.hs:226-230
- **Fix:** Use 64-bit sequence counter in nonce, or re-key before exhaustion.

### HI-010: SQL injection via sqlite3 CLI subprocess
- **Category:** Security
- **File:** Storage/Anthony.hs:411-439
- **Fix:** Migrate to proper SQLite FFI (direct-sqlite) with prepared statements.

### HI-011: Selective mode TOFU accepts all peers without key-change detection
- **Category:** Security
- **File:** Network/Listener.hs:155-163
- **Fix:** Associate peer keys with identity. Reject changed keys, alert user.

### HI-012: Noise transport nonce counter has no overflow check
- **Category:** Security
- **File:** Network/Noise.hs:61-66
- **Fix:** Return Nothing / refuse at nsSendN >= maxBound - 1.

### HI-013: No C compiler hardening flags
- **Category:** Build
- **File:** UmbraVox.cabal c-sources section
- **Fix:** Add cc-options: -fstack-protector-strong -D_FORTIFY_SOURCE=2 -fPIE.

### HI-014: VM firewall disabled in base NixOS image
- **Category:** Build + Supply Chain
- **File:** nix/vm-base.nix:32
- **Fix:** Enable firewall by default, open specific ports where needed.

### HI-015: /work tmpfs world-writable mode 1777
- **Category:** Build
- **File:** nix/vm-base.nix:35-39
- **Fix:** Change to mode=0700.

### HI-016: nixpkgs-unstable branch used
- **Category:** Build + Supply Chain
- **File:** flake.nix:5
- **Fix:** Pin to stable release branch (nixos-24.11).

### HI-017: containsDangerousSQL blocklist bypassable
- **Category:** Security
- **File:** Storage/Anthony.hs:419-439
- **Fix:** Migrate to prepared statements (same as HI-010).

### HI-018: VM base images downloaded without hash verification
- **Category:** Supply Chain
- **Files:** scripts/vm-*-setup.sh
- **Fix:** Pin SHA-256 hashes for each OS image, verify after download.

### HI-019: curl-pipe-sh pattern in build documentation
- **Category:** Supply Chain
- **File:** scripts/release-package-platform.sh:168
- **Fix:** Add hash verification step, point to Nix-based build path.

### HI-020: Pure Haskell crypto default (duplicate of CR-002)
- **Category:** Supply Chain
- **Fix:** Same as CR-002.

------------------------------------------------------------------------

## MEDIUM Findings (32)

### ME-001: StealthKeys derives Show, exposing secrets (Crypto/StealthAddress.hs:53,61)
### ME-002: Route token rotation period too long — 100 msgs / 600s (Protocol/RouteToken.hs:55-58)
### ME-003: Dandelion stem disabled below 5 peers (Network/Dandelion.hs:195-200)
### ME-004: AEAD envelope exposes message type in cleartext AAD (Protocol/WireFormat.hs:201-215)
### ME-005: Export KDF not memory-hard — iterated HKDF not Argon2id (Crypto/Export.hs:77-83)
### ME-006: KeyStore fixed salt + empty passphrase default (Crypto/KeyStore.hs:57-59)
### ME-007: No timing jitter on message forwarding (Network/Listener.hs, Network/Dandelion.hs)
### ME-008: constantEq not truly constant-time in Haskell (Crypto/ConstantTime.hs:23-35)
### ME-009: X3DH/PQXDH key confirmation MAC not wired into handshake (Protocol/Handshake.hs)
### ME-010: PoW difficulty insufficient — 16 bits = ~65K hashes (Protocol/ProofOfWork.hs:43)
### ME-011: Skipped message keys stored indefinitely (Crypto/Signal/DoubleRatchet.hs:134-143)
### ME-012: HKDF info no length prefix — domain separation fragile (Crypto/Signal/X3DH.hs:149)
### ME-013: Ed25519 verify via encodePoint comparison fragile (Crypto/Ed25519.hs:367-369)
### ME-014: PQXDH decaps failure check is dead code (Crypto/Signal/PQXDH.hs:200-203)
### ME-015: No secret key zeroization — SecureBytes migration pending (multiple files)
### ME-016: Sequence window allows delayed replay after 64 msgs (Protocol/WireFormat.hs:280-302)
### ME-017: API auth token comparison not constant-time (Chat/API.hs:79)
### ME-018: IPC hGetLine unbounded before length check (Network/ProviderRuntime.hs:135-143)
### ME-019: SSRF — IPv6 private addresses not blocked (Chat/API.hs:282-304)
### ME-020: DHT value store no entry count limit (Network/DHT/Store.hs:58-62)
### ME-021: Legacy plaintext pass-through undermines encryption (Storage/Anthony.hs:205-213)
### ME-022: Codegen output paths not validated against allowlist (app/codegen/CryptoGen.hs:508-531)
### ME-023: VM auto-login as root with no password (nix/vm-base.nix:17-18)
### ME-024: AGENT_SKIP_CHECKSUM bypass (scripts/vm-integration-agent.sh:212-213)
### ME-025: Makefile eval of config script output (Makefile:1193-1194)
### ME-026: Cabal wide dependency bounds, no freeze file (UmbraVox.cabal:273-287)
### ME-027: VRF challenge truncation 128-bit — 64-bit birthday bound (Crypto/VRF.hs:266-278)
### ME-028: GPG release signing optional and off by default (scripts/release-sign.sh)
### ME-029: Plugin entrypoints can execute arbitrary binaries (plugins/*/manifest.uvx)
### ME-030: Test vectors lack integrity verification (test/vectors/)
### ME-031: Release manifest hardcoded verification claims (scripts/release-sign.sh:66-71)
### ME-032: Reproducibility check not enforced as release gate (scripts/release-reproducibility-check.sh)

------------------------------------------------------------------------

## LOW Findings (15)

### LO-001: RegistrationState exposes private keys via Show (Bridge/Signal/Registration.hs:66,82)
### LO-002: RuntimeLog redaction allowlist fragile (App/RuntimeLog.hs:111-128)
### LO-003: isBundleFresh accepts future timestamps without bound (Crypto/Signal/X3DH.hs:300-305)
### LO-004: PoW challenge no expiry or connection binding (Protocol/ProofOfWork.hs:46-75)
### LO-005: GCM nonce tracker grows unboundedly (Crypto/GCM.hs:234-255)
### LO-006: Handshake unauthenticated amplification (Protocol/Handshake.hs:166)
### LO-007: RateLimit non-atomic IORef access (Network/RateLimit.hs:56-69)
### LO-008: recvLine GC pressure from append pattern (Chat/API.hs:167-180)
### LO-009: getBE16 inconsistent bounds check pattern (Network/PeerExchange.hs:171)
### LO-010: Haskell constantEq lazy length branching (Crypto/ConstantTime.hs)
### LO-011: flake-utils input tracks default branch (flake.lock)
### LO-012: PeerManager evictStale potential race (Network/PeerManager.hs)
### LO-013: VM firewall disabled (duplicate — see HI-014)
### LO-014: No dependency confusion protection for Go tooling (tools/go.mod)
### LO-015: GCM nonce tracker unbounded (duplicate — see LO-005)

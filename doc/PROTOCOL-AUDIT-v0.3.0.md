# Protocol Security Audit — UmbraVox v0.3.0

**Date:** 2026-05-22
**Scope:** Protocol-level analysis of 9 modules.
**Findings:** 1 Critical, 5 High, 12 Medium, 14 Low/Low-Medium — 32 total.

## Critical

### 9.1 WireFormat: Cleartext identity hashes in envelope
envSourceId and envDestId (32-byte identity hashes) are in cleartext header.
Network observer determines who communicates with whom.
**Fix:** Encrypt envelope header under transport session key. Use ephemeral IDs.

## High

### 4.1 Handshake: Unbounded recvBundle allocation
4-byte length prefix → 4GiB allocation. **Fix:** Cap at ~2048 bytes.

### 4.2 Handshake: Unbounded recvInitialMessage allocation
Same issue. **Fix:** Cap at maxInitialMessageSize.

### 6.3 PeerManager: Full table enables eclipse attack
At capacity (1000), new peers silently dropped. Attacker fills table.
**Fix:** Evict lowest-scored peer at capacity. Maintain source diversity.

### 7.1 Chat.API: No authentication
127.0.0.1 binding with no auth. Any local process can send messages.
**Fix:** Bearer token or Unix domain socket with SO_PEERCRED.

### 9.2 WireFormat: No envelope authentication
No MAC on envelope. On-path attacker modifies routing without detection.
**Fix:** Add HMAC/AEAD tag keyed by transport session key.

## Medium (12 findings)

- 1.1 X3DH: No OPK depletion protection
- 1.2 X3DH: No prekey bundle freshness check
- 4.3 Handshake: No version negotiation
- 4.4 Handshake: OPK length not validated
- 5.2 Dandelion: Fixed fluff probability
- 5.3 Dandelion: Stem peer reselection correlation
- 5.4 Dandelion: DropMessage silently discards
- 6.1 PeerManager: Unbounded score manipulation
- 7.2 API: Hand-rolled JSON parser DoS
- 7.3 API: rpcId JSON injection
- 7.5 API: connect SSRF-like behavior
- 9.3 WireFormat: Sequence number not validated

## Low (14 findings)

See full details in agent output. Includes: identity key binding, ML-KEM
failure check, skipped-key DoS, DH replay, evictOldest complexity,
modular bias in uniformIndex, ban expiry not enforced, addPeer lastSeen
reset, PKCS7 deviation, block authentication dependency, payload length
on 32-bit, version validation, status info leak, trust check ordering.

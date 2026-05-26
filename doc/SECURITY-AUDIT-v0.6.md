# Security Audit — v0.6.x

Comprehensive audit performed 2026-05-25 by 8 specialized review agents
covering: side-channel attacks, protocol cryptanalysis, privacy/metadata,
memory safety, key management, supply chain, Haskell correctness, and
test coverage gaps.

## Audit Summary

| Domain | Critical | High | Medium | Low | Total |
|--------|----------|------|--------|-----|-------|
| Side-channel (timing) | 2 | 3 | 4 | 4 | 13 |
| Protocol cryptanalysis | 0 | 3 | 4 | 4 | 11 |
| Privacy / metadata | 0 | 3 | 4 | 5 | 12 |
| Memory safety / input validation | 0 | 1 | 4 | 6 | 11 |
| Key management | 0 | 1 | 5 | 5 | 11 |
| Supply chain / build | 1 | 5 | 6 | 3 | 15 |
| Haskell correctness | 2 | 5 | 6 | 5 | 18 |
| Test coverage gaps | — | — | — | — | 60+ tests needed |
| **Total** | **5** | **21** | **33** | **32** | **91+** |

## Critical Findings (5)

1. Variable-time Ed25519 scalar multiply in fiat-crypto bridge (stub code)
2. memcmp in Ed25519 point decode in fiat-crypto bridge (stub code)
3. pqEncrypt unsafePerformIO sharing risk (live code, has NOINLINE)
4. Ed25519 clampScalar uses partial head/!! (live code)
5. Empty go.sum (no module hash enforcement)

## High Findings (21)

### Side-channel
- ConstantTime.hs length oracle in FFI path
- GCM nonce comparison not constant-time
- ct_helpers.h implementation-defined signed right shift

### Protocol
- Session deserialization discards DH replay detection
- SenderKeys lacks per-message signatures (fixed: ephemeral Ed25519, not identity key)
- PQXDH all-zero KEM check is dead code

### Privacy
- PEX leaks precise peer timestamps
- PEX transmits raw IP addresses
- Storage stores plaintext peer metadata

### Memory
- Variable-time scalar multiply (repeated from side-channel)

### Key Management
- HKDF used instead of Argon2 for passphrase KDF

### Supply Chain
- cabal.project.freeze uses index-state: HEAD
- Builder VM lacks restrict=on network enforcement
- Oracle dependency pins are placeholder hashes
- Signal Server Stage 1 uses unverified git clone
- FreeBSD/OmniOS images use fakeSha256

### Haskell
- Consensus stubs export reachable error "not implemented"
- RateLimit checkRate has TOCTOU race
- Variable-time ML-KEM in production handshake path
- Ed25519 recoverX exported and throws error
- ChaCha20 pure path O(n²) space leak

## Positive Security Controls Observed

The audit identified 30+ well-implemented security controls:
- SecureBytes (mlock, zero-on-finalize) for all ratchet keys
- Constant-time comparison primitives (volatile accumulator)
- Parameterized SQL (no injection vectors)
- Nonce exhaustion guards (2^31 threshold)
- Frame size caps at network boundaries
- Bucket padding for traffic analysis resistance
- Ephemeral mDNS identity (per-boot HKDF)
- PEX opaque tokens (HKDF-derived)
- Dandelion++ stem jitter (CSPRNG delay)
- DH key replay detection (bounded FIFO)
- Ratchet skip rate limiting
- Low-order point rejection in all DH operations
- Key confirmation in PQXDH handshake
- Identity key binding in SPK signatures
- PQ key signature verification before encapsulation

## Test Coverage Gaps

60+ specific test cases identified across 12 security domains:
- 10 crypto primitive test vectors needed (NIST CAVP, Wycheproof)
- 10 negative/invalid input tests
- 5 boundary tests
- 5 adversarial protocol tests
- 5 regression tests for past fixes
- 5 error path tests
- 4 differential tests (Haskell vs C)
- 4 constant-time verification tests
- 5 key management lifecycle tests
- 3 network protocol fuzz tests
- 5 group messaging edge cases
- 5 post-quantum hybrid edge cases

## Remediation Priority

P0 (before any release):
- Replace HKDF with Argon2id for passphrase KDF
- Fix pqEncrypt to return IO (not unsafePerformIO)
- Fix clampScalar partial functions
- Populate go.sum

P1 (security-critical):
- Fix session deserialization DH replay state
- Add per-IP rate limiting to listener
- Encrypt peer metadata in storage
- Quantize PEX timestamps

P2 (defense-in-depth):
- Complete SecureBytes migration for remaining plain ByteStrings
- Add QEMU restrict=on to builder VM
- Pin cabal index-state
- Add compiler barriers to CT helpers

P3 (test coverage):
- Add all 60+ identified test cases
- Add Wycheproof test vector suites
- Add network protocol fuzzing

## Remediation Status (updated 2026-05-26)

P0 Critical: 4/4 fixed
P1 Security-critical: 7/7 fixed
P2 Defense-in-depth: 8/8 fixed
P3 Test coverage: 85+ tests implemented across 10 categories
CVE candidates: 12 identified, 12 addressed

### CVE-4 Deniability Note

SenderKeys per-message signatures use ephemeral Ed25519 key pairs
generated per-group session — NOT the sender's long-term identity key.
The ephemeral private key is held only by the sender; recipients receive
only the public key via SKDM (which travels over deniable pairwise
Double Ratchet channels).  This preserves plausible deniability while
preventing intra-group forgery.

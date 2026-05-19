# Protocol Divergences: UmbraVOX vs libsignal

Known differences between UmbraVOX's protocol implementation and the
canonical Signal / libsignal behavior.  Each entry is classified as
INTENTIONAL (by design), COMPATIBILITY (could be aligned), or UNKNOWN
(needs further investigation).

---

## 1. Wire format: CBOR vs protobuf

**Classification: INTENTIONAL**

libsignal serializes X3DH initial messages, prekey bundles, and
Double Ratchet headers/ciphertext using Protocol Buffers.  UmbraVOX
uses a simple length-prefixed binary format (`Protocol.CBOR` is
misnamed -- it is a 4-byte big-endian length header + raw payload, not
actual CBOR).  The chat wire format (`Chat.Wire`) is a fixed 40-byte
header (32 DH pub + 4 prevChainN + 4 msgN) followed by ciphertext and
a 16-byte GCM tag.  No protobuf dependency exists anywhere in the
codebase.

## 2. HKDF info strings

**Classification: INTENTIONAL**

Signal's X3DH spec uses the info string `"WhisperText"` for the
HKDF derivation step.  UmbraVOX uses project-specific labels:

| Protocol       | UmbraVOX info string          | Signal info string      |
|----------------|-------------------------------|-------------------------|
| X3DH           | `"UmbraVox_X3DH_v1"`         | `"WhisperText"`         |
| PQXDH          | `"UmbraVox_PQXDH_v1"`        | (not yet standardized)  |
| Double Ratchet | `"UmbraVox_Ratchet_v1"`      | `"WhisperRatchet"`      |
| Nonce derive   | `"UmbraVox_Nonce_v1"`        | (no equivalent)         |

All info strings are enumerated in `App/Defaults.hs` (reference
section) and defined in their respective crypto modules.

## 3. Associated data format for X3DH / PQXDH

**Classification: INTENTIONAL**

Signal's X3DH spec defines AD as `Encode(IK_A) || Encode(IK_B)`.
UmbraVOX appends the two X25519 identity public keys directly to the
HKDF info string rather than constructing a separate AD parameter:
`info = "UmbraVox_X3DH_v1" || IK_A_pub || IK_B_pub`.  This achieves
the same identity-binding goal (both identities are committed to the
KDF transcript) but through the info parameter rather than a distinct
AD field.  PQXDH follows the same pattern with `"UmbraVox_PQXDH_v1"`.

## 4. Domain separation labels

**Classification: INTENTIONAL**

UmbraVOX uses versioned, project-namespaced domain separation labels
throughout (e.g. `"UmbraVox_StealthKey_v1"`, `"UmbraVox_ViewTag_v2"`,
`"UmbraVox_KeyStore_v1"`, `"UmbraVox_Export_v1"`,
`"UmbraVox_SafetyNumber_v2"`).  Signal uses unversioned labels like
`"WhisperText"` and `"WhisperRatchet"`.  UmbraVOX's labels are
intentionally incompatible to prevent cross-protocol key confusion.

## 5. PQ wrapping: ML-KEM integration in PQXDH

**Classification: INTENTIONAL**

libsignal's PQXDH concatenates the ML-KEM shared secret into the IKM
for HKDF alongside the DH outputs.  UmbraVOX additionally hashes the
ML-KEM ciphertext and includes it:
`ikm = 0xFF*32 || dh1 || dh2 || dh3 || [dh4] || pq_ss || SHA256(pq_ct)`.
The `SHA256(pq_ct)` binding ensures both parties committed to the same
ciphertext, preventing substitution attacks on the KEM exchange.
UmbraVOX also requires an Ed25519 signature over the ML-KEM
encapsulation key (`pqpkbPQKeySignature`), which was added as a fix
for finding M10.2.1; libsignal's PQXDH draft spec includes a similar
signature requirement.

## 6. Noise transport pattern

**Classification: COMPATIBILITY**

UmbraVOX implements Noise\_IK with the pattern `-> e, es, s, ss / <- e, ee, se`
using `Noise_IK_25519_ChaChaPoly_SHA256` as the protocol name, which
matches the standard Noise naming convention.  The post-handshake
transport uses RFC 8439 ChaCha20-Poly1305 with the final handshake
hash as AAD (channel binding).  Two divergences from canonical Noise:

- **Handshake encryption**: The initiator's static key in message 1 is
  encrypted with ChaCha20 + HMAC-SHA256 (32-byte tag) rather than
  ChaCha20-Poly1305 AEAD.  This uses a separate `encryptWithKey`
  function with a zero nonce and HMAC authentication, producing a
  non-standard 32-byte MAC instead of a 16-byte Poly1305 tag.
- **Prologue**: UmbraVOX mixes `"UmbraVox_v1"` as the prologue;
  Signal uses an empty prologue for its Noise sessions.
- **Key split labels**: `splitKeys` uses HKDF-SHA256 with info labels
  `"enc-send"` / `"enc-recv"` rather than the Noise spec's standard
  `Split()` operation (which takes the first 64 bytes of
  HKDF-Expand with empty info).

## 7. Ratchet state serialization

**Classification: INTENTIONAL**

Signal persists ratchet state via protobuf-encoded session records.
UmbraVOX persists only the send counter as a 4-byte big-endian value
via `RatchetPersist` (atomic POSIX rename for crash safety).  Full
ratchet state is held in memory as a Haskell `RatchetState` record
with `Map (ByteString, Word32) (ByteString, ByteString, Word64)` for
skipped keys.  There is no on-disk serialization of the complete
ratchet state in the current codebase; the in-memory representation
uses native Haskell types, not a portable wire format.

## 8. Skipped message key limits

**Classification: COMPATIBILITY**

| Limit             | UmbraVOX                     | libsignal (Signal Android/iOS) |
|-------------------|------------------------------|-------------------------------|
| Per-step max skip | 1,000 (`defaultMaxSkip`)     | 2,000                         |
| Total cache cap   | 5,000 (`defaultMaxTotalSkipped`) | ~8,000 (undocumented)     |

UmbraVOX's limits are more conservative.  The per-step limit prevents
a single ratchet advance from storing more than 1,000 skipped keys;
the global cap (M7.3.6) evicts the oldest entries by insertion order
(FIFO) when exceeded.  These values are configurable via
`App/Defaults.hs`.

## 9. HKDF hash function

**Classification: INTENTIONAL**

UmbraVOX uses HKDF-SHA-512 for X3DH, PQXDH, and Double Ratchet key
derivation (`hkdfExtract`/`hkdfExpand` use HMAC-SHA-512).  The Noise
handshake uses HKDF-SHA-256 (`hkdfSHA256Extract`/`hkdfSHA256Expand`).
Signal's X3DH and Double Ratchet spec calls for HKDF-SHA-256.  The
SHA-512 choice provides a larger PRK (64 bytes vs 32) and higher
security margin for the key agreement protocols.

## 10. Double Ratchet nonce construction

**Classification: INTENTIONAL**

Signal derives GCM/AEAD nonces as a simple counter or from the message
key.  UmbraVOX derives the nonce from the chain key (not the message
key) via HKDF with a zero salt and info label `"UmbraVox_Nonce_v1"`,
producing an 8-byte base that is XORed with the 8-byte LE message
counter, prepended with 4 zero bytes (matching Noise/RFC 8439 layout).
This enforces key separation between the GCM encryption key and nonce
derivation input (M10.2.5 fix).

## 11. Double Ratchet AEAD cipher

**Classification: UNKNOWN**

The Double Ratchet uses AES-256-GCM (`gcmEncrypt`/`gcmDecrypt`) for
message encryption, while the Noise transport uses ChaCha20-Poly1305.
Signal uses AES-256-CBC + HMAC-SHA-256 in older protocol versions and
AES-256-GCM in newer ones.  The mixed AEAD usage across protocol
layers (GCM for ratchet, ChaCha20-Poly1305 for Noise) is
architecturally sound but differs from libsignal's uniform approach.
Whether this is intentional or an artifact of incremental development
needs clarification.

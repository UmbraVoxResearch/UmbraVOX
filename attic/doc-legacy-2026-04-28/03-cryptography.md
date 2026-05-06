# Cryptographic Architecture

## Implementation Status

All cryptographic primitives below are **pure Haskell reference implementations** using only `base` and standard libraries. They are **NOT constant-time** -- GHC's runtime introduces timing variability through lazy evaluation, garbage collection, and branch prediction artifacts.

Constant-time C FFI backends are **planned but not yet implemented**. The Generated/FFI/ module stubs exist but do not contain working C bindings. Until the FFI backends are complete, these implementations are unsuitable for deployment where timing attacks are a concern.

## Primitives

| Primitive | Standard | Module | Purpose |
|-----------|----------|--------|---------|
| SHA-256 | FIPS 180-4 | `Crypto.SHA256` | Block hashing, Noise handshake hash |
| SHA-512 | FIPS 180-4 | `Crypto.SHA512` | Ed25519 internals, HKDF-SHA-512 |
| SHA-3 / SHAKE | FIPS 202 | `Crypto.Keccak` | SHA3-224/256/384/512, SHAKE-128/256; ML-KEM dependency |
| HMAC | RFC 2104 | `Crypto.HMAC` | HMAC-SHA-256, HMAC-SHA-512; Signal MAC, Noise auth |
| HKDF | RFC 5869 | `Crypto.HKDF` | Key derivation (SHA-512 and SHA-256 variants) |
| AES-256 | FIPS 197 | `Crypto.AES` | Block cipher core for GCM |
| AES-256-GCM | NIST SP 800-38D | `Crypto.GCM` | AEAD encryption for Double Ratchet messages |
| ChaCha20 | RFC 8439 | `Crypto.Random` | Stream cipher, CSPRNG keystream |
| Poly1305 | RFC 8439 | `Crypto.Poly1305` | One-time authenticator |
| X25519 | RFC 7748 | `Crypto.Curve25519` | Diffie-Hellman key exchange |
| Ed25519 | RFC 8032 Sec 5.1 | `Crypto.Ed25519` | Digital signatures (PureEd25519) |
| ML-KEM-768 | FIPS 203 | `Crypto.MLKEM` | Post-quantum key encapsulation |

All module paths are under `UmbraVox.` (e.g., `UmbraVox.Crypto.SHA256`).

Generated pure Haskell duplicates exist under `Crypto.Generated.*` with corresponding FFI stubs under `Crypto.Generated.FFI.*` for: SHA-256, SHA-512, HMAC, HKDF, AES-256, ChaCha20, Poly1305, X25519, Keccak, ML-KEM-768.

### Ed25519 Variant

PureEd25519 per RFC 8032 Section 5.1. Messages are NOT pre-hashed before signing. This is the standard Ed25519 algorithm where the message is passed directly to the signing function (not the Ed25519ph pre-hash variant from Section 5.1.1).

### HKDF Parameters

All HKDF invocations use SHA-512 as the underlying hash (via `Crypto.HKDF.hkdf`/`hkdfExtract`/`hkdfExpand`) and the following parameters:

| Context | Salt | Info String |
|---------|------|-------------|
| PQXDH master secret derivation | 32 zero bytes (0x00 * 32) | `"UmbraVox_PQXDH_v1"` |
| Double Ratchet chain key derivation | 32 zero bytes (0x00 * 32) | `"UmbraVox_Ratchet_v1"` |
| Stealth address view tag | 32 zero bytes (0x00 * 32) | `"UmbraVox_ViewTag_v2"` |
| Stealth address scalar | 32 zero bytes (0x00 * 32) | `"UmbraVox_StealthKey_v1"` |

SHA-256 variants (`hkdfSHA256Extract`, `hkdfSHA256Expand`) are used by the Noise handshake (`Network.Noise.Handshake`).

The HKDF-Expand loop counter uses integer division `(len + hashLen - 1) / hashLen` to compute the number of HMAC iterations.

The domain separator `0xFF * 32` (32 bytes of 0xFF) is prepended to the DH concatenation before HKDF input, per Signal spec convention.

Full PQXDH derivation: `HKDF(salt=0x00*32, ikm=0xFF*32 || dh1 || dh2 || dh3 || dh4 || pq_ss, info="UmbraVox_PQXDH_v1")`

### CSPRNG Specification

ChaCha20-based CSPRNG implemented in `Crypto.Random`:

- **State**: 32-byte key, 12-byte nonce, block counter, output buffer, reseed counter, PID
- **Global state**: `MVar (Maybe CSPRNGState)` -- thread-safe via MVar locking
- **Seed source**: `/dev/urandom`, 44 bytes (32 key + 12 nonce), read at first call (lazy init)
- **Reseed interval**: every 2^20 (1,048,576) outputs; reseeds via `HKDF-Extract(old_key, fresh_entropy)` for backtracking resistance
- **Fork safety**: PID check on each call via `getProcessID`. If PID differs from the PID recorded at last seed/reseed, the CSPRNG is immediately reseeded before producing output
- **API**: `randomBytes :: Int -> IO ByteString`

### X25519 Zero-Check

`Crypto.Curve25519.x25519` rejects all-zero DH outputs per RFC 7748 Section 6.1, raising an error. This prevents small-subgroup and invalid-curve attacks that would produce a trivially predictable shared secret.

## Additional Cryptographic Modules

| Module | Purpose |
|--------|---------|
| `Crypto.StealthAddress` | Dual-Key Stealth Address Protocol (DKSAP): scan key (X25519) + spend key (Ed25519), one-time address derivation, view tags for fast filtering, recipient scanning |
| `Crypto.BIP39` | BIP39 passphrase generation from the 2048-word English wordlist (11 bits per word) |
| `Crypto.ConstantTime` | `constantEq` for constant-time byte comparison (used in Noise MAC verification, stealth address scanning) |
| `Crypto.Export` | Crypto re-exports |
| `Crypto.KeyStore` | Key storage |
| `Crypto.PQWrapper` | Post-quantum wrapper layer |

## Signal Protocol

Implemented across three modules:

- `Crypto.Signal.X3DH` -- classical X3DH key agreement
- `Crypto.Signal.PQXDH` -- hybrid classical + post-quantum key agreement
- `Crypto.Signal.DoubleRatchet` -- Double Ratchet for forward-secure messaging

### Key Types

| Key | Algorithm | Lifetime |
|-----|-----------|----------|
| Identity Key (IK) | Ed25519/X25519 dual-use | Permanent per user |
| Signed PreKey (SPK) | X25519 | Rotated periodically |
| One-Time PreKey (OPK) | X25519 | Single use, consumed on first contact |
| PQ PreKey (PQPK) | ML-KEM-768 | Single use, consumed on first contact |

### Session Establishment (PQXDH)

Hybrid classical + post-quantum key agreement:

```
Alice -> Bob (first message):
  1. Fetch Bob's prekey bundle
  2. Verify SPK signature (Ed25519)
  3. Generate ephemeral X25519 keypair
  4. Compute 4 DH shared secrets:
     dh1 = X25519(Alice_IK, Bob_SPK)
     dh2 = X25519(Alice_EK, Bob_IK)
     dh3 = X25519(Alice_EK, Bob_SPK)
     dh4 = X25519(Alice_EK, Bob_OPK)  -- if available
  5. ML-KEM encapsulate against Bob's PQPK -> (pq_ct, pq_ss)
  6. Master secret = HKDF(salt=0x00*32, ikm=0xFF*32 || dh1 || dh2 || dh3 || [dh4] || pq_ss, info="UmbraVox_PQXDH_v1")
  7. Initialize Double Ratchet with master secret
  8. Encrypt message, include ephemeral key + pq_ct in header
```

### Double Ratchet State

From `Crypto.Signal.DoubleRatchet`:

```haskell
data RatchetState = RatchetState
  { rsDHSend      :: !(ByteString, ByteString)   -- (secret, public) X25519 sending keypair
  , rsDHRecv      :: !(Maybe ByteString)          -- Peer's current X25519 public key
  , rsRootKey     :: !ByteString                  -- 32 bytes
  , rsSendChain   :: !ByteString                  -- 32 bytes
  , rsRecvChain   :: !ByteString                  -- 32 bytes
  , rsSendN       :: !Word32
  , rsRecvN       :: !Word32
  , rsPrevChainN  :: !Word32
  , rsSkippedKeys :: !(Map (ByteString, Word32) ByteString)
  , rsGlobalSeqN  :: !Word64                      -- Monotonic counter for nonce uniqueness
  }
```

### Ratchet Header

```haskell
data RatchetHeader = RatchetHeader
  { rhDHPublic   :: !ByteString   -- 32-byte sender DH public key
  , rhPrevChainN :: !Word32       -- Messages in previous sending chain
  , rhMsgN       :: !Word32       -- Message number in current chain
  }
```

Encryption uses AES-256-GCM (`Crypto.GCM`) with keys derived via HKDF from the ratchet chain key.

The API exports: `ratchetInitAlice`, `ratchetInitBob`, `ratchetEncrypt`, `ratchetDecrypt`.

### Sender Keys

`Crypto.Signal.SenderKeys` provides group messaging sender key distribution.

## Security Fixes Applied

The following security issues were identified and fixed:

1. **X25519 zero-check** (`Crypto.Curve25519`): DH outputs that are all-zero bytes are rejected with an error, per RFC 7748 Section 6.1. Prevents small-subgroup attacks.

2. **HKDF integer math** (`Crypto.HKDF`): The expand loop iteration count uses `(len + hashLen - 1) / hashLen` to compute `ceil(L/HashLen)` correctly, avoiding off-by-one errors that could truncate output.

3. **Noise key separation** (`Network.Noise.Handshake`): The `splitKeys` function derives four independent keys (send-enc, send-mac, recv-enc, recv-mac) from the final chaining key using HKDF-SHA-256 with distinct info strings (`"enc-send"`, `"mac-send"`, `"enc-recv"`, `"mac-recv"`). This prevents key reuse between encryption and authentication.

## Security Model

PQXDH provides IND-CCA2 security under the hybrid assumption: security holds if EITHER the classical DH problem (CDH on Curve25519) OR the ML-KEM problem (Module-LWE) is hard.

UmbraVox follows Signal's PQXDH specification, combining classical X3DH with ML-KEM-768 key encapsulation:

- Compromise of the classical layer alone does not break confidentiality because the ML-KEM shared secret remains secure
- Compromise of the PQ layer alone does not break confidentiality because the classical DH shared secrets remain secure

The Double Ratchet provides forward secrecy and post-compromise security on top of the initial PQXDH key agreement.

## Standard References

| Standard | Title |
|----------|-------|
| FIPS 202 | SHA-3 Standard: Permutation-Based Hash and Extendable-Output Functions |
| FIPS 203 | ML-KEM (Module-Lattice-Based Key-Encapsulation Mechanism) |
| FIPS 180-4 | Secure Hash Standard (SHA-2 family) |
| FIPS 197 | Advanced Encryption Standard (AES) |
| RFC 2104 | HMAC: Keyed-Hashing for Message Authentication |
| RFC 5869 | HMAC-based Extract-and-Expand Key Derivation Function (HKDF) |
| RFC 7748 | Elliptic Curves for Security (X25519) |
| RFC 8032 | Edwards-Curve Digital Signature Algorithm (Ed25519) |
| RFC 8439 | ChaCha20 and Poly1305 for IETF Protocols |
| NIST SP 800-38D | Recommendation for Block Cipher Modes of Operation: GCM |

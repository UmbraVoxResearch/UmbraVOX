# UmbraVOX Persistence Encryption Model

## Overview

UmbraVOX provides application-layer authenticated encryption for persistent
storage. Message content and conversation names are encrypted using
AES-256-GCM before being written to the SQLite database. The SQLite file
format itself is unchanged; there is no dependency on SQLCipher or any
database-level encryption.

## Key Derivation

Storage encryption keys are derived per-identity using HKDF-SHA-256:

- **Input keying material**: identity secret key (32 bytes)
- **Salt**: `UmbraVox_StorageAEAD_v1_salt`
- **Info**: `UmbraVox_StorageAEAD_v1`
- **Output**: 32-byte AES-256-GCM key

Each identity produces a unique, deterministic storage key. Changing the
identity key changes the storage key and renders previously encrypted data
unreadable (fail-closed behavior).

## Encryption Scheme

- **Algorithm**: AES-256-GCM (NIST SP 800-38D)
- **Nonce**: 12 bytes, randomly generated per field encryption
- **AAD**: empty (fields are bound to their database row by storage position)
- **Wire format**: `UVENC1:` prefix followed by hex-encoded
  `nonce (12) || ciphertext || tag (16)`
- **Migration**: fields not prefixed with `UVENC1:` are treated as
  legacy plaintext and returned as-is on read

## What Is Encrypted

| Field | Table | Encrypted | Rationale |
|-------|-------|-----------|-----------|
| Message content | `messages.content` | Yes | Primary confidentiality target |
| Conversation name | `conversations.name` | Yes | May contain identifying information |

## What Is NOT Encrypted (Residual Plaintext Metadata)

| Field | Table | Rationale for plaintext |
|-------|-------|------------------------|
| Peer public key | `peers.pubkey`, `conversations.peer_pubkey` | Already public by design; needed for session matching |
| IP address | `peers.ip` | Needed for reconnection; changes frequently |
| Port | `peers.port` | Needed for reconnection |
| Last seen timestamp | `peers.last_seen` | Ordinal metadata needed for peer selection |
| Peer source | `peers.source` | Operational metadata (mdns/pex/manual) |
| Message sender | `messages.sender` | Display name; needed for rendering without full decrypt |
| Message timestamp | `messages.timestamp` | Needed for ordering queries; ordinal |
| Conversation ID | `conversations.id` | SQLite primary key; needed for joins |
| Conversation created | `conversations.created` | Ordinal metadata |
| Trusted key label | `trusted_keys.label` | User-facing label; not confidential |
| Trusted key pubkey | `trusted_keys.pubkey` | Already public by design |
| Settings key/value | `settings.*` | Configuration; not message content |

## Residual Risk

An attacker with access to the SQLite database file can observe:

- **Number of conversations** and their creation timestamps
- **Number of messages** per conversation and their timestamps
- **Timing patterns** (when messages were sent/received)
- **Peer identifiers** (public keys, IP addresses, ports)
- **Message sizes** (ciphertext length reveals approximate plaintext length)
- **Sender display names** within conversations

The attacker cannot read:

- **Message content** (encrypted with AES-256-GCM)
- **Conversation names** (encrypted with AES-256-GCM)

## Fail-Closed Behavior

- If the storage key does not match (e.g., identity key changed), message
  decryption returns `Nothing` and the conversation is skipped during
  restore.
- A diagnostic log event `persistence.decrypt.failed` is emitted with the
  count of failed conversations.
- The user sees a message indicating how many conversations could not be
  decrypted.
- The application continues with the conversations that did decrypt
  successfully.

## Test Key

A deterministic test key (`testStorageKey`) is available for
non-production testing. It is derived from a fixed 32-byte input and
produces consistent ciphertext for deterministic test assertions.

## Future Work

- Encrypt sender display names (requires rendering changes)
- Encrypt timestamps (requires query restructuring)
- Add key rotation mechanism
- Evaluate full-database encryption via SQLCipher if constant-time and
  key-management requirements are met

# Encrypted Envelope Design (M23.1.1)

**Status:** Draft
**Date:** 2026-05-21
**Fixes:** CRITICAL finding 9.1 — cleartext identity hashes in WireFormat envelope

## 1. Problem Statement

The current wire format exposes `envSourceId` and `envDestId` (32-byte identity
hashes) in the cleartext envelope header:

```
[version:1][type:1][seq:4][srcId:32][dstId:32][payloadLen:4][payload:N][hmac:32]
```

Any network observer can determine who communicates with whom, completely
undermining the privacy properties of Dandelion++ routing.  Even if the payload
is encrypted under the Double Ratchet, the envelope metadata leaks the full
social graph.

## 2. Design Constraints

1. **Routing must work** -- relays in Dandelion++ stem phase need to know where
   to forward, but they must NOT learn real identities.
2. **Sender identity hidden from relays** -- only the final recipient should
   learn who sent the message.
3. **Dandelion++ integration** -- stem relays route by opaque token, not
   identity hash.
4. **Noise IK integration** -- after PQXDH handshake, both peers share a
   transport session key.  The envelope rides inside the encrypted Noise
   transport, but the same format is used for multi-hop stem routing where
   intermediate relays only see the outer layer.
5. **HMAC authentication preserved** -- the existing HMAC-SHA-256 tag (M23.1.6)
   must continue to authenticate the envelope.
6. **DHT compatibility** -- the DHT (M24) stores `SHA-256(identity key)` as
   NodeId for lookups.  The new scheme must not require DHT to store or expose
   real identity hashes.

## 3. Option Evaluation

### Option A: Encrypt entire envelope under transport session key

After Noise IK handshake, encrypt the full envelope (including srcId/dstId)
under the transport key.

- **Pro:** Simple.  One symmetric encryption layer over everything.
- **Con (fatal):** In Dandelion++ stem phase, a relay receives a message from
  the previous stem hop.  The relay has a transport session key with the
  *previous* hop, not with the *originator*.  Once decrypted, the relay sees the
  real srcId/dstId.  To forward, it would re-encrypt under its session key with
  the *next* hop.  But the relay now knows the real identity pair.  This defeats
  the purpose.
- **Con:** Even for direct (non-relayed) connections, if the transport is
  compromised (e.g., Noise session key leaked), all identity information is
  immediately exposed.

**Verdict: REJECTED** -- relays learn real identities.

### Option B: Ephemeral per-session routing tokens

Replace srcId/dstId with short-lived random tokens.  Each transport session
negotiates a token pair during handshake.  Relays route by token.

- **Pro:** Tokens are unlinkable to identity.  Rotation limits correlation
  window.
- **Pro:** Simple to implement -- token assignment during handshake, lookup
  table per session.
- **Con:** For Dandelion++ stem routing, the relay doesn't have a direct session
  with the originator.  Who assigns the token?  The originator can't pre-arrange
  tokens with relays it hasn't connected to.
- **Con:** DHT lookups can't use tokens (tokens are session-local).

**Verdict: PARTIAL** -- good for direct peer-to-peer links, but insufficient
for multi-hop stem routing and initial message delivery.

### Option C: Onion-style routing (nested encryption)

Sender wraps the envelope in layers: innermost for recipient, outermost for
first relay.  Each relay peels one layer, discovers the next hop, and forwards.

- **Pro:** Maximum privacy -- no relay learns both sender and recipient.
- **Pro:** Well-understood (Tor, Sphinx).
- **Con:** Requires knowing the full route in advance (sender must select all
  relays).  Dandelion++ deliberately randomizes the stem path -- each relay
  independently chooses to stem or fluff.  Onion routing and Dandelion++ are
  architecturally incompatible.
- **Con:** Message size grows with hop count (one layer per hop).
- **Con:** Significant complexity increase.

**Verdict: REJECTED** -- incompatible with Dandelion++ probabilistic routing.

### Option D: Stealth addressing for destination + encrypted sender

Use the existing DKSAP (StealthAddress.hs) for the destination field.  Encrypt
the sender identity inside the payload, visible only to the recipient.

- **Pro:** Leverages existing, audited stealth address code.
- **Pro:** Relays see a random-looking one-time destination address that only
  the recipient can recognize (via scan key).
- **Pro:** Sender identity is inside the Double Ratchet encrypted payload --
  invisible to everyone except the recipient.
- **Pro:** DHT integration: recipient periodically scans DHT for entries
  matching their scan key.
- **Con:** Recipient must scan every message to check if it's for them (view
  tag reduces this to ~1/256 full scans).
- **Con:** Requires ephemeral key in the envelope (32 bytes), increasing header
  size.

**Verdict: STRONG CANDIDATE** -- but needs hybrid approach for relay routing.

## 4. Recommended Design: Option D+B Hybrid (Stealth Destination + Session Tokens)

Combine stealth addressing (Option D) for destination privacy with per-session
routing tokens (Option B) for relay-hop routing.

### 4.1 Architecture

Two distinct routing layers:

1. **Transport layer** (relay-to-relay): uses ephemeral session tokens.  Each
   Noise IK session negotiates a pair of 16-byte routing tokens.  Stem relays
   route by token lookup.  Tokens rotate each Dandelion++ epoch.

2. **Envelope layer** (end-to-end): uses stealth addresses for the destination
   and encrypts the sender identity inside the payload.

```
Transport Layer (relay sees):
  [sessionToken:16][encryptedEnvelope:N]

Envelope Layer (recipient decrypts):
  [version:1][type:1][seq:4][ephemeral:32][viewTag:1][payloadLen:4][payload:N][hmac:32]

Payload (inside Double Ratchet encryption):
  [senderId:32][applicationData:M]
```

### 4.2 Layer Details

#### 4.2.1 Transport Layer: Session Routing Tokens

During the Noise IK / PQXDH handshake, both peers derive a pair of routing
tokens using the shared transport key:

```
tokenMaterial = HKDF(salt=0x00*32, ikm=transportKey, info="UmbraVox_RouteToken_v1", len=32)
tokenAtoB = tokenMaterial[0..15]   -- 16 bytes, A uses when sending to B
tokenBtoA = tokenMaterial[16..31]  -- 16 bytes, B uses when sending to A
```

Each peer maintains a `Map ByteString SessionId` for routing incoming messages
to the correct session.

**Token rotation:** When `checkEpoch` triggers a Dandelion++ epoch rotation,
derive new tokens:

```
newTokenMaterial = HKDF(salt=0x00*32, ikm=transportKey, info="UmbraVox_RouteToken_v1_epoch_" <> epochCounter, len=32)
```

Both peers maintain the current AND previous epoch's tokens for a grace period
(one epoch length) to handle in-flight messages.

**For stem relays:** A relay receiving a message sees the session token,
looks up the outbound session for the designated stem peer, and replaces the
token with the outbound session's token before forwarding.  The relay never
sees the envelope contents (which are encrypted under the end-to-end transport).

#### 4.2.2 Envelope Layer: Stealth Destination

The envelope header replaces `srcId` and `dstId` with:

- `ephemeral` (32 bytes): the sender's ephemeral X25519 public key R, used by
  the recipient to recognize the message via their scan key (DKSAP).
- `viewTag` (1 byte): first byte of the DKSAP shared secret, allowing the
  recipient to reject 255/256 of non-matching messages with a single byte
  comparison.

The sender identity (`srcId`) moves INSIDE the encrypted payload, where it is
protected by the Double Ratchet.

**Destination matching by recipient:**

1. Recipient extracts `ephemeral` (R) and `viewTag` from the envelope.
2. Compute shared secret: `ss = x25519(scanSecret, R)`.
3. Derive view tag: `vt = HKDF(0x00*32, ss, "UmbraVox_ViewTag_v2", 32)[0]`.
4. If `vt != viewTag`, skip (fast reject -- happens 255/256 of the time for
   non-matching messages).
5. Derive stealth scalar and check against a known set of pending
   conversations, OR use the session token lookup if already in an active
   session.

**For active sessions:** Once a session is established, the session token
(Section 4.2.1) is sufficient for routing.  The stealth address fields are
only needed for:
- Initial contact (no session exists yet)
- Session recovery after token rotation failure

#### 4.2.3 Sender Identity in Encrypted Payload

The sender's 32-byte identity hash moves from the cleartext header into the
Double Ratchet encrypted payload:

```
encryptedPayload = ratchetEncrypt(senderIdHash || applicationData)
```

Only the recipient, who holds the ratchet state, can decrypt and learn who sent
the message.  Relays, observers, and even other legitimate nodes on the network
never see the sender identity.

### 4.3 New Wire Format

#### 4.3.1 On-the-Wire Layout

```
Outer (per-hop, visible to current relay):
  [routeToken:16][innerLen:4][inner:N]

Inner (end-to-end, encrypted under session key after first handshake):
  [version:1][type:1][seq:4][ephemeralR:32][viewTag:1][payloadLen:4][payload:N][hmac:32]
```

Header size: 1 + 1 + 4 + 32 + 1 + 4 = 43 bytes (down from 74, despite adding
ephemeral key, because srcId and dstId are removed).

Total per-message overhead: 16 (token) + 4 (length) + 43 (header) + 32 (hmac)
= 95 bytes (current: 74 + 32 = 106 bytes).  Net savings of 11 bytes.

#### 4.3.2 Message Types and Their Use of Fields

| type | name      | ephemeralR     | viewTag | payload contains       |
|------|-----------|----------------|---------|------------------------|
| 0    | data      | sender's eph R | yes     | senderId + ciphertext  |
| 1    | ack       | zeroed         | 0x00    | ack data               |
| 2    | handshake | from PQXDH     | yes     | handshake payload      |
| 3    | peer      | zeroed         | 0x00    | peer exchange data     |

For type 1 (ack) and type 3 (peer), the ephemeralR field is zeroed because
these messages travel over established sessions where the session token already
identifies the route.

For type 2 (handshake / initial contact), ephemeralR carries the PQXDH
ephemeral key and viewTag enables recipient scanning.

### 4.4 Key Derivation Summary

All KDF operations use HKDF-SHA-256 with 32-byte zero salt, consistent with
the existing HKDF usage in StealthAddress.hs.

| Derivation | IKM | Info | Length | Purpose |
|---|---|---|---|---|
| Route token | transportKey | `UmbraVox_RouteToken_v1` | 32 | Session routing (16+16) |
| Route token (epoch) | transportKey | `UmbraVox_RouteToken_v1_epoch_` + counter | 32 | Rotated tokens |
| Envelope HMAC key | transportKey | `UmbraVox_EnvelopeHMAC_v1` | 32 | HMAC-SHA-256 tag |
| View tag | ECDH(eph, scan) | `UmbraVox_ViewTag_v2` | 32 | Fast recipient filter |
| Stealth scalar | ECDH(eph, scan) | `UmbraVox_StealthKey_v1` | 32 | Stealth address derivation |

### 4.5 Envelope Encryption (Inner Layer)

After the Noise IK handshake, both peers derive an envelope encryption key:

```
envelopeKey = HKDF(salt=0x00*32, ikm=transportKey, info="UmbraVox_EnvelopeKey_v1", len=32)
```

The inner envelope (version through payload, excluding HMAC) is encrypted with
ChaCha20-Poly1305 using a nonce derived from the sequence number:

```
nonce = 0x00*4 || BE32(sequence) || 0x00*4   -- 12 bytes, unique per message per session
```

The Poly1305 tag replaces the HMAC tag, providing both encryption and
authentication in a single pass.  This is more efficient than encrypt-then-HMAC
and avoids the separate HMAC key.

**Rationale for ChaCha20-Poly1305 over AES-256-GCM:** The codebase already
has both.  ChaCha20 is preferred for wire formats because it is constant-time
in software without AES-NI, which matters on the target platforms (illumos,
BSDs, ARM).

**However:** For the initial handshake message (type 2), there is no session
key yet.  The handshake message uses the cleartext inner format with HMAC
authentication (the handshake itself establishes the session key).  The
ephemeralR and viewTag fields enable the recipient to recognize the handshake
is for them.

### 4.6 Dandelion++ Integration

#### 4.6.1 Stem Phase

1. Originator wraps message in inner envelope (stealth dest + encrypted sender).
2. Originator prepends its outbound session token for the stem peer.
3. Stem relay receives `[routeToken][innerLen][inner]`.
4. Relay looks up session by `routeToken`.  The inner envelope is opaque.
5. Relay makes stem/fluff decision (existing `routeMessage` logic).
6. If stem: replace `routeToken` with the outbound token for the next stem
   peer.  Forward `[newToken][innerLen][inner]`.
7. If fluff: broadcast to all peers, each copy getting the appropriate outbound
   session token.

The relay never decrypts the inner envelope.  It only sees session tokens.

#### 4.6.2 Fluff Phase

During fluff broadcast, the message is sent to all connected peers.  Each
copy gets the per-session outbound token for that peer.  Recipients scan the
inner envelope using their scan key (initial messages) or session token lookup
(established sessions).

#### 4.6.3 Epoch Rotation

When `checkEpoch` fires:
1. Derive new route tokens from the transport key + new epoch counter.
2. Keep old tokens valid for one epoch (grace period).
3. `rotateStemPeer` selects a new stem peer as before.
4. Old session token mappings are garbage-collected after the grace period.

### 4.7 DHT Integration

#### 4.7.1 Publishing Presence

A node publishes its presence to the DHT using a stealth-derived key, NOT its
real identity hash:

```
presenceKey = stealthAddress(recipientScanPub, recipientSpendPub)
DHT.store(SHA256(presenceKey.saAddress), encryptedContactInfo)
```

The `encryptedContactInfo` contains the node's current network address,
encrypted under a key only the original publisher can derive.

#### 4.7.2 Finding a Peer

To reach a peer, the sender:
1. Derives a stealth address for the recipient (using their published scan/spend
   keys from a prior key exchange or out-of-band introduction).
2. Queries `DHT.findValue(SHA256(stealthAddress))`.
3. Decrypts the contact info to get the peer's network address.
4. Initiates a Noise IK handshake, which establishes session tokens.

#### 4.7.3 NodeId Separation

The DHT NodeId (`SHA-256(identity public key)`) is used for routing table
structure only.  It is NOT included in the wire envelope.  DHT RPCs use the
Noise transport layer, so they also benefit from session token routing.

### 4.8 Handshake Flow (Initial Contact)

For the very first message between two peers (no session exists):

1. Sender looks up recipient in DHT (Section 4.7.2) or via mDNS/PEX.
2. Sender initiates TCP connection + Noise IK / PQXDH handshake.
3. Handshake establishes: transport key, session tokens, envelope key.
4. Sender constructs type=2 (handshake) envelope with ephemeralR from PQXDH.
5. Recipient uses scan key to recognize the handshake envelope.
6. After handshake completes, all subsequent messages use session tokens for
   routing and envelope encryption for confidentiality.

For Dandelion++ stem routing of the initial handshake: the sender has a session
with their stem peer.  The handshake message travels through the stem as an
opaque encrypted inner envelope.  When it reaches the recipient (during fluff),
the recipient recognizes it via the stealth addressing fields.

## 5. Data Structures (Haskell)

### 5.1 New Envelope Type

```haskell
data Envelope = Envelope
    { envVersion    :: !Word8          -- protocol version (2)
    , envType       :: !Word8          -- message type
    , envSequence   :: !Word32         -- sequence number (BE)
    , envEphemeralR :: !ByteString     -- 32-byte ephemeral X25519 public key
    , envViewTag    :: !Word8          -- 1-byte view tag for fast scan
    , envPayload    :: !ByteString     -- encrypted payload (includes senderId)
    } deriving (Show, Eq)
```

### 5.2 Route Token State

```haskell
data RouteTokenState = RouteTokenState
    { rtsOutbound    :: !ByteString     -- 16-byte token we send
    , rtsInbound     :: !ByteString     -- 16-byte token we receive
    , rtsPrevInbound :: !(Maybe ByteString)  -- previous epoch's inbound (grace)
    , rtsEpoch       :: !Word64         -- current epoch counter
    }
```

### 5.3 Encrypted Payload Layout

```haskell
-- Inside the Double Ratchet encrypted payload:
data InnerPayload = InnerPayload
    { ipSenderId :: !ByteString    -- 32-byte sender identity hash
    , ipData     :: !ByteString    -- application data
    }

encodeInnerPayload :: ByteString -> ByteString -> ByteString
encodeInnerPayload senderId appData = senderId <> appData
-- senderId is always exactly 32 bytes, so no length prefix needed.

decodeInnerPayload :: ByteString -> Maybe (ByteString, ByteString)
decodeInnerPayload bs
    | BS.length bs < 32 = Nothing
    | otherwise = Just (BS.take 32 bs, BS.drop 32 bs)
```

## 6. Security Analysis

### 6.1 What Each Actor Learns

| Actor | Learns | Does NOT learn |
|---|---|---|
| Network observer | Encrypted blobs, session tokens (random) | Sender, recipient, message content |
| Stem relay | Session token for prev/next hop | Sender, recipient, content, final destination |
| Fluff peer | Session token, encrypted inner envelope | Sender, recipient (unless they are the recipient) |
| Recipient | Sender identity, message content | Nothing hidden from recipient |
| DHT nodes | Stealth-derived presence keys | Real identity hashes |

### 6.2 Correlation Risks

- **Session token correlation:** An observer watching a single link sees the
  same 16-byte token for the duration of an epoch (10 minutes).  This allows
  correlation of messages within an epoch on a single link.  Mitigation: tokens
  rotate each epoch; epoch length is configurable.
- **Traffic analysis:** Message timing and size are still observable.
  Mitigation: existing MessageFormat pads to fixed 1024-byte blocks (M21.2.1).
- **Long-term key compromise:** If a peer's scan key is compromised, an
  attacker can determine which stealth addresses belong to that peer.
  Mitigation: scan keys should be rotated periodically (future work).

### 6.3 Backward Compatibility

Protocol version bumps from 1 to 2.  Nodes running v1 and v2 cannot
interoperate on the wire format.  The version byte in the envelope header
(now inside the encrypted inner layer) distinguishes formats.  During rollout:

1. Nodes advertise supported versions in their DHT presence record.
2. Initiator selects the highest mutually supported version.
3. V1 is deprecated and removed in the release following v2 adoption.

## 7. Implementation TODO Items

### 7.1 WireFormat.hs Changes (M23.1.1a)

- [x] Bump `envVersion` to 2. (`envVersion = 2` in `Envelope`, `wrapEnvelope` sets it; `src/UmbraVox/Protocol/WireFormat.hs`)
- [x] Remove `envSourceId` and `envDestId` fields from `Envelope`. (not present in current `Envelope` type)
- [x] Add `envEphemeralR` (32 bytes) and `envViewTag` (1 byte) fields. (`Envelope` has both; `headerSize` = 45 with `envScanTag` also added)
- [x] Update `headerSize` from 74 to 43. (set to 45; note: final layout also includes 2-byte `envScanTag`, documented in comment)
- [x] Update `encodeEnvelope` to serialize new format. (`encodeEnvelope` in `WireFormat.hs`)
- [x] Update `decodeEnvelope` to parse new format. (`decodeEnvelope` in `WireFormat.hs`)
- [x] Update `wrapEnvelope` signature: remove srcId/dstId, add ephemeralR/viewTag. (current signature takes `msgType seqNum ephR vTag sTag payload`)
- [x] Add version 2 check in decode (accept v2 only, reject v1). (`| BS.index bs 0 /= 2 = Nothing` in `decodeEnvelope`)

### 7.2 Route Token Module (M23.1.1b)

- [x] Create `UmbraVox.Protocol.RouteToken` module. (`src/UmbraVox/Protocol/RouteToken.hs`)
- [x] `deriveRouteTokens :: ByteString -> ByteString` (transportKey -> 32 bytes). (exists; signature takes handshakeHash, transportKey, myIdHash, peerIdHash)
- [x] `deriveEpochTokens :: ByteString -> Word64 -> ByteString` (key + epoch). (exists as `deriveEpochTokens` in `RouteToken.hs`)
- [x] `RouteTokenState` data type with current + previous epoch tokens. (`data RouteTokenState` in `RouteToken.hs`)
- [x] `rotateTokens :: RouteTokenState -> Word64 -> RouteTokenState`. (exists in `RouteToken.hs`)
- [x] Token lookup map: `Map ByteString SessionId`. (`lookupSession`, `registerToken` use `Map ByteString SessionId` in `RouteToken.hs`)

### 7.3 Envelope Encryption (M23.1.1c)

- [x] Derive `envelopeKey` from transport key via HKDF. (`deriveEnvelopeKey` in `WireFormat.hs`)
- [x] Encrypt inner envelope with ChaCha20-Poly1305, nonce from sequence number. (`encodeEnvelopeAEAD` / `decodeEnvelopeAEAD` in `WireFormat.hs`)
- [x] Poly1305 tag replaces HMAC for encrypted envelopes. (AEAD path uses Poly1305 tag; HMAC path retained for handshake)
- [x] Handshake messages (type 2) remain HMAC-authenticated (no session key yet). (`encodeEnvelopeAEAD` delegates to `encodeEnvelope` for handshake type)

### 7.4 Payload Sender Identity (M23.1.1d)

- [x] Define `InnerPayload` type (senderId + applicationData). (`data InnerPayload` in `src/UmbraVox/Chat/Wire.hs`)
- [x] Update `sendChatMessage` to prepend senderId before ratchet encryption. (`sendChatMessage` calls `encodeInnerPayload senderId plaintext` in `Chat/Session.hs`)
- [x] Update `recvChatMessage` to extract senderId after ratchet decryption. (`recvChatMessage` calls `decodeInnerPayload pt` in `Chat/Session.hs`)
- [x] Validate senderId length (exactly 32 bytes) on decode. (`decodeInnerPayload` returns `Nothing` if `BS.length bs < senderIdSize` in `Chat/Wire.hs`)

### 7.5 Handshake Integration (M23.1.1e)

- [x] After PQXDH completes, derive route tokens from shared transport key. (`handshakeInitiator` / `handshakeResponder` in `Protocol/Handshake.hs` call `initChatSession` with shared secret)
- [x] Store `RouteTokenState` alongside `ChatSession`. (`csRouteTokens :: Maybe RouteTokenState` field in `data ChatSession`, `Chat/Session.hs`)
- [ ] Pass ephemeralR from PQXDH result into initial envelope. (not found in `Handshake.hs` — PQXDH result not wired to `wrapEnvelope` ephemeralR)
- [ ] Recipient scanning: check viewTag, then derive stealth scalar. (no scanning path found; `StealthAddress.hs` has scalar derivation but not wired to envelope scanning)

### 7.6 Dandelion.hs Changes (M23.1.1f)

- [x] `RouteDecision` carries `ByteString` (opaque inner envelope), not decoded `Envelope`. (`StemForward String ByteString` in `Network/Dandelion.hs`)
- [ ] `StemForward` includes outbound session token for the next hop. (`StemForward` is `String ByteString` only — no session token field)
- [ ] Add `replaceRouteToken :: ByteString -> ByteString -> ByteString` for relay forwarding. (function not present in `Dandelion.hs` or `RouteToken.hs`)

### 7.7 DHT Presence Integration (M23.1.1g)

- [x] Presence records use stealth-derived keys, not raw identity hashes. (`prStealthPubKey` derived via `derivePresenceKey` in `Network/Presence.hs`)
- [x] Presence lookup returns encrypted contact info. (`prContactInfo` field documented as "Encrypted: address + port + capabilities" in `Presence.hs`)
- [ ] Wire presence publication into `announcePresence` in DHT module. (`announcePresence` function not found; `Presence.hs` exports `createPresenceRecord` / `serializePresence` but no DHT wiring)

### 7.8 Test Updates (M23.1.1h)

- [x] Update all WireFormat round-trip tests for v2 format. (all round-trip tests verify `envVersion == 2`; `test/Test/Protocol/WireFormat.hs`)
- [x] Test: session token derivation determinism. (`testDeterminism` in `test/Test/Protocol/RouteToken.hs`)
- [x] Test: token rotation preserves old tokens for grace period. (`testGracePeriodAccepted` in `RouteToken.hs` test)
- [ ] Test: recipient can scan envelope via stealth address. (no such test found)
- [ ] Test: non-recipient rejects envelope (viewTag mismatch). (no such test found)
- [ ] Test: relay forwarding replaces token without touching inner envelope. (no such test; `replaceRouteToken` not implemented)
- [x] Test: senderId correctly extracted from decrypted payload. (`test/Test/Chat/Wire.hs` — `inner payload: senderId` check)
- [ ] Test: handshake messages use HMAC (not ChaCha20-Poly1305). (AEAD round-trip test is still a stub in `WireFormat.hs` test, line ~394)
- [x] Property test: random envelopes survive encode/decode round-trip. (`checkProperty "inner payload round-trip property (500 iterations)"` in `Chat/Wire.hs` test; WireFormat also has 500-iteration property test)

### 7.9 MitM Protections (M23.1.1j)

- [x] Key confirmation MAC: after handshake, both sides exchange
  `HMAC-SHA-256(tokenMaterial, "UmbraVox_TokenConfirm_" || role || handshakeHash)`
  where role is "initiator" or "responder".  Verification uses constant-time
  comparison.  Mismatch rejects the session (MitM detected).
  Implemented in `UmbraVox.Protocol.Handshake` (`keyConfirmMAC`,
  `verifyKeyConfirmation`).
- [x] Channel binding: `deriveRouteTokens` uses `handshakeHash` (Noise
  transcript hash) as HKDF salt, binding tokens to the full handshake
  transcript.  Documented in `UmbraVox.Protocol.RouteToken`.
- [x] Identity binding: `deriveRouteTokens` includes `myIdHash || peerIdHash`
  in the HKDF info string, binding tokens to both peers' long-term identity
  key hashes.  Documented in `UmbraVox.Protocol.RouteToken`.
- [x] Test: MitM with substituted keys produces mismatched confirmation MACs,
  session rejected.  See `Test.Protocol.RouteToken`.

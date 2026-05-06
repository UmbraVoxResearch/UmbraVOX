# Message Format

## Implementation Status

The wire format for chat messages is implemented in `Chat.Wire`. Protocol-level serialization helpers are in `Protocol.Encoding` and `Protocol.CBOR`. The full 1024-byte block format described in earlier design documents is not yet implemented -- the current wire format is a simplified ratchet-header + ciphertext + tag layout used by the Double Ratchet.

## Chat Wire Format (`Chat.Wire`)

On-the-wire message framing: header || ciphertext || GCM tag.

### Constants

```haskell
headerSize  = 40   -- bytes
tagSize     = 16   -- bytes (AES-256-GCM authentication tag)
minWireSize = 56   -- headerSize + tagSize (zero-length ciphertext)
```

### Header Layout (40 bytes)

| Offset | Size | Field |
|--------|------|-------|
| 0 | 32 | DH public key -- sender's current X25519 ratchet public key |
| 32 | 4 | prevChainN -- number of messages in previous sending chain (big-endian Word32) |
| 36 | 4 | msgN -- message number in current chain (big-endian Word32) |

The header corresponds to the `RatchetHeader` type from `Crypto.Signal.DoubleRatchet`:

```haskell
data RatchetHeader = RatchetHeader
    { rhDHPublic   :: !ByteString   -- 32 bytes
    , rhPrevChainN :: !Word32
    , rhMsgN       :: !Word32
    }
```

### Wire Message Layout

```
[  32 bytes: DH public key  ]
[   4 bytes: prevChainN     ]
[   4 bytes: msgN           ]
[   N bytes: ciphertext     ]   -- variable length, may be 0
[  16 bytes: GCM auth tag   ]
```

Total size: 56 + N bytes, where N is the ciphertext length.

### Encoding/Decoding API

- **`encodeWire :: RatchetHeader -> ByteString -> ByteString -> ByteString`** -- Serialize header, ciphertext, and GCM tag into wire bytes.
- **`decodeWire :: ByteString -> Maybe (RatchetHeader, ByteString, ByteString)`** -- Deserialize wire bytes into (header, ciphertext, tag). Returns `Nothing` if input is shorter than `minWireSize` or if the payload is shorter than `tagSize`.
- **`encodeHeader :: RatchetHeader -> ByteString`** -- Encode just the 40-byte header.

## Serialization Convention

All multi-byte integers are big-endian (network byte order).

### Protocol.Encoding

Shared encoding utilities in `Protocol.Encoding`:

| Function | Description |
|----------|-------------|
| `putWord32BE :: Word32 -> ByteString` | Encode Word32 as 4 big-endian bytes |
| `getWord32BE :: ByteString -> Word32` | Decode 4 big-endian bytes to Word32 (returns 0 if input < 4 bytes) |
| `putWord64BE :: Word64 -> ByteString` | Encode Word64 as 8 big-endian bytes |
| `splitOn :: Char -> String -> [String]` | Split a string on a delimiter character |
| `parseHostPort :: String -> (String, Maybe Int)` | Parse `"host:port"` into (host, Maybe port); defaults host to `"127.0.0.1"` if empty |
| `safeReadPort :: String -> Int` | Parse a port string, falling back to 7853 (first default port) on failure |
| `defaultPorts :: [Int]` | `[7853, 7854, 7855, 9999, 7856, 7857, 7858, 7859, 7860]` |

### Protocol.CBOR

Despite the module name, `Protocol.CBOR` implements **simple length-prefixed framing**, not actual CBOR encoding. Each message is framed as:

```
[  4 bytes: big-endian payload length (Word32)  ]
[  N bytes: payload                              ]
```

API:

- **`encodeMessage :: ByteString -> ByteString`** -- Prepend 4-byte big-endian length header to payload.
- **`decodeMessage :: ByteString -> Maybe (ByteString, ByteString)`** -- Decode a length-prefixed message. Returns `Just (payload, remaining)` on success, `Nothing` if input is too short.

This framing is used by the Noise handshake (`Network.Noise.Handshake.sendFrame`/`recvFrame`) for length-prefixed message exchange over transport, with a 64 KiB maximum frame size.

## Not Implemented

The following message format features from the design documents are not yet implemented in code:

- Full 1024-byte block layout (version, msg_type, sender_id, recipient_id, timestamp, sequence_num, etc.)
- Message types (TEXT, BINARY, KEY_EXCHANGE, RATCHET_REFRESH, ACK, CONTROL, DUMMY)
- Multi-block message reassembly
- PQ wrapper nonce/tag fields in the header
- HMAC-SHA256 trailer
- Random padding
- Transaction envelope (CBOR-encoded tx_header, tx_body, tx_witness)
- Actual CBOR serialization (the CBOR module uses length-prefixed framing only)

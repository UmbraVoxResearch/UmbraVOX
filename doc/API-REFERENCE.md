# UmbraVOX Chat API Reference

JSON-RPC 2.0 API for headless/programmatic access to UmbraVOX.

## Transport

- **Binding:** `127.0.0.1` only (local access)
- **Protocol:** TCP, one request per connection
- **Format:** newline-terminated JSON-RPC 2.0
- **Max request size:** 64 KiB
- **Authentication:** Bearer token via `auth` param (printed to stderr on startup as `API_TOKEN=<hex>`)

## Authentication

Every request must include an `"auth"` key in `params` with the bearer
token printed at server startup. Requests missing or failing auth receive
a `-32600 Unauthorized` error.

## Methods

### `sendMessage`

Send a message to a peer.

**Parameters:**

| Name   | Type   | Required | Description              |
|--------|--------|----------|--------------------------|
| `to`   | string | yes      | Recipient peer identifier |
| `body` | string | yes      | Message body text         |
| `auth` | string | yes      | Bearer token              |

**Returns:** `{"status": "sent"}`

**Example request:**
```json
{"jsonrpc":"2.0","method":"sendMessage","params":{"to":"alice","body":"hello","auth":"<token>"},"id":1}
```

**Example response:**
```json
{"jsonrpc":"2.0","result":{"status":"sent"},"id":1}
```

**Errors:**
- `-32602` — missing required params: `to`, `body`

---

### `getHistory`

Retrieve message history for a peer.

**Parameters:**

| Name    | Type   | Required | Description                      |
|---------|--------|----------|----------------------------------|
| `peer`  | string | yes      | Peer identifier to get history for |
| `limit` | string | no       | Maximum number of messages         |
| `auth`  | string | yes      | Bearer token                       |

**Returns:** `{"messages": [...]}`

**Example request:**
```json
{"jsonrpc":"2.0","method":"getHistory","params":{"peer":"alice","limit":"50","auth":"<token>"},"id":2}
```

**Example response:**
```json
{"jsonrpc":"2.0","result":{"messages":[]},"id":2}
```

**Errors:**
- `-32602` — missing required param: `peer`

---

### `listContacts`

List currently known contacts.

**Parameters:**

| Name   | Type   | Required | Description  |
|--------|--------|----------|--------------|
| `auth` | string | yes      | Bearer token |

**Returns:** `{"contacts": [...]}`

**Example request:**
```json
{"jsonrpc":"2.0","method":"listContacts","params":{"auth":"<token>"},"id":3}
```

**Example response:**
```json
{"jsonrpc":"2.0","result":{"contacts":[]},"id":3}
```

---

### `getStatus`

Get the current node status including port, display name, active
sessions, and connection mode.

**Parameters:**

| Name   | Type   | Required | Description  |
|--------|--------|----------|--------------|
| `auth` | string | yes      | Bearer token |

**Returns:**

| Field            | Type   | Description                        |
|------------------|--------|------------------------------------|
| `port`           | number | Listening port                     |
| `displayName`    | string | Current display name               |
| `sessions`       | number | Number of active sessions          |
| `connectionMode` | string | Current connection mode (e.g. Direct) |

**Example request:**
```json
{"jsonrpc":"2.0","method":"getStatus","params":{"auth":"<token>"},"id":4}
```

**Example response:**
```json
{"jsonrpc":"2.0","result":{"port":9160,"displayName":"node1","sessions":2,"connectionMode":"Direct"},"id":4}
```

---

### `connect`

Initiate a connection to a remote peer.

**Parameters:**

| Name   | Type   | Required | Description                   |
|--------|--------|----------|-------------------------------|
| `host` | string | yes      | Remote host address           |
| `port` | string | yes      | Remote port number            |
| `auth` | string | yes      | Bearer token                  |

**Returns:** `{"status": "connecting"}`

**Example request:**
```json
{"jsonrpc":"2.0","method":"connect","params":{"host":"192.0.2.1","port":"9160","auth":"<token>"},"id":5}
```

**Example response:**
```json
{"jsonrpc":"2.0","result":{"status":"connecting"},"id":5}
```

**Errors:**
- `-32602` — missing required params: `host`, `port`
- `-32602` — connection to private/loopback address rejected (RFC 1918, localhost, ::1, etc.)

---

### `disconnect`

Disconnect from a peer session.

**Parameters:**

| Name        | Type   | Required | Description           |
|-------------|--------|----------|-----------------------|
| `sessionId` | string | yes      | Session to disconnect |
| `auth`      | string | yes      | Bearer token          |

**Returns:** `{"status": "disconnected"}`

**Example request:**
```json
{"jsonrpc":"2.0","method":"disconnect","params":{"sessionId":"abc123","auth":"<token>"},"id":6}
```

**Example response:**
```json
{"jsonrpc":"2.0","result":{"status":"disconnected"},"id":6}
```

**Errors:**
- `-32602` — missing required param: `sessionId`

---

### `exchangePeers`

Trigger a peer exchange (PEX) round.

**Parameters:**

| Name   | Type   | Required | Description  |
|--------|--------|----------|--------------|
| `auth` | string | yes      | Bearer token |

**Returns:** `{"status": "ok", "exchanged": 0}`

**Example request:**
```json
{"jsonrpc":"2.0","method":"exchangePeers","params":{"auth":"<token>"},"id":7}
```

**Example response:**
```json
{"jsonrpc":"2.0","result":{"status":"ok","exchanged":0},"id":7}
```

**Errors:**
- `-32603` — PEX is disabled

---

### `dhtStatus`

Query the DHT (Distributed Hash Table) status.

**Parameters:**

| Name   | Type   | Required | Description  |
|--------|--------|----------|--------------|
| `auth` | string | yes      | Bearer token |

**Returns:**

| Field              | Type    | Description                      |
|--------------------|---------|----------------------------------|
| `enabled`          | boolean | Whether DHT is enabled           |
| `routingTableSize` | number  | Number of entries in routing table (when enabled) |
| `storedValues`     | number  | Number of stored DHT values (when enabled)       |

**Example request:**
```json
{"jsonrpc":"2.0","method":"dhtStatus","params":{"auth":"<token>"},"id":8}
```

**Example response (enabled):**
```json
{"jsonrpc":"2.0","result":{"enabled":true,"routingTableSize":0,"storedValues":0},"id":8}
```

**Example response (disabled):**
```json
{"jsonrpc":"2.0","result":{"enabled":false},"id":8}
```

---

## Error Codes

| Code     | Meaning                                          |
|----------|--------------------------------------------------|
| `-32700` | Parse error (malformed JSON or exceeds 64 KiB)   |
| `-32600` | Invalid request (bad jsonrpc version, invalid id, unauthorized) |
| `-32601` | Method not found                                  |
| `-32602` | Invalid params (missing required fields, private address) |
| `-32603` | Internal error (e.g. PEX disabled)                |

## ID Validation

The `id` field must be a JSON number, a JSON string, or `null`.
Booleans, arrays, and objects are rejected with `-32600`.

## Source

Defined in `src/UmbraVox/Chat/API.hs`. See also `doc/spec/chat.md`.

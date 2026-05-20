UmbraVOX Bridge Plugin Authoring Guide
======================================

What is a bridge plugin?
------------------------

A bridge plugin is an IPC subprocess that connects UmbraVOX to an external
messaging system (Signal, IRC, XMPP, Facebook, etc.). The host process
launches the bridge as a child process and communicates over stdin/stdout
using a line-based text protocol.

Bridge plugins:
- Run as separate OS processes (not linked into the main binary)
- Speak a fixed set of verbs over stdin/stdout
- Use `api=uvx-provider-v1` and `class=closed-bridge` (or `open-bridge`)
- Are registered via a `manifest.uvx` file in `plugins/<bridge-id>/`


IPC Protocol
------------

The host sends commands on the plugin's stdin (one per line). The plugin
replies on stdout (one per line). Stderr is reserved for debug logging.

### Host -> Plugin (commands)

| Verb       | Syntax               | Description                            |
|------------|----------------------|----------------------------------------|
| AUTH       | `AUTH <arg>`         | Authenticate / attach to remote session|
| SEND       | `SEND <hex-data>`    | Send a message (hex-encoded JSON envelope) |
| RECV       | `RECV`               | Request next available inbound message |
| CONTACTS   | `CONTACTS`           | List contacts on the external system   |
| STATUS     | `STATUS`             | Query bridge connection/session state  |
| PING       | `PING`               | Liveness check                         |
| CLOSE      | `CLOSE`              | Graceful shutdown                      |

### Plugin -> Host (responses)

| Response     | Syntax                  | Description                          |
|--------------|-------------------------|--------------------------------------|
| AUTH_OK      | `AUTH_OK`               | Authentication succeeded             |
| OK           | `OK` or `OK <hex>`      | Generic success (optional payload)   |
| DATA         | `DATA <hex-data>`       | Inbound message (response to RECV)   |
| CONTACTS     | `CONTACTS <hex>`        | Contact list (hex-encoded JSON array)|
| STATUS       | `STATUS <hex>`          | Status payload (hex-encoded JSON)    |
| PONG         | `PONG`                  | Liveness reply                       |
| ERR          | `ERR <message>`         | Error (human-readable message)       |

All binary/structured payloads are hex-encoded. JSON envelopes are
serialized to UTF-8, then hex-encoded for transport over the text protocol.


JSON Envelope Format
--------------------

SEND and DATA payloads carry a JSON envelope (hex-encoded on the wire).
The canonical schema is in `envelope-schema.json`. Example:

```json
{
  "to": "+15551234567",
  "body": "Hello from UmbraVOX",
  "timestamp": 1716000000,
  "thread": null
}
```

Required fields: `to`, `body`. Optional: `timestamp`, `thread`.


Manifest Fields for a Bridge
-----------------------------

Bridge manifests live in `plugins/<bridge-id>/manifest.uvx` and must
include all 12 standard fields. Key values for bridges:

| Field        | Bridge value                                             |
|--------------|----------------------------------------------------------|
| api          | `uvx-provider-v1`                                        |
| class        | `closed-bridge` (E2EE) or `open-bridge` (cleartext)     |
| capabilities | Must include `connect,bridge`                            |
| host         | `exec-direct`                                            |
| entrypoint   | `exec:<bridge-binary-name>`                              |
| encryption   | `bridge-adapted` (uses UmbraVOX crypto with bridge params) |

See `plugins/signal-bridge/manifest.uvx` for a live example.


How to Create a New Bridge
--------------------------

1. **Copy the template:**

       cp -r plugins/template-bridge plugins/my-bridge

2. **Edit `manifest.uvx`** -- set `id`, `name`, `endpoint`, `entrypoint`,
   and `notes` to match your target system.

3. **Implement the bridge binary** -- use `Main.hs` as a starting point.
   Replace the stub handlers with real logic:
   - `handleAuth`: connect to the external service
   - `handleSend`: encrypt and forward the message
   - `handleRecv`: poll/receive inbound messages
   - `handleContacts`: query the external contact list
   - `handleStatus`: report connection state

4. **Compile:**

       ghc -O2 Main.hs -o my-bridge-binary

5. **Test manually** by piping commands:

       echo -e "PING\nSTATUS\nCLOSE" | ./my-bridge-binary

6. **Register** -- the provider catalog auto-discovers manifests at startup
   from `plugins/*/manifest.uvx`.


Reference Implementation
-------------------------

The Signal bridge (`src/UmbraVox/Bridge/Signal/Main.hs`) is the first
complete bridge implementation. It demonstrates:
- Session management via IORef
- Hex encoding/decoding for the wire protocol
- Minimal JSON parsing for bridge envelopes
- Integration with UmbraVOX's Signal-compatible crypto

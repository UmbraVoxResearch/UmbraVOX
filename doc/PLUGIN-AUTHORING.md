# Plugin Authoring Guide

This document describes how to create bridge plugins for UmbraVOX.
A bridge plugin adapts an external messaging platform (Signal, UmbraClaw,
etc.) to UmbraVOX's encrypted transport layer.

## 1. Plugin Manifest Format

Every plugin lives in `plugins/<plugin-id>/` and must contain a
`manifest.uvx` file.  The manifest uses a simple `key=value` format
(one field per line, no quoting, `#` comments allowed).

### Required Fields

| Field          | Description                                         | Example                          |
|----------------|-----------------------------------------------------|----------------------------------|
| `id`           | Stable lowercase identifier (must match directory)  | `signal-bridge`                  |
| `api`          | API contract version                                | `uvx-provider-v1`                |
| `name`         | Human-readable display name                         | `Signal Bridge`                  |
| `class`        | Provider class                                      | `closed-bridge`                  |
| `status`       | Development status                                  | `development`                    |
| `endpoint`     | Endpoint address schema hint                        | `e164-or-username`               |
| `inherits`     | Comma-separated parent provider IDs (or empty)      | `signal-server`                  |
| `capabilities` | Comma-separated capability tags                     | `connect,bridge`                 |
| `host`         | How the host launches the plugin                    | `exec-direct`                    |
| `entrypoint`   | Launch target                                       | `exec:umbravox-signal-bridge`    |
| `encryption`   | Encryption mode                                     | `bridge-adapted`                 |
| `notes`        | Free-form description                               | `Signal bridge using UmbraVOX...`|

### Provider Classes

- `direct-carrier` -- built-in transport (TCP, UDP)
- `overlay-carrier` -- network overlay (Tor, WireGuard)
- `open-bridge` -- federated/open protocol bridge (IRC, XMPP)
- `closed-bridge` -- proprietary platform bridge (Signal, WhatsApp)

### Host Types

- `exec-direct` -- host launches the plugin executable directly
- `ipc-stdio` -- host communicates via stdin/stdout IPC
- `in-process` -- host loads the module in-process
- `manifest-only` -- metadata only, no runtime artifact

### Entrypoint Prefixes

- `exec:<executable-name>` -- Cabal-built executable
- `ipc:<path>` -- IPC binary path (relative to manifest directory)
- `module:<Module.Name>` -- in-process Haskell module

### Example Manifests

**Signal Bridge** (`plugins/signal-bridge/manifest.uvx`):

```
id=signal-bridge
api=uvx-provider-v1
name=Signal Bridge
class=closed-bridge
status=development
endpoint=e164-or-username
inherits=signal-server
capabilities=connect,bridge
host=exec-direct
entrypoint=exec:umbravox-signal-bridge
encryption=bridge-adapted
notes=Signal bridge using UmbraVOX crypto with Signal-compatible parameters.
```

**UmbraClaw** (`plugins/umbraclaw/manifest.uvx`):

```
id=umbraclaw
api=uvx-provider-v1
name=UmbraClaw Bridge
class=closed-bridge
status=development
endpoint=umbraclaw-id
inherits=
capabilities=connect,bridge
host=exec-direct
entrypoint=exec:umbravox-umbraclaw
encryption=bridge-adapted
notes=UmbraClaw bridge using UmbraVOX crypto with UmbraClaw-compatible parameters.
```

## 2. IPC Protocol

Bridge plugins communicate with the UmbraVOX host via a line-based
text protocol over stdin/stdout.  Each message is a single line
terminated by a newline character.

### Host -> Plugin Commands

| Command           | Description                              |
|-------------------|------------------------------------------|
| `AUTH <hex-creds>` | Authenticate / attach to a session       |
| `SEND <hex-data>`  | Send a message through the bridge        |
| `RECV`             | Request the next available inbound message |
| `CONTACTS`         | List bridge contacts                     |
| `STATUS`           | Query bridge connection/auth status      |
| `PING`             | Liveness check                           |
| `CLOSE`            | Shut down the plugin                     |

### Plugin -> Host Responses

| Response              | Description                              |
|-----------------------|------------------------------------------|
| `AUTH_OK`              | Authentication succeeded                 |
| `AUTH_FAIL <message>`  | Authentication failed with reason        |
| `DATA <hex-data>`      | Message data (response to RECV)          |
| `CONTACTS <hex-json>`  | Contact list as hex-encoded JSON         |
| `STATUS <hex-json>`    | Status payload as hex-encoded JSON       |
| `PONG`                 | Liveness reply (response to PING)        |
| `OK`                   | Generic acknowledgement                  |
| `ERR <message>`        | Error with human-readable description    |

### Data Encoding

All binary data in the IPC protocol is hex-encoded (lowercase).
Structured payloads (contacts, status) are JSON objects that are then
hex-encoded for transport over the text protocol.

### AUTH Credentials

The `AUTH` command accepts optional hex-encoded credentials in
`username:token` format.  If credentials are empty or omitted, the
plugin may use default/stub credentials for development.

### Status JSON Schema

```json
{
  "connected": true,
  "session": true,
  "auth": "complete"
}
```

The `auth` field values are: `none`, `pending`, `complete`,
`failed:<reason>`.

### Contacts JSON Schema

```json
[
  {"id": "<hex-id>", "name": "Alice", "status": "online"},
  {"id": "<hex-id>", "name": "Bob", "status": "offline"}
]
```

## 3. Bridge Module Structure

Each bridge plugin follows the **Main.hs + Session.hs** pattern:

### Session Module (`src/UmbraVox/Bridge/<Plugin>/Session.hs`)

The Session module manages connection state, authentication, and
message queues.  It is a pure-IO module with no IPC concerns.

Required exports:

- **Session type** -- holds mutable refs for connection state, auth
  state, message queues, and contacts
- **`initSession`** -- create a new session (disconnected state)
- **`authenticate`** -- perform authentication with credentials
- **`syncContacts`** -- retrieve/sync the contact list
- **`enqueueSend`** -- queue an outbound message
- **`dequeueRecv`** -- dequeue the next inbound message

Example from UmbraClaw (`UmbraVox.Bridge.UmbraClaw.Session`):

```haskell
data UmbraClawSession = UmbraClawSession
    { ucsHost       :: !String
    , ucsPort       :: !Int
    , ucsState      :: !(IORef SessionState)
    , ucsReady      :: !(IORef Bool)
    , ucsAuth       :: !(IORef AuthState)
    , ucsContacts   :: !(IORef [UmbraClawContact])
    , ucsSendQueue  :: !(IORef [ByteString])
    , ucsRecvQueue  :: !(IORef [ByteString])
    }

initSession :: String -> Int -> IO UmbraClawSession
authenticate :: UmbraClawSession -> ByteString -> ByteString -> IO AuthState
syncContacts :: UmbraClawSession -> IO [UmbraClawContact]
enqueueSend  :: UmbraClawSession -> ByteString -> IO ()
dequeueRecv  :: UmbraClawSession -> IO (Maybe ByteString)
```

### Main Module (`src/UmbraVox/Bridge/<Plugin>/Main.hs`)

The Main module implements the IPC command dispatch loop.  It:

1. Sets up line-buffered stdin/stdout/stderr
2. Creates a session ref (`IORef (Maybe Session)`)
3. Enters the dispatch loop reading commands from stdin
4. Routes each command to a handler that interacts with the Session

Required exports:

- **`<plugin>Bridge`** -- entry point for the IPC subprocess
- **`dispatch`** -- command dispatch (exported for testing)
- **`bytesToHex` / `hexToBytes`** -- hex encoding utilities

The dispatch loop pattern:

```haskell
bridgeLoop :: IORef (Maybe Session) -> IO ()
bridgeLoop sessionRef = do
    eof <- isEOF
    if eof then pure ()
    else do
        line <- getLine
        dispatch sessionRef line
        hFlush stdout
        bridgeLoop sessionRef

dispatch :: IORef (Maybe Session) -> String -> IO ()
dispatch sessionRef line =
    case words line of
        ("AUTH"     : rest)  -> handleAuth sessionRef (unwords rest)
        ("SEND"     : rest)  -> handleSend sessionRef (unwords rest)
        ("RECV"     : _)     -> handleRecv sessionRef
        ("CONTACTS" : _)     -> handleContacts sessionRef
        ("STATUS"   : _)     -> handleStatus sessionRef
        ("PING"     : _)     -> handlePing
        ("CLOSE"    : _)     -> handleClose
        []                   -> pure ()
        (cmd        : _)     -> respond ("ERR unknown command: " ++ cmd)
```

### Executable Wrapper (`app-<plugin>/Main.hs`)

A thin wrapper that calls the bridge entry point:

```haskell
module Main (main) where
import UmbraVox.Bridge.<Plugin>.Main (<plugin>Bridge)
main :: IO ()
main = <plugin>Bridge
```

## 4. Registration in ProviderCatalog.hs

To make the host aware of a new bridge plugin, add it to
`src/UmbraVox/Network/ProviderCatalog.hs`:

### Step 1: Add a Constructor to `TransportProviderId`

```haskell
data TransportProviderId
    = ProviderTCP
    | ...
    | ProviderMyPlugin       -- add here
    deriving stock (Eq, Ord, Show, Enum, Bounded)
```

### Step 2: Add a Descriptor Case

In the `providerDescriptor` function, add a case clause:

```haskell
ProviderMyPlugin ->
    provider pid "my-plugin" "My Plugin"
        "Description of the plugin."
        ProviderClosedBridge
```

The `stableId` (`"my-plugin"`) must match the `id` field in
`manifest.uvx` and the plugin directory name under `plugins/`.

### Step 3: Add Endpoint Schema

In `providerEndpointSchema`, add:

```haskell
ProviderMyPlugin -> "host:port"
```

### Step 4: Add Endpoint Rendering and Parsing

In `renderProviderEndpoint` and `providerEndpointParser`, add cases
matching the endpoint schema.

## 5. Cabal Executable Stanza

Add an executable stanza to `UmbraVox.cabal`:

```cabal
executable umbravox-my-plugin
  import:           warnings, lang
  main-is:          Main.hs
  hs-source-dirs:   app-my-plugin
  build-depends:
    base         >= 4.18  && < 5,
    UmbraVox
  ghc-options:      -threaded
```

The executable name must match the `entrypoint` value in the manifest
(after the `exec:` prefix).

## 6. Checklist for New Plugins

1. Create `plugins/<plugin-id>/manifest.uvx` with all required fields
2. Implement `src/UmbraVox/Bridge/<Plugin>/Session.hs`
3. Implement `src/UmbraVox/Bridge/<Plugin>/Main.hs`
4. Create `app-<plugin>/Main.hs` executable wrapper
5. Add `TransportProviderId` constructor to `ProviderCatalog.hs`
6. Add descriptor, endpoint schema, render, and parse cases
7. Add `executable` stanza to `UmbraVox.cabal`
8. Add IPC smoke test in `scripts/test-<plugin>-bridge-ipc.sh`
9. Run `./uv help` to verify build system wiring

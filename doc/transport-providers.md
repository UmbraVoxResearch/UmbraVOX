# Transport Providers

## Overview

Transport providers are pluggable communication backends that sit below the
UmbraVOX secure session layer. Each provider adapts a specific network
protocol or messaging service into a uniform `TransportHandle` interface so
the rest of the stack (encryption, framing, chat) never depends on the
underlying wire format.

The provider system is defined across three layers:

- **Interface** -- `src/UmbraVox/Network/TransportClass.hs` declares the
  `TransportHandle` typeclass that every transport must implement.
- **Catalog** -- `src/UmbraVox/Network/ProviderCatalog.hs` holds the typed
  registry of all 13 declared providers, manifest parsing, and endpoint
  schemas.
- **Runtime** -- `src/UmbraVox/Network/ProviderRuntime.hs` wires the active
  provider into listen/connect/accept operations at runtime.

Each provider ships a `manifest.uvx` file under `plugins/<id>/manifest.uvx`.

## Available Providers

| ID              | Name           | Class            | Status    | Endpoint Schema          |
|-----------------|----------------|------------------|-----------|--------------------------|
| tcp             | TCP            | direct-carrier   | active    | host:port                |
| udp             | UDP            | direct-carrier   | stub      | host:port                |
| tor             | Tor            | overlay-carrier  | planned   | onion:port               |
| wireguard       | WireGuard      | overlay-carrier  | planned   | peer@host:port           |
| irc             | IRC            | open-bridge      | planned   | nick@server:port/#chan   |
| aim             | AIM            | closed-bridge    | planned   | screenname@server        |
| xmpp            | XMPP           | open-bridge      | planned   | jid/resource             |
| mastodon        | Mastodon       | open-bridge      | planned   | @user@instance           |
| facebook        | Facebook       | closed-bridge    | planned   | account-id               |
| instagram       | Instagram      | closed-bridge    | planned   | account-id               |
| whatsapp        | WhatsApp       | closed-bridge    | planned   | e164-or-handle           |
| signal          | Signal         | closed-bridge    | planned   | e164-or-username         |
| signal-server   | Signal Server  | closed-bridge    | planned   | server:port/account      |

Provider classes:

- **direct-carrier** -- Raw network protocol (TCP, UDP).
- **overlay-carrier** -- Wraps a carrier with an additional network layer (Tor, WireGuard).
- **open-bridge** -- Adapts an open/federated messaging protocol (IRC, XMPP, Mastodon).
- **closed-bridge** -- Adapts a proprietary messaging service (Signal, WhatsApp, etc.).

## TransportHandle Interface

Every transport implements the four-method typeclass defined in
`src/UmbraVox/Network/TransportClass.hs`:

```haskell
class TransportHandle t where
    thSend  :: t -> ByteString -> IO ()
    thRecv  :: t -> Int -> IO ByteString
    thClose :: t -> IO ()
    thInfo  :: t -> String
```

- `thSend` -- Write a message to the transport. Must not return until the
  data is handed off.
- `thRecv` -- Read exactly `n` bytes. Block until enough data is available
  or the transport reaches EOF.
- `thClose` -- Tear down the transport. Idempotent.
- `thInfo` -- Return a human-readable label (e.g. `"loopback:test/A"`).

The existential wrapper `AnyTransport` allows polymorphic use:

```haskell
data AnyTransport = forall t. TransportHandle t => AnyTransport t
```

## Writing a Provider

### 1. Implement TransportHandle

Create a module under `src/UmbraVox/Network/Transport/`. See
`src/UmbraVox/Network/Transport/Loopback.hs` for a minimal reference
implementation. The key requirements:

- Define a data type holding your connection state.
- Write a `TransportHandle` instance with all four methods.
- Export a constructor or connect/accept pair.

```haskell
data MyTransport = MyTransport { ... }

instance TransportHandle MyTransport where
    thSend t bs = ...
    thRecv t n  = ...
    thClose t   = ...
    thInfo  t   = "my-transport:..."
```

### 2. Create a manifest file

Place a `manifest.uvx` at `plugins/<id>/manifest.uvx`. See the Manifest
Format section below.

### 3. Register in ProviderCatalog

Add a constructor to `TransportProviderId` in
`src/UmbraVox/Network/ProviderCatalog.hs`, then add a case to
`providerDescriptor` and `providerEndpointSchema`. The `Enum` and `Bounded`
derivations auto-include it in `transportProviderRegistry`.

### 4. Wire into ProviderRuntime

Add cases to `bindListenerWithProvider`, `connectWithProvider`, and
`connectWithProviderTryPortsProgress` in
`src/UmbraVox/Network/ProviderRuntime.hs` so the runtime can dispatch to
your transport.

### 5. Test

The Loopback transport is the easiest way to exercise the `TransportHandle`
contract in isolation. For network transports, test against localhost with
the existing connect/listen patterns in `ProviderRuntime`.

## Manifest Format

Manifest files use a line-oriented `key=value` format. Comments start with
`#`. Blank lines are ignored.

Required fields (all 12 must be present):

```
id=tcp
api=uvx-provider-v1
name=TCP
class=direct-carrier
status=active
endpoint=host:port
inherits=
capabilities=connect,listen,discovery
host=in-process
entrypoint=module:UmbraVox.Network.Transport
encryption=core-e2ee
notes=Built-in direct stream carrier used by the current MVP.
```

Field reference:

| Field          | Values / Format                                           |
|----------------|-----------------------------------------------------------|
| id             | Stable lowercase identifier matching `TransportProviderId`|
| api            | Must be `uvx-provider-v1` for the loader to accept it     |
| name           | Human-readable display name                               |
| class          | `direct-carrier`, `overlay-carrier`, `open-bridge`, `closed-bridge` |
| status         | `active`, `stub`, or `planned`                            |
| endpoint       | Endpoint address schema string                            |
| inherits       | Comma-separated list of parent provider IDs (or empty)    |
| capabilities   | Comma-separated: `connect`, `listen`, `discovery`, `group`, `media`, `bridge`, `tunnel` |
| host           | Host mode (see Plugin Host Modes below)                   |
| entrypoint     | Launch target (see Plugin Host Modes below)               |
| encryption     | Encryption posture tag                                    |
| notes          | Freeform description                                      |

## Plugin Host Modes

The `host` and `entrypoint` fields together determine how a provider is
loaded. These modes are shared with the general plugin system
(`src/UmbraVox/BuildProfile.hs`).

| Host Mode          | Entrypoint Format                | Description                              |
|--------------------|----------------------------------|------------------------------------------|
| `manifest-only`    | `manifest-only`                  | Metadata placeholder, no runtime code    |
| `ipc-stdio`        | `ipc:<path>` or `exec:<path>`    | Launch a subprocess, communicate via stdin/stdout |
| `exec-direct`      | `exec:<path>`                    | Launch an executable that handles its own I/O |
| `in-process`       | `module:<Module.Name>`           | Linked Haskell module, loaded in-process |

Entrypoint paths are resolved relative to the manifest file's directory
unless they are absolute.

## Provider Lifecycle

1. **Discovery** -- `loadTransportProviderCatalog` scans `plugins/*/manifest.uvx`
   on disk at startup.
2. **Parsing** -- Each manifest is parsed into a `ProviderManifest` record
   via `loadManifestFile` / `providerManifestFromFields`. All 12 fields must
   be present or the manifest is skipped.
3. **Validation** -- The `api` field is checked against `uvx-provider-v1`.
   Unsupported API versions produce `ProviderLoadUnsupportedApi`.
4. **Launch spec resolution** -- `providerManifestLaunchSpec` combines host +
   entrypoint into a `ProviderLaunchSpec`. Invalid combinations yield
   `ProviderLaunchInvalid`.
5. **Artifact check** -- For IPC and executable modes, the loader checks
   whether the referenced binary exists on disk
   (`ProviderLoadReady` vs `ProviderLoadMissingArtifact`).
6. **Activation** -- `ProviderRuntime.activeRuntimeProvider` currently
   hard-codes `ProviderTCP`. Connect/listen calls dispatch through a
   pattern match on `TransportProviderId`.

The full runtime catalog is available via `loadTransportProviderRuntimeCatalog`,
which returns `CachedTransportProvider` records with resolved launch specs,
load status, inherited provider chains, and capability lists.

## IPC Provider Protocol

Providers hosted via `ipc-stdio` communicate with the UmbraVOX runtime
through a line-based protocol on stdin/stdout.  The runtime spawns the
provider executable specified in the manifest `entrypoint` field and
exchanges newline-terminated commands.

### Host to Provider (stdin)

| Command                    | Description                              |
|----------------------------|------------------------------------------|
| `SEND <hex-encoded-data>`  | Send data through the transport           |
| `RECV`                     | Request next available data               |
| `INFO`                     | Request endpoint info string              |
| `CLOSE`                    | Tear down the transport                   |

### Provider to Host (stdout)

| Response                   | Description                              |
|----------------------------|------------------------------------------|
| `DATA <hex-encoded-data>`  | Response to `RECV` with received data     |
| `OK`                       | Acknowledgement for `SEND` / `CLOSE`      |
| `INFO <label>`             | Response to `INFO` with endpoint label    |
| `ERR <message>`            | Error response                            |

Data payloads are hex-encoded (lowercase, no prefix).  The provider
must flush stdout after every response line.  The runtime wraps the
spawned process in an `IPCTransport` that implements `TransportHandle`,
so the rest of the stack (encryption, framing, chat) treats it
identically to a built-in transport.

Implementation: `src/UmbraVox/Network/ProviderRuntime.hs` exports
`startIPCProvider`, `IPCTransport`, and low-level helpers
`ipcSendCommand` / `ipcRecvResponse` / `ipcClose`.

## Hot-Reload

Provider manifests can be re-scanned at runtime without restarting the
application.  Call `reloadProviders` (exported from
`src/UmbraVox/Network/ProviderRuntime.hs`) to re-run the full
discovery and loading pipeline.  The function returns the refreshed
`[CachedTransportProvider]` catalog; the caller writes it into the
application's mutable state.

Active connections are not affected by a reload.  Only new connections
will use updated provider definitions.

The TUI already triggers a reload when opening the Settings dialog
(via `refreshTransportProviderCatalog` in `UmbraVox.App.Startup`).

## Current Limitations

- **Single active provider** -- `activeRuntimeProvider` is hard-coded to TCP.
  There is no runtime provider selection yet.
- **IPC hosting is minimal** -- The `ipc-stdio` host mode has a working
  `TransportHandle` implementation and process launcher
  (`startIPCProvider`), but `exec-direct` remains unimplemented.  No
  reference provider executable ships with the repository yet.
- **UDP is a stub** -- `src/UmbraVox/Network/Transport/UDP.hs` exports an
  empty module. The datagram transport is reserved but not yet functional.
- **Bridge providers are metadata-only** -- All bridge providers (IRC, AIM,
  XMPP, Mastodon, Facebook, Instagram, WhatsApp, Signal, Signal Server)
  exist only as manifest files and registry entries. No transport code
  backs them.
- **No capability enforcement** -- Declared capabilities are parsed and
  stored but not checked at connect/listen time.
- **No provider negotiation** -- Peers cannot negotiate which transport to
  use. Both sides must be configured for the same provider out of band.

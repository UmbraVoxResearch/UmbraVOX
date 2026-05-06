# UmbraVOX Quickstart

## Prerequisites

- [Nix](https://nixos.org/download.html) package manager (single-user or multi-user install)

All other dependencies (GHC, Cabal, C toolchain, F*, Z3) are provided by the Nix shell.

## Build & Run

```bash
nix-shell              # Enter development environment
cabal build            # Build everything
cabal run umbravox     # Launch the TUI
make test              # Fast messaging-MVP hardening gate
make soak              # Longer soak/stress run with artifact report
make                   # Build + test + verify + lint (full pipeline)
```

Additional targets:

```bash
make test-core         # Deterministic crypto/protocol/TUI/codegen suite
make test-tcp          # Real TCP hardening suite
make test-fault        # Fault-injection hardening suite
make test-recovery     # Persistence and restart-oriented suite
make test-deferred     # Preserved deferred blockchain/economics suite
make verify            # Run F* formal verification (requires F* and Z3)
make complexity        # Check cyclomatic complexity
make quality           # Run all quality gates
```

`make test` covers the active messaging MVP. `make test-deferred` keeps the preserved blockchain, consensus, economics, and related stub suites visible without moving them back into the required MVP gate.

## First Launch

1. UmbraVOX starts with a **Welcome dialog**. Press **Esc** to dismiss it.
2. The node **auto-listens** on port 7853 (or the next available port) immediately at boot. Incoming connections are accepted in the background according to the active trust mode.
3. The TUI shows two panes: **Contacts** (left) and **Chat** (right).
4. Press **Tab** to switch between panes.
5. The menu bar across the top responds to **F1-F5**.

## Connecting Two Peers

You need two terminals (or two machines on the same network).

> **Note:** UmbraVOX auto-listens on startup. Each node automatically binds to
> port 7853 (or the next available port in the default sequence: 7853, 7854,
> 7855, ...) as soon as it launches. There is no need to manually type "listen".

### Peer A

1. Launch UmbraVOX: `cabal run umbravox`
2. Note the port shown at startup (default 7853)

### Peer B

1. Launch UmbraVOX: `cabal run umbravox`
2. Press **F2** (Contacts) -> **New**
3. Select **Single** connection type
4. Type Peer A's address, e.g. `192.168.1.10:7853`, and press Enter
5. The Noise_IK handshake runs automatically

Once connected, type a message in the chat pane and press **Enter** to send. Messages are encrypted through the full pipeline: Signal Double Ratchet -> PQ Wrapper -> Noise_IK -> TCP.

## Connection Trust Modes

UmbraVOX has five trust modes controlling how the node handles incoming connections, discovery, and persistence. Since the node auto-listens on boot, the trust mode determines what happens when a remote peer connects.

| Mode | Accept | mDNS | PEX | DB | Behavior |
|------|--------|------|-----|-----|----------|
| **Swing** | All | On | Auto | On | Most open -- shares peer lists automatically |
| **Promiscuous** | All | On | Manual | On | Accept anyone silently |
| **Selective** | Confirm | On | Off | On | Shows fingerprint, user decides (default) |
| **Chaste** | Trusted only | Off | Off | On | Silent reject indistinguishable from MAC failure |
| **Chastity** | Trusted only | Off | Off | Off | Chaste + no persistence (fully ephemeral) |

### Changing the Trust Mode

1. Press **F4** (Prefs) -> **Settings**
2. Navigate to the connection mode setting
3. Select the desired mode
4. Press **Esc** to close the dialog

The change takes effect immediately for new connections. Existing connections are not affected.

### Adding Trusted Keys

For **Chaste** and **Chastity** modes, you must add peer public keys to the trusted list before they can connect.

1. Press **F4** (Prefs) -> **Keys**
2. Add the peer's public key (hex-encoded Ed25519/X25519 identity key)
3. The key is added to `cfgTrustedKeys` and (in Chaste mode) persisted to Anthony DB

To obtain a peer's public key: the peer can find it in **F4** -> **Keys** in their own TUI, or via an identity export (**F1** -> **Export**).

## Keyboard Reference

| Key | Action |
|-----|--------|
| F1-F5 | Open menus (File, Contacts, Chat, Prefs, Help) |
| Tab | Switch pane (Contacts <-> Chat) |
| Ctrl+N | Quick new connection |
| Ctrl+Q | Quit |
| Up/Down | Navigate / scroll |
| Enter | Send message / select |
| Esc | Close dialog / menu |

## Troubleshooting

### UTF-8 Encoding Errors

UmbraVOX uses Unicode characters for status indicators and UI elements. If you see garbled output:

```bash
# Check your locale
locale

# If not UTF-8, set it before launching
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
cabal run umbravox
```

### Port Conflicts

If port 7853 is already in use, UmbraVOX tries the next port in the default sequence: 7853, 7854, 7855, 9999, 7856, 7857, 7858, 7859, 7860.

To check what is using a port:

```bash
ss -tlnp | grep 7853
```

### mDNS Not Discovering Peers

mDNS discovery requires:

- Both peers on the same LAN subnet
- UDP multicast enabled on the network (some corporate/guest Wi-Fi blocks this)
- Port 5353/UDP open (standard mDNS port)
- Connection mode set to Swing, Promiscuous, or Selective (Chaste and Chastity disable mDNS)

To verify mDNS is working:

```bash
# Check if the mDNS multicast group is joined
ss -umnp | grep 5353
```

If mDNS is unavailable, connect directly by typing `host:port` instead of relying on automatic discovery.

### Terminal Too Small

UmbraVOX requires a minimum terminal size to render properly. If the UI looks broken, resize your terminal to at least 80 columns by 24 rows. The TUI handles SIGWINCH and will re-render on resize.

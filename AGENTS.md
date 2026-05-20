# Agent Development Guide

Instructions for AI agents working on UmbraVOX. All build, test, and
verification MUST happen inside NixOS VMs. The host system's nix store
should never be touched by compilation.

## VM-First Development

**Rule: Never compile on the host.** Use VMs for all:
- Haskell builds (`cabal build`, `cabal test`)
- C compilation (generated crypto code)
- F* verification (`fstar.exe`)
- Coq proofs (`coqc`)
- Signal-Server (Maven/Java)

### Quick Reference

```bash
make vm-build-only       # Build in VM (host needs only qemu + nix)
make vm-dev              # Interactive dev shell in VM (serial)
make vm-run-gui          # Interactive dev shell (QEMU GTK window)
make vm-test             # Run tests in VM
make vm-verify           # F* verification in VM
make vm-signal-server-build-jar  # Build Signal-Server JAR in VM
make vm-signal-server    # Boot Signal-Server runtime VM
make check-isolation     # Verify host nix store is clean
```

### Local Escape Hatch

```bash
UMBRAVOX_LOCAL=1 make build    # explicitly opt into host build
UMBRAVOX_LOCAL=1 make test     # explicitly opt into host test
```

Without `UMBRAVOX_LOCAL=1`, targets default to VM and warn on host.

## Languages (strict)

- **Haskell**: all application code, crypto, protocol, TUI, bridge plugins
- **C**: generated crypto + FFI (csrc/)
- **Shell**: scripts/, Makefile
- **Nix**: VM images, environments
- **F***: formal specs (test/evidence/formal-proofs/fstar/)
- **Coq**: external proofs (test/evidence/formal-proofs/coq/)

No Rust, Go, or Python in production. Python for cert-gen scripts only.

## Project Structure

```
src/UmbraVox/Crypto/           Pure crypto (SHA, AES, X25519, Ed25519, ML-KEM)
src/UmbraVox/Crypto/Signal/    Signal protocol (X3DH, PQXDH, DoubleRatchet)
src/UmbraVox/Crypto/Signal/Compat.hs  Signal-compat params (WhisperText)
src/UmbraVox/Protocol/         Wire formats (CBOR, SignalWire, Handshake)
src/UmbraVox/Network/          Transport (TCP, UDP, SOCKS5, IPC, Providers)
src/UmbraVox/Bridge/Signal/    Signal bridge plugin (IPC subprocess)
src/UmbraVox/TUI/              Terminal UI (Render, Input, Menu, Dialog)
src/UmbraVox/App/              Core app (Config, Types, State, Defaults)
test/evidence/formal-proofs/coq/   14 Coq files, 460 Qed, 0 Admitted
test/evidence/formal-proofs/fstar/ 24 F* specs, 0 admit(), 25 assume val
nix/                           VM images + build configs
plugins/                       Bridge plugin manifests + templates
scripts/                       VM orchestration + test scripts
```

## Building

```bash
make vm-build-only                    # VM (preferred)
UMBRAVOX_LOCAL=1 nix-shell --run "cabal build all"  # local (escape hatch)
```

## Testing

```bash
make vm-test                          # full suite in VM
make test-signal-bridge-ipc           # bridge IPC smoke test
make test-signal-compat               # wire-compat (needs Signal-Server VM)
```

## Coq Proofs

```bash
# Build all 14 files:
nix-shell --run "cd test/evidence/formal-proofs/coq && make"

# Coqprime files need -native-compiler no:
coqc -native-compiler no -R . UmbraVox Ed25519GroupUniversal.v

# Count Qed:
grep -ch 'Qed\.' test/evidence/formal-proofs/coq/*.v | paste -sd+ | bc
```

## Signal Bridge

IPC subprocess protocol:
```
Host → Plugin: AUTH, SEND, RECV, CONTACTS, STATUS, PING, CLOSE
Plugin → Host: AUTH_OK, OK, DATA, CONTACTS, STATUS, PONG, ERR
```

Uses own crypto with Signal params — no libsignal dependency.

## Conventions

- **Zero warnings**: `cabal build all` clean
- **Zero Admitted**: all Coq files
- **No Co-Authored-By**: never add attribution to commits
- **VM isolation**: never compile on host; `make check-isolation` to verify
- **Commit prefix**: feat/fix/proof/test/docs/infra/chore
- **Security fixes**: require Finding/Vulnerability/Fix/Verified documentation

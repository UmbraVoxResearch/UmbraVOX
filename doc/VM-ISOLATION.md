# VM Isolation Audit

**Goal:** The host system should only need `qemu` and `nix` (for building
VM images).  ALL compilation (Haskell/GHC, C/GCC, F\*/Z3, Coq, Java/Maven)
must happen inside VMs.  The host nix store should contain only VM image
derivations, not GHC, cabal, Maven, etc.

**Date:** 2026-05-20

---

## Status Update (Post-Audit)

This document is an audit snapshot. Some findings below are intentionally
preserved for historical traceability and may no longer reflect current code.
Resolved after this snapshot:

- `./uv run` now aliases VM GUI launch; host-local compile bypass is disabled.
- `test-offline-parity` now routes through `vm_or_local`.
- `test-core*`, `test-tcp`, `test-fault`, `test-recovery`, `test-tui-sim`,
  `test-integrity`, `test-mdns`, `test-deferred`, and `test-differential`
  route through `vm_or_local`.
- VM image smoke boot now skips when `.vm-init.sh` is present (dev/exec mode).
- VM source export now uses the current worktree instead of `git archive HEAD`.
- VM network ALLOW rules now fail closed unless enforcement is implemented.

## Current Architecture

The project has two nix-shell environments:

| Shell               | Purpose                                 | Pulls compilers into host nix store? |
|---------------------|-----------------------------------------|--------------------------------------|
| `shell.nix`         | Full local dev (GHC, F\*, Coq, AFL++)   | **YES** -- violation                 |
| `shell-minimal.nix` | VM orchestration only (qemu, git, ./uv) | No -- clean                          |

The `./uv` driver now routes `build`, `test`, and `verify` through the VM path.
When the image is absent, these commands fail with an explicit
`./uv vm build-image` instruction.

---

## Target Classification

### VM-Only Targets (GOOD -- no host compiler needed)

These targets delegate all compilation to a QEMU guest via `./uv exec --`:

| Target                        | Mechanism                                        |
|-------------------------------|--------------------------------------------------|
| `vm-build`                    | `./uv exec -- cabal build all ...`               |
| `vm-build-only`               | `./uv vm build-image` then `./uv exec -- ...`   |
| `vm-test`                     | `./uv exec -- cabal build && cabal test`         |
| `vm-verify`                   | `./uv exec -- cabal run fstar-verify`            |
| `vm-dev`                      | Interactive shell inside VM                      |
| `vm-run-gui`                  | GUI shell inside VM                              |
| `vm-screenshot`               | Build + screenshot inside VM                     |
| `vm-record`                   | Build + record inside VM                         |
| `vm-visual-regression`        | Build + visual regression inside VM              |
| `vm-socks5-test`              | Runs test script inside VM                       |
| `vm-smoke-freebsd`            | QEMU FreeBSD guest (compiles inside guest)       |
| `vm-smoke-illumos`            | QEMU OmniOS guest (compiles inside guest)        |
| `vm-smoke-openbsd`            | QEMU OpenBSD guest (compiles inside guest)       |
| `vm-smoke-netbsd`             | QEMU NetBSD guest (compiles inside guest)        |
| `vm-smoke-dragonfly`          | QEMU DragonFlyBSD guest                          |
| `vm-smoke-arm64`              | QEMU aarch64 guest                               |
| `vm-signal-server-build-jar`  | Maven build inside VM                            |
| `vm-signal-server`            | Runtime VM (no host compilers)                   |
| `differential-vectors`        | `vm-differential-run.sh vectors` (VM)            |

### Nix-Build-on-Host Targets (ACCEPTABLE -- only VM image derivations)

These run `nix build` or `nix-build` to produce VM images.  The host nix
store gets the image derivation and its closure, but not GHC/cabal directly
(they are inside the NixOS image).

| Target               | Command                                              |
|----------------------|------------------------------------------------------|
| `vm-image-build`     | `nix build .#vm-image` or `nix-build nix/vm-image.nix` |
| `vm-signal-server-build` | `nix-build nix/vm-signal-server.nix`              |

**Caveat:** `./uv vm build-image` has a fallback `cabal run umbravox -- vm-image-build`
that would pull cabal into the host if both nix commands fail.  This fallback
is a violation and should be removed or gated.

### Host-Compiler Targets (VIOLATIONS -- run compilers on host)

These targets invoke `cabal`, `ghc`, `coqc`, `fstar.exe`, or `nix-shell`
(with `shell.nix` which pulls in GHC) directly on the host:

| Target | Violating command(s) | Notes |
|--------|---------------------|-------|
| `build` (fallback) | `cabal build all` | Falls through when no VM image present |
| `test` (fallback) | `cabal test ...` | Falls through when no VM image present |
| `verify` (fallback) | `cabal run fstar-verify` | Falls through when no VM image present |
| `run` | `cabal build umbravox && cabal run umbravox` | Always local, by design |
| `all` / `quality` | Chains `build test verify complexity lint license format-check` | Inherits violations from sub-targets |
| `complexity` | `cabal run check-complexity` | Runs on host |
| `codegen` | `cabal run codegen` | Runs on host |
| `mcdc-report` | `cabal configure/build/test --enable-coverage` | Runs on host |
| `test-core` | `cabal test ...` | Runs on host (no VM fallback) |
| `test-core-crypto` | `cabal test ...` | Runs on host |
| `test-core-network` | `cabal test ...` | Runs on host |
| `test-core-chat` | `cabal test ...` | Runs on host |
| `test-core-tui` | `cabal test ...` | Runs on host |
| `test-core-tools` | `cabal test ...` | Runs on host |
| `test-tcp` | `cabal test ...` | Runs on host |
| `test-fault` | `cabal test ...` | Runs on host |
| `test-recovery` | `cabal test ...` | Runs on host |
| `test-tui-sim` | `cabal test ...` | Runs on host |
| `test-integrity` | `cabal test ...` | Runs on host |
| `test-mdns` | `cabal test ...` | Runs on host |
| `test-deferred` | `cabal test ...` | Runs on host |
| `test-differential` | `cabal test ...` | Runs on host |
| `test-differential-oracle` | `cabal test ...` | Runs on host |
| `test-differential-full` | `cabal test ...` | Runs on host |
| `fuzz-differential` | `cabal test ...` | Runs on host |
| `fuzz-afl` | `ghc -O2 -isrc ...` (3 invocations) | Direct GHC on host |
| `soak` | `cabal test ...` | Runs on host |
| `signal-bridge-build` | `cabal build umbravox-signal-bridge` | Runs on host |
| `test-signal-compat` | `scripts/vm-signal-test.sh` | Needs audit |
| `assurance` | `coqc` (via `make -C .../coq`), `cabal test` | Runs on host |
| `vm-smoke` | `cabal run umbravox -- vm-smoke` | Runs cabal on host to launch orchestrator |
| `firecracker-smoke` | `cabal run umbravox -- firecracker-smoke` | Runs cabal on host |
| `firecracker-image-build` | `cabal run umbravox -- firecracker-image-build` | Runs cabal on host |
| `vm-integration-test` | `cabal run umbravox -- vm-integration-test` | Runs cabal on host |
| `vm-integration-test-dual-lan` | `cabal run umbravox -- vm-integration-test --dual-lan` | Runs cabal on host |
| `vm-forensics` | `cabal run umbravox -- vm-forensics` | Runs cabal on host |
| `release-smoke-linux` | `cabal run umbravox -- smoke-linux` | Runs cabal on host |
| `release-smoke-appimage` | `cabal run umbravox -- smoke-appimage` | Runs cabal on host |
| `release-lane-qemu` | `cabal run umbravox -- lane-qemu` | Runs cabal on host |
| `release-lane-firecracker` | `cabal run umbravox -- lane-firecracker` | Runs cabal on host |
| `release-lane-readiness-haskell` | `cabal run umbravox -- release-lane-readiness` | Runs cabal on host |
| `release-gate-assurance` | `cabal run umbravox -- gate-assurance` | Runs cabal on host |
| `verify-traffic` | `cabal run umbravox -- verify-traffic` | Runs cabal on host |
| `release-sbom-generate` | `cabal run umbravox -- release-sbom-generate` | Runs cabal on host |
| `release-license-bundle-generate` | `cabal run umbravox -- release-license-bundle-generate` | Runs cabal on host |
| `release-license-check` | `cabal run umbravox -- release-license-check` | Runs cabal on host |
| `release-linking` | `cabal run umbravox -- release-linking` | Runs cabal on host |
| `release-manifest` | `cabal run umbravox -- release-manifest` | Runs cabal on host |
| `release-checksums` | `cabal run umbravox -- release-checksums` | Runs cabal on host |
| `screenshot-local` | Needs `tmux` only (no compiler) | OK if binary pre-built |
| `release-freebsd` | `./uv release freebsd` | Uses Go release tool |
| `release-openbsd` | `./uv release openbsd` | Uses Go release tool |
| `release-netbsd` | `./uv release netbsd` | Uses Go release tool |
| `release-illumos` | `./uv release illumos` | Uses Go release tool |
| `release-linux-arm64` | `./uv release arm64` | Uses Go release tool |
| `build-haskell` | `cabal run umbravox -- build` or `./uv build` | Runs on host |
| `test-haskell` | `cabal run umbravox -- test` or `./uv test` | Runs on host |
| `verify-haskell` | `cabal run umbravox -- verify` or `./uv verify` | Runs on host |

### Shell-Only / No-Compiler Targets (CLEAN)

These use only `grep`, `bash`, `find`, `test`, `git`, etc. -- no compilers:

| Target | Notes |
|--------|-------|
| `lint` | grep/shell only |
| `license` / `license-fix` | grep/sed only |
| `format-check` | grep only |
| `assurance-fast` | grep/shell only (no compilation) |
| `check-evidence` | shell script |
| `clean` / `cleandb` / `cleanall` | rm only |
| `help` | echo only |
| `sanity` / `platform-sanity` | `test -f` only |
| `vm-image-clean` / `vm-cache-clean` | rm only |
| `vm-extract` | ls only |
| `image-clean` | alias for `vm-image-clean` |
| `visual-reference-update` | cp only |
| `release-source` | shell script (packaging only) |
| `release-linux` | shell script (packaging only, but may need pre-built binary) |
| `release-appimage` | shell script (packaging only) |
| `release-windows-cli` | shell script (source packaging) |
| `release-macos-terminal` | shell script (source packaging) |
| `release-bsd-terminal` | shell script (source packaging) |
| `release-freedos` | shell script (source packaging) |
| `release-smoke-qemu` | shell script (QEMU microVM) |
| `release-smoke-qemu-profile` | shell script |
| `release-smoke-firecracker` | shell script |
| `release-smoke-firecracker-pinned` | shell script |
| `release-smoke-qemu-nix` | shell script |
| `platform-lane-qemu` / `platform-lane-firecracker` | shell script |
| `platform-smoke-qemu-profile` | shell script |
| `release-lane-readiness` | shell scripts (check for tools, don't run them) |
| `release-compliance` / `release-sbom` / `release-license-bundle` | Tool presence check only |
| `test-infra` | shell script |
| `test-shells` | Checks tool presence (runs inside nix-shell, but just version queries) |
| `test-vm` | shell script |
| `test-offline-parity` | Runs `./uv build` + `./uv test-core-crypto` (inherits violations) |
| `evidence` | Chains `./uv quality` (inherits violations) |

---

## flake.nix Violations

The flake defines several nix derivations that pull the full toolchain
(`devTools` including GHC, cabal, F\*, Coq, etc.) into the host nix store:

- `packages.default` -- runs `./uv build` inside a derivation with `devTools`
- `packages.release-linux` -- runs `./uv release-linux` with `devTools`
- `packages.release-windows-cli-source` -- with `devTools`
- `packages.release-macos-terminal-source` -- with `devTools`
- `packages.release-bsd-terminal-source` -- with `devTools`
- `packages.release-freedos-source` -- with `devTools`
- `checks.build` / `checks.test` / `checks.verify` / `checks.quality` -- with `devTools`
- `devShells.default` -- full dev shell with `devTools`

These are acceptable for CI (where the nix store is ephemeral) but violate
the "host only needs qemu+nix" goal for developer machines.

---

## Recommended Migration Path

### Phase 1: Make fallbacks fail-safe (low effort)

1. **`build`/`test`/`verify` fallback:** Change the "no VM image" path from
   silently running locally to erroring with a message like
   `"No VM image. Run './uv vm build-image' first."`
   This prevents accidentally pulling compilers into the host store.

2. **`./uv vm build-image` cabal fallback:** Remove the
   `cabal run umbravox -- vm-image-build` fallback.  If both nix commands
   fail, just error out.

### Phase 2: Route remaining test suites through VM (medium effort)

All the `test-core-*`, `test-tcp`, `test-fault`, etc. targets currently
always run on host.  Add the same VM-dispatch pattern that `build`/`test`/
`verify` already have:

```sh
# example: route test-core through the VM
./uv exec -- cabal test umbravox-test --test-options='core'
# fails with "No VM image -- run './uv vm build-image'" when image is absent
```

Affected targets: `test-core`, `test-core-crypto`, `test-core-network`,
`test-core-chat`, `test-core-tui`, `test-core-tools`, `test-tcp`,
`test-fault`, `test-recovery`, `test-tui-sim`, `test-integrity`,
`test-mdns`, `test-deferred`, `test-differential`, `soak`.

### Phase 3: Route Haskell CLI orchestrators through VM (medium effort)

Many targets use `cabal run umbravox -- <subcommand>` as a host-side CLI
orchestrator.  These need to either:

- Run inside the VM (preferred), or
- Be rewritten as shell scripts that call `./uv exec -- ...`

Affected: `vm-smoke`, `firecracker-smoke`, `firecracker-image-build`,
`vm-integration-test`, `vm-integration-test-dual-lan`, `vm-forensics`,
`release-smoke-linux`, `release-smoke-appimage`, `release-lane-qemu`,
`release-lane-firecracker`, `release-lane-readiness-haskell`,
`release-gate-assurance`, `verify-traffic`, `release-sbom-generate`,
`release-license-bundle-generate`, `release-license-check`,
`release-linking`, `release-manifest`, `release-checksums`.

### Phase 4: Route complexity/codegen/fuzz/mcdc through VM (low effort)

- `complexity`: route through `./uv exec --`
- `codegen`: route through `./uv exec --`
- `fuzz-afl`: route through `./uv exec --` (GHC invocations)
- `mcdc-report`: route through `./uv exec --`
- `signal-bridge-build`: route through `./uv exec --`

### Phase 5: Platform release packaging (low effort)

The `release-freebsd`, `release-openbsd`, `release-netbsd`,
`release-illumos`, and `release-linux-arm64` targets use
`nix-shell --run ...` which pulls in `shell.nix` (full toolchain).
These should use `nix-shell shell-minimal.nix --run ...` since they
only package files (they don't compile).

### Phase 6: Flake cleanup (optional)

For strict host isolation, the flake's `packages.*` and `checks.*`
derivations should either be removed or documented as CI-only.
The `devShells.default` could be changed to use `shell-minimal.nix`
equivalents, with the full shell available as `devShells.full`.

### Items that CANNOT be VM-only

| Item | Reason |
|------|--------|
| `vm-image-build` (nix build) | Must run nix on the host to produce the VM image |
| `run` (TUI application) | Interactive app needs host terminal |
| `screenshot-local` | Captures host tmux session |
| `release-linux` packaging | Packages pre-built binaries (no compiler, but needs binary from VM) |

---

## Summary

| Category | Count |
|----------|-------|
| VM-only targets (clean) | 19 |
| Shell-only targets (clean) | ~25 |
| Nix-build on host (acceptable) | 2 |
| Host-compiler violations | ~50 targets |

The VM-dispatch pattern in `build`/`test`/`verify` is the right architecture.
The main gap was applying it to the remaining targets that invoked `cabal`
or `ghc` on the host.
The `shell-minimal.nix` file is ready and correct -- developers just need
to use it instead of `shell.nix`, and `./uv` needs to stop silently
falling back to host compilation.

# UmbraVOX Releases

UmbraVOX now defines explicit release targets instead of treating
`dist-newstyle/.../umbravox` as a portable deliverable.

## Targets

| Target | Artifact | Current result |
| --- | --- | --- |
| `./uv release linux` | `.tar.gz` | Native Linux x86_64 terminal bundle with patched local loader and copied shared libraries |
| `./uv release appimage` | `.tar.gz` | Experimental AppDir-style AppImage scaffold derived from the Linux bundle |
| `./uv release windows-cli` | `.zip` | Windows CLI source release with native build instructions |
| `./uv release macos-terminal` | `.tar.gz` | macOS terminal source release with native build instructions |
| `./uv release bsd-terminal` | `.tar.gz` | BSD terminal source release with native build instructions |
| `./uv release freedos` | `.zip` | FreeDOS research/source release with explicit unsupported-runtime note |
| `./uv release all` | mixed | Builds every target above |

Artifacts are written under `build/releases/`.
Each staged release artifact also includes:

- `RELEASE-MANIFEST.txt` with policy and build metadata
- `CONTENTS.SHA256` for in-package file integrity
- `RELEASE-SCRIPT.SHA256` identifying the packaging script revision

## Smoke Validation Status

Current smoke coverage is split between a working container-based check and
microVM entrypoints that now support direct pinned boot for both VMMs:

- `./uv release --smoke linux` performs an isolated Linux bundle smoke check
  with `podman` or `docker` when available.
- Additional smoke entrypoints:
  - `scripts/release-smoke-linux.sh` -- container-based Linux bundle smoke.
  - `scripts/release-smoke-appimage.sh` -- experimental AppImage scaffold smoke.
  - `./uv vm smoke release qemu` -- QEMU microVM smoke (Go implementation).
  - `./uv vm smoke release firecracker` -- Firecracker microVM smoke (Go implementation).
  - `scripts/release-smoke-qemu-profile.sh` -- QEMU with `QEMU_SMOKE_PROFILE`.

## Orchestration Migration

Current release orchestration is bridge-mode:

- `./uv` (Go binary) dispatches to shell scripts for release packaging,
  lane checks, and smoke scaffolding.
- That shell layer is a compatibility bridge, not the final orchestration
  boundary.
- Haskell orchestration entrypoints exist (`runOrchestratedBuild`,
  `runOrchestratedTest`, `runOrchestratedVerify`) and can be activated
  via `UMBRAVOX_USE_HASKELL_ORCH=1` inside the VM.
- Exit codes from orchestrated subcommands follow stable mapping:
  0 = success, 1 = failure, 124 = timeout, 127 = missing tool.
- The migration is phased:
  1. add minimal Haskell entrypoints that mirror existing shell behavior
  2. keep the shell wrappers as thin adapters while parity is proven
  3. move documentation and `./uv` targets to the Haskell entrypoints
  4. retire shell-specific logic only after logs, exit codes, and coverage
     match the existing bridge behavior

Until those phases complete, release/readiness scripts should be treated as
current operational glue rather than the desired end state.

## Assurance Release Gate

The assurance gate (available via `./uv check assurance` or
the Haskell entrypoint) verifies that the assurance matrix is present,
complete, and not stale relative to crypto source changes. This gate
should be included in release checklists to ensure that material assurance
changes are reflected in the documentation before release.

## Isolated VM Pipeline

All standard `./uv` targets (`build`, `test`, `verify`, etc.)
now route through the NixOS VM by default.  The VM is not limited to
`vm *` targets -- it is the primary execution environment for all
development commands. Host-local compile bypass is disabled.

`./uv vm smoke` runs the full build/test/release pipeline inside an
isolated NixOS QEMU VM:

1. Builds (or reuses cached) NixOS VM image via `nix build .#vm-image`
2. Creates a read-only ext2 source disk from the repository via `genext2fs`
3. Boots QEMU with KVM acceleration
4. Inside the guest: copies source to writable tmpfs, runs
   `./uv build && ./uv test && ./uv verify && ./uv complexity &&
   ./uv license && ./uv format-check && ./uv release linux`
5. Reports pass/fail based on guest exit

The VM image contains all development tools (GHC, cabal, F*, Z3, etc.)
with zero external dependencies. No network access is needed in-guest.

Image caching:

- The VM image is cached at `build/vm/image` (a Nix store symlink)
- It is only rebuilt when `flake.nix` or `flake.lock` change
- Use `./uv vm clean-image` to force a rebuild on next invocation

This closes the gap between host-trusted builds and authoritative
isolated release execution (M2.4).

### Firecracker Lane

`./uv vm firecracker-smoke` provides the same isolated pipeline via
Firecracker instead of QEMU:

1. Builds (or reuses cached) Firecracker image via
   `nix build .#firecracker-image` (vmlinux + ext4 rootfs)
2. Creates source disk and generates Firecracker JSON config at runtime
3. Boots Firecracker with KVM acceleration
4. Guest runs the same pipeline as the QEMU lane

The Firecracker image is cached at `build/vm/firecracker-image`.

## VM Integration Testing

In addition to release smoke testing, the VM infrastructure supports
functional and visual testing:

These capabilities exist as shell scripts but are not yet wired into `./uv vm`
subcommands. Run them directly via `scripts/vm-screenshot-capture.sh` and
`scripts/vm-tui-scenario.sh`.

Screenshots are captured as ANSI text files (with escape codes) and
optionally converted to HTML via `aha`. Reference captures live in
`test/evidence/visual-reference/` and are committed to git.

The SOCKS5 test also has a host-side variant in `Test.Network.Socks5Live`
that spawns microsocks locally (skips gracefully when unavailable).

## Compliance Placeholder Gates

The repository now provides real compliance tooling implemented in Haskell.
These are invoked via `./uv release --compliance`, which runs the Haskell
entrypoints for SBOM generation, license bundle generation, license policy
enforcement, and linking obligation analysis in sequence.

## Release Provenance

The Haskell release binary can generate a structured provenance manifest
(`release-manifest`) and SHA-256 checksums (`release-checksums`) for all
release artifacts under `build/releases/`. These are invoked as part of
the `./uv release --compliance` pipeline or directly via the Haskell binary
inside the VM.

The microVM smoke script always does these baseline checks first:

1. confirms a Linux release artifact exists under `build/releases/`
2. checks the selected VMM binary is installed
3. checks `/dev/kvm` is present

After those checks:

1. QEMU can run a host-supplied smoke command via
   `UMBRAVOX_QEMU_SMOKE_RUNNER`
2. Firecracker can run a host-supplied smoke command via
   `UMBRAVOX_FIRECRACKER_SMOKE_RUNNER`
3. QEMU can invoke a direct pinned-input boot path from
   `UMBRAVOX_QEMU_KERNEL`, `UMBRAVOX_QEMU_INITRD`,
   `UMBRAVOX_QEMU_ROOTFS`, and either `UMBRAVOX_QEMU_APPEND` or
   `UMBRAVOX_QEMU_PROFILE`
4. Firecracker can invoke `firecracker --config-file` when
   `UMBRAVOX_FIRECRACKER_KERNEL`, `UMBRAVOX_FIRECRACKER_ROOTFS`, and
   `UMBRAVOX_FIRECRACKER_CONFIG` are supplied
5. QEMU and Firecracker lane checks remain prerequisite-only checks
6. Platform sanity checks only verify helper wiring

What is still not claimed here:

- UmbraVOX does not yet ship a maintained guest image/rootfs for these paths.
- The default microVM smoke command remains scaffold-only when no runner hook
  or pinned inputs are provided.
- Firecracker does not yet have the same documented deterministic smoke-profile
  helper that QEMU has via `scripts/release-smoke-qemu-profile.sh`.
- Firecracker pinned boot is an invocation path, not yet a repository-owned
  maintained guest/config profile that proves in-guest bundle verification by
  default, and the M2.4.4.c.4 guest-image/config evidence task remains open.
- The AppImage track is scaffold-only and does not yet claim a maintained,
  supported single-file release artifact.
- These entrypoints are not yet evidence that release packaging is executed
  end-to-end inside a guest by default.

## Reproducibility Lane (M4.2.3)

The two-stage VM image build provides the infrastructure for a
reproducibility rebuild-and-compare lane:

1. **First build**: `./uv vm smoke` builds and tests in isolated NixOS VM
2. **Second build**: Run `./uv vm smoke` again from the same commit
3. **Compare**: SHA-256 of both release artifacts should match

The VM ensures identical build environment (same Nix store, same
toolchain, no host state leakage). Nix's content-addressing guarantees
that the same inputs produce the same outputs.

Current status: infrastructure is in place via the two-stage VM build.
Formal automated comparison (build twice, diff artifacts) is tracked
as future work.

To verify reproducibility manually:
```bash
./uv vm smoke  # produces build/releases/umbravox-*.tar.gz
sha256sum build/releases/umbravox-*-linux-x86_64.tar.gz > /tmp/hash1.txt

./uv vm clean-image && ./uv vm smoke
sha256sum build/releases/umbravox-*-linux-x86_64.tar.gz > /tmp/hash2.txt

diff /tmp/hash1.txt /tmp/hash2.txt
```

## Linux Binary Portability

The raw executable produced by:

```sh
cabal build exe:umbravox
```

is not a portable release binary on its own when built from this `nix-shell`.
It is dynamically linked and points at a Nix store loader path.

`./uv release linux` fixes that by:

1. locating the built `umbravox` executable
2. copying the executable into a release staging directory
3. copying the ELF interpreter and shared-library closure into a local `lib/`
4. launching through a bundled loader wrapper (`run-umbravox.sh`)
5. archiving the result as a portable Linux x86_64 terminal bundle

This is still a Linux terminal release, not a static binary. Kernel/ABI
compatibility still matters.

Current minimum compatibility policy for Linux release artifacts:

- Architecture: `x86_64`
- Minimum kernel ABI target: `3.10`
- Minimum glibc target: `2.31`

These values are recorded in each artifact `RELEASE-MANIFEST.txt` and may be
overridden for controlled CI release lanes via:
`UMBRAVOX_RELEASE_ABI_ARCH`, `UMBRAVOX_RELEASE_ABI_KERNEL_MIN`,
`UMBRAVOX_RELEASE_ABI_GLIBC_MIN`.

## Non-Linux Targets

The current repository does not ship cross-compilers or platform-native CI
toolchains for Windows, macOS, or BSD in this `nix-shell`. Those targets
therefore emit platform-specific source releases rather than pretending to
produce unverified cross-built binaries.

That keeps the release surface explicit:

- Linux gets a real native-binary bundle today.
- Windows CLI, macOS terminal, and BSD terminal get source bundles with
  platform-native build instructions.
- FreeDOS gets an explicit research/source bundle marked unsupported for
  native runtime use with the current Haskell implementation.

### BSD Source-Only Policy (M3.1.5)

BSD targets (FreeBSD, OpenBSD, NetBSD) remain source-only releases until
repo-owned native BSD builders are available. This means:

- `./uv release bsd-terminal` produces a source tarball with build
  instructions, not a prebuilt binary.
- No BSD-specific CI lane exists. Build and test evidence must come from
  a native BSD machine.
- The BSD source release includes the same `nix-shell`-based build
  instructions as other source targets. Users with a working Nix
  installation on BSD can build from source using the same workflow.
- Cross-building BSD binaries from Linux is not attempted. If it were,
  those artifacts would be treated as non-authoritative per M3.2.1.
- This policy will be revisited when a repo-owned BSD builder is
  available and can produce native build/test/release evidence.

## Examples

```sh
nix-shell
./uv release linux
./uv release appimage
./uv release all
./uv release --smoke linux
./uv release --compliance
scripts/release-smoke-linux.sh
scripts/release-smoke-appimage.sh
./uv vm smoke release qemu
./uv vm smoke release firecracker
UMBRAVOX_QEMU_PROFILE=bundle-basic \
UMBRAVOX_QEMU_KERNEL=/path/to/bzImage \
UMBRAVOX_QEMU_INITRD=/path/to/initrd \
UMBRAVOX_QEMU_ROOTFS=/path/to/rootfs.img \
./uv vm smoke release qemu
UMBRAVOX_FIRECRACKER_KERNEL=/path/to/vmlinux \
UMBRAVOX_FIRECRACKER_ROOTFS=/path/to/rootfs.img \
UMBRAVOX_FIRECRACKER_CONFIG=/path/to/firecracker.json \
./uv vm smoke release firecracker
```

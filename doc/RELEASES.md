# UmbraVOX Releases

UmbraVOX now defines explicit release targets instead of treating
`dist-newstyle/.../umbravox` as a portable deliverable.

## Targets

| Target | Artifact | Current result |
| --- | --- | --- |
| `make release-linux` | `.tar.gz` | Native Linux x86_64 terminal bundle with patched local loader and copied shared libraries |
| `make release-windows-cli` | `.zip` | Windows CLI source release with native build instructions |
| `make release-macos-terminal` | `.tar.gz` | macOS terminal source release with native build instructions |
| `make release-bsd-terminal` | `.tar.gz` | BSD terminal source release with native build instructions |
| `make release-freedos` | `.zip` | FreeDOS research/source release with explicit unsupported-runtime note |
| `make release` | mixed | Builds every target above |

Artifacts are written under `build/releases/`.
Each staged release artifact also includes:

- `RELEASE-MANIFEST.txt` with policy and build metadata
- `CONTENTS.SHA256` for in-package file integrity
- `RELEASE-SCRIPT.SHA256` identifying the packaging script revision

## Smoke Validation Status

Current smoke coverage is split between a working container-based check and
scaffolded microVM entrypoints:

- `make release-smoke-linux` runs today and performs an isolated Linux bundle
  smoke check with `podman` or `docker` when available.
- `./scripts/release-smoke-microvm.sh qemu`
- `./scripts/release-smoke-microvm.sh firecracker`

The microVM smoke script currently does three things only:

1. confirms a Linux release artifact exists under `build/releases/`
2. checks the selected VMM binary is installed
3. checks `/dev/kvm` is present

After those checks it prints the next implementation step. It does not yet boot
a guest VM or microVM, and it does not yet execute bundle checks in-guest.

## Linux Binary Portability

The raw executable produced by:

```sh
cabal build exe:umbravox
```

is not a portable release binary on its own when built from this `nix-shell`.
It is dynamically linked and points at a Nix store loader path.

`make release-linux` fixes that by:

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

## Examples

```sh
nix-shell
make release-linux
make release-smoke-linux
./scripts/release-smoke-microvm.sh qemu
./scripts/release-smoke-microvm.sh firecracker
make release
```

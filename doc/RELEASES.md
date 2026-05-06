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
3. copying the ELF interpreter and direct shared-library dependencies into a local `lib/`
4. patching the executable to use the bundled loader and RPATH
5. archiving the result as a portable Linux x86_64 terminal bundle

This is still a Linux terminal release, not a static binary. Kernel/ABI
compatibility still matters.

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
make release
```

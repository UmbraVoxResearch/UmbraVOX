# Software Bill of Materials (SBOM) Strategy

## 1. What Is an SBOM?

A Software Bill of Materials is a machine-readable inventory of every
component that ships in (or contributes to) a software artifact. It lists
each dependency's name, version, supplier, and license so that consumers
can assess supply-chain risk, verify license compliance, and respond
quickly to vulnerability disclosures.

## 2. Why UmbraVOX Needs One

UmbraVOX is a cryptographic messaging system. Its users need to know
exactly what code runs on their machine, what trust properties that code
carries, and whether any component introduces a license obligation.
An SBOM makes that audit mechanical rather than manual.

Because UmbraVOX vendors its C dependencies and avoids third-party
Haskell packages beyond GHC boot libraries, the SBOM is unusually short
and stable. That is a feature, not an accident.

## 3. Dependency Inventory

### 3.1 Runtime (compiled into binary)

These components end up in the final executable.

| Component | Location | License (SPDX) | Notes |
|-----------|----------|-----------------|-------|
| GHC runtime (RTS) | linked by GHC | BSD-3-Clause | Haskell runtime system |
| base | GHC boot lib | BSD-3-Clause | Prelude, IO, basic types |
| bytestring | GHC boot lib | BSD-3-Clause | Byte arrays |
| containers | GHC boot lib | BSD-3-Clause | Maps, sets |
| network | GHC boot lib | BSD-3-Clause | TCP/UDP sockets |
| text | GHC boot lib | BSD-3-Clause | Unicode text |
| time | GHC boot lib | BSD-3-Clause | Clock, calendar |
| unix | GHC boot lib | BSD-3-Clause | POSIX bindings |
| array | GHC boot lib | BSD-3-Clause | Mutable/immutable arrays |
| stm | GHC boot lib | BSD-3-Clause | Software transactional memory |
| directory | GHC boot lib | BSD-3-Clause | Filesystem operations |
| filepath | GHC boot lib | BSD-3-Clause | Path manipulation |
| process | GHC boot lib | BSD-3-Clause | Process spawning |
| CryptoGen C | `csrc/generated/*.c` | Apache-2.0 | Our generated C code |
| HACL* | `csrc/hacl/` | Apache-2.0 | Verified crypto (when vendored) |
| fiat-crypto | `csrc/fiat/` | MIT | Verified field arithmetic (when vendored) |
| ct_helpers.h | `csrc/ct_helpers.h` | Apache-2.0 | Constant-time helpers (our code) |
| constant_time.c | `csrc/constant_time.c` | Apache-2.0 | Constant-time comparisons (our code) |
| secure_zero.c | `csrc/secure_zero.c` | Apache-2.0 | Secure memory zeroing (our code) |
| secure_mlock.c | `csrc/secure_mlock.c` | Apache-2.0 | Memory locking (our code) |
| sqlite3_shim.c | `csrc/sqlite3_shim.c` | Apache-2.0 | SQLite FFI shim (our code) |
| SQLite | linked via pkgconfig | Public-domain | Embedded database engine |

See [VERIFIED-C.md](VERIFIED-C.md) for provenance and trust properties of
all C dependencies.

### 3.2 Build-time Only (not in binary)

These tools participate in the build but produce no code in the final
artifact.

| Component | Location / Source | License (SPDX) | Notes |
|-----------|-------------------|-----------------|-------|
| Go tools | `tools/` | Apache-2.0 | Our code; zero external Go deps |
| Nix / nixpkgs | NixOS project | MIT | Reproducible build environment |
| GHC | haskell.org | BSD-3-Clause | Haskell compiler |
| Cabal | haskell.org | BSD-3-Clause | Haskell build system |
| F* | Project Everest | Apache-2.0 | Proof language for HACL* |
| Z3 | Microsoft | MIT | SMT solver for proofs |
| Coq | Inria | LGPL-2.1-only | Proof assistant for fiat-crypto |
| Maven / JDK | Apache / Oracle | GPL-2.0-only WITH Classpath-exception-2.0 | Signal Server build only |

### 3.3 Test-time Only

These are used in differential testing and never linked into the
production binary.

| Component | Source | License (SPDX) | Notes |
|-----------|--------|-----------------|-------|
| libsodium | libsodium.org | ISC | Differential oracle |
| HACL* reference | Project Everest | Apache-2.0 | Differential oracle |

## 4. SBOM Format

UmbraVOX targets **CycloneDX** (OWASP) as its SBOM format. CycloneDX
provides:

- JSON and XML serialization
- First-class support for license expressions (SPDX identifiers)
- Component classification (library, framework, application, firmware)
- Vulnerability correlation via CPE and PURL
- Composition completeness assertions (the SBOM can declare itself
  "complete" or "incomplete")

CycloneDX is preferred over SPDX for this project because the dependency
set is small and hand-audited, and CycloneDX's JSON schema integrates
more naturally with CI tooling.

## 5. Generation: `./uv sbom` (Future)

A planned `./uv sbom` command will generate a CycloneDX JSON document
covering all runtime dependencies. The command will:

1. Read the pinned dependency list from the build configuration.
2. Walk `csrc/` to enumerate vendored C components and their licenses.
3. Query the Cabal plan for GHC boot library versions.
4. Emit a CycloneDX 1.5+ JSON file to `dist/sbom.cdx.json`.
5. Validate the output against the CycloneDX JSON schema.

Until `./uv sbom` is implemented, this document serves as the
authoritative SBOM.

## 6. SPDX License Summary

All runtime components use one of:

- `Apache-2.0` -- our code, HACL*
- `BSD-3-Clause` -- GHC and its boot libraries
- `MIT` -- fiat-crypto
- `Public-domain` -- SQLite

No copyleft licenses appear in the runtime dependency set.

See [LICENSING.md](LICENSING.md) for the full compatibility analysis.

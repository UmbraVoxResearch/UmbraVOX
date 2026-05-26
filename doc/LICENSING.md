# License Compatibility Analysis

## 1. Project License

UmbraVOX is licensed under **Apache-2.0**.

This document analyzes every dependency for license compatibility with
Apache-2.0 and documents any obligations that distribution creates.

## 2. Compatibility Matrix

| Dependency | License (SPDX) | Compatible? | Category | Notes |
|-----------|-----------------|-------------|----------|-------|
| GHC boot libs | BSD-3-Clause | Yes | Runtime | Permissive; requires copyright notice |
| SQLite | Public-domain | Yes | Runtime | No restrictions whatsoever |
| HACL* | Apache-2.0 | Yes | Runtime | Same license as UmbraVOX |
| fiat-crypto | MIT | Yes | Runtime | Permissive; requires copyright notice |
| CryptoGen C | Apache-2.0 | Yes | Runtime | Our own code |
| Helper C files | Apache-2.0 | Yes | Runtime | Our own code |
| Go tools | Apache-2.0 | Yes | Build-time | Our own code; zero external deps |
| Nix / nixpkgs | MIT | Yes | Build-time | Not linked; build environment only |
| GHC (compiler) | BSD-3-Clause | Yes | Build-time | Not linked; compiler only |
| Cabal | BSD-3-Clause | Yes | Build-time | Not linked; build tool only |
| F* | Apache-2.0 | Yes | Build-time | Not linked; proof language only |
| Z3 | MIT | Yes | Build-time | Not linked; solver only |
| Coq | LGPL-2.1-only | Yes | Build-time | Not linked; proof assistant only |
| Maven / JDK | GPL-2.0 WITH Classpath-exception-2.0 | N/A | Build-time | Signal Server VM only |
| Signal Server | AGPL-3.0-only | N/A | Separate VM | Never linked into UmbraVOX |
| libsodium | ISC | Yes | Test-time | Differential oracle only |
| HACL* reference | Apache-2.0 | Yes | Test-time | Differential oracle only |

## 3. Analysis Notes

### 3.1 No Copyleft in the Runtime

Every component compiled into the UmbraVOX binary uses a permissive
license (Apache-2.0, BSD-3-Clause, MIT, or public domain). No copyleft
obligation propagates to the distributed binary.

### 3.2 AGPL-3.0 Signal Server

The Signal Server is licensed AGPL-3.0. It runs inside its own VM and
communicates with UmbraVOX only over the network. It is never linked,
statically or dynamically, into the UmbraVOX binary. The AGPL's
copyleft therefore does not apply to UmbraVOX itself. Users who modify
the Signal Server and expose it over a network must comply with the
AGPL independently.

### 3.3 Coq LGPL-2.1

Coq is used at build time to check proofs that generate fiat-crypto C
files. The Coq runtime and proof checker are never linked into the
UmbraVOX binary. The generated C files carry their own MIT license (from
the fiat-crypto project), not the Coq LGPL. The proof evidence artifacts
in `test/evidence/formal-proofs/coq/` are outputs of Coq, not
derivative works of Coq itself.

### 3.4 fiat-crypto MIT Obligations

The MIT license requires that the copyright notice and permission notice
be included in all copies or substantial portions of the software. This
is satisfied by the vendored `csrc/fiat/README.md` which preserves the
original copyright notice and license text.

### 3.5 HACL* Apache-2.0 Obligations

Apache-2.0 requires preservation of NOTICE files. When HACL* files are
vendored into `csrc/hacl/`, the upstream NOTICE file (if present) must
be preserved alongside the source. Apache-2.0 also requires that
modified files carry a notice stating they have been changed.

### 3.6 GHC Boot Libraries BSD-3-Clause

BSD-3-Clause requires that the copyright notice and list of conditions
appear in redistributions. Because GHC boot libraries are compiled into
the binary, the relevant copyright notices must be accessible. These
are included in the GHC distribution and preserved by the Nix build.

## 4. Distribution Checklist

When distributing UmbraVOX binaries:

1. Include the Apache-2.0 LICENSE file (repository root).
2. Preserve `csrc/fiat/README.md` (MIT copyright notice for fiat-crypto).
3. Preserve any HACL* NOTICE file in `csrc/hacl/`.
4. Include GHC boot library copyright notices (shipped with GHC; the Nix
   closure retains them).
5. No action needed for SQLite (public domain).
6. No action needed for build-time or test-time dependencies.

## 5. Related Documents

- [SBOM.md](SBOM.md) -- full dependency inventory with versions
- [VERIFIED-C.md](VERIFIED-C.md) -- provenance and trust properties of C code
- [LEGAL-NOTICE.md](LEGAL-NOTICE.md) -- general legal notice for the project

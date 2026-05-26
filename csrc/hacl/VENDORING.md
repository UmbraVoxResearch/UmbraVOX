# HACL* Vendoring Procedure

This document records exactly how to populate `csrc/hacl/` with the upstream
verified C sources. Run these steps inside the project root.

## Upstream

- Repository: https://github.com/cryspen/hacl-packages
- Target release: the most recent tagged release on the `main` branch
  (currently `v0.4.x` series; verify at fetch time)
- License: MIT (all extracted C files carry an MIT license header)

## Step-by-step

### 1. Pin to a specific commit

Before fetching, decide on a fixed commit or tag. Record it below in the
"Current vendor state" section. Never vendor from a floating branch.

### 2. Fetch the distribution archive

```sh
VERSION=0.4.2   # replace with the actual release tag
curl -L "https://github.com/cryspen/hacl-packages/archive/refs/tags/v${VERSION}.tar.gz" \
    -o /tmp/hacl-packages-${VERSION}.tar.gz
```

Verify the SHA-256 checksum against the GitHub release page before
extracting:

```sh
sha256sum /tmp/hacl-packages-${VERSION}.tar.gz
```

Record the checksum in the "Current vendor state" section below.

### 3. Extract needed files

The layout inside the archive is:

```
hacl-packages-<VERSION>/
  src/
    Hacl_SHA2_256.c
    Hacl_SHA2_512.c
    Hacl_Chacha20.c
    Hacl_Poly1305_32.c
    Hacl_SHA3.c
    Hacl_HMAC.c
    Hacl_HKDF.c
  include/
    Hacl_SHA2_256.h
    Hacl_SHA2_512.h
    Hacl_Chacha20.h
    Hacl_Poly1305_32.h
    Hacl_SHA3.h
    Hacl_HMAC.h
    Hacl_HKDF.h
    Hacl_Krmllib.h
    krml/
      FStar_UInt128.h
      FStar_UInt_8_16_32_64.h
      internal/target.h
      lowstar_ignore.h
    lib/
      memfunctions.h
```

Copy them into this directory, preserving the `krml/` and `lib/` subdirectory
structure:

```sh
DIST=/tmp/hacl-packages-${VERSION}
tar -xzf /tmp/hacl-packages-${VERSION}.tar.gz -C /tmp

DEST=csrc/hacl

# Algorithm C sources
cp ${DIST}/src/Hacl_SHA2_256.c    ${DEST}/
cp ${DIST}/src/Hacl_SHA2_512.c    ${DEST}/
cp ${DIST}/src/Hacl_Chacha20.c    ${DEST}/
cp ${DIST}/src/Hacl_Poly1305_32.c ${DEST}/
cp ${DIST}/src/Hacl_SHA3.c        ${DEST}/
cp ${DIST}/src/Hacl_HMAC.c        ${DEST}/
cp ${DIST}/src/Hacl_HKDF.c        ${DEST}/

# Headers (algorithm-specific)
cp ${DIST}/include/Hacl_SHA2_256.h    ${DEST}/
cp ${DIST}/include/Hacl_SHA2_512.h    ${DEST}/
cp ${DIST}/include/Hacl_Chacha20.h    ${DEST}/
cp ${DIST}/include/Hacl_Poly1305_32.h ${DEST}/
cp ${DIST}/include/Hacl_SHA3.h        ${DEST}/
cp ${DIST}/include/Hacl_HMAC.h        ${DEST}/
cp ${DIST}/include/Hacl_HKDF.h        ${DEST}/
cp ${DIST}/include/Hacl_Krmllib.h     ${DEST}/

# Shared KaRaMeL runtime headers
mkdir -p ${DEST}/krml/internal
cp ${DIST}/include/krml/FStar_UInt128.h          ${DEST}/krml/
cp ${DIST}/include/krml/FStar_UInt_8_16_32_64.h  ${DEST}/krml/
cp ${DIST}/include/krml/internal/target.h         ${DEST}/krml/internal/
cp ${DIST}/include/krml/lowstar_ignore.h          ${DEST}/krml/

# Shared memfunctions
mkdir -p ${DEST}/lib
cp ${DIST}/include/lib/memfunctions.h ${DEST}/lib/
```

### 4. Verify file checksums

After copying, record individual SHA-256 hashes for each vendored file:

```sh
find csrc/hacl -type f \( -name '*.c' -o -name '*.h' \) | sort | xargs sha256sum
```

Paste the output into the "File checksums" section below.

### 5. Update this document

Fill in the "Current vendor state" section with:
- Upstream tag / commit hash
- Archive SHA-256
- Date fetched
- Individual file checksums

---

## Current vendor state

**Status: NOT YET VENDORED**

Files have not been fetched. This directory contains only documentation.
To complete vendoring, follow the steps above and update this section.

| Field | Value |
|---|---|
| Upstream tag | (none) |
| Upstream commit | (none) |
| Archive SHA-256 | (none) |
| Date fetched | (none) |

### File checksums

(none — files not yet present)

---

## Updating to a new release

1. Review the HACL* changelog for breaking API changes.
2. Repeat steps 1–5 above with the new tag.
3. Run the differential test suite (`./uv test`) to confirm no regressions.
4. Update the "Current vendor state" table with the new values.
5. Commit the updated files and this document together in one commit.

## Nix integration note

The Nix build does not currently pull HACL* automatically. The `nixpkgs`
`hacl-star` package exists but vendors a different file layout; vendoring
directly from `cryspen/hacl-packages` is preferred for a predictable,
auditable file set.

If a Nix derivation is needed in the future, a `fetchFromGitHub` fixed-output
derivation using the pinned commit hash is the correct approach. See
`nix/vm-image.nix` for examples of fixed-output fetches elsewhere in this
project.

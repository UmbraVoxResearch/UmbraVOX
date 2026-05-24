#!/usr/bin/env bash
# scripts/release-package-platform.sh — Platform release packaging (M14.4.1-5).
#
# Creates a source+binary release tarball for a named platform:
#   umbravox-{version}-{platform}-{arch}.tar.gz
#
# Contents:
#   umbravox          — the built binary (or a build note if cross-build unavailable)
#   README.md         — project readme
#   LICENSE           — Apache-2.0 license
#   LEGAL-NOTICE.md   — legal notice
#   doc/QUICKSTART.md — quick-start guide
#   RELEASE-MANIFEST.txt — provenance metadata
#   BUILDING-{PLATFORM}.txt — platform-specific build notes
#
# Usage:
#   scripts/release-package-platform.sh <platform>
#
# Platforms:
#   freebsd        FreeBSD 14.x amd64
#   openbsd        OpenBSD 7.x amd64
#   netbsd         NetBSD 10.x amd64
#   illumos        OmniOS/illumos amd64
#   linux-arm64    Linux aarch64
#
# Environment overrides:
#   UMBRAVOX_ROOT                 — repo root (default: cwd)
#   UMBRAVOX_RELEASE_VERSION      — version string (default: git describe)
#   UMBRAVOX_RELEASE_COMMIT       — commit hash (default: git rev-parse HEAD)
#   UMBRAVOX_RELEASE_STAMP        — timestamp (default: current UTC)
#   UMBRAVOX_ALLOW_DIRTY_RELEASE  — set to 1 to allow dirty worktree
#   UMBRAVOX_ALLOW_UNTAGGED_RELEASE — set to 1 to allow untagged commit
#
# Run inside nix-shell for a reproducible build environment:
#   nix-shell --run 'scripts/release-package-platform.sh freebsd'

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/lib-release.sh"

ROOT="$UMBRAVOX_ROOT"
cd "$ROOT"

OUT_DIR="$RELEASE_OUT_DIR"
PLATFORM="${1:-}"
VERSION="$RELEASE_VERSION"
COMMIT="$RELEASE_COMMIT"

# --------------------------------------------------------------------------
# Platform definitions
# --------------------------------------------------------------------------

# Associative arrays require bash 4+; use parallel plain arrays instead.
# Order: freebsd openbsd netbsd illumos linux-arm64
PLATFORMS="freebsd openbsd netbsd illumos linux-arm64"

platform_arch() {
    case "$1" in
        freebsd)     echo "amd64"   ;;
        openbsd)     echo "amd64"   ;;
        netbsd)      echo "amd64"   ;;
        illumos)     echo "amd64"   ;;
        linux-arm64) echo "aarch64" ;;
        *)           echo "unknown" ;;
    esac
}

platform_os() {
    case "$1" in
        freebsd)     echo "FreeBSD"  ;;
        openbsd)     echo "OpenBSD"  ;;
        netbsd)      echo "NetBSD"   ;;
        illumos)     echo "illumos"  ;;
        linux-arm64) echo "Linux"    ;;
        *)           echo "unknown"  ;;
    esac
}

platform_build_note() {
    local platform="$1"
    case "$platform" in
        freebsd)
            cat <<'EOF'
UmbraVOX FreeBSD release

This tarball contains the UmbraVOX source tree and build notes for FreeBSD.
A native binary is included when this package is built inside a FreeBSD VM
runner; otherwise it is a source release requiring a native build.

Recommended native build on FreeBSD 14.x amd64:
  pkg install ghc cabal-hs gmake git curl ca_root_nss
  cabal update
  cabal build all
  cabal test umbravox-test --test-options=required

The in-guest build+test script is at scripts/vm-build-test.sh.
EOF
            ;;
        openbsd)
            cat <<'EOF'
UmbraVOX OpenBSD release

This tarball contains the UmbraVOX source tree and build notes for OpenBSD.
A native binary is included when this package is built inside an OpenBSD VM
runner; otherwise it is a source release requiring a native build.

Recommended native build on OpenBSD 7.x amd64:
  pkg_add ghc cabal-install
  cabal update
  cabal build all
  cabal test umbravox-test --test-options=required

The in-guest build+test script is at scripts/vm-build-test.sh.
EOF
            ;;
        netbsd)
            cat <<'EOF'
UmbraVOX NetBSD release

This tarball contains the UmbraVOX source tree and build notes for NetBSD.
A native binary is included when this package is built inside a NetBSD VM
runner; otherwise it is a source release requiring a native build.

Recommended native build on NetBSD 10.x amd64:
  pkgin install ghc cabal-install
  cabal update
  cabal build all
  cabal test umbravox-test --test-options=required

The in-guest build+test script is at scripts/vm-build-test.sh.
EOF
            ;;
        illumos)
            cat <<'EOF'
UmbraVOX illumos/OmniOS release

This tarball contains the UmbraVOX source tree and build notes for
OmniOS CE (illumos). A native binary is included when this package is
built inside an OmniOS VM runner; otherwise it is a source release.

Recommended native build on OmniOS CE r151052 amd64:
  pkg set-publisher -g https://pkg.ooce.omnios.org/omnios/r151052 ooce
  pkg install ooce/lang/ghc ooce/developer/cabal developer/gcc14 \
      developer/gnu-binutils system/header developer/build/gnu-make \
      scm/git web/curl
  export PATH="/opt/ooce/bin:/opt/ooce/sbin:$PATH"
  cabal update
  cabal build all
  cabal test umbravox-test --test-options=required

The in-guest build+test script is at scripts/vm-build-test.sh.
EOF
            ;;
        linux-arm64)
            cat <<'EOF'
UmbraVOX Linux arm64 release

This tarball contains the UmbraVOX source tree and build notes for
Linux aarch64. A native binary is included when this package is built
on an arm64 runner (native or QEMU emulation); otherwise it is a source release.

Recommended native build on Linux aarch64 (Debian/Ubuntu example):
  apt-get install -y ghc cabal-install make git curl
  cabal update
  cabal build all
  cabal test umbravox-test --test-options=required

Alternatively, use ghcup for the latest GHC release (download, verify,
then execute — never pipe curl directly to sh):
  curl --proto '=https' --tlsv1.2 -sSf -o /tmp/ghcup-install.sh https://get-ghcup.haskell.org
  # TODO: Replace with current ghcup installer SHA-256 from https://www.haskell.org/ghcup/
  echo "TODO_INSERT_ACTUAL_SHA256  /tmp/ghcup-install.sh" | sha256sum -c -
  sh /tmp/ghcup-install.sh
  rm /tmp/ghcup-install.sh

The in-guest build+test script is at scripts/vm-build-test.sh.
EOF
            ;;
        *)
            echo "UmbraVOX platform release (${platform})"
            ;;
    esac
}

# --------------------------------------------------------------------------
# Usage
# --------------------------------------------------------------------------

usage() {
    cat <<EOF
usage: scripts/release-package-platform.sh <platform>

platforms: ${PLATFORMS}
EOF
}

# --------------------------------------------------------------------------
# Guards — delegated to lib-release.sh
# --------------------------------------------------------------------------

require_cmd()         { lib_require_cmd "$@"; }
ensure_clean_tree()   { lib_ensure_clean_tree; }
ensure_tagged_release() { lib_ensure_tagged_release; }

validate_platform() {
    local p="$1"
    for known in $PLATFORMS; do
        if [[ "$p" == "$known" ]]; then
            return 0
        fi
    done
    echo "unknown platform: $p" >&2
    usage >&2
    exit 1
}

# --------------------------------------------------------------------------
# Staging helpers — delegated to lib-release.sh
# --------------------------------------------------------------------------

stage_source_tree()   { lib_stage_source_tree "$@"; }

write_manifest() {
    local stage="$1"
    local platform="$2"
    local arch="$3"
    local artifact_kind="$4"
    lib_write_manifest "$stage" \
        "platform=${platform}" \
        "arch=${arch}" \
        "artifact_kind=${artifact_kind}"
}

copy_docs()           { lib_copy_common_docs "$@"; }
archive_tgz()         { lib_archive_tgz "$@"; }

# --------------------------------------------------------------------------
# Attempt to locate a pre-built binary
# --------------------------------------------------------------------------

try_copy_binary() {
    local stage="$1"
    # In-VM or post-build scenarios: look for a built binary in dist-newstyle.
    local bin
    bin="$(cabal list-bin exe:umbravox 2>/dev/null || true)"
    if [[ -n "$bin" ]] && [[ -f "$bin" ]]; then
        mkdir -p "$stage/bin"
        cp "$bin" "$stage/bin/umbravox"
        echo "binary=bin/umbravox" >> "$stage/RELEASE-MANIFEST.txt"
        echo "  binary: bin/umbravox ($(file "$stage/bin/umbravox" | cut -d: -f2- | xargs))"
        return 0
    fi
    # Fallback: search dist-newstyle
    bin="$(find dist-newstyle -path '*/build/*/umbravox/umbravox' -type f 2>/dev/null | head -1 || true)"
    if [[ -n "$bin" ]]; then
        mkdir -p "$stage/bin"
        cp "$bin" "$stage/bin/umbravox"
        echo "binary=bin/umbravox" >> "$stage/RELEASE-MANIFEST.txt"
        echo "  binary: bin/umbravox ($(file "$stage/bin/umbravox" | cut -d: -f2- | xargs))"
        return 0
    fi
    echo "  binary: not found (source-only release)"
    echo "binary=none" >> "$stage/RELEASE-MANIFEST.txt"
    return 0
}

# --------------------------------------------------------------------------
# Main packaging function
# --------------------------------------------------------------------------

build_platform_release() {
    local platform="$1"
    local arch
    arch="$(platform_arch "$platform")"
    local os
    os="$(platform_os "$platform")"
    local pkg="umbravox-${VERSION}-${platform}-${arch}"
    local stage="$OUT_DIR/$pkg"
    local artifact="$OUT_DIR/$pkg.tar.gz"

    echo ""
    echo "  Platform:  ${os} ${arch}"
    echo "  Version:   ${VERSION}"
    echo "  Commit:    ${COMMIT}"
    echo "  Artifact:  ${artifact}"
    echo ""

    rm -rf "$stage"
    stage_source_tree "$stage"
    copy_docs "$stage"
    write_manifest "$stage" "$platform" "$arch" "platform-source-release"
    platform_build_note "$platform" > "$stage/BUILDING-${platform^^}.txt"

    # Include ./uv bootstrap for convenience inside the release
    if [[ -f uv ]]; then
        cp uv "$stage/uv"
        chmod +x "$stage/uv"
    fi

    try_copy_binary "$stage"

    (cd "$stage" && find . -type f | sort | xargs sha256sum > CONTENTS.SHA256)
    sha256sum "$ROOT/scripts/release-package-platform.sh" > "$stage/RELEASE-SCRIPT.SHA256"
    cat >>"$stage/RELEASE-MANIFEST.txt" <<EOF
contents_sha256_file=CONTENTS.SHA256
release_script_sha256_file=RELEASE-SCRIPT.SHA256
EOF

    archive_tgz "$stage" "$artifact"
    echo "$artifact"
}

# --------------------------------------------------------------------------
# Entry point
# --------------------------------------------------------------------------

main() {
    if [[ -z "$PLATFORM" ]]; then
        usage >&2
        exit 1
    fi

    validate_platform "$PLATFORM"

    require_cmd git
    require_cmd tar
    require_cmd sha256sum

    mkdir -p "$OUT_DIR"
    ensure_clean_tree
    ensure_tagged_release

    build_platform_release "$PLATFORM"
}

main "$@"

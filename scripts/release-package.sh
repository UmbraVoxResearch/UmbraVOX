#!/usr/bin/env bash
set -euo pipefail

ROOT="${UMBRAVOX_ROOT:-$(pwd)}"
cd "$ROOT"

OUT_DIR="$ROOT/build/releases"
TARGET="${1:-}"
VERSION="$(git describe --tags --always --dirty 2>/dev/null || git rev-parse --short HEAD 2>/dev/null || echo unknown)"
COMMIT="$(git rev-parse HEAD 2>/dev/null || echo unknown)"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"

usage() {
    cat <<'EOF'
usage: scripts/release-package.sh <target>

targets:
  source
  linux
  windows-cli
  macos-terminal
  bsd-terminal
  freedos
EOF
}

require_cmd() {
    local cmd="$1"
    if ! command -v "$cmd" >/dev/null 2>&1; then
        echo "missing required command: $cmd" >&2
        exit 1
    fi
}

stage_source_tree() {
    local stage="$1"
    mkdir -p "$stage"
    git ls-files -z | tar --null -T - -cf - | tar -xf - -C "$stage"
}

write_manifest() {
    local stage="$1"
    local target="$2"
    local artifact_kind="$3"
    cat >"$stage/RELEASE-MANIFEST.txt" <<EOF
name=UmbraVOX
target=$target
artifact_kind=$artifact_kind
version=$VERSION
commit=$COMMIT
timestamp_utc=$STAMP
builder=$(uname -srm)
EOF
}

copy_common_docs() {
    local stage="$1"
    mkdir -p "$stage/doc"
    cp README.md LICENSE LEGAL-NOTICE.md PUBLISHING-NOTE.md "$stage/"
    cp doc/QUICKSTART.md doc/RELEASES.md "$stage/doc/"
}

write_target_note() {
    local path="$1"
    local body="$2"
    cat >"$path" <<EOF
$body
EOF
}

archive_tgz() {
    local stage="$1"
    local artifact="$2"
    tar -C "$(dirname "$stage")" -czf "$artifact" "$(basename "$stage")"
    sha256sum "$artifact" > "$artifact.sha256"
}

archive_zip() {
    local stage="$1"
    local artifact="$2"
    rm -f "$artifact"
    (
        cd "$(dirname "$stage")"
        zip -qr "$artifact" "$(basename "$stage")"
    )
    sha256sum "$artifact" > "$artifact.sha256"
}

resolve_bin() {
    cabal build exe:umbravox >/dev/null
    cabal list-bin exe:umbravox
}

build_linux_release() {
    require_cmd cabal
    require_cmd patchelf
    require_cmd file
    require_cmd ldd
    require_cmd sha256sum

    local pkg="umbravox-${VERSION}-linux-x86_64"
    local stage="$OUT_DIR/$pkg"
    local artifact="$OUT_DIR/$pkg.tar.gz"
    local bin_path
    local interp
    local interp_base

    rm -rf "$stage"
    mkdir -p "$stage/bin" "$stage/lib"
    copy_common_docs "$stage"
    write_manifest "$stage" "linux-x86_64" "native-binary"
    write_target_note "$stage/PORTABILITY.txt" \
"UmbraVOX Linux release

This bundle is built on Linux x86_64 from the current nix-shell and repackages
the built executable with a local ELF interpreter and shared-library set.

It is intended for Linux terminal use on compatible x86_64 systems.
It is not a static binary and it is not a Windows, BSD, macOS, or DOS binary."

    bin_path="$(resolve_bin)"
    cp "$bin_path" "$stage/bin/umbravox"

    interp="$(patchelf --print-interpreter "$bin_path")"
    interp_base="$(basename "$interp")"

    mapfile -t deps < <(
        {
            printf '%s\n' "$interp"
            ldd "$bin_path" | awk '{for (i = 1; i <= NF; i++) if ($i ~ /^\//) print $i}'
        } | sort -u
    )

    for dep in "${deps[@]}"; do
        local dep_base
        dep_base="$(basename "$dep")"
        if [[ -e "$stage/lib/$dep_base" ]]; then
            continue
        fi
        cp -L "$dep" "$stage/lib/$dep_base"
        chmod u+w "$stage/lib/$dep_base" 2>/dev/null || true
    done

    patchelf \
        --set-interpreter "\$ORIGIN/../lib/$interp_base" \
        --force-rpath \
        --set-rpath "\$ORIGIN/../lib" \
        "$stage/bin/umbravox"

    cat >"$stage/run-umbravox.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
HERE="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
exec "$HERE/bin/umbravox" "$@"
EOF
    chmod +x "$stage/run-umbravox.sh"

    "$stage/lib/$interp_base" --list "$stage/bin/umbravox" >"$stage/LINKAGE.txt"
    file "$stage/bin/umbravox" >"$stage/FILE.txt"

    archive_tgz "$stage" "$artifact"
    echo "$artifact"
}

build_source_release() {
    local target="$1"
    local package_suffix="$2"
    local archive_kind="$3"
    local stage="$OUT_DIR/umbravox-${VERSION}-${package_suffix}"
    local artifact

    rm -rf "$stage"
    stage_source_tree "$stage"
    write_manifest "$stage" "$target" "source"

    case "$target" in
        windows-cli)
            write_target_note "$stage/BUILDING-WINDOWS.txt" \
"UmbraVOX Windows CLI source release

This target currently emits a source release, not a prebuilt Windows executable.

Recommended native build approach:
1. Install ghcup on Windows.
2. Install GHC and Cabal.
3. Install the required C toolchain/runtime dependencies.
4. Build with cabal on the native host."
            artifact="$OUT_DIR/umbravox-${VERSION}-${package_suffix}.zip"
            ;;
        macos-terminal)
            write_target_note "$stage/BUILDING-MACOS.txt" \
"UmbraVOX macOS terminal source release

This target currently emits a source release, not a cross-built macOS binary.

Recommended native build approach:
1. Install ghcup on macOS.
2. Install GHC and Cabal.
3. Build and run on the native host terminal."
            artifact="$OUT_DIR/umbravox-${VERSION}-${package_suffix}.tar.gz"
            ;;
        bsd-terminal)
            write_target_note "$stage/BUILDING-BSD.txt" \
"UmbraVOX BSD terminal source release

This target currently emits a source release, not a cross-built BSD binary.

Recommended native build approach:
1. Install GHC and Cabal through the BSD package system or ghcup.
2. Build on the native BSD host."
            artifact="$OUT_DIR/umbravox-${VERSION}-${package_suffix}.tar.gz"
            ;;
        freedos)
            write_target_note "$stage/FREEDOS-STATUS.txt" \
"UmbraVOX FreeDOS release note

The current Haskell implementation does not provide a native FreeDOS runtime.
This artifact is intentionally a research/source package with an explicit
unsupported-runtime note, not a claim of executable DOS support."
            artifact="$OUT_DIR/umbravox-${VERSION}-${package_suffix}.zip"
            ;;
        source)
            write_target_note "$stage/BUILDING.txt" \
"UmbraVOX generic source release

Enter nix-shell on a supported development host and use the documented
build/test/release commands."
            artifact="$OUT_DIR/umbravox-${VERSION}-${package_suffix}.tar.gz"
            ;;
        *)
            echo "unsupported source target: $target" >&2
            exit 1
            ;;
    esac

    if [[ "$archive_kind" == "zip" ]]; then
        require_cmd zip
        archive_zip "$stage" "$artifact"
    else
        archive_tgz "$stage" "$artifact"
    fi

    echo "$artifact"
}

main() {
    require_cmd git
    require_cmd tar
    require_cmd sha256sum
    mkdir -p "$OUT_DIR"

    case "$TARGET" in
        linux)
            build_linux_release
            ;;
        windows-cli)
            build_source_release "windows-cli" "windows-cli-source" "zip"
            ;;
        macos-terminal)
            build_source_release "macos-terminal" "macos-terminal-source" "tgz"
            ;;
        bsd-terminal)
            build_source_release "bsd-terminal" "bsd-terminal-source" "tgz"
            ;;
        freedos)
            build_source_release "freedos" "freedos-source" "zip"
            ;;
        source)
            build_source_release "source" "source" "tgz"
            ;;
        *)
            usage >&2
            exit 1
            ;;
    esac
}

main "$@"

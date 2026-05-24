#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/lib-release.sh"

ROOT="$UMBRAVOX_ROOT"
cd "$ROOT"

OUT_DIR="$RELEASE_OUT_DIR"
TARGET="${1:-}"
VERSION="$RELEASE_VERSION"
ABI_ARCH="${UMBRAVOX_RELEASE_ABI_ARCH:-x86_64}"
ABI_KERNEL_MIN="${UMBRAVOX_RELEASE_ABI_KERNEL_MIN:-3.10}"
ABI_GLIBC_MIN="${UMBRAVOX_RELEASE_ABI_GLIBC_MIN:-2.31}"

usage() {
    cat <<'EOF'
usage: scripts/release-package.sh <target>

targets:
  source
  linux
  appimage
  windows-cli
  macos-terminal
  bsd-terminal
  freedos
EOF
}

# Guards and staging helpers delegated to lib-release.sh.
require_cmd()         { lib_require_cmd "$@"; }
ensure_clean_tree()   { lib_ensure_clean_tree; }
ensure_tagged_release() { lib_ensure_tagged_release; }
stage_source_tree()   { lib_stage_source_tree "$@"; }

write_manifest() {
    local stage="$1"
    local target="$2"
    local artifact_kind="$3"
    lib_write_manifest "$stage" \
        "target=$target" \
        "artifact_kind=$artifact_kind" \
        "abi_arch=$ABI_ARCH" \
        "abi_kernel_min=$ABI_KERNEL_MIN" \
        "abi_glibc_min=$ABI_GLIBC_MIN"
}

copy_common_docs()    { lib_copy_common_docs "$@"; }
write_target_note()   { lib_write_target_note "$@"; }
archive_tgz()         { lib_archive_tgz "$@"; }
archive_zip()         { lib_archive_zip "$@"; }

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
    local wrapped_bin

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
    cp "$bin_path" "$stage/bin/umbravox.bin"
    wrapped_bin="$stage/bin/umbravox.bin"

    interp="$(patchelf --print-interpreter "$bin_path")"
    interp_base="$(basename "$interp")"

    mapfile -t deps < <(
        {
            printf '%s\n' "$interp"
            ldd "$bin_path" | awk '{for (i = 1; i <= NF; i++) if ($i ~ /^\//) print $i}'
        } | sort -u
    )

    copy_dep() {
        local dep="$1"
        local dep_base
        dep_base="$(basename "$dep")"
        if [[ -e "$stage/lib/$dep_base" ]]; then
            return
        fi
        cp -L "$dep" "$stage/lib/$dep_base"
        chmod u+w "$stage/lib/$dep_base" 2>/dev/null || true
    }

    # Seed closure from binary direct deps.
    for dep in "${deps[@]}"; do
        copy_dep "$dep"
    done

    # Expand transitive ELF closure for copied libs.
    local changed=1
    while [[ "$changed" -eq 1 ]]; do
        changed=0
        while IFS= read -r libpath; do
            mapfile -t transitive < <(ldd "$libpath" 2>/dev/null | awk '{for (i = 1; i <= NF; i++) if ($i ~ /^\//) print $i}' | sort -u)
            for tdep in "${transitive[@]}"; do
                local tbase
                tbase="$(basename "$tdep")"
                if [[ ! -e "$stage/lib/$tbase" ]]; then
                    copy_dep "$tdep"
                    changed=1
                fi
            done
        done < <(find "$stage/lib" -maxdepth 1 -type f)
    done

    cat >"$stage/run-umbravox.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
HERE="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
LOADER="$(find "$HERE/lib" -maxdepth 1 -type f \( -name 'ld-linux*.so*' -o -name 'ld-musl-*.so*' \) | head -n1)"
if [[ -z "${LOADER:-}" ]]; then
  echo "missing bundled dynamic loader under $HERE/lib" >&2
  exit 127
fi
exec "$LOADER" --library-path "$HERE/lib" "$HERE/bin/umbravox.bin" "$@"
EOF
    chmod +x "$stage/run-umbravox.sh"
    ln -sf run-umbravox.sh "$stage/umbravox"

    "$stage/lib/$interp_base" --list "$wrapped_bin" >"$stage/LINKAGE.txt"
    file "$wrapped_bin" >"$stage/FILE.txt"
    (cd "$stage" && find . -type f | sort | xargs sha256sum > CONTENTS.SHA256)
    sha256sum "$ROOT/scripts/release-package.sh" > "$stage/RELEASE-SCRIPT.SHA256"
    cat >>"$stage/RELEASE-MANIFEST.txt" <<EOF
contents_sha256_file=CONTENTS.SHA256
release_script_sha256_file=RELEASE-SCRIPT.SHA256
EOF

    archive_tgz "$stage" "$artifact"
    echo "$artifact"
}

build_appimage_scaffold() {
    require_cmd cabal
    require_cmd sha256sum
    require_cmd tar

    local linux_artifact
    local linux_stage
    local linux_dir
    local pkg="umbravox-${VERSION}-linux-x86_64-appimage-scaffold"
    local stage="$OUT_DIR/$pkg"
    local artifact="$OUT_DIR/$pkg.tar.gz"

    linux_artifact="$(build_linux_release)"

    linux_stage="$(mktemp -d "${TMPDIR:-/tmp}/umbravox-appimage-src.XXXXXX")"
    tar -xzf "$linux_artifact" -C "$linux_stage"
    linux_dir="$(find "$linux_stage" -maxdepth 1 -mindepth 1 -type d | head -n1)"
    if [[ -z "$linux_dir" ]]; then
        echo "unable to unpack Linux release artifact for AppImage scaffold" >&2
        exit 1
    fi

    rm -rf "$stage"
    mkdir -p "$stage/AppDir"
    cp -a "$linux_dir/." "$stage/AppDir/"

    cat >"$stage/AppDir/AppRun" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
HERE="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
exec "$HERE/run-umbravox.sh" "$@"
EOF
    chmod +x "$stage/AppDir/AppRun"

    cat >"$stage/AppDir/umbravox.desktop" <<EOF
[Desktop Entry]
Type=Application
Name=UmbraVOX
Exec=AppRun
Icon=umbravox
Terminal=true
Categories=Network;Chat;
Comment=Experimental AppImage scaffold for UmbraVOX
EOF

    write_manifest "$stage" "linux-x86_64" "experimental-appimage-scaffold"
    write_target_note "$stage/APPIMAGE-PLACEHOLDER.txt" \
"UmbraVOX experimental AppImage scaffold

This package is a non-authoritative AppImage track scaffold.
It packages the current Linux bundle contents into an AppDir-style layout,
but it does not yet claim the status of a maintained, supported AppImage
release artifact.

Smoke validation against this target is intentionally placeholder-only until
the AppImage support policy and parity evidence are proven."

    (cd "$stage" && find AppDir -type f | sort | xargs sha256sum > CONTENTS.SHA256)
    sha256sum "$ROOT/scripts/release-package.sh" > "$stage/RELEASE-SCRIPT.SHA256"
    cat >>"$stage/RELEASE-MANIFEST.txt" <<EOF
artifact_kind_note=experimental_appimage_scaffold
contents_sha256_file=CONTENTS.SHA256
release_script_sha256_file=RELEASE-SCRIPT.SHA256
EOF

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

    (cd "$stage" && find . -type f | sort | xargs sha256sum > CONTENTS.SHA256)
    sha256sum "$ROOT/scripts/release-package.sh" > "$stage/RELEASE-SCRIPT.SHA256"
    cat >>"$stage/RELEASE-MANIFEST.txt" <<EOF
contents_sha256_file=CONTENTS.SHA256
release_script_sha256_file=RELEASE-SCRIPT.SHA256
EOF

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
    ensure_clean_tree
    ensure_tagged_release

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
        appimage)
            build_appimage_scaffold
            ;;
        *)
            usage >&2
            exit 1
            ;;
    esac
}

main "$@"

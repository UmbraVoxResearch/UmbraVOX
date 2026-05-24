#!/usr/bin/env bash
# scripts/lib-release.sh — Shared release packaging functions (M20.7.3).
#
# Sourced by release-package.sh and release-package-platform.sh to
# eliminate duplicated guards, staging helpers, and archive routines.
#
# Usage (from a release script):
#   SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
#   source "${SCRIPT_DIR}/lib-release.sh"
set -euo pipefail

# --------------------------------------------------------------------------
# Version / provenance defaults
# --------------------------------------------------------------------------

UMBRAVOX_ROOT="${UMBRAVOX_ROOT:-$(pwd)}"
RELEASE_OUT_DIR="${UMBRAVOX_ROOT}/build/releases"
RELEASE_VERSION="${UMBRAVOX_RELEASE_VERSION:-$(git describe --tags --always 2>/dev/null || git rev-parse --short HEAD 2>/dev/null || echo unknown)}"
RELEASE_COMMIT="${UMBRAVOX_RELEASE_COMMIT:-$(git rev-parse HEAD 2>/dev/null || echo unknown)}"
RELEASE_STAMP="${UMBRAVOX_RELEASE_STAMP:-$(date -u +%Y%m%dT%H%M%SZ)}"

# --------------------------------------------------------------------------
# Guards
# --------------------------------------------------------------------------

# Require a command on PATH or exit.
lib_require_cmd() {
    local cmd="$1"
    if ! command -v "$cmd" >/dev/null 2>&1; then
        echo "missing required command: $cmd" >&2
        exit 1
    fi
}

# Refuse to release from a dirty worktree unless overridden.
lib_ensure_clean_tree() {
    if [[ "${UMBRAVOX_ALLOW_DIRTY_RELEASE:-0}" == "1" ]]; then
        return
    fi
    if [[ -n "$(git status --porcelain 2>/dev/null)" ]]; then
        echo "refusing release from dirty worktree; commit or stash changes, or set UMBRAVOX_ALLOW_DIRTY_RELEASE=1" >&2
        exit 1
    fi
}

# Refuse to release from an untagged commit unless overridden.
lib_ensure_tagged_release() {
    if [[ "${UMBRAVOX_ALLOW_UNTAGGED_RELEASE:-0}" == "1" ]]; then
        return
    fi
    local tag=""
    tag="$(git describe --tags --exact-match HEAD 2>/dev/null || true)"
    if [[ -z "$tag" ]]; then
        echo "refusing release from untagged commit; tag HEAD or set UMBRAVOX_ALLOW_UNTAGGED_RELEASE=1" >&2
        exit 1
    fi
}

# --------------------------------------------------------------------------
# Staging helpers
# --------------------------------------------------------------------------

# Copy the git-tracked source tree into a staging directory.
lib_stage_source_tree() {
    local stage="$1"
    mkdir -p "$stage"
    git ls-files -z | tar --null -T - -cf - | tar -xf - -C "$stage"
}

# Copy standard documentation files into a staging directory.
lib_copy_common_docs() {
    local stage="$1"
    mkdir -p "$stage/doc"
    cp README.md LICENSE LEGAL-NOTICE.md "$stage/"
    if [[ -f PUBLISHING-NOTE.md ]]; then cp PUBLISHING-NOTE.md "$stage/"; fi
    if [[ -f doc/QUICKSTART.md ]]; then cp doc/QUICKSTART.md "$stage/doc/"; fi
    if [[ -f doc/RELEASES.md ]]; then cp doc/RELEASES.md "$stage/doc/"; fi
}

# Write a target-specific note file.
lib_write_target_note() {
    local path="$1"
    local body="$2"
    cat >"$path" <<EOF
$body
EOF
}

# --------------------------------------------------------------------------
# Manifest and checksums
# --------------------------------------------------------------------------

# Write a RELEASE-MANIFEST.txt with standard provenance fields.
# Arguments: stage_dir [extra_key=value ...]
lib_write_manifest() {
    local stage="$1"; shift
    cat >"$stage/RELEASE-MANIFEST.txt" <<EOF
name=UmbraVOX
$(for kv in "$@"; do echo "$kv"; done)
version=${RELEASE_VERSION}
commit=${RELEASE_COMMIT}
timestamp_utc=${RELEASE_STAMP}
builder=$(uname -srm)
EOF
}

# Append contents + script checksums to RELEASE-MANIFEST.txt.
# Arguments: stage_dir script_path
lib_finalize_checksums() {
    local stage="$1"
    local script_path="$2"
    (cd "$stage" && find . -type f | sort | xargs sha256sum > CONTENTS.SHA256)
    sha256sum "$script_path" > "$stage/RELEASE-SCRIPT.SHA256"
    cat >>"$stage/RELEASE-MANIFEST.txt" <<EOF
contents_sha256_file=CONTENTS.SHA256
release_script_sha256_file=RELEASE-SCRIPT.SHA256
EOF
}

# --------------------------------------------------------------------------
# Archive helpers
# --------------------------------------------------------------------------

# Create a .tar.gz archive with a companion .sha256 file.
lib_archive_tgz() {
    local stage="$1"
    local artifact="$2"
    tar -C "$(dirname "$stage")" -czf "$artifact" "$(basename "$stage")"
    sha256sum "$artifact" > "$artifact.sha256"
}

# Create a .zip archive with a companion .sha256 file.
lib_archive_zip() {
    local stage="$1"
    local artifact="$2"
    rm -f "$artifact"
    (
        cd "$(dirname "$stage")"
        zip -qr "$artifact" "$(basename "$stage")"
    )
    sha256sum "$artifact" > "$artifact.sha256"
}

# SPDX-License-Identifier: Apache-2.0
# Local wrapper around nixpkgs make-disk-image.nix that replaces
# cptofs (from the lkl package, 100MB RAM, breaks on large images)
# with a mke2fs -d shim (native e2fsprogs, no RAM limit).
#
# The upstream make-disk-image.nix includes `lkl` in its binPath for
# the `cptofs` command. We override `pkgs.lkl` with a package that
# provides a `cptofs` shim using mke2fs -d instead.
#
# When nixpkgs upstream adopts mke2fs -d, delete this file and
# import make-disk-image.nix from pkgs.path directly.
{ pkgs, lib, config, diskSize ? "auto", additionalSpace ? "512M",
  format ? "raw", partitionTableType ? "legacy", copyChannel ? false,
  ... }@args:

let
  # A fake "lkl" package that provides a cptofs shim using mke2fs -d.
  # The upstream binPath includes `lkl` solely for its `cptofs` binary.
  cptofs-shim = pkgs.writeShellScriptBin "cptofs" ''
    set -euo pipefail

    # Parse cptofs arguments
    PART=""
    IMAGE=""
    while [[ $# -gt 0 ]]; do
      case "$1" in
        -p) shift ;;
        -P) PART="$2"; shift 2 ;;
        -t) shift 2 ;;
        -i) IMAGE="$2"; shift 2 ;;
        *)  break ;;
      esac
    done

    if [[ -z "$IMAGE" ]]; then
      echo "cptofs-shim: missing -i image" >&2
      exit 1
    fi

    # Collect source paths (everything except last arg which is "/")
    SOURCES=()
    while [[ $# -gt 1 ]]; do
      SOURCES+=("$1")
      shift
    done

    # Create staging dir with all sources merged
    STAGING=$(mktemp -d)
    trap "chmod -R u+w $STAGING 2>/dev/null; rm -rf $STAGING" EXIT
    for src in "''${SOURCES[@]}"; do
      cp -a "$src" "$STAGING/" 2>/dev/null || true
    done

    if [[ -n "$PART" ]]; then
      # Partitioned image: get offset and size
      eval $(${pkgs.util-linux}/bin/partx "$IMAGE" -o START,SECTORS --nr "$PART" --pairs)
      OFFSET=$((START * 512))
      SIZE_KB=$((SECTORS * 512 / 1024))
      # Re-create filesystem with -d to populate in one step
      ${pkgs.e2fsprogs}/bin/mke2fs -t ext4 -b 4096 -F -d "$STAGING" \
        "$IMAGE" -E offset="$OFFSET" "''${SIZE_KB}K" 2>/dev/null
    else
      ${pkgs.e2fsprogs}/bin/mke2fs -t ext4 -b 4096 -F -d "$STAGING" \
        "$IMAGE" 2>/dev/null
    fi
  '';

  patchedPkgs = pkgs // { lkl = cptofs-shim; };

in import (pkgs.path + "/nixos/lib/make-disk-image.nix") (args // {
  pkgs = patchedPkgs;
})

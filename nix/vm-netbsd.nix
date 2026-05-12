# nix/vm-netbsd.nix — Nix shell environment for the NetBSD QEMU VM smoke lane.
#
# This file does NOT build a NixOS guest image.  NetBSD ships cloud images
# (raw/qcow2) via cdn.netbsd.org; the nix expression here provides all
# HOST-SIDE tools required by scripts/vm-netbsd-setup.sh.
#
# Usage (via Makefile):
#   make vm-smoke-netbsd
#
# Or directly:
#   nix-shell nix/vm-netbsd.nix --run ./scripts/vm-netbsd-setup.sh
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

pkgs.mkShell {
  name = "umbravox-vm-netbsd";

  buildInputs = with pkgs; [
    # QEMU — boots the NetBSD guest
    qemu_kvm

    # Download tooling — fetches NetBSD cloud image if not cached
    wget
    curl

    # Remote access helpers used by the setup script
    openssh
    expect           # drive pkgin-based install steps non-interactively

    # Archive / checksum utilities
    gnutar
    gzip
    gnupg            # optional: verify NetBSD SHA512 signatures

    # qemu-img for image conversion (cloud images ship as raw or qcow2)
    # qemu_kvm already bundles qemu-img, listed here for documentation clarity

    # General utilities
    coreutils
    bash
    jq
  ];

  shellHook = ''
    export UMBRAVOX_ROOT="$(pwd)"
    echo "NetBSD VM smoke shell — host tools ready."
    echo "Run: ./scripts/vm-netbsd-setup.sh"
  '';
}

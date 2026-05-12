# nix/vm-openbsd.nix — Nix shell environment for the OpenBSD QEMU VM smoke lane.
#
# This file does NOT build a NixOS guest image.  OpenBSD ships its own
# install/cloud media; the nix expression here provides all HOST-SIDE tools
# (qemu, wget, openssh, expect) required by scripts/vm-openbsd-setup.sh.
#
# Usage (via Makefile):
#   make vm-smoke-openbsd
#
# Or directly:
#   nix-shell nix/vm-openbsd.nix --run ./scripts/vm-openbsd-setup.sh
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

pkgs.mkShell {
  name = "umbravox-vm-openbsd";

  buildInputs = with pkgs; [
    # QEMU — boots the OpenBSD guest
    qemu_kvm

    # Download tooling — fetches OpenBSD install image if not cached
    wget
    curl

    # Remote access helpers used by the setup script
    openssh
    expect           # drive interactive OpenBSD installer if needed

    # Archive / checksum utilities
    gnutar
    gzip
    gnupg            # optional: verify OpenBSD SHA256 signatures

    # General utilities
    coreutils
    bash
    jq
  ];

  shellHook = ''
    export UMBRAVOX_ROOT="$(pwd)"
    echo "OpenBSD VM smoke shell — host tools ready."
    echo "Run: ./scripts/vm-openbsd-setup.sh"
  '';
}

# nix/vm-dragonfly.nix — Nix shell environment for the DragonFlyBSD QEMU VM smoke lane.
#
# This file does NOT build a NixOS guest image.  DragonFlyBSD ships ISO
# installers via mirror.racket-lang.org and other mirrors.  The nix
# expression here provides HOST-SIDE tools required by
# scripts/vm-dragonfly-setup.sh.
#
# INFO: GHC availability on DragonFlyBSD
# ----------------------------------------
# As of DragonFlyBSD 6.x, GHC is present in dports (the FreeBSD-derived
# package collection ported to DragonFly) as the "lang/ghc" port, but binary
# packages are not always pre-built for every release on the public pkg mirror
# (avalon.dragonflybsd.org/packages/).  The setup script therefore checks for
# a pre-built binary package first and documents the result rather than
# failing hard; if GHC is unavailable the lane exits 0 with an INFO notice so
# CI stays green while the gap is tracked.
#
# Usage (via Makefile):
#   make vm-smoke-dragonfly   (not wired by default — lane is INFO-only)
#
# Or directly:
#   nix-shell nix/vm-dragonfly.nix --run ./scripts/vm-dragonfly-setup.sh
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

pkgs.mkShell {
  name = "umbravox-vm-dragonfly";

  buildInputs = with pkgs; [
    # QEMU — boots the DragonFlyBSD guest
    qemu_kvm

    # Download tooling — fetches DragonFlyBSD ISO if not cached
    wget
    curl

    # Remote access helpers
    openssh
    expect

    # Archive / checksum utilities
    gnutar
    gzip

    # General utilities
    coreutils
    bash
    jq
  ];

  shellHook = ''
    export UMBRAVOX_ROOT="$(pwd)"
    echo "DragonFlyBSD VM smoke shell — host tools ready."
    echo "INFO: GHC on DragonFlyBSD may not have a pre-built binary package."
    echo "      The setup script will detect availability and document the result."
    echo "Run: ./scripts/vm-dragonfly-setup.sh"
  '';
}

# Base VM tier: boots, has a shell, can mount disks, can run a binary.
# This is the minimal tier — no network, no nix daemon, no toolchain.
# Used by: Firecracker runtime, QEMU runtime, smoke guests.
#
# Tier hierarchy:
#   base → network → builder → dev
{ config, lib, modulesPath, pkgs, ... }:

{
  imports = [ ../vm-base.nix ];

  environment.systemPackages = with pkgs; [
    bashInteractive
    coreutils
    util-linux    # mount, blkid, etc.
    ncurses       # terminal apps need terminfo
    sqlite        # UmbraVOX runtime dependency
  ];
}

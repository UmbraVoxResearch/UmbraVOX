# Shared NixOS module for UmbraVOX VM images.
#
# Provides the common boilerplate every VM config needs:
#   QEMU guest profile, serial console, boot params, virtio modules,
#   root filesystem, tmpfs /work, FHS compat, minimization, stateVersion.
#
# Usage:
#   imports = [ ./vm-base.nix ];
#   environment.systemPackages = myPackages;
#
{ config, lib, modulesPath, pkgs, ... }:

{
  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

  # Serial console — auto-login root
  systemd.services."serial-getty@ttyS0".enable = true;
  services.getty.autologinUser = "root";

  boot.kernelParams = [ "console=ttyS0" "panic=1" ];
  boot.initrd.availableKernelModules = [
    "virtio_pci" "virtio_blk" "virtio_net" "ext4"
  ];

  fileSystems."/" = {
    device = "/dev/vda1";
    fsType = "ext4";
  };

  swapDevices = [];
  networking.hostName = lib.mkDefault "umbravox-vm";
  # Security: enable the firewall by default so the VM does not expose
  # services to the host network unless explicitly allowed.
  networking.firewall.enable = true;

  # Writable workspace (tmpfs, sized for build artifacts)
  fileSystems."/work" = {
    device = "tmpfs";
    fsType = "tmpfs";
    # Security: mode 0700 restricts /work to root only, preventing
    # world-writable tmpfs that could allow symlink or hardlink attacks.
    options = [ "size=8G" "mode=0700" ];
  };

  # FHS compatibility for Makefile (SHELL := /bin/bash) and shebangs
  system.activationScripts.fhsCompat = ''
    mkdir -p /bin /usr/bin
    ln -sf /run/current-system/sw/bin/bash /bin/bash
    ln -sf /run/current-system/sw/bin/sh /bin/sh
    ln -sf /run/current-system/sw/bin/env /usr/bin/env
  '';

  # Minimize image size
  documentation.enable = false;
  programs.command-not-found.enable = false;
  services.udisks2.enable = false;
  security.polkit.enable = false;
  xdg.mime.enable = false;
  xdg.icons.enable = false;
  nix.enable = false;
  # Avahi (mDNS) is pulled in by qemu-guest.nix but not needed in the
  # build VM — disable it to shrink the closure.
  services.avahi.enable = false;

  system.stateVersion = "25.05";
}

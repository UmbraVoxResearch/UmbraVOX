# NixOS VM image for aarch64 (ARM64) integration testing.
#
# This image is intended for cross-architecture testing via QEMU
# system emulation on x86_64 hosts. It is significantly slower
# than KVM-accelerated x86_64 VMs (~10-50x) because QEMU must
# emulate the ARM instruction set in software.
#
# Build (requires aarch64 builder or binfmt_misc + qemu-user):
#   nix build .#vm-image-aarch64
#
# Boot on x86_64 host:
#   qemu-system-aarch64 -machine virt -cpu cortex-a72 -m 2048 \
#     -nographic -kernel <kernel> -initrd <initrd> \
#     -append "console=ttyAMA0" -drive if=virtio,file=<rootfs>
#
# Status: STUB — requires aarch64 Nix builder or QEMU binfmt_misc
# registration to actually build. See doc/runner-classes.md.
{ pkgs ? import <nixpkgs> { system = "aarch64-linux"; } }:

let
  nixosConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

    boot.loader.grub.enable = false;
    boot.kernelParams = [ "console=ttyAMA0" "panic=1" ];
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_net" "ext4"
    ];

    fileSystems."/" = {
      device = "/dev/vda";
      fsType = "ext4";
    };

    swapDevices = [];
    networking.hostName = "umbravox-arm64";
    networking.firewall.enable = false;

    services.getty.autologinUser = "root";

    # Minimal packages for test agent
    environment.systemPackages = with pkgs; [
      bashInteractive coreutils findutils gnugrep gnused
      gnutar gzip iproute2 procps which
    ];

    fileSystems."/work" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "size=2G" "mode=1777" ];
    };

    system.activationScripts.fhsCompat = ''
      mkdir -p /bin /usr/bin
      ln -sf /run/current-system/sw/bin/bash /bin/bash
      ln -sf /run/current-system/sw/bin/sh /bin/sh
      ln -sf /run/current-system/sw/bin/env /usr/bin/env
    '';

    documentation.enable = false;
    programs.command-not-found.enable = false;
    services.udisks2.enable = false;
    security.polkit.enable = false;
    xdg.mime.enable = false;
    xdg.icons.enable = false;
    nix.enable = false;

    system.stateVersion = "25.05";
  };

  nixos = import (pkgs.path + "/nixos") {
    system = "aarch64-linux";
    configuration = nixosConfig;
  };

  image = import (pkgs.path + "/nixos/lib/make-disk-image.nix") {
    inherit pkgs;
    lib = pkgs.lib;
    config = nixos.config;
    diskSize = "auto";
    additionalSpace = "256M";
    format = "raw";
    partitionTableType = "none";
    copyChannel = false;
  };

in image

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
    imports = [ ./tiers/dev.nix ];

    # aarch64 uses UEFI, not GRUB
    boot.loader.grub.enable = false;
    # ARM serial console (ttyAMA0), overriding base's ttyS0
    boot.kernelParams = [ "console=ttyAMA0" "panic=1" ];

    # aarch64 root is the whole disk (no partition table)
    fileSystems."/" = {
      device = "/dev/vda";
      fsType = "ext4";
    };

    networking.hostName = "umbravox-arm64";

    # Additional aarch64-specific packages beyond what dev tier provides
    environment.systemPackages = with pkgs; [
      iproute2
    ];
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

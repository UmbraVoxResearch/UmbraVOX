# Minimal NixOS VM image for running the UmbraVOX binary.
#
# Unlike vm-image.nix (full dev toolchain), this image contains only
# the runtime dependencies needed to execute a pre-built umbravox
# binary.  The binary is expected on a secondary disk (/dev/vdb)
# mounted read-only at /app.
#
# Two variants are produced:
#   - Firecracker: raw rootfs (no partition table, no bootloader)
#   - QEMU:        bootable disk with GRUB + legacy partition table
#
# Build:
#   nix-build nix/vm-runtime.nix -A firecrackerRootfs
#   nix-build nix/vm-runtime.nix -A qemu
#
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  # Shared NixOS module for both Firecracker and QEMU runtime images.
  runtimeBase = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ./tiers/base.nix ];

    # Mount the application disk and exec the umbravox binary.
    systemd.services.umbravox-runtime = {
      description = "UmbraVOX runtime application";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" ];
      path = config.environment.systemPackages ++ [ pkgs.mount ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "umbravox-runtime-start" ''
          set -euo pipefail
          export PATH=/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH

          # Mount the application disk read-only
          mkdir -p /app
          mount -o ro -t ext4 /dev/vdb /app

          # Provide runtime library path for dynamically-linked deps
          export LD_LIBRARY_PATH=/app/lib

          if [ -x /app/bin/umbravox ]; then
            echo "[RUNTIME] Starting /app/bin/umbravox"
            /app/bin/umbravox; rc=$?
          else
            echo "[RUNTIME] /app/bin/umbravox not found — dropping to shell"
            exec /run/current-system/sw/bin/bash
          fi

          # Record exit status for the host to inspect
          echo "$rc" > /run/umbravox-exit-status
          exit "$rc"
        '';
        StandardOutput = "journal+console";
        StandardError = "journal+console";
        TimeoutStartSec = "3600";
      };
    };

    # Power off after the runtime service completes (success or failure).
    systemd.services.umbravox-shutdown = {
      description = "Shutdown after umbravox-runtime completes";
      wantedBy = [ "multi-user.target" ];
      after = [ "umbravox-runtime.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.systemd}/bin/systemctl poweroff";
      };
    };
  };

  # -- Firecracker variant ---------------------------------------------------

  firecrackerConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ runtimeBase ];

    # Firecracker provides its own boot mechanism; no GRUB needed.
    boot.loader.grub.enable = false;
    boot.kernelParams = [ "console=ttyS0" "panic=1" "reboot=k" ];

    # Firecracker uses virtio_mmio (not virtio_pci like QEMU).
    # Force-load these in initrd — availableKernelModules only loads on demand.
    boot.initrd.kernelModules = [ "virtio_mmio" "virtio_blk" "ext4" ];
    boot.initrd.availableKernelModules = [
      "virtio_mmio" "virtio_blk" "virtio_net" "ext4"
    ];

    # Firecracker rootfs is /dev/vda directly (no partition table).
    # Override vm-base.nix which sets /dev/vda1.
    fileSystems."/" = lib.mkForce {
      device = "/dev/vda";
      fsType = "ext4";
    };
  };

  firecrackerNixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = firecrackerConfig;
  };

  firecrackerImage = import (pkgs.path + "/nixos/lib/make-disk-image.nix") {
    inherit pkgs;
    lib = pkgs.lib;
    config = firecrackerNixos.config;
    diskSize = "auto";
    additionalSpace = "256M";
    format = "raw";
    partitionTableType = "none";
    copyChannel = false;
  };

  # -- QEMU variant -----------------------------------------------------------

  qemuConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ runtimeBase ];

    boot.loader.grub.device = "/dev/vda";
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "ext4"
    ];

    # VGA console for GUI mode (override vm-base.nix serial-only default)
    boot.kernelParams = lib.mkForce [ "console=tty0" "panic=1" ];

    # Auto-login on VGA console so the app can render its TUI there
    services.getty.autologinUser = lib.mkForce "root";
    systemd.services."getty@tty1".enable = true;

    # Wire the runtime service to VGA tty1 so the TUI app renders there
    systemd.services.umbravox-runtime.serviceConfig.StandardInput = lib.mkForce "tty";
    systemd.services.umbravox-runtime.serviceConfig.StandardOutput = lib.mkForce "tty";
    systemd.services.umbravox-runtime.serviceConfig.StandardError = lib.mkForce "tty";
    systemd.services.umbravox-runtime.serviceConfig.TTYPath = "/dev/tty1";
    systemd.services.umbravox-runtime.serviceConfig.TTYReset = true;
    systemd.services.umbravox-runtime.serviceConfig.TTYVHangup = true;
  };

  qemuNixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = qemuConfig;
  };

  qemuImage = import ./make-disk-image.nix {
    inherit pkgs;
    lib = pkgs.lib;
    config = qemuNixos.config;
    diskSize = "auto";
    additionalSpace = "512M";
    format = "raw";
    partitionTableType = "legacy";
    copyChannel = false;
  };

  # Bundle Firecracker rootfs + kernel + initrd into a single derivation.
  # Firecracker needs all three: the kernel (vmlinux), the initrd (to load
  # virtio_mmio/virtio_blk modules before mounting root), and the rootfs.
  firecrackerBundle = pkgs.runCommand "umbravox-firecracker-runtime" {
    nativeBuildInputs = [ pkgs.zstd ];
  } ''
    mkdir -p $out
    zstd -19 -T0 ${firecrackerImage}/nixos.img -o $out/rootfs.ext4.zst
    cp ${firecrackerNixos.config.system.build.kernel.dev}/vmlinux $out/vmlinux
    cp ${firecrackerNixos.config.system.build.initialRamdisk}/initrd $out/initrd
    echo -n "${firecrackerNixos.config.system.build.toplevel}/init" > $out/init-path
  '';

in {
  # Firecracker: rootfs + kernel bundled (nix-build -A firecracker)
  firecracker = firecrackerBundle;

  # Firecracker: rootfs-only (for flake compatibility)
  firecrackerRootfs = firecrackerImage;

  # Firecracker: kernel path string (for flake compatibility)
  firecrackerKernel = "${firecrackerNixos.config.system.build.kernel.dev}/vmlinux";

  # Firecracker: initrd path string (for flake compatibility)
  firecrackerInitrd = "${firecrackerNixos.config.system.build.initialRamdisk}/initrd";

  # QEMU: full bootable disk image with GRUB + partition table (for ./uv run gui)
  qemu = qemuImage;
}

# Minimal NixOS VM image for running the UmbraVOX binary.
#
# Unlike vm-image.nix (full dev toolchain), this image contains only
# the runtime dependencies needed to execute a pre-built umbravox
# binary.  The binary is expected on a secondary disk (/dev/vdb)
# mounted read-only at /app.
#
# Build:
#   nix-build nix/vm-runtime.nix -A qemu
#
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  # Shared NixOS module for QEMU runtime images.
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

          # Mount the application disk (rw so we can fix permissions)
          mkdir -p /app
          mount -o exec -t ext4 /dev/vdb /app

          # Fix permissions (genext2fs may not preserve execute bits)
          chmod +x /app/bin/umbravox 2>/dev/null || true
          chmod +x /app/lib/ld-linux-x86-64.so.2 2>/dev/null || true

          # Provide runtime library path for dynamically-linked deps
          export LD_LIBRARY_PATH=/app/lib

          if [ -f /app/bin/umbravox ]; then
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

in {
  # QEMU: full bootable disk image with GRUB + partition table (for ./uv run gui)
  qemu = qemuImage;
}

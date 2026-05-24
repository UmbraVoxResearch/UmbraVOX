# Minimal NixOS configuration for the seed VM.
#
# The seed VM downloads and caches the build closure from cache.nixos.org,
# then runs nix-build to produce the full builder VM image.  It is a
# stripped-down variant of vm-builder.nix with network enabled and no
# dev toolchain.
#
# Disks:
#   /dev/vda  — boot disk (this NixOS image, COW overlay)
#   /dev/vdb  — project source (ext2, read-only, from genext2fs)
#   /dev/vdc  — scratch disk (qcow2, 60GB) mounted at /nix-scratch
#
# 9p shares:
#   output    — host directory where the built image is deposited
#
# Network: enabled (DHCP) — seed downloads packages from cache.nixos.org
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  seedConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

    # Serial console — auto-login root
    systemd.services."serial-getty@ttyS0".enable = true;
    services.getty.autologinUser = "root";

    boot.loader.grub.device = "/dev/vda";
    boot.kernelParams = [ "console=ttyS0" "panic=1" ];
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_scsi" "virtio_net"
      "9p" "9pnet" "9pnet_virtio" "ext4"
    ];

    fileSystems."/" = {
      device = "/dev/vda1";
      fsType = "ext4";
    };

    swapDevices = [];
    networking.hostName = "umbravox-seed";
    networking.useDHCP = true;
    networking.firewall.enable = true;

    # Nix daemon enabled — seed fetches from binary cache
    nix.enable = true;
    nix.settings = {
      experimental-features = [ "nix-command" "flakes" ];
      # Keep default substituters (cache.nixos.org) — seed needs binary cache
      # Sandbox disabled: the seed VM is already QEMU-isolated.
      # With sandbox=true, nix creates build temp dirs on the boot
      # disk's /tmp which has <1GB free — too small for the 3.7GB
      # builder disk image. Without sandbox, TMPDIR redirects the
      # build to the 60GB scratch disk.
      sandbox = false;
    };

    # Minimize image size (no docs, no avahi, no polkit)
    documentation.enable = false;
    programs.command-not-found.enable = false;
    services.udisks2.enable = false;
    security.polkit.enable = false;
    xdg.mime.enable = false;
    xdg.icons.enable = false;
    services.avahi.enable = false;

    # FHS compatibility
    system.activationScripts.fhsCompat = ''
      mkdir -p /bin /usr/bin
      ln -sf /run/current-system/sw/bin/bash /bin/bash
      ln -sf /run/current-system/sw/bin/sh /bin/sh
      ln -sf /run/current-system/sw/bin/env /usr/bin/env
    '';

    environment.systemPackages = with pkgs; [
      bashInteractive
      coreutils
      findutils
      gnugrep
      gnused
      gnutar
      gzip
      which
      git
      e2fsprogs
      util-linux
      mount
    ];

    # Disable PrivateTmp for nix-daemon so our /tmp bind-mount
    # (to the scratch disk) is visible to the daemon process.
    systemd.services.nix-daemon.serviceConfig.PrivateTmp = lib.mkForce false;

    # Seed service: mount disks, run nix-build for builder image, shut down
    systemd.services.umbravox-seed-builder = {
      description = "UmbraVOX seed VM builder";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" "nix-daemon.service" "network-online.target" ];
      wants = [ "network-online.target" "nix-daemon.service" ];
      # Use 'wants' not 'requires' for nix-daemon — we stop/restart it
      # during the bind-mount overlay, and 'requires' would kill us too.
      path = config.environment.systemPackages ++ [ pkgs.nix ];
      environment = {
        HOME = "/root";
        NIX_PATH = "nixpkgs=${pkgs.path}";
      };
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        Restart = "no";
        ExecStart = pkgs.writeShellScript "umbravox-seed-builder-run" ''
          set -euo pipefail
          export PATH="/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH"

          echo "[SEED] Starting seed VM build..."

          # Mount source disk (project tree)
          mkdir -p /mnt/src
          mount -o ro /dev/vdb /mnt/src || {
              echo "[SEED] ERROR: /dev/vdb not found (source disk missing)"
              systemctl poweroff; exit 1
          }

          # Mount and format scratch disk for Nix store overlay
          mkdir -p /nix-scratch
          if ! blkid /dev/vdc >/dev/null 2>&1; then
              echo "[SEED] Formatting scratch disk..."
              mkfs.ext4 -q -L nix-scratch /dev/vdc
          fi
          mount /dev/vdc /nix-scratch

          # Bind-mount the scratch space over /nix/store so all build
          # artifacts land on the scratch disk, not the boot disk.
          mkdir -p /nix-scratch/store
          # Preserve existing store contents (from the boot image) by
          # copying them to scratch first.
          cp -a /nix/store/. /nix-scratch/store/ 2>/dev/null || true
          mount --bind /nix-scratch/store /nix/store

          # Bind-mount /tmp to scratch disk so the nix daemon's build
          # temp files (3.7GB disk image) don't fill the boot disk.
          mkdir -p /nix-scratch/tmp
          mount --bind /nix-scratch/tmp /tmp

          # Stop and start nix-daemon (not restart, to avoid systemd
          # dependency cycle that kills this service).
          # The daemon must restart AFTER the /tmp bind-mount so it
          # sees the scratch-backed /tmp.
          systemctl stop nix-daemon.socket nix-daemon.service 2>/dev/null || true
          sleep 1
          systemctl start nix-daemon.socket nix-daemon.service
          sleep 2

          # Mount 9p output share
          mkdir -p /output
          mount -t 9p -o trans=virtio,version=9p2000.L output /output 2>/dev/null || {
              echo "[SEED] ERROR: Failed to mount 9p output share"
              printf "1\n" > /tmp/seed-status
              systemctl poweroff
              exit 1
          }

          # Copy source to a writable workspace
          mkdir -p /nix-scratch/workspace
          cp -a /mnt/src/. /nix-scratch/workspace/
          cd /nix-scratch/workspace

          # Configure git safe directory (nix build needs it)
          git config --global --add safe.directory /nix-scratch/workspace

          echo "[SEED] Running nix-build for vm-builder.nix ..."
          # TMPDIR on scratch disk so build temp files (including the
          # 3.7GB raw disk image) don't fill the boot disk.
          mkdir -p /nix-scratch/tmp
          set +e
          TMPDIR=/nix-scratch/tmp nix-build /nix-scratch/workspace/nix/vm-builder.nix \
              -o /nix-scratch/workspace/result 2>&1
          BUILD_STATUS=$?
          set -e

          if [ "$BUILD_STATUS" -ne 0 ]; then
              echo "[SEED] ERROR: nix-build failed with exit $BUILD_STATUS"
              printf "%s\n" "$BUILD_STATUS" > /output/seed-status
              sync
              systemctl poweroff
              exit 1
          fi

          echo "[SEED] Build succeeded. Copying image to output share..."
          # The result is a symlink to the nix store path containing nixos.img
          if [ -L /nix-scratch/workspace/result ] && [ -e /nix-scratch/workspace/result/nixos.img ]; then
              cp /nix-scratch/workspace/result/nixos.img /output/nixos.img
              echo "[SEED] Image copied successfully."
              printf "0\n" > /output/seed-status
          else
              echo "[SEED] ERROR: Expected result/nixos.img not found"
              ls -la /nix-scratch/workspace/result/ 2>/dev/null || true
              printf "1\n" > /output/seed-status
          fi

          sync
          echo "[SEED] Done. Shutting down."
          systemctl poweroff
        '';
        StandardOutput = "journal+console";
        StandardError = "journal+console";
        TimeoutStartSec = "7200";  # 2 hours for full image build
      };
    };

    system.stateVersion = "25.05";
  };

  nixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = seedConfig;
  };

  image = import (pkgs.path + "/nixos/lib/make-disk-image.nix") {
    inherit pkgs;
    lib = pkgs.lib;
    config = nixos.config;
    diskSize = "auto";
    additionalSpace = "1024M";
    format = "raw";
    partitionTableType = "legacy";
    copyChannel = false;
  };

in image

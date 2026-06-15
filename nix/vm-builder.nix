# Single-stage NixOS builder VM for UmbraVOX.
#
# Downloads the dev toolchain closure from cache.nixos.org via a
# host-side network filter (UNIX socket, allowlist-only), then
# builds the full dev VM image natively.
#
# Disks:
#   /dev/vda  — boot disk (this NixOS image, COW overlay)
#   /dev/vdb  — project source with .git (ext2, read-only)
#   /dev/vdc  — scratch disk (qcow2, 100GB) for /nix/store overlay + build
#
# 9p shares:
#   output    — host directory where the built image is deposited
#
# Network: QEMU user-mode with restrict=on + guestfwd through
#          host-side allowlist filter (cache.nixos.org:443 only)
#
# nixpkgs is pinned from flake.lock (not the ambient `<nixpkgs>` channel) so the
# builder image is realised from the SAME nixpkgs rev that flake.lock pins for
# `.#vm-image`.  This matters because the builder evaluates `.#vm-image` offline
# (see ExecStart): it cannot fetch the locked nixpkgs source from github, so we
# seed it by building the builder from that exact rev (its source store path then
# gets copied into the builder store) and override the `.#vm-image` nixpkgs input
# to that local path.  Falls back to the ambient channel only if flake.lock is
# absent (e.g. ad-hoc `nix-build nix/vm-builder.nix` outside a checkout).
{ pkgs ? let
    lock    = builtins.fromJSON (builtins.readFile ../flake.lock);
    nixpkgs = fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
      sha256 = lock.nodes.nixpkgs.locked.narHash;
    };
  in import nixpkgs { system = "x86_64-linux"; }
}:

let
  builderConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ./tiers/builder.nix ];

    boot.loader.grub.device = "/dev/vda";
    boot.loader.timeout = 0;
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_scsi" "virtio_net"
      "9p" "9pnet" "9pnet_virtio" "ext4"
    ];

    networking.hostName = "umbravox-builder";

    # Builder-specific nix settings beyond the tier defaults
    nix.settings = {
      experimental-features = [ "nix-command" "flakes" ];
      sandbox = true;
    };

    environment.systemPackages = with pkgs; [
      findutils
      gnugrep
      gnused
      gnutar
      gzip
      zstd
      which
      gnumake
      e2fsprogs
      mount
    ];

    # Builder service: mount disks, download + build, extract, shut down
    systemd.services.umbravox-builder = {
      description = "UmbraVOX VM image builder";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" "nix-daemon.service" "network-online.target" ];
      wants = [ "network-online.target" "nix-daemon.service" ];
      path = config.environment.systemPackages ++ [ pkgs.nix ];
      environment = {
        HOME = "/root";
        NIX_PATH = "nixpkgs=${pkgs.path}";
      };
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        Restart = "no";
        ExecStart = pkgs.writeShellScript "umbravox-builder-run" ''
          set -euo pipefail
          export PATH="/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH"

          echo "[BUILDER] Starting VM image build..."

          # Mount source disk (project tree with .git)
          mkdir -p /mnt/src
          mount -o ro /dev/vdb /mnt/src || {
              echo "[BUILDER] ERROR: /dev/vdb not found (source disk missing)"
              systemctl poweroff; exit 1
          }

          # Mount and format scratch disk
          mkdir -p /nix-scratch
          if ! blkid /dev/vdc >/dev/null 2>&1; then
              echo "[BUILDER] Formatting scratch disk..."
              mkfs.ext4 -q -L nix-scratch /dev/vdc
          fi
          mount /dev/vdc /nix-scratch

          # Overlay /nix/store onto scratch disk
          mkdir -p /nix-scratch/store
          cp -a /nix/store/. /nix-scratch/store/ 2>/dev/null || true
          mount --bind /nix-scratch/store /nix/store

          # Redirect /tmp and build temp to scratch disk
          mkdir -p /nix-scratch/tmp
          mount --bind /nix-scratch/tmp /tmp

          # Restart nix-daemon to see new mounts
          systemctl stop nix-daemon.socket nix-daemon.service 2>/dev/null || true
          sleep 1
          systemctl start nix-daemon.socket nix-daemon.service
          sleep 2

          # Mount 9p output share
          mkdir -p /output
          mount -t 9p -o trans=virtio,version=9p2000.L output /output 2>/dev/null || {
              echo "[BUILDER] ERROR: Failed to mount 9p output share"
              printf "1\n" > /tmp/builder-status
              systemctl poweroff
              exit 1
          }

          # Copy source to writable workspace
          mkdir -p /nix-scratch/workspace
          cp -a /mnt/src/. /nix-scratch/workspace/
          cd /nix-scratch/workspace

          # Configure git safe directory
          git config --global --add safe.directory /nix-scratch/workspace

          echo "[BUILDER] Running nix build .#vm-image ..."
          export TMPDIR=/nix-scratch/tmp
          set +e
          # The builder VM has no network during eval (SNI allowlist permits only
          # cache.nixos.org, and that is unreachable here too).  Flake input
          # resolution ignores NIX_PATH, so .#vm-image would try to fetch the
          # nixpkgs source tarball from github and fail offline.  Override the
          # nixpkgs input to the local store path the builder was realised from
          # (${pkgs.path} is interpolated here, making it a runtime dependency of
          # the builder image and thus guaranteed present in this store).  It is
          # the same nixpkgs rev that flake.lock pins, so evaluation is identical.
          nix build \
              --option build-dir /nix-scratch/tmp \
              --option sandbox-build-dir /build \
              --override-input nixpkgs ${pkgs.path} \
              -L .#vm-image \
              -o /nix-scratch/workspace/result 2>&1
          BUILD_STATUS=$?
          set -e

          if [ "$BUILD_STATUS" -ne 0 ]; then
              echo "[BUILDER] ERROR: nix build failed with exit $BUILD_STATUS"
              printf "%s\n" "$BUILD_STATUS" > /output/builder-status
              sync
              systemctl poweroff
              exit 1
          fi

          echo "[BUILDER] Build succeeded. Compressing and copying image..."
          if [ -L /nix-scratch/workspace/result ] && [ -e /nix-scratch/workspace/result/nixos.img ]; then
              zstd -T0 -3 /nix-scratch/workspace/result/nixos.img -o /output/nixos.img.zst 2>&1
              echo "[BUILDER] Image compressed and copied successfully."
              printf "0\n" > /output/builder-status
          else
              echo "[BUILDER] ERROR: Expected result/nixos.img not found"
              ls -la /nix-scratch/workspace/result/ 2>/dev/null || true
              printf "1\n" > /output/builder-status
          fi

          sync
          echo "[BUILDER] Done. Shutting down."
          systemctl poweroff
        '';
        StandardOutput = "journal+console";
        StandardError = "journal+console";
        TimeoutStartSec = "7200";
      };
    };
  };

  nixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = builderConfig;
  };

  image = import ./make-disk-image.nix {
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

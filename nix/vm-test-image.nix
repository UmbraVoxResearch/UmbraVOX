# Minimal NixOS VM image for UmbraVOX integration test agents.
#
# Contains only runtime dependencies (glibc, bash, networking tools).
# NO GHC, NO cabal, NO F*, NO Z3 — agents run the pre-built release bundle.
#
# Build:  nix build .#vm-test-image
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  nixosConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ./tiers/base.nix ];

    boot.loader.grub.device = "/dev/vda";
    # Add virtio_scsi on top of the modules provided by vm-base.nix
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_scsi" "virtio_net" "ext4"
    ];

    networking.hostName = "umbravox-agent";

    # Test-specific runtime packages (no build tools)
    environment.systemPackages = with pkgs; [
      findutils
      gnugrep
      gnused
      gnutar
      gzip
      iproute2
      iputils
      netcat-gnu
      procps
      which
      tcpdump
      microsocks
    ];

    # Test agents need less workspace than build VMs
    fileSystems."/work" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "size=2G" "mode=0700" ];
    };

    # Auto-run integration test agent on boot
    systemd.services.umbravox-agent = {
      description = "UmbraVOX integration test agent";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" "network.target" ];
      path = config.environment.systemPackages;
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "umbravox-agent-run" ''
          set -euo pipefail
          export PATH=/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH

          # Mount bundle disk (/dev/vdb)
          mkdir -p /mnt/bundle
          mount -o ro /dev/vdb /mnt/bundle 2>/dev/null || true

          # Extract and run the integration agent script if present
          if [ -f /mnt/bundle/scripts/vm-integration-agent.sh ]; then
            exec bash /mnt/bundle/scripts/vm-integration-agent.sh
          else
            echo "AGENT_RESULT=FAIL (no integration script found)"
          fi
        '';
        StandardOutput = "journal+console";
        StandardError = "journal+console";
        TimeoutStartSec = "300";
      };
    };

    # Shut down after agent completes
    systemd.services.umbravox-agent-shutdown = {
      description = "Shutdown after agent";
      wantedBy = [ "multi-user.target" ];
      after = [ "umbravox-agent.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.systemd}/bin/systemctl poweroff";
      };
    };

  };

  nixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = nixosConfig;
  };

  image = import (pkgs.path + "/nixos/lib/make-disk-image.nix") {
    inherit pkgs;
    lib = pkgs.lib;
    config = nixos.config;
    diskSize = "auto";
    additionalSpace = "256M";
    format = "raw";
    partitionTableType = "legacy";
    copyChannel = false;
  };

in image

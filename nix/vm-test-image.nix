# Minimal NixOS VM image for UmbraVOX integration test agents.
#
# Contains only runtime dependencies (glibc, bash, networking tools).
# NO GHC, NO cabal, NO F*, NO Z3 — agents run the pre-built release bundle.
#
# Build:  nix build .#vm-test-image
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  nixosConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

    boot.loader.grub.device = "/dev/vda";
    boot.kernelParams = [ "console=ttyS0" "panic=1" ];
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_scsi" "virtio_net" "ext4"
    ];

    fileSystems."/" = {
      device = "/dev/vda1";
      fsType = "ext4";
    };

    swapDevices = [];
    networking.hostName = "umbravox-agent";
    networking.firewall.enable = false;

    # Serial console
    systemd.services."serial-getty@ttyS0".enable = true;
    services.getty.autologinUser = "root";

    # Minimal runtime packages (no build tools)
    environment.systemPackages = with pkgs; [
      bashInteractive
      coreutils
      findutils
      gnugrep
      gnused
      gnutar
      gzip
      iproute2
      iputils
      procps
      util-linux
      which
      tcpdump
    ];

    # Workspace for bundle extraction
    fileSystems."/work" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "size=2G" "mode=1777" ];
    };

    # FHS compatibility
    system.activationScripts.fhsCompat = ''
      mkdir -p /bin /usr/bin
      ln -sf /run/current-system/sw/bin/bash /bin/bash
      ln -sf /run/current-system/sw/bin/sh /bin/sh
      ln -sf /run/current-system/sw/bin/env /usr/bin/env
    '';

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

    # Minimize image size
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

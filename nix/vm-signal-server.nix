# NixOS VM for self-hosted Signal-Server (AGPL)
# Used for wire-compatibility testing of UmbraVOX's Signal bridge plugin.
# All services run locally — no external Signal servers contacted.
#
# Build:
#   nix-build nix/vm-signal-server.nix -A qemu -o build/vm-signal-server/image
#
# The output is a raw disk image: result/nixos.raw
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  nixosConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ./vm-base.nix ];

    boot.loader.grub.device = "/dev/vda";
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_scsi" "virtio_net" "ext4"
    ];

    networking.hostName = "umbravox-signal";

    # PostgreSQL for account storage
    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_15;
      # Signal-Server needs specific databases
      initialScript = pkgs.writeText "signal-db-init.sql" ''
        CREATE DATABASE signal;
        CREATE DATABASE signal_abuse;
      '';
    };

    # Redis for cache and pubsub
    services.redis.servers.signal = {
      enable = true;
      port = 6379;
    };

    # ZooKeeper for coordination
    services.zookeeper = {
      enable = true;
    };

    # Java runtime for Signal-Server
    environment.systemPackages = with pkgs; [
      jdk21_headless
      curl
      jq
    ];

    # NOTE: Signal-Server JAR and DynamoDB-local setup are complex.
    # For now, create a scaffold that boots the services and provides
    # a health-check endpoint. The actual Signal-Server JAR integration
    # will be added incrementally.

    # Placeholder: Signal-Server systemd service
    systemd.services.signal-server-placeholder = {
      description = "Signal-Server placeholder (services ready)";
      wantedBy = [ "multi-user.target" ];
      after = [ "postgresql.service" "redis-signal.service" "zookeeper.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "signal-server-check" ''
          echo "Signal-Server VM services:"
          echo "  PostgreSQL: $(systemctl is-active postgresql)"
          echo "  Redis: $(systemctl is-active redis-signal)"
          echo "  ZooKeeper: $(systemctl is-active zookeeper)"
          echo "  Java: $(java -version 2>&1 | head -1)"
          echo ""
          echo "Signal-Server JAR not yet integrated."
          echo "Services are ready for integration testing."
        '';
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
    additionalSpace = "2048M";
    format = "raw";
    partitionTableType = "legacy";
    copyChannel = false;
  };
in {
  qemu = image;
}

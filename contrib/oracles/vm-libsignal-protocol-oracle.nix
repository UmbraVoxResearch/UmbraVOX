# NixOS VM for libsignal-protocol differential testing oracle.
#
# Builds Signal's libsignal (Rust) from the vendored source tree in
# contrib/oracles/src/libsignal/.  At boot the oracle CLI generates test
# vectors, writes them to /output, and shuts down.  No network access at
# runtime -- all source trees are vendored and compiled during the Nix build.
#
# Build (inside builder VM):
#   nix-build contrib/oracles/vm-libsignal-protocol-oracle.nix
#
# The output is a raw disk image: result/nixos.raw
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  # ---------------------------------------------------------------------------
  # libsignal source (vendored in contrib/oracles/src/)
  # ---------------------------------------------------------------------------
  libsignalSrc = ./src/libsignal;

  # ---------------------------------------------------------------------------
  # Build libsignal's Rust workspace (the parts we need for oracle vectors)
  # ---------------------------------------------------------------------------
  libsignal = pkgs.rustPlatform.buildRustPackage {
    pname   = "libsignal-oracle";
    version = "0.0.0-oracle";
    src     = libsignalSrc;

    cargoLock.lockFile = "${libsignalSrc}/Cargo.lock";

    nativeBuildInputs = with pkgs; [
      cmake
      protobuf
      pkg-config
    ];

    buildInputs = with pkgs; [
      openssl
    ];

    # Only build the workspace members we care about for vector generation.
    # Adjust cargoBuildFlags once the oracle CLI crate exists.
    cargoBuildFlags = [
      # "--package" "libsignal-oracle"   # uncomment when crate is added
    ];

    # Tests are not needed in the oracle image -- skip to save build time.
    doCheck = false;

    meta = with pkgs.lib; {
      description = "libsignal oracle binary for UmbraVOX differential testing";
      license     = licenses.agpl3Plus;
    };
  };

  # ---------------------------------------------------------------------------
  # Packages available inside the VM
  # ---------------------------------------------------------------------------
  oraclePkgs = with pkgs; [
    libsignal
    bashInteractive
    coreutils
    jq              # handy for inspecting JSON vectors
    util-linux
  ];

  # ---------------------------------------------------------------------------
  # NixOS configuration
  # ---------------------------------------------------------------------------
  nixosConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ../../nix/tiers/base.nix ];

    boot.loader.grub.device = "/dev/vda";

    # Override hostname to identify oracle VM
    networking.hostName = "umbravox-libsignal-oracle";

    # ---- Network deny-all at runtime ----------------------------------------
    # The VM must never reach the network once booted; all deps are baked in.
    networking.firewall.enable = true;
    networking.firewall.allowedTCPPorts = [];
    networking.firewall.allowedUDPPorts = [];
    # Drop everything outbound (iptables OUTPUT chain default policy)
    networking.firewall.extraCommands = ''
      iptables -P OUTPUT DROP
      iptables -A OUTPUT -o lo -j ACCEPT
    '';

    environment.systemPackages = oraclePkgs;

    # ---- Output volume (host extracts vectors from here) --------------------
    fileSystems."/output" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "size=512M" "mode=0700" ];
    };

    # ---- Oracle boot service ------------------------------------------------
    # Runs the oracle CLI, writes test vectors to /output, then triggers
    # shutdown.
    systemd.services.libsignal-oracle = {
      description = "libsignal protocol oracle -- generate test vectors";
      wantedBy = [ "multi-user.target" ];
      after    = [ "local-fs.target" ];
      path     = oraclePkgs;
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "libsignal-oracle-run" ''
          set -euo pipefail
          export PATH=/run/current-system/sw/bin:$PATH

          echo "=== libsignal oracle: generating test vectors ==="

          # Mount the command disk if present (carries oracle CLI config)
          mkdir -p /mnt/cmd
          mount -o ro /dev/vdb /mnt/cmd 2>/dev/null || true

          # Run the oracle binary.
          # TODO: uncomment / adjust once the oracle CLI crate exists.
          # libsignal-oracle generate \
          #   --config /mnt/cmd/oracle.toml \
          #   --output /output/libsignal-vectors.json

          echo "=== libsignal oracle: done ==="
        '';
        StandardOutput = "journal+console";
        StandardError  = "journal+console";
        TimeoutStartSec = "600";
      };
    };

    # ---- Shutdown after oracle completes ------------------------------------
    systemd.services.libsignal-oracle-shutdown = {
      description = "Shutdown after libsignal oracle";
      wantedBy = [ "multi-user.target" ];
      after    = [ "libsignal-oracle.service" ];
      serviceConfig = {
        Type     = "oneshot";
        ExecStart = "${pkgs.systemd}/bin/systemctl poweroff";
      };
    };
  };

  # ---------------------------------------------------------------------------
  # Build the disk image
  # ---------------------------------------------------------------------------
  nixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = nixosConfig;
  };

  image = import ../../nix/make-disk-image.nix {
    inherit pkgs;
    lib   = pkgs.lib;
    config = nixos.config;
    diskSize          = "auto";
    additionalSpace   = "1024M";
    format            = "raw";
    partitionTableType = "legacy";
    copyChannel       = false;
  };

in image

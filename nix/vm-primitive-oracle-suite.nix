# NixOS VM for multi-library cryptographic primitive oracle suite.
#
# Bundles several independent C/Rust crypto libraries so the oracle CLI can
# exercise the same primitives (X25519, Ed25519, AES-GCM, ChaCha20-Poly1305,
# HKDF, Kyber/ML-KEM, ...) across multiple implementations and compare output
# vectors against UmbraVOX's own code.
#
# At boot the oracle CLI generates vectors, writes them to /output, and shuts
# down.  No network access at runtime -- every library source tree is fetched
# and compiled during the Nix build.
#
# Build:
#   nix-build nix/vm-primitive-oracle-suite.nix
#
# The output is a raw disk image: result/nixos.raw
#
# NOTE: placeholder hashes -- this will not build until they are filled in.
#       Run `nix-prefetch-git` or `nix-prefetch-url --unpack` to obtain real
#       values.
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  # ===========================================================================
  # Library sources (pinned)
  # ===========================================================================

  # -- Monocypher (compact, auditable C crypto) --------------------------------
  # Available in some nixpkgs channels; fetch from GitHub for a pinned version.
  monocypherSrc = pkgs.fetchFromGitHub {
    owner  = "LoupVaillant";
    repo   = "Monocypher";
    # TODO: replace with real rev (e.g. tag "4.0.2")
    rev    = "0000000000000000000000000000000000000000";
    # TODO: replace with real hash -- run:
    #   nix-prefetch-git --url https://github.com/LoupVaillant/Monocypher \
    #       --rev <tag-or-commit>
    sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  # -- HACL* (formally verified C/ASM from Project Everest) --------------------
  haclStarSrc = pkgs.fetchFromGitHub {
    owner  = "hacl-star";
    repo   = "hacl-star";
    # TODO: replace with real rev
    rev    = "0000000000000000000000000000000000000000";
    # TODO: replace with real hash
    sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  # -- BoringSSL (Google's OpenSSL fork) ---------------------------------------
  boringSslSrc = pkgs.fetchFromGitHub {
    owner  = "google";
    repo   = "boringssl";
    # TODO: replace with real rev
    rev    = "0000000000000000000000000000000000000000";
    # TODO: replace with real hash
    sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  # -- PQClean (clean reference PQ implementations) ----------------------------
  pqcleanSrc = pkgs.fetchFromGitHub {
    owner  = "PQClean";
    repo   = "PQClean";
    # TODO: replace with real rev
    rev    = "0000000000000000000000000000000000000000";
    # TODO: replace with real hash
    sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  # -- liboqs (Open Quantum Safe -- ML-KEM / ML-DSA reference) -----------------
  liboqsSrc = pkgs.fetchFromGitHub {
    owner  = "open-quantum-safe";
    repo   = "liboqs";
    # TODO: replace with real rev
    rev    = "0000000000000000000000000000000000000000";
    # TODO: replace with real hash
    sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  # ===========================================================================
  # Library derivations
  # ===========================================================================

  # -- Monocypher (simple make-based build) ------------------------------------
  monocypher = pkgs.stdenv.mkDerivation {
    pname   = "monocypher";
    version = "4.0.2-oracle";
    src     = monocypherSrc;

    nativeBuildInputs = [ pkgs.gnumake ];

    buildPhase = ''
      make -C src
    '';

    installPhase = ''
      mkdir -p $out/lib $out/include
      cp src/monocypher.o $out/lib/  # static object; link into oracle CLI
      cp src/monocypher.h $out/include/
    '';

    meta.description = "Monocypher -- compact crypto (oracle build)";
  };

  # -- HACL* (cmake build for the C extracted code) ----------------------------
  haclStar = pkgs.stdenv.mkDerivation {
    pname   = "hacl-star";
    version = "0.0.0-oracle";
    src     = haclStarSrc;

    nativeBuildInputs = with pkgs; [ cmake gnumake ];

    # HACL*'s dist/gcc-compatible directory has a standalone cmake build.
    cmakeDir = "../dist/gcc-compatible";

    meta.description = "HACL* verified crypto (oracle build)";
  };

  # -- BoringSSL (cmake build) -------------------------------------------------
  boringSsl = pkgs.stdenv.mkDerivation {
    pname   = "boringssl";
    version = "0.0.0-oracle";
    src     = boringSslSrc;

    nativeBuildInputs = with pkgs; [ cmake go perl gnumake ];

    cmakeFlags = [
      "-DBUILD_SHARED_LIBS=OFF"
    ];

    # BoringSSL does not install by default; copy artifacts manually.
    installPhase = ''
      mkdir -p $out/lib $out/include
      cp ssl/libssl.a crypto/libcrypto.a $out/lib/
      cp -r ../include/openssl $out/include/
    '';

    meta.description = "BoringSSL (oracle build)";
  };

  # -- PQClean (header-only / per-algorithm make) ------------------------------
  pqclean = pkgs.stdenv.mkDerivation {
    pname   = "pqclean";
    version = "0.0.0-oracle";
    src     = pqcleanSrc;

    nativeBuildInputs = [ pkgs.gnumake ];

    # Build only the algorithms the oracle needs.
    # TODO: adjust targets once the oracle CLI defines which PQ algos to test.
    buildPhase = ''
      echo "PQClean: placeholder build -- adjust once oracle CLI is ready"
    '';

    installPhase = ''
      mkdir -p $out/include $out/lib
      # TODO: install selected algorithm .a / .h files
    '';

    meta.description = "PQClean reference PQ implementations (oracle build)";
  };

  # -- liboqs (cmake) ----------------------------------------------------------
  liboqs = pkgs.stdenv.mkDerivation {
    pname   = "liboqs";
    version = "0.0.0-oracle";
    src     = liboqsSrc;

    nativeBuildInputs = with pkgs; [ cmake gnumake pkg-config ];

    cmakeFlags = [
      "-DBUILD_SHARED_LIBS=OFF"
      "-DOQS_BUILD_ONLY_LIB=ON"
    ];

    meta.description = "liboqs -- Open Quantum Safe (oracle build)";
  };

  # ===========================================================================
  # Packages available inside the VM
  # ===========================================================================
  oraclePkgs = [
    # Crypto libraries (from nixpkgs)
    pkgs.libsodium

    # Crypto libraries (built from source above)
    monocypher
    haclStar
    boringSsl
    pqclean
    liboqs

    # Rust toolchain for the oracle CLI itself
    pkgs.rustc
    pkgs.cargo

    # Build tools needed if oracle CLI links against C libs at runtime
    pkgs.pkg-config
    pkgs.gnumake
    pkgs.gcc

    # Shell / utilities
    pkgs.bashInteractive
    pkgs.coreutils
    pkgs.jq
    pkgs.util-linux
  ];

  # ===========================================================================
  # NixOS configuration
  # ===========================================================================
  nixosConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ./vm-base.nix ];

    boot.loader.grub.device = "/dev/vda";

    # Override hostname to identify oracle VM
    networking.hostName = "umbravox-primitive-oracle";

    # ---- Network deny-all at runtime ----------------------------------------
    networking.firewall.enable = true;
    networking.firewall.allowedTCPPorts = [];
    networking.firewall.allowedUDPPorts = [];
    networking.firewall.extraCommands = ''
      iptables -P OUTPUT DROP
      iptables -A OUTPUT -o lo -j ACCEPT
    '';

    environment.systemPackages = oraclePkgs;

    # ---- Output volume (host extracts vectors from here) --------------------
    fileSystems."/output" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "size=512M" "mode=1777" ];
    };

    # ---- Oracle boot service ------------------------------------------------
    # Runs the primitive oracle CLI, writes test vectors to /output, then
    # triggers shutdown.
    systemd.services.primitive-oracle = {
      description = "Crypto primitive oracle suite -- generate test vectors";
      wantedBy = [ "multi-user.target" ];
      after    = [ "local-fs.target" ];
      path     = oraclePkgs;
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "primitive-oracle-run" ''
          set -euo pipefail
          export PATH=/run/current-system/sw/bin:$PATH

          echo "=== primitive oracle suite: generating test vectors ==="

          # Mount the command disk if present (carries oracle CLI config)
          mkdir -p /mnt/cmd
          mount -o ro /dev/vdb /mnt/cmd 2>/dev/null || true

          # Run the oracle binary for each library backend.
          # TODO: uncomment / adjust once the oracle CLI crate exists.
          # primitive-oracle generate \
          #   --backends libsodium,monocypher,hacl-star,boringssl,pqclean,liboqs \
          #   --config /mnt/cmd/oracle.toml \
          #   --output /output/primitive-vectors.json

          echo "=== primitive oracle suite: done ==="
        '';
        StandardOutput = "journal+console";
        StandardError  = "journal+console";
        TimeoutStartSec = "600";
      };
    };

    # ---- Shutdown after oracle completes ------------------------------------
    systemd.services.primitive-oracle-shutdown = {
      description = "Shutdown after primitive oracle suite";
      wantedBy = [ "multi-user.target" ];
      after    = [ "primitive-oracle.service" ];
      serviceConfig = {
        Type     = "oneshot";
        ExecStart = "${pkgs.systemd}/bin/systemctl poweroff";
      };
    };
  };

  # ===========================================================================
  # Build the disk image
  # ===========================================================================
  nixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = nixosConfig;
  };

  image = import (pkgs.path + "/nixos/lib/make-disk-image.nix") {
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

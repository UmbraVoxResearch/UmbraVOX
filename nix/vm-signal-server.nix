# NixOS VM for self-hosted Signal-Server (AGPL)
# Used for wire-compatibility testing of UmbraVOX's Signal bridge plugin.
# All services run locally — no external Signal servers contacted.
#
# Services:  PostgreSQL, Redis (local-mode), DynamoDB-local, ZooKeeper,
#            Signal-Server (Dropwizard fat JAR).
#
# Build:
#   nix-build nix/vm-signal-server.nix -A qemu -o build/vm-signal-server/image
#
# The output is a raw disk image: result/nixos.raw
#
# NOTE: placeholder hashes — this will not build until they are filled in.
#       Run the build once and Nix will print the expected hash.  Alternatively:
#         nix-prefetch-url --unpack <url>
#         nix-prefetch-git --url https://github.com/signalapp/Signal-Server --rev <tag>
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  # ---------------------------------------------------------------------------
  # Signal-Server source (pinned)
  # ---------------------------------------------------------------------------
  # Signal-Server uses date-based tags: v20260519.1.0, v20260518.0.0, etc.
  # Pin to a specific tag for reproducibility.
  signalServerSrc = pkgs.fetchFromGitHub {
    owner  = "signalapp";
    repo   = "Signal-Server";
    rev    = "v20260519.1.0";
    # TODO: replace with real hash — run:
    #   nix-prefetch-git --url https://github.com/signalapp/Signal-Server \
    #       --rev v20260519.1.0
    sha256 = pkgs.lib.fakeSha256;
  };

  # ---------------------------------------------------------------------------
  # Signal-Server fat JAR (built from source via Maven)
  # ---------------------------------------------------------------------------
  # The server is a Dropwizard app.  Maven shade plugin produces an uber-JAR
  # in service/target/TextSecureServer-*.jar with main class
  # org.whispersystems.textsecuregcm.WhisperServerService.
  #
  # FoundationDB client library is a build-time (and runtime) dependency.
  signalServerJar = pkgs.stdenv.mkDerivation {
    pname   = "signal-server";
    version = "20260519.1.0";
    src     = signalServerSrc;

    nativeBuildInputs = with pkgs; [
      maven
      jdk21_headless
      protobuf
      pkg-config
    ];

    buildInputs = with pkgs; [
      foundationdb71   # libfdb_c.so — required by Signal-Server
    ];

    # Maven needs a local repo; pre-populate during the fixed-output fetch
    # phase if using a FOD pattern.  For now, allow network during build
    # (impure) or use a pre-built JAR fetched separately.
    buildPhase = ''
      export JAVA_HOME=${pkgs.jdk21_headless}
      export FDB_LIBRARY_PATH=${pkgs.foundationdb71}/lib
      # Skip tests — we only need the fat JAR for wire-compat testing.
      ./mvnw -B -ntp -pl service -am package -DskipTests \
        -Dmaven.javadoc.skip=true
    '';

    installPhase = ''
      mkdir -p $out/lib
      cp service/target/TextSecureServer-*.jar $out/lib/signal-server.jar
    '';

    meta = with pkgs.lib; {
      description = "Signal-Server fat JAR for UmbraVOX wire-compat testing";
      license     = licenses.agpl3Plus;
    };
  };

  # ---------------------------------------------------------------------------
  # DynamoDB-local (Amazon's standalone JAR for local development)
  # ---------------------------------------------------------------------------
  dynamodbLocalTarball = pkgs.fetchurl {
    url    = "https://d1ni2b6xgvw0s0.cloudfront.net/v2.x/dynamodb_local_latest.tar.gz";
    # TODO: replace with real hash — run:
    #   nix-prefetch-url https://d1ni2b6xgvw0s0.cloudfront.net/v2.x/dynamodb_local_latest.tar.gz
    sha256 = pkgs.lib.fakeSha256;
  };

  dynamodbLocal = pkgs.stdenv.mkDerivation {
    pname   = "dynamodb-local";
    version = "latest";
    src     = dynamodbLocalTarball;

    nativeBuildInputs = [ pkgs.gnutar pkgs.gzip ];

    unpackPhase = ''
      mkdir -p src
      tar xzf $src -C src
    '';

    installPhase = ''
      mkdir -p $out
      cp -r src/* $out/
    '';
  };

  # ---------------------------------------------------------------------------
  # Configuration files baked into the image
  # ---------------------------------------------------------------------------
  signalServerConfig = ./signal-server-config.yml;
  signalSecretsBundle = ./signal-server-secrets.yml;

  # ---------------------------------------------------------------------------
  # NixOS configuration
  # ---------------------------------------------------------------------------
  nixosConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ./vm-base.nix ];

    boot.loader.grub.device = "/dev/vda";
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_scsi" "virtio_net" "ext4"
    ];

    networking.hostName = "umbravox-signal";

    # ---- PostgreSQL (account storage, abuse DB) -----------------------------
    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_15;
      initialScript = pkgs.writeText "signal-db-init.sql" ''
        CREATE DATABASE signal;
        CREATE DATABASE signal_abuse;
      '';
    };

    # ---- Redis (cache, pubsub — Signal-Server test config uses local mode) --
    services.redis.servers.signal = {
      enable = true;
      port = 6379;
    };

    # ---- ZooKeeper (coordination) -------------------------------------------
    services.zookeeper = {
      enable = true;
    };

    # ---- System packages ----------------------------------------------------
    environment.systemPackages = with pkgs; [
      jdk21_headless
      curl
      jq
    ];

    # ---- DynamoDB-local systemd service -------------------------------------
    systemd.services.dynamodb-local = {
      description = "DynamoDB Local";
      wantedBy    = [ "multi-user.target" ];
      after       = [ "network.target" ];
      serviceConfig = {
        Type             = "simple";
        WorkingDirectory = "${dynamodbLocal}";
        ExecStart        = "${pkgs.jdk21_headless}/bin/java -Djava.library.path=${dynamodbLocal}/DynamoDBLocal_lib -jar ${dynamodbLocal}/DynamoDBLocal.jar -sharedDb -port 8000";
        Restart          = "on-failure";
        RestartSec       = "5";
      };
    };

    # ---- Deploy config files ------------------------------------------------
    system.activationScripts.signalServerConfig = ''
      mkdir -p /etc/signal-server
      cp ${signalServerConfig}  /etc/signal-server/config.yml
      cp ${signalSecretsBundle} /etc/signal-server/secrets.yml
      chmod 600 /etc/signal-server/secrets.yml
    '';

    # ---- Signal-Server systemd service --------------------------------------
    # Main class: org.whispersystems.textsecuregcm.WhisperServerService
    # Dropwizard command: server <config.yml>
    systemd.services.signal-server = {
      description = "Signal-Server (wire-compat test instance)";
      wantedBy    = [ "multi-user.target" ];
      after       = [
        "postgresql.service"
        "redis-signal.service"
        "zookeeper.service"
        "dynamodb-local.service"
      ];
      wants       = [
        "postgresql.service"
        "redis-signal.service"
        "zookeeper.service"
        "dynamodb-local.service"
      ];
      environment = {
        JAVA_HOME          = "${pkgs.jdk21_headless}";
        # FoundationDB client library path (required at runtime)
        LD_LIBRARY_PATH    = "${pkgs.foundationdb71}/lib";
      };
      serviceConfig = {
        Type             = "simple";
        ExecStart        = pkgs.writeShellScript "signal-server-start" ''
          set -euo pipefail

          echo "=== Signal-Server: starting ==="
          echo "  Config:  /etc/signal-server/config.yml"
          echo "  Secrets: /etc/signal-server/secrets.yml"

          # Wait for DynamoDB-local to be reachable
          for i in $(seq 1 30); do
            if ${pkgs.curl}/bin/curl -sf http://localhost:8000/ >/dev/null 2>&1; then
              break
            fi
            echo "  Waiting for DynamoDB-local ($i/30)..."
            sleep 1
          done

          # Wait for PostgreSQL to accept connections
          for i in $(seq 1 30); do
            if ${pkgs.postgresql_15}/bin/pg_isready -q; then
              break
            fi
            echo "  Waiting for PostgreSQL ($i/30)..."
            sleep 1
          done

          exec ${pkgs.jdk21_headless}/bin/java \
            -Dsecrets.bundle.filename=/etc/signal-server/secrets.yml \
            -Xmx512m \
            -jar ${signalServerJar}/lib/signal-server.jar \
            server \
            /etc/signal-server/config.yml
        '';
        Restart          = "on-failure";
        RestartSec       = "10";
        TimeoutStartSec  = "120";
        StandardOutput   = "journal+console";
        StandardError    = "journal+console";
      };
    };

    # ---- Health check (runs after signal-server is up) ----------------------
    systemd.services.signal-server-healthcheck = {
      description = "Signal-Server health check";
      wantedBy    = [ "multi-user.target" ];
      after       = [ "signal-server.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "signal-server-healthcheck" ''
          echo "Signal-Server VM services:"
          echo "  PostgreSQL:    $(systemctl is-active postgresql)"
          echo "  Redis:         $(systemctl is-active redis-signal)"
          echo "  ZooKeeper:     $(systemctl is-active zookeeper)"
          echo "  DynamoDB-local:$(systemctl is-active dynamodb-local)"
          echo "  Signal-Server: $(systemctl is-active signal-server)"
          echo "  Java:          $(java -version 2>&1 | head -1)"
          echo ""

          # Probe the Dropwizard admin health endpoint
          for i in $(seq 1 60); do
            if ${pkgs.curl}/bin/curl -sf http://localhost:8081/healthcheck >/dev/null 2>&1; then
              echo "Signal-Server admin endpoint is UP."
              ${pkgs.curl}/bin/curl -s http://localhost:8081/healthcheck | ${pkgs.jq}/bin/jq .
              exit 0
            fi
            sleep 2
          done
          echo "WARNING: Signal-Server admin endpoint not reachable after 120s."
          echo "Check: journalctl -u signal-server"
        '';
        TimeoutStartSec = "180";
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

  image = import (pkgs.path + "/nixos/lib/make-disk-image.nix") {
    inherit pkgs;
    lib    = pkgs.lib;
    config = nixos.config;
    diskSize          = "auto";
    additionalSpace   = "4096M";   # larger: JAR + DynamoDB + runtime
    format            = "raw";
    partitionTableType = "legacy";
    copyChannel       = false;
  };
in {
  qemu = image;
}

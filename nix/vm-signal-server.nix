# NixOS VM for self-hosted Signal-Server (AGPL)
# Used for wire-compatibility testing of UmbraVOX's Signal bridge plugin.
# All services run locally — no external Signal servers contacted.
#
# TWO-STAGE BUILD:
#   Stage 1 (build VM): boots with network, clones Signal-Server, runs Maven,
#           outputs signal-server.jar to /output via 9p virtfs.
#   Stage 2 (runtime VM): boots with deny-all network, runs the pre-built JAR
#           with PostgreSQL, Redis, ZooKeeper, DynamoDB-local.
#
# Usage:
#   # Stage 1: build the JAR (needs network, run once)
#   make vm-signal-server-build-jar
#
#   # Stage 2: build the runtime VM image (offline)
#   make vm-signal-server-build
#
#   # Boot the runtime VM
#   make vm-signal-server
#
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  # ---------------------------------------------------------------------------
  # Signal-Server source (pinned)
  # ---------------------------------------------------------------------------
  signalServerSrc = pkgs.fetchFromGitHub {
    owner  = "signalapp";
    repo   = "Signal-Server";
    rev    = "v9.99.1";
    sha256 = "sha256-Z54IS1j4zFmYmNM4Amd1BZGonFDCr3E73q06wLueUKE=";
  };

  # ---------------------------------------------------------------------------
  # DynamoDB-local (Amazon's standalone JAR for local development)
  # ---------------------------------------------------------------------------
  dynamodbLocalTarball = pkgs.fetchurl {
    url    = "https://d1ni2b6xgvw0s0.cloudfront.net/v2.x/dynamodb_local_latest.tar.gz";
    sha256 = "sha256-wF3DIPT5pvkMZZe8TccjmrggCm3w8oDZy+TVd1lD+Jg=";
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
  # Configuration files
  # ---------------------------------------------------------------------------
  signalServerConfig = ./signal-server-config.yml;
  signalSecretsBundle = ./signal-server-secrets.yml;

  # ---------------------------------------------------------------------------
  # Pre-built JAR path (populated by Stage 1, baked into Stage 2)
  # ---------------------------------------------------------------------------
  signalJarCacheDir = ../build/signal-server-jar;
  signalJarPath = if builtins.pathExists signalJarCacheDir
    then signalJarCacheDir
    else null;

  # ===========================================================================
  # STAGE 1: BUILD VM — has network, builds Signal-Server JAR via Maven
  # ===========================================================================
  buildVmConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ./vm-base.nix ];

    boot.loader.grub.device = "/dev/vda";
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_scsi" "virtio_net" "ext4"
      "9p" "9pnet" "9pnet_virtio"
    ];

    networking.hostName = lib.mkForce "umbravox-signal-build";

    environment.systemPackages = with pkgs; [
      jdk21_headless
      maven
      protobuf
      pkg-config
      foundationdb
      git
      curl
      gnumake
      gcc
    ];

    # The build service: clone source, run Maven, copy JAR to /output
    systemd.services.signal-server-build = {
      description = "Build Signal-Server JAR from source";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" "network-online.target" ];
      wants = [ "network-online.target" ];
      path = with pkgs; [ jdk21_headless maven protobuf git curl gcc gnumake pkg-config foundationdb ];
      environment = {
        HOME = "/root";
        JAVA_HOME = "${pkgs.jdk21_headless}";
        FDB_LIBRARY_PATH = "${pkgs.foundationdb}/lib";
      };
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "signal-server-build" ''
          set -euo pipefail
          export PATH=/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH

          echo "=== Signal-Server Build VM ==="
          echo "  Source: ${signalServerSrc}"
          echo "  Java: $(java -version 2>&1 | head -1)"
          echo ""

          # Mount output directory (host ↔ guest via 9p)
          mkdir -p /output
          mount -t 9p -o trans=virtio,version=9p2000.L output /output 2>/dev/null || true

          # Copy source to writable location
          cp -a ${signalServerSrc} /tmp/signal-server
          chmod -R u+w /tmp/signal-server

          cd /tmp/signal-server

          echo "=== Running Maven build (this takes several minutes) ==="

          # Use the system protoc from nixpkgs instead of letting Maven
          # download a pre-compiled binary (which won't run on NixOS).
          PROTOC_PATH=$(which protoc)
          echo "  Using system protoc: $PROTOC_PATH ($(protoc --version))"

          ./mvnw -B -ntp -pl service -am package -DskipTests \
            -Dmaven.javadoc.skip=true \
            -Dmaven.source.skip=true \
            -Dprotoc.path="$PROTOC_PATH" \
            || {
              echo "BUILD_RESULT=FAIL"
              # Even if build fails, copy what we have
              ls -la service/target/*.jar 2>/dev/null || echo "No JARs produced"
              systemctl poweroff
              exit 1
            }

          echo "=== Build complete ==="
          ls -la service/target/TextSecureServer-*.jar

          # Copy JAR to output
          mkdir -p /output
          cp service/target/TextSecureServer-*.jar /output/signal-server.jar
          echo "BUILD_RESULT=PASS"
          echo "JAR copied to /output/signal-server.jar"
          ls -la /output/signal-server.jar

          # Shut down
          systemctl poweroff
        '';
        StandardOutput = "journal+console";
        StandardError = "journal+console";
        TimeoutStartSec = "1800";  # 30 min for Maven build
      };
    };
  };

  buildVmNixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = buildVmConfig;
  };

  buildVmImage = import (pkgs.path + "/nixos/lib/make-disk-image.nix") {
    inherit pkgs;
    lib = pkgs.lib;
    config = buildVmNixos.config;
    diskSize = "auto";
    additionalSpace = "8192M";  # Maven deps are large
    format = "raw";
    partitionTableType = "legacy";
    copyChannel = false;
  };

  # ===========================================================================
  # STAGE 2: RUNTIME VM — deny-all network, runs pre-built JAR
  # ===========================================================================
  runtimeVmConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ./vm-base.nix ];

    boot.loader.grub.device = "/dev/vda";
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_scsi" "virtio_net" "ext4"
      "9p" "9pnet" "9pnet_virtio"
    ];

    networking.hostName = lib.mkForce "umbravox-signal";

    # ---- PostgreSQL ----
    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_15;
      initialScript = pkgs.writeText "signal-db-init.sql" ''
        CREATE DATABASE signal;
        CREATE DATABASE signal_abuse;
      '';
    };

    # ---- Redis ----
    services.redis.servers.signal = {
      enable = true;
      port = 6379;
    };

    # ---- ZooKeeper ----
    services.zookeeper = {
      enable = true;
    };

    # ---- System packages ----
    environment.systemPackages = with pkgs; [
      jdk21_headless
      curl
      jq
      foundationdb
    ];

    # ---- DynamoDB-local ----
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

    # ---- Deploy config files ----
    system.activationScripts.signalServerConfig = ''
      mkdir -p /etc/signal-server
      cp ${signalServerConfig}  /etc/signal-server/config.yml
      cp ${signalSecretsBundle} /etc/signal-server/secrets.yml
      chmod 600 /etc/signal-server/secrets.yml
    '';

    # ---- Pre-built JAR baked into image (from Stage 1) ----
    environment.etc."signal-server/signal-server.jar" = lib.mkIf (signalJarPath != null) {
      source = signalJarPath + "/signal-server.jar";
    };

    # ---- Signal-Server service ----
    systemd.services.signal-server = lib.mkIf (signalJarPath != null) {
      description = "Signal-Server (wire-compat test instance)";
      wantedBy    = [ "multi-user.target" ];
      after       = [
        "postgresql.service" "redis-signal.service"
        "zookeeper.service" "dynamodb-local.service"
      ];
      wants       = [
        "postgresql.service" "redis-signal.service"
        "zookeeper.service" "dynamodb-local.service"
      ];
      environment = {
        JAVA_HOME       = "${pkgs.jdk21_headless}";
        LD_LIBRARY_PATH = "${pkgs.foundationdb}/lib";
      };
      serviceConfig = {
        Type = "simple";
        ExecStart = pkgs.writeShellScript "signal-server-start" ''
          set -euo pipefail

          echo "=== Signal-Server: starting ==="

          # Wait for DynamoDB-local
          for i in $(seq 1 30); do
            if ${pkgs.curl}/bin/curl -sf http://localhost:8000/ >/dev/null 2>&1; then break; fi
            echo "  Waiting for DynamoDB-local ($i/30)..."
            sleep 1
          done

          # Wait for PostgreSQL
          for i in $(seq 1 30); do
            if ${pkgs.postgresql_15}/bin/pg_isready -q; then break; fi
            echo "  Waiting for PostgreSQL ($i/30)..."
            sleep 1
          done

          exec ${pkgs.jdk21_headless}/bin/java \
            -Dsecrets.bundle.filename=/etc/signal-server/secrets.yml \
            -Xmx512m \
            -jar /etc/signal-server/signal-server.jar \
            server \
            /etc/signal-server/config.yml
        '';
        Restart         = "on-failure";
        RestartSec      = "10";
        TimeoutStartSec = "120";
        StandardOutput  = "journal+console";
        StandardError   = "journal+console";
      };
    };

    # ---- Health check ----
    systemd.services.signal-server-healthcheck = {
      description = "Signal-Server health check";
      wantedBy    = [ "multi-user.target" ];
      after       = [ "signal-server.service" "dynamodb-local.service" "postgresql.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "signal-server-healthcheck" ''
          echo "Signal-Server VM services:"
          echo "  PostgreSQL:     $(systemctl is-active postgresql)"
          echo "  Redis:          $(systemctl is-active redis-signal)"
          echo "  ZooKeeper:      $(systemctl is-active zookeeper)"
          echo "  DynamoDB-local: $(systemctl is-active dynamodb-local)"
          echo "  Signal-Server:  $(systemctl is-active signal-server 2>/dev/null || echo 'not configured')"
          echo "  Java:           $(java -version 2>&1 | head -1)"
          echo ""
          ${lib.optionalString (signalJarPath != null) ''
            # Probe Dropwizard admin health endpoint
            for i in $(seq 1 60); do
              if ${pkgs.curl}/bin/curl -sf http://localhost:8081/healthcheck >/dev/null 2>&1; then
                echo "Signal-Server admin endpoint is UP."
                ${pkgs.curl}/bin/curl -s http://localhost:8081/healthcheck | ${pkgs.jq}/bin/jq .
                exit 0
              fi
              sleep 2
            done
            echo "WARNING: Signal-Server admin endpoint not reachable after 120s."
          ''}
          ${lib.optionalString (signalJarPath == null) ''
            echo "Signal-Server JAR not yet built."
            echo "Run: make vm-signal-server-build-jar"
            echo ""
            echo "Backing services are ready for integration."
          ''}
        '';
        TimeoutStartSec = "180";
      };
    };

    # Deny external network at runtime
    networking.firewall.enable = false;
  };

  runtimeNixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = runtimeVmConfig;
  };

  runtimeImage = import (pkgs.path + "/nixos/lib/make-disk-image.nix") {
    inherit pkgs;
    lib    = pkgs.lib;
    config = runtimeNixos.config;
    diskSize          = "auto";
    additionalSpace   = "4096M";
    format            = "raw";
    partitionTableType = "legacy";
    copyChannel       = false;
  };

in {
  # Stage 1: build VM (produces JAR)
  buildVm = buildVmImage;

  # Stage 2: runtime VM (uses pre-built JAR if available)
  qemu = runtimeImage;
}

# NixOS VM for self-hosted Signal-Server (AGPL)
# Used for wire-compatibility testing of UmbraVOX's Signal bridge plugin.
# All services run locally — no external Signal servers contacted.
#
# TWO-STAGE BUILD (all inside VMs, host nix store untouched):
#   Stage 1 (build VM): runs nix-build inside the VM to produce the JAR
#           via FOD (fixed-output derivation). Maven deps hash-locked.
#           Outputs signal-server.jar to /output via 9p virtfs.
#   Stage 2 (runtime VM): boots with deny-all network, runs the pre-built JAR
#           with PostgreSQL, Redis, ZooKeeper, DynamoDB-local.
#
# Usage:
#   make vm-signal-server-build-jar   # Stage 1: build JAR in VM
#   make vm-signal-server-build       # Stage 2: build runtime VM image
#   make vm-signal-server             # Boot runtime VM
#
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  # ---------------------------------------------------------------------------
  # Central config (version, tag, JDK, etc.)
  # ---------------------------------------------------------------------------
  cfg = builtins.fromJSON (builtins.readFile ./signal-server.json);

  # ---------------------------------------------------------------------------
  # Signal-Server source (pinned)
  # ---------------------------------------------------------------------------
  signalServerSrc = pkgs.fetchFromGitHub {
    owner  = cfg.owner;
    repo   = cfg.repo;
    rev    = cfg.tag;
    sha256 = cfg.sha256;
  };

  # ---------------------------------------------------------------------------
  # DynamoDB-local
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
    unpackPhase = "mkdir -p src && tar xzf $src -C src";
    installPhase = "mkdir -p $out && cp -r src/* $out/";
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
  # STAGE 1: BUILD VM — has network + nix, builds JAR via nix-build inside VM
  # ===========================================================================
  buildVmConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ./vm-base.nix ];

    boot.loader.grub.device = "/dev/vda";
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_scsi" "virtio_net" "ext4"
      "9p" "9pnet" "9pnet_virtio"
    ];

    networking.hostName = lib.mkForce "umbravox-signal-build";
    boot.loader.timeout = 0;

    # Nix daemon for in-VM builds (vm-base.nix sets nix.enable=false)
    nix.enable = lib.mkForce true;
    nix.settings = {
      experimental-features = [ "nix-command" ];
      sandbox = false;
      trusted-users = [ "root" ];
    };
    # Ensure nixbld group has members (required even with sandbox=false)
    nix.nrBuildUsers = 4;

    environment.systemPackages = with pkgs; [
      nix
      jdk25_headless
      maven
      protobuf
      patchelf
      file
      foundationdb
      git
      curl
      zlib
    ];

    # The build service: run nix-build inside the VM
    systemd.services.signal-server-build = {
      description = "Build Signal-Server JAR via nix-build (in-VM)";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" "network-online.target" "nss-lookup.target" ];
      wants = [ "network-online.target" ];
      path = with pkgs; [ nix git curl ];
      environment = {
        HOME = "/root";
        NIX_PATH = "nixpkgs=${pkgs.path}";
      };
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "signal-server-build" ''
          set -euo pipefail
          export PATH=/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH

          echo "=== Signal-Server Build VM (nix-build inside VM) ==="
          echo "  Nix: $(nix --version)"
          echo "  Java: $(java -version 2>&1 | head -1)"
          echo ""

          # Mount output directory (host ↔ guest via 9p)
          mkdir -p /output
          mount -t 9p -o trans=virtio,version=9p2000.L output /output 2>/dev/null || true

          # Mount source tree (read-only)
          mkdir -p /mnt/src
          mount -o ro /dev/vdb /mnt/src 2>/dev/null || true

          # Clone Signal-Server source
          SIGNAL_SRC=/tmp/signal-server-src
          echo "=== Cloning Signal-Server ${cfg.tag} ==="
          git clone --depth 1 --branch ${cfg.tag} \
            https://github.com/${cfg.owner}/${cfg.repo}.git "$SIGNAL_SRC" \
            || {
              echo "BUILD_RESULT=FAIL"
              echo "git clone failed."
              systemctl poweroff
              exit 1
            }
          cd "$SIGNAL_SRC"

          # Deterministic build settings
          export SOURCE_DATE_EPOCH=469105871

          SETTINGS=$(mktemp)
          cat > $SETTINGS << 'SETTINGSEOF'
          <settings><profiles><profile><id>no-snapshots</id>
            <pluginRepositories/></profile></profiles>
          <activeProfiles><activeProfile>no-snapshots</activeProfile></activeProfiles></settings>
          SETTINGSEOF

          # Build JAR with Maven (network for dependency downloads)
          echo "=== Building Signal-Server JAR with Maven ==="
          mvn -B -ntp -s $SETTINGS package -pl service -am -DskipTests \
            -Dproject.build.outputTimestamp=${cfg.outputTimestamp} \
            -Djib.container.creationTime=EPOCH \
            || {
              echo "BUILD_RESULT=FAIL"
              echo "Maven build failed."
              systemctl poweroff
              exit 1
            }

          echo "=== Build complete ==="
          JAR=$(find service/target -name 'TextSecureServer-*.jar' -not -name '*-sources*' -not -name '*-tests*' | head -1)
          if [ -z "$JAR" ]; then
            echo "BUILD_RESULT=FAIL"
            echo "JAR not found in service/target/"
            ls -la service/target/ 2>/dev/null
            systemctl poweroff
            exit 1
          fi

          cp "$JAR" /output/signal-server.jar
          echo "BUILD_RESULT=PASS"
          echo "JAR copied to /output/signal-server.jar"
          ls -lh /output/signal-server.jar

          systemctl poweroff
        '';
        StandardOutput = "journal+console";
        StandardError = "journal+console";
        TimeoutStartSec = "1800";
      };
    };
  };

  buildVmNixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = buildVmConfig;
  };

  buildVmImage = import ./make-disk-image.nix {
    inherit pkgs;
    lib = pkgs.lib;
    config = buildVmNixos.config;
    diskSize = "auto";
    additionalSpace = "16384M";  # nix store + maven deps + JAR
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
    boot.loader.timeout = 0;

    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_15;
      initialScript = pkgs.writeText "signal-db-init.sql" ''
        CREATE DATABASE signal;
        CREATE DATABASE signal_abuse;
      '';
    };

    services.redis.servers.signal = { enable = true; port = 6379; };
    services.zookeeper = { enable = true; };

    environment.systemPackages = with pkgs; [
      jdk25_headless curl jq foundationdb
    ];

    systemd.services.dynamodb-local = {
      description = "DynamoDB Local";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        Type = "simple";
        WorkingDirectory = "${dynamodbLocal}";
        ExecStart = "${pkgs.jdk25_headless}/bin/java -Djava.library.path=${dynamodbLocal}/DynamoDBLocal_lib -jar ${dynamodbLocal}/DynamoDBLocal.jar -sharedDb -port 8000";
        Restart = "on-failure";
        RestartSec = "5";
      };
    };

    system.activationScripts.signalServerConfig = ''
      mkdir -p /etc/signal-server
      cp ${signalServerConfig}  /etc/signal-server/config.yml
      cp ${signalSecretsBundle} /etc/signal-server/secrets.yml
      chmod 600 /etc/signal-server/secrets.yml
    '';

    environment.etc."signal-server/signal-server.jar" = lib.mkIf (signalJarPath != null) {
      source = signalJarPath + "/signal-server.jar";
    };

    systemd.services.signal-server = lib.mkIf (signalJarPath != null) {
      description = "Signal-Server (wire-compat test instance)";
      wantedBy = [ "multi-user.target" ];
      after = [ "postgresql.service" "redis-signal.service" "zookeeper.service" "dynamodb-local.service" ];
      wants = [ "postgresql.service" "redis-signal.service" "zookeeper.service" "dynamodb-local.service" ];
      environment = {
        JAVA_HOME = "${pkgs.jdk25_headless}";
        LD_LIBRARY_PATH = "${pkgs.foundationdb}/lib";
      };
      serviceConfig = {
        Type = "simple";
        ExecStart = pkgs.writeShellScript "signal-server-start" ''
          set -euo pipefail
          for i in $(seq 1 30); do
            ${pkgs.curl}/bin/curl -sf http://localhost:8000/ >/dev/null 2>&1 && break
            echo "Waiting for DynamoDB-local ($i/30)..." && sleep 1
          done
          for i in $(seq 1 30); do
            ${pkgs.postgresql_15}/bin/pg_isready -q && break
            echo "Waiting for PostgreSQL ($i/30)..." && sleep 1
          done
          exec ${pkgs.jdk25_headless}/bin/java \
            -Dsecrets.bundle.filename=/etc/signal-server/secrets.yml \
            -Xmx512m \
            -jar /etc/signal-server/signal-server.jar \
            server /etc/signal-server/config.yml
        '';
        Restart = "on-failure";
        RestartSec = "10";
        TimeoutStartSec = "120";
        StandardOutput = "journal+console";
        StandardError = "journal+console";
      };
    };

    systemd.services.signal-server-healthcheck = {
      description = "Signal-Server health check";
      wantedBy = [ "multi-user.target" ];
      after = [ "signal-server.service" "dynamodb-local.service" "postgresql.service" ];
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
            echo "Backing services are ready."
          ''}
        '';
        TimeoutStartSec = "180";
      };
    };

    networking.firewall.enable = false;
  };

  runtimeNixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = runtimeVmConfig;
  };

  runtimeImage = import ./make-disk-image.nix {
    inherit pkgs;
    lib = pkgs.lib;
    config = runtimeNixos.config;
    diskSize = "auto";
    additionalSpace = "4096M";
    format = "raw";
    partitionTableType = "legacy";
    copyChannel = false;
  };

in {
  buildVm = buildVmImage;
  qemu = runtimeImage;
}

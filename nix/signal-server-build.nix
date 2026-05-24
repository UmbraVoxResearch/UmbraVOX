# Pure Nix build of Signal-Server fat JAR.
#
# Two-phase FOD (fixed-output derivation) approach:
#   Phase 1 (mvnDeps): Maven downloads all dependencies (has network, content-hashed)
#   Phase 2 (signalServerJar): Maven builds offline using cached deps (no network, pure)
#
# Version, tag, JDK, and source coordinates are read from signal-server.json
# in this directory.  Edit that file to bump versions; this file adapts automatically.
#
# This file is evaluated INSIDE the build VM where nix is available.
# The host nix store is never touched.
#
# Usage (inside VM):
#   nix-build /mnt/src/nix/signal-server-build.nix
#   # Result: result/lib/signal-server.jar
#
# BOOTSTRAP PROCESS (M19.4.6):
#   The end-to-end JAR build requires a valid FOD hash for the Maven
#   dependency fetch (Phase 1).  This hash cannot be computed without
#   network access, so the bootstrap is a two-pass process:
#
#   Pass 1 — Obtain the FOD hash:
#     1. Run: ./uv vm signal build-jar 2>&1 | tee build/signal-server-build.log
#     2. The build VM boots, runs nix-build, Maven fetches deps.
#     3. nix-build fails with "hash mismatch" and prints the correct hash.
#     4. Run: ./uv vm signal extract-hash
#        (auto-extracts the hash from the log and patches this file)
#     Or manually: copy the "got: sha256-..." value and paste below.
#
#   Pass 2 — Build the JAR:
#     1. Run: ./uv vm signal build-jar  (again, with correct hash)
#     2. Phase 1 succeeds (hash matches), Phase 2 builds the JAR offline.
#     3. JAR lands in build/signal-server-jar/signal-server.jar.
#
#   Pass 3 — Build and boot runtime VM:
#     1. Run: ./uv vm signal run    (boots runtime VM with pre-built JAR)
#     2. Run: ./uv vm signal health (health-checks services)
#
#   WHAT'S NEEDED TO COMPLETE THE BUILD:
#   - [x] Source pinning (signalServerSrc fetchFromGitHub — done)
#   - [x] FOD derivation for Maven deps (mvnDeps — scaffolded)
#   - [x] Offline JAR build derivation (signalServerJar — scaffolded)
#   - [x] Build VM config (vm-signal-server.nix Stage 1 — done)
#   - [x] Runtime VM config (vm-signal-server.nix Stage 2 — done)
#   - [ ] FOD hash: requires running Pass 1 on a machine with KVM
#   - [ ] Verification: run Pass 2 + Pass 3 after hash is set
#
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  cfg = builtins.fromJSON (builtins.readFile ./signal-server.json);

  jdk = pkgs.${"jdk" + cfg.jdk + "_headless"};

  signalServerSrc = pkgs.fetchFromGitHub {
    owner  = cfg.owner;
    repo   = cfg.repo;
    rev    = cfg.tag;
    sha256 = cfg.sha256;
  };

  # Phase 1: Fixed-output derivation that fetches all Maven dependencies.
  # This has network access (FOD sandbox exception). The output is
  # content-addressed — if deps change, the hash changes.
  mvnDeps = pkgs.stdenv.mkDerivation {
    pname = "signal-server-maven-deps";
    version = cfg.version;
    src = signalServerSrc;

    nativeBuildInputs = (with pkgs; [
      maven
      protobuf
      git
    ]) ++ [ jdk ];

    # FOD: Nix allows network for fixed-output derivations
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    # TODO(M19.4.5): Compute FOD hash by running Stage 1 build VM:
    #   ./uv vm signal build-jar 2>&1 | tee build/signal-server-build.log
    #   ./uv vm signal extract-hash   # auto-extracts and patches this file
    # The first run will fail with a hash mismatch.  Nix prints:
    #   error: hash mismatch ...
    #     specified: sha256-AAAA...
    #     got:       sha256-XXXX...
    # Alternatively, from inside the VM:
    #   nix-build /mnt/src/nix/signal-server-build.nix -A mvnDeps 2>&1 | grep "got:"
    #
    # The hash is empty until the first successful network fetch.
    # This is intentional: FOD hashes cannot be computed without
    # actually running Maven with network access (inside the build VM).
    # Deterministic hash: SOURCE_DATE_EPOCH=469105871 (1984-11-11) +
    # timestamp normalization + metadata cleanup.
    outputHash = "sha256-mi7sRP2cDdvY/lWZHNjXbs0XbyWt0S+EYUFMSiQapDo=";

    buildPhase = ''
      export HOME=$TMPDIR
      export JAVA_HOME=${jdk}

      # Deterministic timestamps: Maven metadata includes download
      # timestamps. SOURCE_DATE_EPOCH makes tools produce consistent
      # output. We also normalize file timestamps after download.
      export SOURCE_DATE_EPOCH=469105871

      # Download all dependencies to a local repo
      mvn -B -ntp dependency:go-offline \
        -pl service -am \
        -Dmaven.repo.local=$out \
        -DskipTests \
        || true

      # Also resolve plugin dependencies
      mvn -B -ntp dependency:resolve-plugins \
        -pl service -am \
        -Dmaven.repo.local=$out \
        -DskipTests \
        || true

      # Download the specific protoc and grpc-java plugin artifacts
      # that the protobuf-maven-plugin needs
      mvn -B -ntp process-sources \
        -pl websocket-resources,service \
        -Dmaven.repo.local=$out \
        -DskipTests \
        || true

      # Remove timestamps and non-deterministic metadata from the repo
      # so the hash is reproducible across builds
      find $out -name '_remote.repositories' -delete
      find $out -name 'maven-metadata-*.xml' -delete
      find $out -name '*.lastUpdated' -delete
      find $out -name 'resolver-status.properties' -delete

      # Normalize all file timestamps to SOURCE_DATE_EPOCH
      find $out -exec touch -d @$SOURCE_DATE_EPOCH {} + 2>/dev/null || true
    '';

    installPhase = "true";  # output is already $out (the maven repo)
  };

  # Phase 2: Build the fat JAR offline using the cached dependencies.
  # No network access — pure Nix derivation.
  signalServerJar = pkgs.stdenv.mkDerivation {
    pname = "signal-server";
    version = cfg.version;
    src = signalServerSrc;

    nativeBuildInputs = (with pkgs; [
      maven
      protobuf
      patchelf
      file
      foundationdb
    ]) ++ [ jdk ];

    buildPhase = ''
      export HOME=$TMPDIR
      export JAVA_HOME=${jdk}
      export FDB_LIBRARY_PATH=${pkgs.foundationdb}/lib

      # Copy the cached Maven repo to a writable location
      cp -r ${mvnDeps} $TMPDIR/m2repo
      chmod -R u+w $TMPDIR/m2repo

      # Patch any pre-compiled protoc/grpc binaries in the Maven cache
      # for NixOS dynamic linker compatibility
      INTERP=$(cat ${pkgs.stdenv.cc}/nix-support/dynamic-linker)
      RPATH="${pkgs.lib.makeLibraryPath [ pkgs.stdenv.cc.cc.lib pkgs.zlib ]}"
      find $TMPDIR/m2repo -type f \( -name 'protoc-*' -o -name 'grpc_*' -o -name '*.exe' \) | while read f; do
        if file "$f" | grep -q 'ELF.*dynamically linked'; then
          patchelf --set-interpreter "$INTERP" "$f" 2>/dev/null || true
          patchelf --set-rpath "$RPATH" "$f" 2>/dev/null || true
          chmod +x "$f"
        fi
      done

      # Build offline using the cached + patched deps
      mvn -B -ntp -o \
        -pl service -am \
        package -DskipTests \
        -Dmaven.repo.local=$TMPDIR/m2repo \
        -Dmaven.javadoc.skip=true \
        -Dmaven.source.skip=true \
        -Dproject.build.outputTimestamp=${cfg.outputTimestamp}
    '';

    installPhase = ''
      mkdir -p $out/lib
      cp service/target/TextSecureServer-*.jar $out/lib/signal-server.jar
    '';

    meta = with pkgs.lib; {
      description = "Signal-Server fat JAR for UmbraVOX wire-compat testing";
      license = licenses.agpl3Plus;
    };
  };

in signalServerJar

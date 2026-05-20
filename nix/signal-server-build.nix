# Pure Nix build of Signal-Server fat JAR.
#
# Two-phase FOD (fixed-output derivation) approach:
#   Phase 1: Maven downloads all dependencies (has network, content-hashed)
#   Phase 2: Maven builds offline using cached deps (no network, pure)
#
# This file is evaluated INSIDE the build VM where nix is available.
# The host nix store is never touched.
#
# Usage (inside VM):
#   nix-build /mnt/src/nix/signal-server-build.nix
#   # Result: result/lib/signal-server.jar
#
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  signalServerSrc = pkgs.fetchFromGitHub {
    owner  = "signalapp";
    repo   = "Signal-Server";
    rev    = "v9.99.1";
    sha256 = "sha256-Z54IS1j4zFmYmNM4Amd1BZGonFDCr3E73q06wLueUKE=";
  };

  # Phase 1: Fixed-output derivation that fetches all Maven dependencies.
  # This has network access (FOD sandbox exception). The output is
  # content-addressed — if deps change, the hash changes.
  mvnDeps = pkgs.stdenv.mkDerivation {
    pname = "signal-server-maven-deps";
    version = "9.99.1";
    src = signalServerSrc;

    nativeBuildInputs = with pkgs; [
      maven
      jdk21_headless
      protobuf
      git
    ];

    # FOD: Nix allows network for fixed-output derivations
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    # Empty string = Nix will fail and print the correct hash.
    # Paste the hash from the error message after first run.
    outputHash = "";

    buildPhase = ''
      export HOME=$TMPDIR
      export JAVA_HOME=${pkgs.jdk21_headless}

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
    '';

    installPhase = "true";  # output is already $out (the maven repo)
  };

  # Phase 2: Build the fat JAR offline using the cached dependencies.
  # No network access — pure Nix derivation.
  signalServerJar = pkgs.stdenv.mkDerivation {
    pname = "signal-server";
    version = "9.99.1";
    src = signalServerSrc;

    nativeBuildInputs = with pkgs; [
      maven
      jdk21_headless
      protobuf
      patchelf
      file
      foundationdb
    ];

    buildPhase = ''
      export HOME=$TMPDIR
      export JAVA_HOME=${pkgs.jdk21_headless}
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
        -Dmaven.source.skip=true
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

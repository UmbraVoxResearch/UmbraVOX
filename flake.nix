{
  description = "UmbraVOX reproducible build and release entrypoints";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hp = pkgs.haskell.packages.ghc96;

        devTools = with pkgs; [
          (hp.ghcWithPackages (p: [ p.network ]))
          cabal-install
          gcc
          gdb
          valgrind
          curl
          coq
          tlaplus
          fstar
          z3
          go
          sqlite
          aflplusplus
          graphviz
          jq
          patchelf
          file
          zip
          gnumake
          git
          pkg-config
        ];

        mkMakeApp = target: pkgs.writeShellApplication {
          name = "umbravox-${target}";
          runtimeInputs = [ pkgs.gnumake pkgs.bash ];
          text = ''
            set -euo pipefail
            exec make ${target} "$@"
          '';
        };
        mkCheck = name: target: pkgs.runCommand "umbravox-${name}-check" { nativeBuildInputs = devTools; } ''
          cp -r ${./.} src
          chmod -R +w src
          cd src
          make ${target}
          mkdir -p $out
          echo ok > $out/${name}
        '';
      in
      {
        devShells.default = pkgs.mkShell {
          name = "umbravox-dev";
          buildInputs = devTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.glibc ];
          shellHook = ''
            export UMBRAVOX_ROOT="$(pwd)"
            export UMBRAVOX_DATA="$UMBRAVOX_ROOT/.umbravox-data"
            export PATH="$UMBRAVOX_ROOT/scripts:$PATH"
            echo "UmbraVOX flake dev shell ready: use make build/test/verify/release-*"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "umbravox-make-build";
          version = "dev";
          src = ./.;
          nativeBuildInputs = devTools;
          buildPhase = ''
            runHook preBuild
            make build
            runHook postBuild
          '';
          installPhase = ''
            mkdir -p $out
            echo "UmbraVOX build completed in derivation context" > $out/README
          '';
        };
        packages.release-linux = pkgs.stdenv.mkDerivation {
          pname = "umbravox-release-linux";
          version = "dev";
          src = ./.;
          nativeBuildInputs = devTools;
          buildPhase = ''
            runHook preBuild
            make release-linux
            runHook postBuild
          '';
          installPhase = ''
            mkdir -p $out
            cp -r build/releases $out/
          '';
        };
        packages.release-windows-cli-source = pkgs.stdenv.mkDerivation {
          pname = "umbravox-release-windows-cli-source";
          version = "dev";
          src = ./.;
          nativeBuildInputs = devTools;
          buildPhase = ''
            runHook preBuild
            make release-windows-cli
            runHook postBuild
          '';
          installPhase = ''
            mkdir -p $out
            cp -r build/releases $out/
          '';
        };
        packages.release-macos-terminal-source = pkgs.stdenv.mkDerivation {
          pname = "umbravox-release-macos-terminal-source";
          version = "dev";
          src = ./.;
          nativeBuildInputs = devTools;
          buildPhase = ''
            runHook preBuild
            make release-macos-terminal
            runHook postBuild
          '';
          installPhase = ''
            mkdir -p $out
            cp -r build/releases $out/
          '';
        };
        packages.release-bsd-terminal-source = pkgs.stdenv.mkDerivation {
          pname = "umbravox-release-bsd-terminal-source";
          version = "dev";
          src = ./.;
          nativeBuildInputs = devTools;
          buildPhase = ''
            runHook preBuild
            make release-bsd-terminal
            runHook postBuild
          '';
          installPhase = ''
            mkdir -p $out
            cp -r build/releases $out/
          '';
        };
        packages.release-freedos-source = pkgs.stdenv.mkDerivation {
          pname = "umbravox-release-freedos-source";
          version = "dev";
          src = ./.;
          nativeBuildInputs = devTools;
          buildPhase = ''
            runHook preBuild
            make release-freedos
            runHook postBuild
          '';
          installPhase = ''
            mkdir -p $out
            cp -r build/releases $out/
          '';
        };

        checks = {
          build = mkCheck "build" "build";
          test = mkCheck "test" "test";
          verify = mkCheck "verify" "verify";
          quality = mkCheck "quality" "quality";
        };

        apps = {
          build = { type = "app"; program = "${mkMakeApp "build"}/bin/umbravox-build"; };
          test = { type = "app"; program = "${mkMakeApp "test"}/bin/umbravox-test"; };
          verify = { type = "app"; program = "${mkMakeApp "verify"}/bin/umbravox-verify"; };
          quality = { type = "app"; program = "${mkMakeApp "quality"}/bin/umbravox-quality"; };
          release-linux = { type = "app"; program = "${mkMakeApp "release-linux"}/bin/umbravox-release-linux"; };
          release-windows-cli = { type = "app"; program = "${mkMakeApp "release-windows-cli"}/bin/umbravox-release-windows-cli"; };
          release-macos-terminal = { type = "app"; program = "${mkMakeApp "release-macos-terminal"}/bin/umbravox-release-macos-terminal"; };
          release-bsd-terminal = { type = "app"; program = "${mkMakeApp "release-bsd-terminal"}/bin/umbravox-release-bsd-terminal"; };
          release-freedos = { type = "app"; program = "${mkMakeApp "release-freedos"}/bin/umbravox-release-freedos"; };
          release = { type = "app"; program = "${mkMakeApp "release"}/bin/umbravox-release"; };
        };
      });
}

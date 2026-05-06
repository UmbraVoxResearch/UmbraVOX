{ pkgs ? import <nixpkgs> {} }:

let
  # GHC 9.6 with the 'network' package (only non-boot dependency)
  ghc = pkgs.haskell.packages.ghc96.ghcWithPackages (hp: [ hp.network ]);

  # Haskell tools
  cabal = pkgs.cabal-install;
  hpc = ghc;  # HPC ships with GHC

  # C toolchain for FFI constant-time implementations
  cc = pkgs.gcc;
  valgrind = pkgs.valgrind;  # for ctgrind constant-time verification

  # Formal verification
  coq = pkgs.coq;
  tlaplus = pkgs.tlaplus;
  tlc = pkgs.tlaplus;
  z3 = pkgs.z3;
  fstar = pkgs.fstar;

  # Fuzzing
  afl = pkgs.aflplusplus;

  # Documentation and analysis
  graphviz = pkgs.graphviz;  # dependency graphs
  jq = pkgs.jq;              # JSON processing for test evidence

  # Build essentials
  gnumake = pkgs.gnumake;
  git = pkgs.git;
  pkg-config = pkgs.pkg-config;

in pkgs.mkShell {
  name = "umbravox-dev";

  buildInputs = [
    # Haskell
    ghc
    cabal

    # C toolchain (FFI targets)
    cc
    pkgs.gdb
    valgrind

    # Networking (for cabal update)
    pkgs.curl

    # Formal verification
    coq
    tlaplus
    fstar
    z3

    # Go (for anthony DB tool)
    pkgs.go

    # SQLite (direct DB access, fallback for anthony)
    pkgs.sqlite

    # Fuzzing
    afl

    # Utilities
    graphviz
    jq
    gnumake
    git
    pkg-config
  ];

  shellHook = ''
    export UMBRAVOX_ROOT="$(pwd)"
    export UMBRAVOX_DATA="$UMBRAVOX_ROOT/.umbravox-data"
    export PATH="$UMBRAVOX_ROOT/scripts:$PATH"

    echo ""
    echo -e "\033[36m  ╦ ╦╔╦╗╔╗ ╦═╗╔═╗╦  ╦╔═╗═╗ ╦\033[0m"
    echo -e "\033[36m  ║ ║║║║╠╩╗╠╦╝╠═╣╚╗╔╝║ ║╔╩╦╝\033[0m"
    echo -e "\033[36m  ╚═╝╩ ╩╚═╝╩╚═╩ ╩ ╚╝ ╚═╝╩ ╚═\033[0m"
    echo -e "\033[33m  Post-Quantum Encrypted Messaging\033[0m"
    echo ""
    echo -e "  \033[32mToolchain:\033[0m"
    echo "    GHC $(ghc --numeric-version) | Cabal $(cabal --numeric-version)"
    echo "    F* $(fstar.exe --version 2>/dev/null | head -1 | sed 's/F\* //' || echo 'N/A')"
    echo "    Z3 $(z3 --version 2>/dev/null | sed 's/Z3 version //' || echo 'N/A')"
    echo "    Coq $(coqc --version 2>/dev/null | head -1 | sed 's/The Rocq Prover, version //' || echo 'N/A')"
    echo ""
    echo -e "  \033[32mProject:\033[0m"
    echo "    216 tests | 11 F* specs | 10 .spec files | 30 generated outputs"
    echo "    Crypto: SHA-2/3, AES-GCM, X25519, Ed25519, ML-KEM-768, Poly1305"
    echo "    Protocol: PQXDH, Signal Double Ratchet, Stealth Addresses"
    echo ""
    echo -e "  \033[32mCommands:\033[0m"
    echo "    make help      Show all targets and shortcuts"
    echo "    make build     Build library + executables"
    echo "    make run       Launch UmbraVOX TUI"
    echo "    make test      Run 216 tests (KAT, fuzz, security, integration)"
    echo "    make verify    Run F* formal verification"
    echo "    make quality   Run all quality gates"
    echo "    make codegen   Generate Haskell + C + FFI from .spec files"
    echo ""
  '';

  # Ensure GHC finds system libraries for FFI
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.glibc ];
}

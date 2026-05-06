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

    echo "UmbraVOX Development Environment"
    echo "  GHC:   $(ghc --numeric-version)"
    echo "  Cabal: $(cabal --numeric-version)"
    echo "  Coq:   $(coqc --version 2>/dev/null | head -1 || echo 'available')"
    echo "  F*:    $(fstar.exe --version 2>/dev/null | head -1 || echo 'available')"
    echo "  Z3:    $(z3 --version 2>/dev/null | head -1 || echo 'available')"
    echo ""
    echo "Commands:"
    echo "  cabal build        - Build all"
    echo "  cabal test         - Run test suite"
    echo "  cabal run codegen  - Run code generators"
    echo "  ./test/evidence/formal-proofs/fstar/verify.sh  - Verify F* specs"
    echo ""
  '';

  # Ensure GHC finds system libraries for FFI
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.glibc ];
}

# Finding:   shell.nix previously used `import <nixpkgs> {}` which resolves
#            against the operator's mutable ambient nixpkgs channel.  This
#            means two developers (or two CI runs on different machines) can
#            get different package versions despite using the same source tree.
#
# Fix:       Read the rev and narHash from flake.lock at evaluation time and
#            fetch a reproducible nixpkgs tarball.  This makes `nix-shell`
#            resolve the same package set as `nix develop` / `nix build`.
#
# Verified:  The lock file is tracked in git; any change to the pinned
#            nixpkgs will produce a visible diff in flake.lock and shell.nix
#            will automatically follow without a manual update here.
#
# NOTE:      If you need to use the ambient channel intentionally (e.g. for
#            quick local experiments), pass `--arg pkgs 'import <nixpkgs> {}'`
#            on the nix-shell command line.  The flake (`nix develop`) remains
#            the canonical reproducible development path.
{ pkgs ? let
    lock     = builtins.fromJSON (builtins.readFile ./flake.lock);
    nixpkgs  = fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
      sha256 = lock.nodes.nixpkgs.locked.narHash;
    };
  in import nixpkgs {}
}:

let
  # GHC 9.14.1 with the 'network' package (only non-boot dependency)
  ghc = pkgs.haskell.packages.ghc9141.ghcWithPackages (hp: [ hp.network ]);

  # Haskell tools
  cabal = pkgs.cabal-install;
  hpc = ghc;  # HPC ships with GHC

  # C toolchain for FFI constant-time implementations
  cc = pkgs.gcc;
  valgrind = pkgs.valgrind;  # for ctgrind constant-time verification

  # Formal verification
  coq = pkgs.coq;
  coq-stdlib = pkgs.coqPackages.stdlib;
  coq-bignums = pkgs.coqPackages.bignums;
  coqprime = pkgs.coqPackages.coqprime;
  tlaplus = pkgs.tlaplus;
  z3 = pkgs.z3;
  fstar = pkgs.fstar;
  # fiat-crypto: Coq-verified constant-time C field arithmetic (Curve25519/Ed25519).
  # Generated C files land under $out/include/ and $out/src/ in the nix store.
  # M13.15.2-M13.15.5 will extract these for FFI integration.
  fiat-crypto = pkgs.fiat-crypto;

  # Fuzzing
  afl = pkgs.aflplusplus;

  # Documentation and analysis
  graphviz = pkgs.graphviz;  # dependency graphs
  jq = pkgs.jq;              # JSON processing for test evidence
  patchelf = pkgs.patchelf;
  file = pkgs.file;
  zip = pkgs.zip;

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
    coq-stdlib
    coq-bignums
    coqprime
    tlaplus
    fstar
    z3
    fiat-crypto

    # Go (for ./uv build system tools)
    pkgs.go

    # SQLite (direct DB access, fallback for anthony)
    pkgs.sqlite

    # Fuzzing
    afl

    # Utilities
    pkgs.nix
    graphviz
    jq
    patchelf
    file
    zip
    gnumake
    git
    pkg-config

    # Image building
    pkgs.genext2fs
    pkgs.e2fsprogs

    # VM smoke testing
    pkgs.qemu_kvm
    pkgs.firecracker

    # VM integration testing (SOCKS5 proxy, screenshots, recording)
    pkgs.microsocks
    pkgs.tmux
    pkgs.asciinema
  ];

  shellHook = ''
    export UMBRAVOX_ROOT="$(pwd)"
    export UMBRAVOX_DATA="$UMBRAVOX_ROOT/.umbravox-data"
    export UMBRAVOX_SHELL_KIND="full"
    export PATH="$UMBRAVOX_ROOT/scripts:$PATH"

    echo ""
    echo -e "\033[36m  ╦ ╦╔╦╗╔╗ ╦═╗╔═╗╦  ╦╔═╗═╗ ╦\033[0m"
    echo -e "\033[36m  ║ ║║║║╠╩╗╠╦╝╠═╣╚╗╔╝║ ║╔╩╦╝\033[0m"
    echo -e "\033[36m  ╚═╝╩ ╩╚═╝╩╚═╩ ╩ ╚╝ ╚═╝╩ ╚═\033[0m"
    echo -e "\033[33m  Post-Quantum Encrypted Messaging\033[0m"
    echo ""
    echo -e "  \033[1;36m[ FULL LOCAL SHELL ]\033[0m  \033[90m(shell.nix)\033[0m"
    echo ""
    echo -e "  \033[32mToolchain:\033[0m"
    echo "    GHC $(ghc --numeric-version) | Cabal $(cabal --numeric-version)"
    echo "    F* $(fstar.exe --version 2>/dev/null | head -1 | sed 's/F\* //' || echo 'N/A')"
    echo "    Z3 $(z3 --version 2>/dev/null | sed 's/Z3 version //' || echo 'N/A')"
    echo "    Coq $(coqc --version 2>/dev/null | head -1 | sed 's/The Rocq Prover, version //' || echo 'N/A')"
    echo ""
    echo -e "  \033[32mProject:\033[0m"
    echo "    Tiered messaging hardening | 32 F* specs | 19 Coq files | 30 generated outputs"
    echo "    Crypto: SHA-2/3, AES-GCM, X25519, Ed25519, ML-KEM-768, Poly1305"
    echo "    Protocol: PQXDH, Signal Double Ratchet, Stealth Addresses"
    echo ""
    echo -e "  \033[32mCommands (run in VM by default):\033[0m"
    echo "    ./uv build          Build library + executables (VM)"
    echo "    ./uv test           Run fast messaging-MVP hardening gate (VM)"
    echo "    ./uv verify         Run F* formal verification (VM)"
    echo "    ./uv quality        Run all quality gates (VM)"
    echo "    ./uv vm build-image Build NixOS VM image"
    echo "    ./uv help           Show all targets and shortcuts"
    echo ""
    echo -e "  \033[32mVM Development:\033[0m"
    echo "    ./uv dev            Interactive dev shell in NixOS VM"
    echo "    Host-local compile bypass is disabled"
    echo ""
  '';

  # Ensure GHC finds system libraries for FFI
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.glibc ];
}

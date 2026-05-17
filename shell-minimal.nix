# Minimal orchestration-only shell for VM-first development (M13.13).
#
# This shell provides ONLY the tools needed to drive the NixOS dev VM:
# QEMU, git, ssh, make, and nix (for building VM images).  No cabal, GHC,
# or other Haskell toolchain is included.
#
# The full toolchain (GHC, F*, Z3, Coq, AFL++, valgrind, etc.) lives
# inside the VM image (nix/vm-image.nix).  All standard make targets
# (build, test, verify, quality, etc.) route through the VM by default.
#
# Usage:
#   nix-shell shell-minimal.nix
#   make vm-image-build   # build/cache VM image (uses nix build, no cabal)
#   make build            # build inside VM
#   make test             # test inside VM
#   make verify           # F* verification inside VM
#   make vm-dev           # interactive dev shell inside the VM
#
# To use the full local toolchain instead, use:
#   nix-shell shell.nix
{ pkgs ? let
    lock     = builtins.fromJSON (builtins.readFile ./flake.lock);
    nixpkgs  = fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
      sha256 = lock.nodes.nixpkgs.locked.narHash;
    };
  in import nixpkgs {}
}:

pkgs.mkShell {
  name = "umbravox-vm-orch";

  buildInputs = with pkgs; [
    # VM orchestration
    qemu_kvm
    genext2fs
    e2fsprogs

    # Source control
    git

    # Build system (Makefile targets)
    gnumake

    # Nix (for building VM images)
    nix

    # Utilities used by Makefile / scripts
    coreutils
    findutils
    gnugrep
    gnused
    gnutar
    gzip
    jq
    file
    which
    openssh
  ];

  shellHook = ''
    export UMBRAVOX_ROOT="$(pwd)"
    export UMBRAVOX_DATA="$UMBRAVOX_ROOT/.umbravox-data"
    export PATH="$UMBRAVOX_ROOT/scripts:$PATH"

    echo ""
    echo -e "\033[36m  UmbraVOX VM-First Development Shell\033[0m"
    echo -e "\033[33m  (minimal — full toolchain is inside the VM)\033[0m"
    echo ""
    echo -e "  \033[32mStandard Commands (all run in VM):\033[0m"
    echo "    make build        Build library + executables"
    echo "    make test         Run fast messaging-MVP hardening gate"
    echo "    make verify       Run F* formal verification"
    echo "    make quality      Run all quality gates"
    echo ""
    echo -e "  \033[32mVM Development:\033[0m"
    echo "    make vm-dev           Interactive dev shell inside the NixOS VM"
    echo "    make vm-image-build   Build/cache the NixOS VM image (no cabal needed)"
    echo "    make vm-image-clean   Remove the cached VM image"
    echo ""
    echo -e "  \033[33mNOTE:\033[0m For the full local toolchain, use: nix-shell shell.nix"
    echo "        then: UMBRAVOX_LOCAL=1 make build"
    echo ""
  '';
}

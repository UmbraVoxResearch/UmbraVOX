# Minimal orchestration-only shell for VM-based development (M13.13).
#
# This shell provides ONLY the tools needed to drive the NixOS dev VM:
# QEMU, git, ssh, make, and cabal (for bridge commands that boot the VM).
#
# The full toolchain (GHC, F*, Z3, Coq, AFL++, valgrind, etc.) lives
# inside the VM image (nix/vm-image.nix).  All compilation, testing,
# and verification happens inside the VM via `make vm-dev`, `make vm-build`,
# `make vm-test`, and `make vm-verify`.
#
# Usage:
#   nix-shell shell-minimal.nix
#   make vm-dev       # interactive dev shell inside the VM
#   make vm-build     # build inside VM
#   make vm-test      # test inside VM
#   make vm-verify    # F* verification inside VM
#
# To use the full local toolchain instead, use the original:
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
    echo -e "\033[36m  UmbraVOX VM Orchestration Shell\033[0m"
    echo -e "\033[33m  (minimal — full toolchain is inside the VM)\033[0m"
    echo ""
    echo -e "  \033[32mVM Development:\033[0m"
    echo "    make vm-dev       Interactive dev shell inside the NixOS VM"
    echo "    make vm-build     Build inside VM (cabal build all)"
    echo "    make vm-test      Test inside VM (cabal run umbravox-test -- required)"
    echo "    make vm-verify    F* formal verification inside VM"
    echo ""
    echo -e "  \033[32mVM Management:\033[0m"
    echo "    make vm-image-build   Build/cache the NixOS VM image"
    echo "    make vm-image-clean   Remove the cached VM image"
    echo ""
    echo -e "  \033[33mNOTE:\033[0m For the full local toolchain, use: nix-shell shell.nix"
    echo ""
  '';
}

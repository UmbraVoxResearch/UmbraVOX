# Minimal shell for ./uv host-side operations.
# Contains only the tools needed by the host orchestrator:
#   qemu, genext2fs, go, git, curl, nix
#
# The full toolchain (GHC, F*, Z3, Coq, etc.) is in shell.nix
# and is only needed inside VMs, not on the host.
#
# Usage:
#   nix-shell shell-minimal.nix    (fast, seconds)
#   nix-shell shell.nix            (slow, full toolchain)
{ pkgs ? let
    lock     = builtins.fromJSON (builtins.readFile ./flake.lock);
    nixpkgs  = fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
      sha256 = lock.nodes.nixpkgs.locked.narHash;
    };
  in import nixpkgs {}
}:

pkgs.mkShell {
  name = "umbravox-minimal";

  buildInputs = with pkgs; [
    qemu_kvm
    genext2fs
    go
    git
    curl
    jq
    nix
  ];

  shellHook = ''
    export UMBRAVOX_ROOT="$(pwd)"
    export PATH="$UMBRAVOX_ROOT/scripts:$PATH"
  '';
}

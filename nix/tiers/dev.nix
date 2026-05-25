# Dev VM tier: builder + full development toolchain.
# Contains everything needed to build, test, verify, and release UmbraVOX.
# Used by: dev VM (./uv dev, ./uv build, ./uv test, ./uv verify).
#
# Tier hierarchy:
#   base → network → builder → dev
{ config, lib, modulesPath, pkgs, ... }:

let
  hp = pkgs.haskell.packages.ghc9141;

  # Rocq 9.1.1 separates the stdlib (ZArith, Arith, etc.) from the core.
  # Conditionally include the stdlib package: try coqPackages.stdlib first
  # (modern nixpkgs name), then coqPackages.coq-stdlib (legacy alias).
  # This ensures Ed25519Constants.v can "Require Import ZArith." in-guest.
  coqStdlib =
    if builtins.hasAttr "stdlib" pkgs.coqPackages then [ pkgs.coqPackages.stdlib ]
    else if builtins.hasAttr "coq-stdlib" pkgs.coqPackages then [ pkgs.coqPackages.coq-stdlib ]
    else [];

  # coqprime provides Pocklington primality proofs and field tactics.
  # bignums is a dependency. Both needed for Ed25519 group law (M13.11.4).
  coqPrime =
    if builtins.hasAttr "coqprime" pkgs.coqPackages then [ pkgs.coqPackages.coqprime ]
    else [];
  coqBignums =
    if builtins.hasAttr "bignums" pkgs.coqPackages then [ pkgs.coqPackages.bignums ]
    else [];

  devToolsPkgs = with pkgs; [
    (hp.ghcWithPackages (p: [ p.network ]))
    cabal-install
    gcc
    gdb
    valgrind
    coq
  ] ++ coqStdlib ++ coqPrime ++ coqBignums ++ (with pkgs; [
    tlaplus
    fstar
    z3
    go
    sqlite
    sqlite.dev
    pkg-config
    aflplusplus
    graphviz
    jq
    patchelf
    file
    zip
    gnumake
    git
    genext2fs
    bashInteractive
    coreutils
    findutils
    gnugrep
    gnused
    gnutar
    gzip
    which
    diffutils
    gnupatch
    tmux
    aha
    asciinema
  ]);
in

{
  imports = [ ./builder.nix ];

  environment.systemPackages = devToolsPkgs;
}

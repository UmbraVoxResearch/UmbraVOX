# Dev VM tier: builder + full development toolchain.
# Contains everything needed to build, test, verify, and release UmbraVOX.
# Used by: dev VM (./uv dev, ./uv build, ./uv test, ./uv verify).
#
# Tier hierarchy:
#   base → network → builder → dev
#
# M36A: KaRaMeL (krml) for F* Low* → C extraction.
# karamelHome is passed via specialArgs from the caller (vm-image.nix).
# If null (lock not yet updated), KaRaMeL is unavailable but the VM boots normally.
# To enable: run 'nix flake lock' in dev VM to populate the karamel lock entry.
{ config, lib, modulesPath, pkgs, karamelHome ? null, ... }:

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

  # fiat-crypto: Coq-verified formally-correct constant-time C field arithmetic.
  # Derives from Coq proofs and generates machine-checked C for:
  #   - Curve25519 / X25519 field operations (field element mul, sqr, add, sub, inv)
  #   - Ed25519 field arithmetic (GF(2^255-19) ops)
  # The generated C files land in $out/ (e.g. $(nix-build -A fiat-crypto)/).
  # Common outputs include src/ExtractionOCaml/word_by_word_montgomery.c and
  # related files; on nixpkgs, all generated C lives under $out/include/ and $out/src/.
  # M13.15.2-M13.15.5 will extract these C files and wire them into FFI.
  # Guard with hasAttr: fiat-crypto availability varies across nixpkgs versions/channels.
  fiatCrypto = if builtins.hasAttr "fiat-crypto" pkgs then [ pkgs.fiat-crypto ] else [];

  devToolsPkgs = with pkgs; [
    (hp.ghcWithPackages (p: [ p.network ]))
    cabal-install
    gcc
    gdb
    valgrind
    coq
  ] ++ coqStdlib ++ coqPrime ++ coqBignums ++ fiatCrypto ++ (with pkgs; [
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

  # The karamel "home" output places the krml binary at its root ($KRML_HOME/krml),
  # alongside krmllib/include/runtime — the canonical KRML_HOME layout — not bin/krml.
  environment.systemPackages = devToolsPkgs
    ++ (if karamelHome != null then [ (pkgs.writeShellScriptBin "krml" ''
         exec ${karamelHome}/krml "$@"
       '') ] else []);

  # M36A: KRML_HOME for F* Low* → C extraction (M36B).
  # Populated from the vendored contrib/karamel derivation (flake.nix mkKaramelHome).
  # Run: $KRML_HOME/krml --version to verify.
  environment.variables = lib.mkIf (karamelHome != null) {
    KRML_HOME = karamelHome;
  };
}

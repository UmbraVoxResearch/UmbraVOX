# tiny-core-linux (TCL) QEMU VM image descriptor — runtime-only integration tests.
#
# tiny-core-linux is NOT a NixOS target, so (like nix/vm-freebsd.nix and
# nix/vm-omnios.nix) this expression does not build a system from source.  It
# downloads the official CorePure64 ISO pinned by SHA256 and exposes it as a
# Nix store path.  The VM lifecycle (boot a ~5s tiny VM, drop in the
# statically-linked build/runtime/bin/umbravox binary via a 9p/virtio disk, run
# integration/e2e smoke tests) is handled by scripts/vm-tinycore-setup.sh and
# the Go orchestrator target `tinycore`.
#
# WHY CorePure64 (the ~24 MB ISO) over a hand-rolled initramfs:
#   * Reproducible by SHA256 pin, exactly matching the repo's pinned-artifact,
#     offline-VM philosophy used for the BSD/illumos targets.
#   * No GHC/F*/Coq toolchain — this is a runtime-only test VM, distinct from
#     the heavyweight NixOS dev/build VM.  Boot is ~5s.
#   * A custom initramfs would require pinning a kernel + a busybox/glibc build
#     and would not be meaningfully more reproducible than a SHA256-pinned ISO,
#     while being far more code to maintain.
# The umbravox binary produced at build/runtime/ is expected to be statically
# linked (or carry its own /app/lib), so TCL's minimal userland is sufficient.
#
# Usage:
#   nix-build nix/vm-tinycore.nix           # fetches ISO into Nix store
#   ./uv vm smoke tinycore                  # full smoke: fetch + boot + run
#
# The fetchurl hash must be updated when the TinyCore release changes.
# Run: nix-prefetch-url <url> to obtain the sha256 (inside the builder VM).
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  # Tiny Core Linux CorePure64 — 64-bit, no X, minimal command-line system.
  # Source: http://tinycorelinux.net/  (release archive is version-pinned).
  tinycoreVersion = "15.0";
  tinycoreArch    = "x86_64";
  isoFile         = "CorePure64-${tinycoreVersion}.iso";
  # Release archive path is stable per major version (e.g. 15.x → 15.x/x86_64).
  isoUrl          = "http://tinycorelinux.net/15.x/x86_64/release/${isoFile}";

  # Fetch the ISO.
  #
  # M41.1 HASH RESOLUTION — run inside the builder VM (not on host):
  #
  #   Official checksum source (TinyCore publishes a per-file .md5.txt sidecar,
  #   and the release dir is GPG-signed):
  #     http://tinycorelinux.net/15.x/x86_64/release/CorePure64-15.0.iso.md5.txt
  #
  #   Steps to resolve:
  #     1. Inside builder VM, run:
  #          nix-prefetch-url \
  #            http://tinycorelinux.net/15.x/x86_64/release/CorePure64-15.0.iso
  #        This prints the Nix base32 sha256 — use that value below.
  #     2. Alternatively fetch the .md5.txt sidecar to cross-check the download,
  #        then convert the sha256 you computed: nix hash to-base32 --type sha256 <hex>
  #     3. Replace pkgs.lib.fakeSha256 with the resulting string.
  #
  #   Expected format: "1abc2def3..." (Nix base32, 52 chars)
  #   or sri format:   "sha256-<base64>==" (use sha256 = "" with fetchurl)
  iso = pkgs.fetchurl {
    url    = isoUrl;
    sha256 = pkgs.lib.fakeSha256; # TODO(M41.1): replace with real hash — see instructions above
    name   = "corepure64-${tinycoreVersion}.iso";
  };

in {
  # The ready-to-boot TinyCore CorePure64 ISO.
  inherit iso;

  # Human-readable metadata for scripts.
  meta = {
    version = tinycoreVersion;
    arch    = tinycoreArch;
    url     = isoUrl;
  };
}

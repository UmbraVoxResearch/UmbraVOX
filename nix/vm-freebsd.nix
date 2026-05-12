# FreeBSD QEMU VM image descriptor for UmbraVOX cross-platform smoke testing.
#
# This Nix expression does not build a FreeBSD system from source — FreeBSD
# is not a NixOS target. Instead, it provides a derivation that downloads the
# official FreeBSD 14 RELEASE VM image (qcow2) from the FreeBSD CDN and
# exposes it as a Nix store path. The actual VM lifecycle (boot, install deps,
# build, test) is handled by scripts/vm-freebsd-setup.sh.
#
# Usage:
#   nix-build nix/vm-freebsd.nix           # fetches image into Nix store
#   make vm-smoke-freebsd                   # full smoke: fetch + boot + build + test
#
# The fetchurl hash must be updated when the FreeBSD release version changes.
# Run: nix-prefetch-url <url> to obtain the sha256.
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  # FreeBSD 14.2-RELEASE amd64 VM image (qcow2, xz-compressed).
  # Source: https://download.freebsd.org/releases/VM-IMAGES/
  freebsdVersion = "14.2-RELEASE";
  freebsdArch    = "amd64";
  imageFile      = "FreeBSD-${freebsdVersion}-${freebsdArch}.qcow2.xz";
  imageUrl       = "https://download.freebsd.org/releases/VM-IMAGES/${freebsdVersion}/${freebsdArch}/Latest/${imageFile}";

  # Fetch and decompress the VM image.
  # sha256 is set to lib.fakeSha256 so the first `nix-build` prints the real
  # hash; replace it after running: nix-prefetch-url <imageUrl>
  compressedImage = pkgs.fetchurl {
    url    = imageUrl;
    sha256 = pkgs.lib.fakeSha256;
    name   = "freebsd-${freebsdVersion}-${freebsdArch}.qcow2.xz";
  };

  # Decompress to a plain qcow2 so QEMU can boot it directly.
  image = pkgs.runCommand "freebsd-${freebsdVersion}-${freebsdArch}.qcow2" {
    nativeBuildInputs = [ pkgs.xz ];
  } ''
    xz --decompress --keep --stdout ${compressedImage} > $out
  '';

in {
  # The ready-to-boot FreeBSD qcow2 image.
  inherit image;

  # Convenience: expose the raw compressed download for caching scripts.
  inherit compressedImage;

  # Human-readable metadata for scripts.
  meta = {
    version = freebsdVersion;
    arch    = freebsdArch;
    url     = imageUrl;
  };
}

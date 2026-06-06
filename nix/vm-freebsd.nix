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
  #
  # M22.1 HASH RESOLUTION — run inside the builder VM (not on host):
  #
  #   Official checksum source (SHA-256, BSD-style format):
  #     https://download.freebsd.org/releases/VM-IMAGES/14.2-RELEASE/amd64/Latest/CHECKSUM.SHA256
  #
  #   Steps to resolve:
  #     1. Inside builder VM, run:
  #          nix-prefetch-url \
  #            https://download.freebsd.org/releases/VM-IMAGES/14.2-RELEASE/amd64/Latest/FreeBSD-14.2-RELEASE-amd64.qcow2.xz
  #        This prints the Nix base32 sha256 — use that value below.
  #     2. Alternatively, fetch CHECKSUM.SHA256 from the URL above,
  #        extract the hex SHA256 for FreeBSD-14.2-RELEASE-amd64.qcow2.xz,
  #        then convert: nix hash to-base32 --type sha256 <hex>
  #     3. Replace pkgs.lib.fakeSha256 with the resulting string.
  #
  #   Expected format: "1abc2def3..." (Nix base32, 52 chars)
  #   or sri format:   "sha256-<base64>==" (use sha256 = "" with fetchurl)
  compressedImage = pkgs.fetchurl {
    url    = imageUrl;
    sha256 = pkgs.lib.fakeSha256; # TODO(M22.1): replace with real hash — see instructions above
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

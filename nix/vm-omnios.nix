# OmniOS (illumos) QEMU VM image descriptor for UmbraVOX cross-platform smoke.
#
# OmniOS is not a NixOS target, so this Nix expression downloads the official
# OmniOS CE cloud/VM image and exposes it as a Nix store path. The VM lifecycle
# (boot, install GHC via ooce packages, build, test) is handled by
# scripts/vm-illumos-setup.sh.
#
# Usage:
#   nix-build nix/vm-omnios.nix             # fetches image into Nix store
#   make vm-smoke-illumos                   # full smoke: fetch + boot + build + test
#
# OmniOS release page: https://omnios.org/download
# The sha256 must be updated when the OmniOS release changes.
# Run: nix-prefetch-url <url> to obtain the sha256.
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  # OmniOS CE r151052 LTS — latest stable at time of writing.
  # The cloud image is a raw disk image wrapped in a bzip2 archive.
  omniosRelease = "r151052";
  imageFile     = "omnios-${omniosRelease}.cloud.vmdk.bz2";
  imageUrl      = "https://downloads.omnios.org/media/${omniosRelease}/${imageFile}";

  # M22.2 HASH RESOLUTION — run inside the builder VM (not on host):
  #
  #   Official checksum source:
  #     https://downloads.omnios.org/media/r151052/omnios-r151052.cloud.vmdk.bz2.sha256
  #   (OmniOS publishes a per-file .sha256 sidecar next to each image.)
  #
  #   Steps to resolve:
  #     1. Inside builder VM, run:
  #          nix-prefetch-url \
  #            https://downloads.omnios.org/media/r151052/omnios-r151052.cloud.vmdk.bz2
  #        This prints the Nix base32 sha256 — use that value below.
  #     2. Alternatively, fetch the .sha256 sidecar:
  #          curl https://downloads.omnios.org/media/r151052/omnios-r151052.cloud.vmdk.bz2.sha256
  #        then convert the hex digest: nix hash to-base32 --type sha256 <hex>
  #     3. Replace pkgs.lib.fakeSha256 with the resulting string.
  #
  #   Expected format: "1abc2def3..." (Nix base32, 52 chars)
  compressedImage = pkgs.fetchurl {
    url    = imageUrl;
    sha256 = pkgs.lib.fakeSha256; # TODO(M22.2): replace with real hash — see instructions above
    name   = "omnios-${omniosRelease}.cloud.vmdk.bz2";
  };

  # Decompress VMDK, then convert to qcow2 so QEMU can boot it directly.
  image = pkgs.runCommand "omnios-${omniosRelease}.qcow2" {
    nativeBuildInputs = [ pkgs.bzip2 pkgs.qemu ];
  } ''
    bzcat ${compressedImage} > omnios.vmdk
    qemu-img convert -f vmdk -O qcow2 omnios.vmdk $out
  '';

in {
  # The ready-to-boot OmniOS qcow2 image.
  inherit image;

  # Convenience: expose the raw compressed download for caching scripts.
  inherit compressedImage;

  # Human-readable metadata for scripts.
  meta = {
    release = omniosRelease;
    url     = imageUrl;
  };
}

# NixOS VM disk image for isolated UmbraVOX build/test/release.
#
# Contains the full development toolchain (GHC 9.6, cabal, F*, Z3, etc.)
# with zero external dependencies — no network access needed in-guest.
#
# The guest boots, mounts the source tree from /dev/vdb, copies it to a
# writable tmpfs, runs the pipeline, and shuts down.
#
# Build:
#   nix build .#vm-image            (via flake)
#   nix-build nix/vm-image.nix      (standalone)
#
# The output is a raw disk image: result/nixos.raw
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  hp = pkgs.haskell.packages.ghc96;

  devToolsPkgs = with pkgs; [
    (hp.ghcWithPackages (p: [ p.network ]))
    cabal-install
    gcc
    gdb
    valgrind
    coq
    tlaplus
    fstar
    z3
    go
    sqlite
    aflplusplus
    graphviz
    jq
    patchelf
    file
    zip
    gnumake
    git
    pkg-config
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
  ];

  # Pre-built F* verification cache.  Runs fstar.exe on all 17 specs
  # during the Nix build so the VM boots with warm .checked files.
  # Nix rebuilds this when spec sources or fstar/z3 versions change.
  # F* self-validates: stale .checked files are rejected at load time.
  fstarCache = pkgs.runCommand "umbravox-fstar-cache" {
    nativeBuildInputs = [ pkgs.fstar pkgs.z3 pkgs.coreutils ];
    src = ../test/evidence/formal-proofs/fstar;
  } ''
    mkdir -p work/_cache work/_output
    cp $src/Spec.*.fst work/

    # Resolve F* ulib path for standard library includes
    FSTAR_REAL=$(readlink -f $(which fstar.exe))
    FSTAR_HOME=$(dirname $(dirname $FSTAR_REAL))
    ULIB=""
    for d in "$FSTAR_HOME/lib/fstar/ulib" "$FSTAR_HOME/ulib" "$FSTAR_HOME/share/fstar/ulib"; do
      [ -d "$d" ] && ULIB="$d" && break
    done
    ULIB_FLAG=""
    [ -n "$ULIB" ] && ULIB_FLAG="--include $ULIB"

    # Two passes: first populates independent modules, second resolves
    # cross-module dependencies using the cache from pass one.
    for pass in 1 2; do
      echo "=== F* cache build pass $pass ==="
      for f in work/Spec.*.fst; do
        echo "  verifying $(basename $f) ..."
        fstar.exe \
          --cache_checked_modules \
          --already_cached "Prims,FStar" \
          --include work \
          --odir work/_output \
          --cache_dir work/_cache \
          --z3rlimit 600 \
          $ULIB_FLAG \
          "$f" 2>&1 || true
      done
    done

    mkdir -p $out
    cp work/_cache/*.checked $out/ 2>/dev/null || true
    count=$(ls $out/*.checked 2>/dev/null | wc -l)
    echo "F* cache complete: $count checked files"
  '';

  nixosConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

    boot.loader.grub.device = "/dev/vda";
    boot.kernelParams = [ "console=ttyS0" "panic=1" ];
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_scsi" "virtio_net" "ext4"
    ];

    fileSystems."/" = {
      device = "/dev/vda1";
      fsType = "ext4";
    };

    swapDevices = [];
    networking.hostName = "umbravox-vm";
    networking.firewall.enable = false;

    # Serial console — auto-login root
    systemd.services."serial-getty@ttyS0".enable = true;
    services.getty.autologinUser = "root";

    # All dev tools pre-installed
    environment.systemPackages = devToolsPkgs;

    # Writable workspace (tmpfs, sized for build artifacts)
    fileSystems."/work" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "size=8G" "mode=1777" ];
    };

    # Auto-run smoke pipeline on boot, then shut down
    systemd.services.umbravox-smoke = {
      description = "UmbraVOX isolated build/test/release smoke";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" "network.target" ];
      path = devToolsPkgs ++ [ pkgs.mount pkgs.util-linux ];
      environment = {
        HOME = "/root";
        CABAL_DIR = "/root/.cabal";
      };
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "umbravox-vm-smoke" ''
          set -euo pipefail
          export PATH=/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH

          # Offline cabal config (no network in guest)
          mkdir -p /root/.cabal
          cat > /root/.cabal/config << 'CABALEOF'
offline: True
nix: False
CABALEOF

          # Mount source disk and delegate to the in-guest script
          mkdir -p /mnt/src
          mount -o ro /dev/vdb /mnt/src
          exec /run/current-system/sw/bin/bash /mnt/src/scripts/vm-smoke-run.sh
        '';
        StandardOutput = "journal+console";
        StandardError = "journal+console";
        TimeoutStartSec = "1800";
      };
    };

    # Shut down after smoke completes (success or failure)
    systemd.services.umbravox-shutdown = {
      description = "Shutdown after smoke";
      wantedBy = [ "multi-user.target" ];
      after = [ "umbravox-smoke.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.systemd}/bin/systemctl poweroff";
      };
    };

    # Pre-built F* verification cache baked into the image.
    # vm-smoke-run.sh copies this to the workspace before running verify.
    environment.etc."umbravox-fstar-cache".source = fstarCache;

    # FHS compatibility for Makefile (SHELL := /bin/bash) and shebangs
    system.activationScripts.fhsCompat = ''
      mkdir -p /bin /usr/bin
      ln -sf /run/current-system/sw/bin/bash /bin/bash
      ln -sf /run/current-system/sw/bin/sh /bin/sh
      ln -sf /run/current-system/sw/bin/env /usr/bin/env
    '';

    # Minimize image size
    documentation.enable = false;
    programs.command-not-found.enable = false;
    services.udisks2.enable = false;
    security.polkit.enable = false;
    xdg.mime.enable = false;
    xdg.icons.enable = false;
    nix.enable = false;

    system.stateVersion = "25.05";
  };

  nixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = nixosConfig;
  };

  image = import (pkgs.path + "/nixos/lib/make-disk-image.nix") {
    inherit pkgs;
    lib = pkgs.lib;
    config = nixos.config;
    diskSize = "auto";
    additionalSpace = "1024M";
    format = "raw";
    partitionTableType = "legacy";
    copyChannel = false;
  };

  # Firecracker-specific NixOS config: no GRUB, rootfs is /dev/vda directly
  firecrackerNixosConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

    boot.loader.grub.enable = false;
    boot.kernelParams = [ "console=ttyS0" "panic=1" "reboot=k" ];
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_net" "ext4"
    ];

    fileSystems."/" = {
      device = "/dev/vda";
      fsType = "ext4";
    };

    swapDevices = [];
    networking.hostName = "umbravox-vm";
    networking.firewall.enable = false;

    systemd.services."serial-getty@ttyS0".enable = true;
    services.getty.autologinUser = "root";

    environment.systemPackages = devToolsPkgs;

    fileSystems."/work" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "size=8G" "mode=1777" ];
    };

    documentation.enable = false;
    programs.command-not-found.enable = false;
    services.udisks2.enable = false;
    security.polkit.enable = false;
    xdg.mime.enable = false;
    xdg.icons.enable = false;
    nix.enable = false;

    system.stateVersion = "25.05";
  };

  firecrackerNixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = firecrackerNixosConfig;
  };

  firecrackerRootfs = import (pkgs.path + "/nixos/lib/make-disk-image.nix") {
    inherit pkgs;
    lib = pkgs.lib;
    config = firecrackerNixos.config;
    diskSize = "auto";
    additionalSpace = "512M";
    format = "raw";
    partitionTableType = "none";
    copyChannel = false;
  };

in {
  # QEMU: full bootable disk image with GRUB + partition table
  qemu = image;

  # Firecracker: rootfs-only ext4 image (no partition table, no bootloader)
  firecrackerRootfs = firecrackerRootfs;

  # Firecracker: uncompressed vmlinux kernel (ELF, not bzImage)
  firecrackerKernel = "${firecrackerNixos.config.system.build.kernel.dev}/vmlinux";
}

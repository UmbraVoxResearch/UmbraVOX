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
  # Auto-detect F* cache from the source tree.
  # The two-stage VM build copies .checked files to nix/fstar-cache/
  # before rebuilding the image. If present, they're baked in.
  fstarCacheDir = ../nix/fstar-cache;
  fstarCachePath = if builtins.pathExists fstarCacheDir
    then fstarCacheDir
    else null;

  hp = pkgs.haskell.packages.ghc96;

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
    tmux
    aha
    asciinema
  ]);

  nixosConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ./vm-base.nix ];

    boot.loader.grub.device = "/dev/vda";
    # QEMU image also loads virtio_scsi (not needed for Firecracker)
    boot.initrd.availableKernelModules = [
      "virtio_pci" "virtio_blk" "virtio_scsi" "virtio_net" "ext4"
    ];

    # All dev tools pre-installed
    environment.systemPackages = devToolsPkgs;

    # Auto-run smoke pipeline on boot, then shut down
    systemd.services.umbravox-smoke = {
      description = "UmbraVOX isolated build/test/release smoke";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" "umbravox-dev-init.service" ];
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
          # vm-dev/vm-build/vm-test inject .vm-init.sh; skip smoke in that mode.
          if [ -f /mnt/src/.vm-init.sh ]; then
            echo "[VM-SMOKE] vm-dev mode detected (.vm-init.sh present); skipping smoke pipeline."
            exit 0
          fi
          exec /run/current-system/sw/bin/bash /mnt/src/scripts/vm-smoke-run.sh
        '';
        StandardOutput = "journal+console";
        StandardError = "journal+console";
        TimeoutStartSec = "1800";
      };
    };

    # Auto-run .vm-init.sh from source disk if present (vm-dev exec mode)
    systemd.services.umbravox-dev-init = {
      description = "UmbraVOX VM development init";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" ];
      before = [ "umbravox-smoke.service" ];
      path = devToolsPkgs ++ [ pkgs.mount pkgs.util-linux pkgs.e2fsprogs ];
      environment = {
        HOME = "/root";
        CABAL_DIR = "/root/.cabal";
        TERM = "xterm-256color";
      };
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "umbravox-vm-dev-init" ''
          set -euo pipefail
          export PATH=/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH

          # Mount source disk
          mkdir -p /mnt/src
          mount -o ro /dev/vdb /mnt/src 2>/dev/null || true

          # Run .vm-init.sh if it exists on the source disk
          if [ -f /mnt/src/.vm-init.sh ]; then
            exec /run/current-system/sw/bin/bash /mnt/src/.vm-init.sh
          fi
        '';
        StandardOutput = "journal+console";
        StandardError = "journal+console";
        TimeoutStartSec = "3600";
      };
    };

    # Shut down after both smoke and dev-init complete (success or failure)
    systemd.services.umbravox-shutdown = {
      description = "Shutdown after smoke and dev-init";
      wantedBy = [ "multi-user.target" ];
      after = [ "umbravox-smoke.service" "umbravox-dev-init.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.systemd}/bin/systemctl poweroff";
      };
    };

    # Pre-built F* cache baked into image (from two-stage VM build).
    # When fstarCachePath is provided, .checked files are pre-installed;
    # when null, the VM runs F* verification cold (slower but still works).
    environment.etc."umbravox-fstar-cache" = lib.mkIf (fstarCachePath != null) {
      source = fstarCachePath;
    };
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
    # The full dev-toolchain closure (GHC, F*, Coq, GCC, AFL++, etc.) is
    # ~14 GB.  16384M gives enough headroom for the rootfs plus logs.
    # TODO: trim closure — avahi comes from qemu-guest.nix profile,
    # GCC/valgrind/gdb are only needed for C builds, and coq/tlaplus
    # could be split into a separate "proof" image layer.
    # NOTE: The full dev-toolchain closure requires ~30GB total disk image.
    # If `make vm-image-build` fails with "cptofs failed", check `df -h /`
    # — the Nix sandbox needs enough free space on the root filesystem to
    # create the raw disk image. With 16GB of additional space on top of
    # the auto-calculated closure, the total image is ~30GB.
    # To reduce image size, use M20.5.8 (Nix-native build in builder VM)
    # which eliminates GHC/Coq/F* from the runtime image.
    additionalSpace = "16384M";
    format = "raw";
    partitionTableType = "legacy";
    copyChannel = false;
  };

  # Firecracker-specific NixOS config: no GRUB, rootfs is /dev/vda directly
  firecrackerNixosConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ./vm-base.nix ];

    boot.loader.grub.enable = false;
    boot.kernelParams = [ "console=ttyS0" "panic=1" "reboot=k" ];

    # Firecracker rootfs is /dev/vda (no partition table)
    fileSystems."/" = {
      device = "/dev/vda";
      fsType = "ext4";
    };

    environment.systemPackages = devToolsPkgs;
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
    additionalSpace = "2048M";
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

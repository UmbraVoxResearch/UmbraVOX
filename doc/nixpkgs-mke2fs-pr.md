# nixpkgs PR: Replace cptofs with mke2fs -d in make-disk-image.nix

## Title

make-disk-image: replace cptofs (LKL) with mke2fs -d

## Description

`make-disk-image.nix` currently uses `cptofs` to populate disk images.
cptofs relies on LKL (Linux Kernel Library) which runs a userspace Linux
kernel with only 100MB of RAM. This causes I/O errors when building large
disk images (>4GB), especially inside nested VMs or constrained sandboxes.

`mke2fs -d` (part of e2fsprogs, available since 1.43/2016) does the same
thing — creates and populates an ext4 filesystem from a directory — but
runs natively with no RAM limitation. It is already a build dependency
via `e2fsprogs`.

## The change

In `nixos/lib/make-disk-image.nix`, replace the cptofs invocation with
`mke2fs -d` by adding `-d $root` to the existing `mkfs.ext4` call and
removing the separate cptofs step.

### Before (lines 614-632)

```nix
${
  if partitionTableType != "none" then
    ''
      eval $(partx $diskImage -o START,SECTORS --nr ${rootPartition} --pairs)
      mkfs.${fsType} -b ${blockSize} -F -L ${label} $diskImage -E offset=$(sectorsToBytes $START) $(sectorsToKilobytes $SECTORS)K
    ''
  else
    ''
      mkfs.${fsType} -b ${blockSize} -F -L ${label} $diskImage
    ''
}

echo "copying staging root to image..."
cptofs -p ${lib.optionalString (partitionTableType != "none") "-P ${rootPartition}"} \
       -t ${fsType} \
       -i $diskImage \
       $root${lib.optionalString onlyNixStore builtins.storeDir}/* / ||
  (echo >&2 "ERROR: cptofs failed. diskSize might be too small for closure."; exit 1)
```

### After

```nix
${
  if partitionTableType != "none" then
    ''
      eval $(partx $diskImage -o START,SECTORS --nr ${rootPartition} --pairs)
      mkfs.${fsType} -b ${blockSize} -F -L ${label} \
        -d $root${lib.optionalString onlyNixStore builtins.storeDir} \
        $diskImage -E offset=$(sectorsToBytes $START) $(sectorsToKilobytes $SECTORS)K
    ''
  else
    ''
      mkfs.${fsType} -b ${blockSize} -F -L ${label} \
        -d $root${lib.optionalString onlyNixStore builtins.storeDir} \
        $diskImage
    ''
}
```

The `cptofs` call and its error handling are removed entirely.

## Why this is better

| | cptofs (LKL) | mke2fs -d |
|---|---|---|
| RAM usage | Fixed 100MB (LKL kernel) | Native, uses host memory |
| Large images (>4GB) | Fails with I/O errors | Works at any size |
| Nested VM builds | Unreliable | Works identically |
| Dependency | cptofs + LKL (separate package) | e2fsprogs (already required for mkfs) |
| Speed | Boots userspace Linux kernel | Direct filesystem creation |
| Permissions | Limited (no ownership, only modes) | Full (via -d directory traversal) |
| Available since | N/A | e2fsprogs 1.43 (2016) |

## Testing

- Built a 26GB NixOS disk image (full dev toolchain: GHC, Coq, F*, Z3,
  AFL++, GCC, valgrind) with `mke2fs -d`. Image boots and functions
  identically to the cptofs-built version.
- Tested with `partitionTableType = "legacy"` (MBR) and `format = "raw"`.
- The `diskSize = "auto"` auto-sizing logic is unchanged.

## Affected files

- `nixos/lib/make-disk-image.nix` — replace cptofs with mke2fs -d
- Optionally remove `cptofs` from `buildInputs` if no other consumer

## Notes

- `mke2fs -d` handles symlinks, permissions, and directory structure
  correctly. The `-d` flag was specifically designed for this use case.
- The `rsync --chown` workaround in the VM phase (for setting ownerships
  that cptofs couldn't handle) may still be needed, as `mke2fs -d` sets
  ownership from the source directory's metadata.
- Only affects ext2/ext3/ext4 filesystems. Other fsTypes (btrfs, xfs)
  would need different tools, but ext4 covers the vast majority of
  NixOS disk image use cases.

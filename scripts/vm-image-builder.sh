#!/usr/bin/env bash
# ── UmbraVOX VM Image Builder ──────────────────────────────────────────
# Builds the NixOS VM image inside a builder VM instead of on the host.
# This keeps the host /nix/store untouched and avoids consuming ~30GB
# of host disk space for the image build.
#
# The builder VM boots with:
#   - The project source mounted via ext2 disk (read-only)
#   - A 60GB scratch qcow2 disk for /nix/store
#   - A 9p output share for extracting the built image
#
# After the build completes, the result is at build/vm/image/nixos.img
# and the scratch disk is deleted (no persistent builder state).
#
# Usage (called by Makefile, not directly by users):
#   scripts/vm-image-builder.sh
#
# Requires: qemu-system-x86_64, genext2fs, nix (for building the builder image), /dev/kvm
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
VM_CACHE_DIR="$REPO_ROOT/build/vm"
VM_TMP_DIR="$VM_CACHE_DIR/tmp"
BUILDER_IMAGE_DIR="$VM_CACHE_DIR/builder-image"
OUTPUT_DIR="$VM_CACHE_DIR/builder-output"

mkdir -p "$VM_TMP_DIR" "$OUTPUT_DIR"

# ── Preflight ──────────────────────────────────────────────────────────

preflight_check() {
    local ok=1
    if [ ! -e /dev/kvm ]; then
        echo -e "${RED}[VM-BUILDER]${NC} /dev/kvm not found; KVM required"
        ok=0
    fi
    if ! command -v qemu-system-x86_64 >/dev/null 2>&1; then
        echo -e "${RED}[VM-BUILDER]${NC} qemu-system-x86_64 not on PATH"
        ok=0
    fi
    if ! command -v genext2fs >/dev/null 2>&1; then
        echo -e "${RED}[VM-BUILDER]${NC} genext2fs not on PATH"
        ok=0
    fi
    if [ "$ok" -eq 0 ]; then
        exit 1
    fi
}

# ── Build or locate builder VM image ──────────────────────────────────

ensure_builder_image() {
    if [ -L "$BUILDER_IMAGE_DIR" ] && [ -e "$BUILDER_IMAGE_DIR/nixos.img" ]; then
        echo -e "${BLUE}[VM-BUILDER]${NC} Builder image already cached."
        return 0
    fi

    echo -e "${BLUE}[VM-BUILDER]${NC} Building builder VM image (lightweight, one-time)..."

    # Resolve nix on PATH
    if [ -z "$(command -v nix 2>/dev/null)" ] && [ -x /nix/var/nix/profiles/default/bin/nix ]; then
        export PATH="/nix/var/nix/profiles/default/bin:$PATH"
    fi

    if ! command -v nix >/dev/null 2>&1 && ! command -v nix-build >/dev/null 2>&1; then
        echo -e "${RED}[VM-BUILDER]${NC} Neither 'nix' nor 'nix-build' is available on PATH."
        echo "  The builder VM image must be built once with Nix."
        echo "  Install Nix or add /nix/var/nix/profiles/default/bin to PATH."
        exit 1
    fi

    local tmpdir="$VM_TMP_DIR"
    mkdir -p "$tmpdir"
    export TMPDIR="$tmpdir"

    if ! (
        nix --extra-experimental-features "nix-command flakes" \
            --option build-dir "$tmpdir" \
            build -L "$REPO_ROOT/nix/vm-builder.nix" -o "$BUILDER_IMAGE_DIR" 2>&1 || \
        nix-build --option build-dir "$tmpdir" \
            "$REPO_ROOT/nix/vm-builder.nix" -o "$BUILDER_IMAGE_DIR" 2>&1
    ); then
        echo -e "${RED}[VM-BUILDER]${NC} Failed to build builder VM image."
        exit 1
    fi

    echo -e "${GREEN}[VM-BUILDER]${NC} Builder image ready at $BUILDER_IMAGE_DIR"
}

# ── Create source disk ────────────────────────────────────────────────

create_source_disk() {
    local disk_path
    disk_path="$(mktemp "$VM_TMP_DIR/umbravox-builder-src.XXXXXX.ext2")"
    rm -f "$disk_path"
    local src_dir
    src_dir="$(mktemp -d "$VM_TMP_DIR/umbravox-builder-srcdir.XXXXXX")"

    echo -e "${BLUE}[VM-BUILDER]${NC} Exporting source tree..." >&2
    tar -C "$REPO_ROOT" \
        --exclude='.git' \
        --exclude='dist-newstyle' \
        --exclude='build' \
        --exclude='result' \
        -cf - . | tar -xf - -C "$src_dir"

    echo -e "${BLUE}[VM-BUILDER]${NC} Creating source disk..." >&2
    genext2fs -b 1048576 -d "$src_dir" "$disk_path"
    rm -rf "$src_dir"
    echo "$disk_path"
}

# ── Main ──────────────────────────────────────────────────────────────

preflight_check

# Check if image is already built
if [ -d "$VM_CACHE_DIR/image" ] || [ -L "$VM_CACHE_DIR/image" ]; then
    if [ -e "$VM_CACHE_DIR/image/nixos.img" ]; then
        echo -e "${GREEN}[VM-BUILDER]${NC} VM image already cached at build/vm/image"
        exit 0
    fi
fi

# Step 1: Ensure builder VM image exists
ensure_builder_image

# Step 2: Create source disk
SRC_DISK="$(create_source_disk)"

# Step 3: Create scratch disk (60GB, thin-provisioned qcow2)
SCRATCH_DISK="$(mktemp "$VM_TMP_DIR/umbravox-builder-scratch.XXXXXX.qcow2")"
rm -f "$SCRATCH_DISK"
echo -e "${BLUE}[VM-BUILDER]${NC} Creating 60GB scratch disk (thin-provisioned)..."
qemu-img create -f qcow2 "$SCRATCH_DISK" 60G >/dev/null 2>&1

# Step 4: Create COW overlay on the builder image
BOOT_IMG="$(readlink -f "$BUILDER_IMAGE_DIR/nixos.img")"
OVERLAY="$(mktemp "$VM_TMP_DIR/umbravox-builder-overlay.XXXXXX.qcow2")"
rm -f "$OVERLAY"
echo -e "${BLUE}[VM-BUILDER]${NC} Creating COW overlay on builder image..."
qemu-img create -f qcow2 -b "$BOOT_IMG" -F raw "$OVERLAY" >/dev/null 2>&1

# Step 5: Clean output directory
rm -f "$OUTPUT_DIR/nixos.img" "$OUTPUT_DIR/builder-status" 2>/dev/null || true

# Step 6: Auto-scale VM resources (give builder generous resources)
HOST_CORES=$(nproc 2>/dev/null || echo 4)
HOST_MEM_MB=$(awk '/MemTotal/{printf "%d", $2/1024}' /proc/meminfo 2>/dev/null || echo 8192)
# Builder gets 75% of host resources (it's a batch job)
VM_CORES=$(( HOST_CORES * 3 / 4 ))
VM_MEM_MB=$(( HOST_MEM_MB * 3 / 4 ))
[ "$VM_CORES" -lt 2 ] && VM_CORES=2
[ "$VM_MEM_MB" -lt 4096 ] && VM_MEM_MB=4096

echo -e "${BLUE}[VM-BUILDER]${NC} VM resources: ${VM_CORES} cores, ${VM_MEM_MB}MB RAM (host: ${HOST_CORES} cores, ${HOST_MEM_MB}MB)"
echo -e "${BLUE}[VM-BUILDER]${NC} Network: disabled (offline build)"
echo -e "${BLUE}[VM-BUILDER]${NC} Booting builder VM..."
echo ""

# Step 7: Boot the builder VM
set +e
qemu-system-x86_64 \
    -machine "q35,accel=kvm" \
    -cpu max \
    -m "$VM_MEM_MB" \
    -smp "$VM_CORES" \
    -drive "if=virtio,format=qcow2,file=$OVERLAY" \
    -drive "if=virtio,format=raw,file=$SRC_DISK,readonly=on" \
    -drive "if=virtio,format=qcow2,file=$SCRATCH_DISK" \
    -virtfs "local,path=$OUTPUT_DIR,mount_tag=output,security_model=mapped-xattr,id=output" \
    -nic none \
    -nographic -nodefaults -serial stdio \
    -no-reboot
QEMU_EXIT=$?
set -e

echo ""

# Step 8: Cleanup temporary files
echo -e "${BLUE}[VM-BUILDER]${NC} Cleaning up scratch disk and overlays..."
rm -f "$OVERLAY" "$SRC_DISK" "$SCRATCH_DISK" 2>/dev/null || true

# Step 9: Check build status
BUILD_STATUS=1
if [ -f "$OUTPUT_DIR/builder-status" ]; then
    STATUS_RAW="$(head -n 1 "$OUTPUT_DIR/builder-status" 2>/dev/null || true)"
    if [[ "$STATUS_RAW" =~ ^[0-9]+$ ]]; then
        BUILD_STATUS="$STATUS_RAW"
    fi
fi

if [ "$BUILD_STATUS" -ne 0 ]; then
    echo -e "${RED}[VM-BUILDER]${NC} Builder VM reported failure (status $BUILD_STATUS)."
    echo "  Check the console output above for details."
    rm -f "$OUTPUT_DIR/builder-status" 2>/dev/null || true
    exit 1
fi

# Step 10: Move the built image into place
if [ -f "$OUTPUT_DIR/nixos.img" ]; then
    mkdir -p "$VM_CACHE_DIR/image"
    mv "$OUTPUT_DIR/nixos.img" "$VM_CACHE_DIR/image/nixos.img"
    rm -f "$OUTPUT_DIR/builder-status" 2>/dev/null || true
    echo -e "${GREEN}[VM-BUILDER]${NC} VM image built successfully at build/vm/image/nixos.img"
else
    echo -e "${RED}[VM-BUILDER]${NC} Builder reported success but no nixos.img found in output."
    rm -f "$OUTPUT_DIR/builder-status" 2>/dev/null || true
    exit 1
fi

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

source "$(dirname "$0")/lib-vm.sh" 2>/dev/null || true

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
VM_CACHE_DIR="$REPO_ROOT/build/vm"
VM_TMP_DIR="$VM_CACHE_DIR/tmp"
BUILDER_IMAGE_DIR="$VM_CACHE_DIR/builder-image"
OUTPUT_DIR="$VM_CACHE_DIR/builder-output"
SEED_IMAGE_DIR="$VM_CACHE_DIR/seed-image"
SEED_VERSION="0.1.0"
SEED_SHA256="cad98e2f2c29d4c1bdf7f08c46cebe51ffb5ee60cda7d71efc38f185653f2914"
SEED_DEFAULT_URL="${UMBRAVOX_SEED_URL:-https://github.com/UmbraVoxResearch/UmbraVOX/releases/download/seed-v${SEED_VERSION}/umbravox-seed.img}"

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

# ── Seed image (two-stage bootstrap, stage 1) ────────────────────────

ensure_seed_image() {
    if [ -e "$SEED_IMAGE_DIR/nixos.img" ]; then
        echo -e "${BLUE}[VM-BUILDER]${NC} Seed image already cached."
        return 0
    fi

    echo -e "${YELLOW}[VM-BUILDER]${NC} Builder VM image not found. Bootstrap options:"
    echo "  [A] Download seed image (~300MB) and build in VM (default, no host writes)"
    echo "  [B] Build locally with nix-build (faster, writes to /nix/store)"
    printf "Choose [A/b]: "
    read -r ans
    case "$ans" in
        [Bb]*)
            # Option B: local nix-build
            if [ -z "$(command -v nix 2>/dev/null)" ] && [ -x /nix/var/nix/profiles/default/bin/nix ]; then
                export PATH="/nix/var/nix/profiles/default/bin:$PATH"
            fi
            if ! command -v nix-build >/dev/null 2>&1; then
                echo -e "${RED}[VM-BUILDER]${NC} nix-build not found."
                echo "  Install Nix (https://nixos.org/) or add it to PATH."
                exit 1
            fi
            echo -e "${YELLOW}[VM-BUILDER]${NC} WARNING: This writes to /nix/store on the host."
            echo -e "${BLUE}[VM-BUILDER]${NC} Building seed image locally (may take several minutes)..."
            mkdir -p "$VM_TMP_DIR"
            if ! TMPDIR="$VM_TMP_DIR" nix-build "$REPO_ROOT/nix/vm-seed.nix" -o "$SEED_IMAGE_DIR"; then
                echo -e "${RED}[VM-BUILDER]${NC} Failed to build seed image."
                exit 1
            fi
            echo -e "${GREEN}[VM-BUILDER]${NC} Seed image ready at $SEED_IMAGE_DIR"
            ;;
        *)
            # Option A (default): download seed image
            mkdir -p "$SEED_IMAGE_DIR"
            if [ "$SEED_SHA256" = "PLACEHOLDER_HASH_UPDATE_AFTER_FIRST_BUILD" ]; then
                echo -e "${YELLOW}[VM-BUILDER]${NC} WARNING: Seed hash is placeholder (dev mode) — skipping verification."
                echo -e "${BLUE}[VM-BUILDER]${NC} Downloading seed image from $SEED_DEFAULT_URL ..."
                if command -v curl >/dev/null 2>&1; then
                    curl -L --progress-bar -o "$SEED_IMAGE_DIR/nixos.img" "$SEED_DEFAULT_URL"
                elif command -v wget >/dev/null 2>&1; then
                    wget -q --show-progress -O "$SEED_IMAGE_DIR/nixos.img" "$SEED_DEFAULT_URL"
                else
                    echo -e "${RED}[VM-BUILDER]${NC} Neither curl nor wget found."
                    exit 1
                fi
            else
                if ! vm_download_and_verify "$SEED_DEFAULT_URL" "$SEED_IMAGE_DIR/nixos.img" "$SEED_SHA256"; then
                    echo -e "${RED}[VM-BUILDER]${NC} Seed image download/verification failed."
                    rm -f "$SEED_IMAGE_DIR/nixos.img"
                    exit 1
                fi
            fi
            echo -e "${GREEN}[VM-BUILDER]${NC} Seed image downloaded to $SEED_IMAGE_DIR"
            ;;
    esac
}

# ── Build builder image from seed (two-stage bootstrap, stage 2) ─────

build_builder_from_seed() {
    echo -e "${BLUE}[VM-BUILDER]${NC} Building builder image from seed (stage 2)..."

    # Create COW overlay on seed image
    local SEED_IMG
    SEED_IMG="$(readlink -f "$SEED_IMAGE_DIR/nixos.img")"
    local SEED_OVERLAY
    SEED_OVERLAY="$(mktemp "$VM_TMP_DIR/umbravox-seed-overlay.XXXXXX.qcow2")"
    rm -f "$SEED_OVERLAY"
    echo -e "${BLUE}[VM-BUILDER]${NC} Creating COW overlay on seed image..."
    qemu-img create -f qcow2 -b "$SEED_IMG" -F raw "$SEED_OVERLAY" >/dev/null 2>&1

    # Create source disk
    local SRC_DISK
    SRC_DISK="$(create_source_disk)"

    # Ensure nix cache disk exists
    NIX_CACHE_DISK="$VM_CACHE_DIR/nix-cache.qcow2"
    if [ ! -f "$NIX_CACHE_DISK" ]; then
        echo -e "${BLUE}[VM-BUILDER]${NC} Creating 60GB nix store cache disk (first build, thin-provisioned)..."
        qemu-img create -f qcow2 "$NIX_CACHE_DISK" 60G >/dev/null 2>&1
    else
        echo -e "${BLUE}[VM-BUILDER]${NC} Reusing nix store cache disk (offline build possible)."
    fi

    # Prepare output directory
    mkdir -p "$BUILDER_IMAGE_DIR"
    rm -f "$BUILDER_IMAGE_DIR/seed-status" 2>/dev/null || true

    # Auto-scale VM resources
    local HOST_CORES HOST_MEM_MB VM_CORES VM_MEM_MB
    HOST_CORES=$(nproc 2>/dev/null || echo 4)
    HOST_MEM_MB=$(awk '/MemTotal/{printf "%d", $2/1024}' /proc/meminfo 2>/dev/null || echo 8192)
    VM_CORES=$(( HOST_CORES * 3 / 4 ))
    VM_MEM_MB=$(( HOST_MEM_MB * 3 / 4 ))
    [ "$VM_CORES" -lt 2 ] && VM_CORES=2
    [ "$VM_MEM_MB" -lt 4096 ] && VM_MEM_MB=4096

    echo -e "${BLUE}[VM-BUILDER]${NC} VM resources: ${VM_CORES} cores, ${VM_MEM_MB}MB RAM (host: ${HOST_CORES} cores, ${HOST_MEM_MB}MB)"
    echo -e "${BLUE}[VM-BUILDER]${NC} Network: enabled (seed bootstrap)"
    echo -e "${BLUE}[VM-BUILDER]${NC} Booting seed VM..."
    echo ""

    set +e
    qemu-system-x86_64 \
        -machine "q35,accel=kvm" \
        -cpu max \
        -m "$VM_MEM_MB" \
        -smp "$VM_CORES" \
        -drive "if=virtio,format=qcow2,file=$SEED_OVERLAY" \
        -drive "if=virtio,format=raw,file=$SRC_DISK,readonly=on" \
        -drive "if=virtio,format=qcow2,file=$NIX_CACHE_DISK" \
        -virtfs "local,path=$BUILDER_IMAGE_DIR,mount_tag=output,security_model=mapped-xattr,id=output" \
        -nic user,model=virtio \
        -nographic -nodefaults -serial stdio \
        -no-reboot
    local QEMU_EXIT=$?
    set -e

    echo ""

    # Clean up overlay and source disk (keep nix-cache)
    echo -e "${BLUE}[VM-BUILDER]${NC} Cleaning up seed overlay and source disk..."
    rm -f "$SEED_OVERLAY" "$SRC_DISK" 2>/dev/null || true

    # Check seed build status
    if [ -f "$BUILDER_IMAGE_DIR/seed-status" ]; then
        local STATUS_RAW
        STATUS_RAW="$(head -n 1 "$BUILDER_IMAGE_DIR/seed-status" 2>/dev/null || true)"
        if [[ "$STATUS_RAW" =~ ^[0-9]+$ ]] && [ "$STATUS_RAW" -eq 0 ]; then
            if [ -e "$BUILDER_IMAGE_DIR/nixos.img" ]; then
                echo -e "${GREEN}[VM-BUILDER]${NC} Builder image built successfully from seed."
                rm -f "$BUILDER_IMAGE_DIR/seed-status" 2>/dev/null || true
                return 0
            fi
        fi
    fi

    echo -e "${RED}[VM-BUILDER]${NC} Seed VM failed to produce builder image."
    echo "  Check the console output above for details."
    rm -f "$BUILDER_IMAGE_DIR/seed-status" 2>/dev/null || true
    exit 1
}

# ── Ensure builder VM image exists ───────────────────────────────────

ensure_builder_image() {
    if [ -e "$BUILDER_IMAGE_DIR/nixos.img" ]; then
        echo -e "${BLUE}[VM-BUILDER]${NC} Builder image already cached."
        return 0
    fi
    ensure_seed_image
    build_builder_from_seed
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

# Step 3: Use persistent nix store cache disk (survives across builds)
# This avoids re-downloading ~6GB of GHC/Coq/F*/Z3 on every build and
# enables fully offline rebuilds after the first successful build.
NIX_CACHE_DISK="$VM_CACHE_DIR/nix-cache.qcow2"
if [ ! -f "$NIX_CACHE_DISK" ]; then
    echo -e "${BLUE}[VM-BUILDER]${NC} Creating 60GB nix store cache disk (first build, thin-provisioned)..."
    qemu-img create -f qcow2 "$NIX_CACHE_DISK" 60G >/dev/null 2>&1
else
    echo -e "${BLUE}[VM-BUILDER]${NC} Reusing nix store cache disk (offline build possible)."
fi
SCRATCH_DISK="$NIX_CACHE_DISK"

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

# Step 8: Cleanup temporary files (keep nix cache disk for offline rebuilds)
echo -e "${BLUE}[VM-BUILDER]${NC} Cleaning up overlays and source disk (keeping nix cache)..."
rm -f "$OVERLAY" "$SRC_DISK" 2>/dev/null || true
# NIX_CACHE_DISK is NOT deleted — it persists at build/vm/nix-cache.qcow2
# for offline rebuilds. Use `make vm-cache-clean` to delete it.

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

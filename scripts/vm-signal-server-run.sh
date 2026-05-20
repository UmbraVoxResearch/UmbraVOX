#!/usr/bin/env bash
# ── UmbraVOX Signal-Server VM Runner ────────────────────────────────
# Two-stage approach:
#   build-jar:   Stage 1 — boot build VM with network, run Maven, output JAR
#   interactive: Stage 2 — boot runtime VM with services
#   check:       Stage 2 — boot runtime VM, health-check, exit
#
# Usage:
#   vm-signal-server-run.sh build-jar    # Stage 1: build Signal-Server JAR
#   vm-signal-server-run.sh interactive  # Stage 2: interactive runtime VM
#   vm-signal-server-run.sh check        # Stage 2: health-check and exit
#
# Requires: qemu-system-x86_64, /dev/kvm
set -euo pipefail

MODE="${1:-check}"

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
VM_CACHE_DIR="$REPO_ROOT/build/vm-signal-server"
VM_IMAGE_PATH="$VM_CACHE_DIR/image"
BUILD_VM_IMAGE_PATH="$VM_CACHE_DIR/build-image"
JAR_OUTPUT_DIR="$REPO_ROOT/build/signal-server-jar"

# ── Preflight ──────────────────────────────────────────────────────────

preflight_check() {
    local ok=1
    if [ ! -e /dev/kvm ]; then
        echo -e "${RED}[SIGNAL-VM]${NC} /dev/kvm not found; KVM required"
        ok=0
    fi
    if ! command -v qemu-system-x86_64 >/dev/null 2>&1; then
        echo -e "${RED}[SIGNAL-VM]${NC} qemu-system-x86_64 not on PATH"
        ok=0
    fi
    if [ "$ok" -eq 0 ]; then
        exit 1
    fi
}

# ── VM resource scaling ───────────────────────────────────────────────

scale_resources() {
    local fraction="${1:-4}"
    HOST_CORES=$(nproc 2>/dev/null || echo 4)
    HOST_MEM_MB=$(awk '/MemTotal/{printf "%d", $2/1024}' /proc/meminfo 2>/dev/null || echo 8192)
    VM_CORES=$(( HOST_CORES / fraction ))
    VM_MEM_MB=$(( HOST_MEM_MB / fraction ))
    [ "$VM_CORES" -lt 2 ] && VM_CORES=2
    [ "$VM_MEM_MB" -lt 2048 ] && VM_MEM_MB=2048
}

# ── Stage 1: Build JAR ───────────────────────────────────────────────

build_jar() {
    if [ ! -d "$BUILD_VM_IMAGE_PATH" ]; then
        echo -e "${RED}[SIGNAL-VM]${NC} Build VM image not found at $BUILD_VM_IMAGE_PATH"
        echo -e "${YELLOW}[SIGNAL-VM]${NC} Building it now..."
        mkdir -p "$BUILD_VM_IMAGE_PATH"
        nix-build "$REPO_ROOT/nix/vm-signal-server.nix" -A buildVm \
            -o "$BUILD_VM_IMAGE_PATH" 2>&1
    fi

    local disk_img
    disk_img="$(readlink -f "$BUILD_VM_IMAGE_PATH/nixos.img")"
    local overlay
    overlay="$(mktemp /tmp/umbravox-signal-build-overlay.XXXXXX.qcow2)"

    echo -e "${BLUE}[SIGNAL-VM]${NC} Creating COW overlay for build VM..." >&2
    qemu-img create -f qcow2 -b "$disk_img" -F raw "$overlay" >/dev/null 2>&1

    # Output directory: host ↔ guest via 9p virtfs
    mkdir -p "$JAR_OUTPUT_DIR"

    # Build VM gets more resources (Maven is hungry) — 50% of host
    scale_resources 2

    echo -e "${BLUE}[SIGNAL-VM]${NC} Stage 1: Building Signal-Server JAR in VM..."
    echo -e "${BLUE}[SIGNAL-VM]${NC} VM resources: ${VM_CORES} cores, ${VM_MEM_MB}MB RAM"
    echo -e "${BLUE}[SIGNAL-VM]${NC} Network: enabled (Maven needs to fetch deps)"
    echo -e "${BLUE}[SIGNAL-VM]${NC} Output: $JAR_OUTPUT_DIR/signal-server.jar"
    echo ""

    qemu-system-x86_64 \
        -machine "q35,accel=kvm" \
        -cpu max \
        -m "$VM_MEM_MB" \
        -smp "$VM_CORES" \
        -nographic \
        -serial mon:stdio \
        -drive "if=virtio,format=qcow2,file=$overlay" \
        -nic user,model=virtio \
        -virtfs "local,path=$JAR_OUTPUT_DIR,mount_tag=output,security_model=mapped-xattr,id=output" \
        -no-reboot

    rm -f "$overlay" 2>/dev/null || true

    if [ -f "$JAR_OUTPUT_DIR/signal-server.jar" ]; then
        echo ""
        echo -e "${GREEN}[SIGNAL-VM]${NC} Signal-Server JAR built successfully!"
        echo -e "${GREEN}[SIGNAL-VM]${NC} Location: $JAR_OUTPUT_DIR/signal-server.jar"
        ls -lh "$JAR_OUTPUT_DIR/signal-server.jar"
        echo ""
        echo -e "${BLUE}[SIGNAL-VM]${NC} Now rebuild the runtime VM image:"
        echo -e "${BLUE}[SIGNAL-VM]${NC}   make vm-signal-server-build"
    else
        echo ""
        echo -e "${RED}[SIGNAL-VM]${NC} Build failed — no JAR produced."
        echo -e "${RED}[SIGNAL-VM]${NC} Check the VM console output above for errors."
        exit 1
    fi
}

# ── Stage 2: Runtime VM ──────────────────────────────────────────────

run_runtime() {
    local mode="$1"

    if [ ! -d "$VM_IMAGE_PATH" ]; then
        echo -e "${RED}[SIGNAL-VM]${NC} Runtime VM image not found at $VM_IMAGE_PATH"
        echo -e "${YELLOW}[SIGNAL-VM]${NC} Run 'make vm-signal-server-build' first."
        exit 1
    fi

    local disk_img
    disk_img="$(readlink -f "$VM_IMAGE_PATH/nixos.img")"
    local overlay
    overlay="$(mktemp /tmp/umbravox-signal-vm-overlay.XXXXXX.qcow2)"

    echo -e "${BLUE}[SIGNAL-VM]${NC} Creating COW overlay..." >&2
    qemu-img create -f qcow2 -b "$disk_img" -F raw "$overlay" >/dev/null 2>&1

    # Runtime VM: 25% of host
    scale_resources 4

    echo -e "${BLUE}[SIGNAL-VM]${NC} Stage 2: Booting Signal-Server runtime VM..."
    echo -e "${BLUE}[SIGNAL-VM]${NC} VM resources: ${VM_CORES} cores, ${VM_MEM_MB}MB RAM"
    echo -e "${BLUE}[SIGNAL-VM]${NC} Network: deny-all (runtime isolation)"
    if [ -f "$JAR_OUTPUT_DIR/signal-server.jar" ]; then
        echo -e "${GREEN}[SIGNAL-VM]${NC} Signal-Server JAR: present"
    else
        echo -e "${YELLOW}[SIGNAL-VM]${NC} Signal-Server JAR: not built (backing services only)"
    fi
    echo -e "${BLUE}[SIGNAL-VM]${NC} Mode: $mode"
    echo ""

    local qemu_args=(
        -machine "q35,accel=kvm"
        -cpu max
        -m "$VM_MEM_MB"
        -smp "$VM_CORES"
        -nographic
        -nodefaults
        -serial stdio
        -drive "if=virtio,format=qcow2,file=$overlay"
        -nic none
    )

    if [ "$mode" != "interactive" ]; then
        qemu_args+=(-no-reboot)
    fi

    qemu-system-x86_64 "${qemu_args[@]}"
    local exit_code=$?

    rm -f "$overlay" 2>/dev/null || true
    exit $exit_code
}

# ── Main ──────────────────────────────────────────────────────────────

preflight_check

case "$MODE" in
    build-jar)
        build_jar
        ;;
    interactive|check)
        run_runtime "$MODE"
        ;;
    *)
        echo "Usage: $0 {build-jar|interactive|check}"
        exit 1
        ;;
esac

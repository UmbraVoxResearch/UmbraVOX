#!/usr/bin/env bash
# ── UmbraVOX Signal-Server VM Runner ────────────────────────────────
# Boots the NixOS Signal-Server VM for wire-compatibility testing.
# Services: PostgreSQL, Redis, ZooKeeper, Java 21.
#
# Usage (called by Makefile, not directly by users):
#   vm-signal-server-run.sh              # boot and check services
#   vm-signal-server-run.sh interactive  # boot with interactive shell
#
# Requires: qemu-system-x86_64, /dev/kvm
# The VM image must be pre-built via `make vm-signal-server-build`.
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
    if [ ! -d "$VM_IMAGE_PATH" ]; then
        echo -e "${RED}[SIGNAL-VM]${NC} VM image not found at $VM_IMAGE_PATH"
        echo -e "${YELLOW}[SIGNAL-VM]${NC} Run 'make vm-signal-server-build' first."
        ok=0
    fi
    if [ "$ok" -eq 0 ]; then
        exit 1
    fi
}

# ── Main ───────────────────────────────────────────────────────────────

preflight_check

DISK_IMG="$(readlink -f "$VM_IMAGE_PATH/nixos.img")"
OVERLAY="$(mktemp /tmp/umbravox-signal-vm-overlay.XXXXXX.qcow2)"

echo -e "${BLUE}[SIGNAL-VM]${NC} Creating COW overlay..." >&2
qemu-img create -f qcow2 -b "$DISK_IMG" -F raw "$OVERLAY" >/dev/null 2>&1

# Auto-scale VM resources: lighter than dev VM
HOST_CORES=$(nproc 2>/dev/null || echo 4)
HOST_MEM_MB=$(awk '/MemTotal/{printf "%d", $2/1024}' /proc/meminfo 2>/dev/null || echo 8192)
VM_CORES=$(( HOST_CORES / 4 ))
VM_MEM_MB=$(( HOST_MEM_MB / 4 ))
# Enforce minimums
[ "$VM_CORES" -lt 2 ] && VM_CORES=2
[ "$VM_MEM_MB" -lt 2048 ] && VM_MEM_MB=2048

echo -e "${BLUE}[SIGNAL-VM]${NC} VM resources: ${VM_CORES} cores, ${VM_MEM_MB}MB RAM" >&2

QEMU_ARGS=(
    -machine "q35,accel=kvm"
    -cpu max
    -m "$VM_MEM_MB"
    -smp "$VM_CORES"
    -nographic
    -nodefaults
    -serial stdio
    -drive "if=virtio,format=qcow2,file=$OVERLAY"
    -nic none
)

# For non-interactive mode, add -no-reboot so VM exits after poweroff
if [ "$MODE" != "interactive" ]; then
    QEMU_ARGS+=(-no-reboot)
fi

echo -e "${BLUE}[SIGNAL-VM]${NC} Booting Signal-Server VM..."
echo -e "${BLUE}[SIGNAL-VM]${NC} Mode: $MODE"
echo ""

qemu-system-x86_64 "${QEMU_ARGS[@]}"
QEMU_EXIT=$?

# Cleanup
rm -f "$OVERLAY" 2>/dev/null || true

exit $QEMU_EXIT

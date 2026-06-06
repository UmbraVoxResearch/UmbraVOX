#!/usr/bin/env bash
# scripts/lib-vm.sh — Shared VM/QEMU/SSH helper functions (M20.7.4).
#
# Sourced by platform-setup scripts (vm-freebsd-setup.sh, vm-openbsd-setup.sh,
# vm-netbsd-setup.sh, vm-dragonfly-setup.sh, vm-illumos-setup.sh) which are
# intentionally kept as shell per project policy (see doc/TODO.txt).
#
# Shell-to-Go migration status (as of v0.6.3):
#   MIGRATED (removed from this file, Go implementation is authoritative):
#     vm_download_and_verify  → tools/pkg/download.FetchFile
#     vm_build_seed_disk      → tools/pkg/disk.CreateCloudInitSeedDisk
#     vm_copy_source_ssh      → removed (no callers; Go uses 9p shares instead)
#
#   KEPT (platform scripts still source these; platform scripts intentionally
#         stay as shell for portability to FreeBSD/OpenBSD/NetBSD/DragonFlyBSD/illumos):
#     vm_log, vm_ok, vm_fail, vm_die  — logging
#     vm_require_cmd                  — prerequisite checks
#     vm_detect_accel                 — KVM detection
#     vm_ensure_ssh_key               — ephemeral SSH key generation
#     vm_wait_for_ssh                 — SSH readiness polling
#     vm_guest_cmd                    — SSH command execution
#     vm_create_overlay               — qcow2 COW overlay (freebsd, illumos)
#     vm_build_source_fat             — FAT32 source disk (freebsd, illumos)
#     vm_build_init_iso               — ISO builder (freebsd, illumos)
#     vm_check_smoke_result           — smoke test result parsing
#
# Usage (from a VM setup script):
#   SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
#   source "${SCRIPT_DIR}/lib-vm.sh"

# --------------------------------------------------------------------------
# Terminal colors
# --------------------------------------------------------------------------

_VM_RED='\033[0;31m'
_VM_GREEN='\033[0;32m'
_VM_BLUE='\033[0;34m'
_VM_NC='\033[0m'

# --------------------------------------------------------------------------
# Logging — callers set VM_LOG_PREFIX before sourcing or calling.
# --------------------------------------------------------------------------

VM_LOG_PREFIX="${VM_LOG_PREFIX:-VM}"

vm_log()  { echo -e "${_VM_BLUE}[${VM_LOG_PREFIX}]${_VM_NC} $*"; }
vm_ok()   { echo -e "${_VM_GREEN}[${VM_LOG_PREFIX}]${_VM_NC} $*"; }
vm_fail() { echo -e "${_VM_RED}[${VM_LOG_PREFIX}]${_VM_NC} $*"; }
vm_die()  { echo "error: $*" >&2; exit 1; }

# --------------------------------------------------------------------------
# Prerequisite checks
# --------------------------------------------------------------------------

# Require a command or exit with a helpful message.
vm_require_cmd() {
    local cmd="$1"
    local hint="${2:-run inside the appropriate nix-shell}"
    command -v "$cmd" >/dev/null 2>&1 || vm_die "$cmd not found — $hint"
}

# Detect KVM availability; sets VM_ACCEL to "kvm" or "tcg".
vm_detect_accel() {
    if [[ -e /dev/kvm ]]; then
        export VM_ACCEL="kvm"
    else
        vm_log "WARNING: /dev/kvm not present — falling back to TCG (slow)"
        export VM_ACCEL="tcg"
    fi
}

# --------------------------------------------------------------------------
# Overlay image creation
# --------------------------------------------------------------------------

# Create a disposable qcow2 overlay backed by a base image.
# Arguments: base_image overlay_path
vm_create_overlay() {
    local base="$1"
    local overlay="$2"
    vm_log "Creating disposable overlay..."
    rm -f "$overlay"
    qemu-img create -f qcow2 -b "$base" -F qcow2 "$overlay"
}

# --------------------------------------------------------------------------
# SSH key and helpers
# --------------------------------------------------------------------------

# Generate an ephemeral ed25519 SSH key if it does not exist.
# Arguments: key_path [comment]
vm_ensure_ssh_key() {
    local key_path="$1"
    local comment="${2:-umbravox-vm}"
    if [[ ! -f "$key_path" ]]; then
        vm_log "Generating ephemeral SSH key pair..."
        ssh-keygen -t ed25519 -N "" -f "$key_path" -C "$comment"
    fi
}

# Wait for SSH to become reachable on a given port.
# Arguments: port timeout_seconds ssh_key_path
vm_wait_for_ssh() {
    local port="$1"
    local timeout="$2"
    local key="$3"
    local elapsed=0

    vm_log "Waiting for SSH on localhost:$port (timeout ${timeout}s)..."
    while ! ssh -o StrictHostKeyChecking=no \
                -o ConnectTimeout=3 \
                -o BatchMode=yes \
                -i "$key" \
                -p "$port" root@127.0.0.1 true 2>/dev/null; do
        sleep 5
        elapsed=$((elapsed + 5))
        if ((elapsed >= timeout)); then
            vm_die "timed out waiting for guest SSH after ${timeout}s"
        fi
        vm_log "  still waiting... (${elapsed}s)"
    done
    vm_log "SSH ready after ${elapsed}s"
}

# Run a command in the guest over SSH.
# Arguments: ssh_key_path ssh_port command...
vm_guest_cmd() {
    local key="$1"; shift
    local port="$1"; shift
    ssh -o StrictHostKeyChecking=no \
        -o BatchMode=yes \
        -i "$key" \
        -p "$port" \
        root@127.0.0.1 "$@"
}

# --------------------------------------------------------------------------
# FAT source disk (for non-SSH VM setups)
# --------------------------------------------------------------------------

# Package the git-tracked source tree into a FAT32 disk image.
# Arguments: src_dir dest_img [size_mb]
vm_build_source_fat() {
    local src_dir="$1"
    local dest_img="$2"
    local size_mb="${3:-256}"

    vm_log "Packaging source tree into FAT disk image..."
    rm -f "$dest_img"
    dd if=/dev/zero of="$dest_img" bs=1M count="$size_mb" 2>/dev/null
    mkfs.fat -F 32 "$dest_img"

    if command -v mcopy >/dev/null 2>&1; then
        (
            cd "$src_dir"
            git ls-files | while IFS= read -r f; do
                mmd -i "$dest_img" -D s "::$(dirname "$f")" 2>/dev/null || true
                mcopy -i "$dest_img" -D o "$f" "::$f"
            done
        )
    else
        vm_fail "mtools (mcopy) not found. Install mtools in nix-shell for FAT image creation."
        return 1
    fi
    vm_ok "Source disk image: $dest_img"
}

# --------------------------------------------------------------------------
# Init ISO builder
# --------------------------------------------------------------------------

# Build an ISO from a directory using whichever tool is available.
# Arguments: source_dir output_iso
vm_build_init_iso() {
    local source_dir="$1"
    local output_iso="$2"

    if command -v genisoimage >/dev/null 2>&1; then
        genisoimage -o "$output_iso" -R -J "$source_dir"
    elif command -v mkisofs >/dev/null 2>&1; then
        mkisofs -o "$output_iso" -R -J "$source_dir"
    elif command -v xorriso >/dev/null 2>&1; then
        xorriso -as mkisofs -o "$output_iso" -R -J "$source_dir"
    else
        vm_fail "No ISO builder found (genisoimage / mkisofs / xorriso)."
        return 1
    fi
    vm_ok "Init ISO built: $output_iso"
}

# --------------------------------------------------------------------------
# QEMU smoke result checking
# --------------------------------------------------------------------------

# Check a smoke log for PASS/FAIL and report.
# Arguments: log_path qemu_exit_code timeout_seconds platform_name
vm_check_smoke_result() {
    local log="$1"
    local exit_code="$2"
    local timeout="$3"
    local platform="$4"

    if [[ "$exit_code" -eq 124 ]]; then
        vm_fail "VM timed out after ${timeout}s."
        vm_fail "Log: $log"
        return 1
    fi

    if grep -q "SMOKE_RESULT=PASS" "$log" 2>/dev/null; then
        vm_ok "${platform} smoke: PASS"
        vm_ok "Log: $log"
        return 0
    else
        vm_fail "${platform} smoke: FAIL"
        vm_fail "Log: $log"
        return 1
    fi
}

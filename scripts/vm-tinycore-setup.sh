#!/usr/bin/env bash
# vm-tinycore-setup.sh — Boot a tiny-core-linux (TCL) VM and run UmbraVOX
# runtime integration/e2e smoke tests against the pre-built umbravox binary.
#
# Runs under nix-shell (qemu_kvm available in the dev environment).
#
# This is the runtime-only integration-test target ("option A"): a ~5s-boot,
# tiny-footprint VM that contains NO GHC/F*/Coq toolchain.  It boots the
# SHA256-pinned CorePure64 ISO (nix/vm-tinycore.nix), drops in the
# statically-linked binary produced at build/runtime/bin/umbravox via a virtio
# app disk, and exercises it.  Distinct from the heavyweight NixOS dev/build VM.
#
# Steps:
#   1. Resolve the CorePure64 ISO from nix/vm-tinycore.nix (pinned by SHA256).
#   2. Build an ext2 "app disk" from build/runtime/ (umbravox + libs).
#   3. Boot TCL with serial console, the app disk on virtio, and an output 9p
#      share (guest /output -> host build/vm-output) for results.
#   4. In-guest: mount the app disk, run umbravox smoke, write status to /output.
#   5. Report SMOKE_RESULT=PASS or SMOKE_RESULT=FAIL.
#
# Reproducibility / offline: the ISO is pinned by SHA256 in nix/vm-tinycore.nix;
# the VM has NO network (the runtime binary is self-contained).
#
# Usage:
#   nix-shell --run 'bash scripts/vm-tinycore-setup.sh'
#   ./uv vm smoke tinycore
#
# NOTE: The exact in-guest boot incantation for auto-running a script on TCL
# (bootcode / bootlocal.sh / init override) cannot be validated on the host
# (booting VMs is out of scope for the authoring agent).  See doc/TODO.txt
# M41 for the boot-dependent items that must be finished by an agent that can
# boot the VM.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export VM_LOG_PREFIX="VM-TINYCORE"
source "${SCRIPT_DIR}/lib-vm.sh"

# Aliases for backward compatibility within this script
log()  { vm_log "$@"; }
ok()   { vm_ok "$@"; }
fail() { vm_fail "$@"; }

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

REPO_ROOT="${UMBRAVOX_ROOT:-$(cd "${SCRIPT_DIR}/.." && pwd)}"
CACHE_DIR="${REPO_ROOT}/build/vm-cache/tinycore"
OUTPUT_DIR="${REPO_ROOT}/build/vm-output"
RUNTIME_DIR="${REPO_ROOT}/build/runtime"
RUNTIME_BIN="${RUNTIME_DIR}/bin/umbravox"

OVERLAY="${CACHE_DIR}/tinycore-smoke-overlay.qcow2"
APP_IMG="${CACHE_DIR}/tinycore-app.ext2"
SMOKE_LOG="${CACHE_DIR}/tinycore-smoke.log"

QEMU_MEM="512"
QEMU_CPUS="1"
QEMU_TIMEOUT=300   # 5 minutes (matches vm-defs/tinycore.yaml boot.timeout)

mkdir -p "${CACHE_DIR}" "${OUTPUT_DIR}"

# --------------------------------------------------------------------------
# Step 0: Preconditions — the runtime bundle must exist
# --------------------------------------------------------------------------

if [ ! -x "${RUNTIME_BIN}" ]; then
    fail "Runtime bundle missing: ${RUNTIME_BIN}"
    fail "Build it first with: ./uv build"
    exit 1
fi
ok "Runtime bundle present: ${RUNTIME_BIN}"

# --------------------------------------------------------------------------
# Step 1: Resolve the CorePure64 ISO (pinned by SHA256 in nix/vm-tinycore.nix)
# --------------------------------------------------------------------------
#
# The ISO is fetched into the Nix store and exposed as the `iso` attribute.
# This MUST be built inside the builder VM, never on the host (project policy).

ISO_LINK="${CACHE_DIR}/corepure64.iso"
if [ ! -e "${ISO_LINK}" ]; then
    log "Building/resolving CorePure64 ISO from nix/vm-tinycore.nix..."
    # TODO(M41.1): nix-build must run inside the builder VM, and the SHA256 in
    # nix/vm-tinycore.nix must be resolved first (currently lib.fakeSha256).
    nix-build "${REPO_ROOT}/nix/vm-tinycore.nix" -A iso -o "${ISO_LINK}"
fi
BASE_ISO="$(readlink -f "${ISO_LINK}")"
ok "ISO ready: ${BASE_ISO}"

# --------------------------------------------------------------------------
# Step 2: Build the ext2 app disk from build/runtime/
# --------------------------------------------------------------------------
#
# Mirror ./uv run: genext2fs packs build/runtime/ (bin/umbravox + lib/) into a
# read-only ext2 image presented to the guest as a virtio-blk device.

log "Building app disk from ${RUNTIME_DIR}..."
APP_SIZE_KB=$(( $(du -sk "${RUNTIME_DIR}" | cut -f1) + 10240 ))
genext2fs -b "${APP_SIZE_KB}" -d "${RUNTIME_DIR}" "${APP_IMG}"
ok "App disk ready: ${APP_IMG}"

# --------------------------------------------------------------------------
# Step 3: In-guest smoke script
# --------------------------------------------------------------------------
#
# TODO(M41.2): TCL has no persistent rc.local by default.  The mechanism to
# auto-run this on boot (bootcode in isolinux.cfg, a remastered core.gz with
# /opt/bootlocal.sh, or an `init=` override) must be wired and validated by an
# agent that can boot the VM.  The script body below is the intended payload.

GUEST_SMOKE="${CACHE_DIR}/umbravox-tinycore-smoke.sh"
cat > "${GUEST_SMOKE}" << 'GUEST_SCRIPT'
#!/bin/sh
set -e

echo "========================================"
echo "  UmbraVOX tiny-core-linux Smoke Pipeline"
echo "========================================"
echo "kernel:   $(uname -r)"
echo "date:     $(date -u '+%Y-%m-%dT%H:%M:%SZ')"
echo ""

PASS=0
FAIL=0
step() {
    label="$1"; shift
    echo ""
    echo "-- ${label} --"
    if "$@" 2>&1; then
        echo "  STEP PASS: ${label}"
        PASS=$((PASS + 1))
    else
        echo "  STEP FAIL: ${label}"
        FAIL=$((FAIL + 1))
    fi
}

# Mount the read-only app disk (first virtio-blk after the boot media).
mkdir -p /app
step "mount-app"  mount -o ro /dev/vda /app || mount -o ro /dev/sda /app

# Mount the 9p output share (guest /output -> host build/vm-output).
mkdir -p /output
mount -t 9p -o trans=virtio,version=9p2000.L output /output 2>/dev/null || true

export LD_LIBRARY_PATH=/app/lib
chmod +x /app/bin/umbravox 2>/dev/null || true

# Runtime smoke: the binary must at least report its version cleanly.
# TODO(M41.3): extend with real integration/e2e assertions once the boot path
# is validated (e.g. a non-interactive self-test subcommand).
step "umbravox-version"  /app/bin/umbravox --version

echo ""
echo "========================================"
echo "  SMOKE SUMMARY: ${PASS} passed, ${FAIL} failed"
echo "========================================"

if [ "${FAIL}" -eq 0 ]; then
    echo "SMOKE_RESULT=PASS"
    [ -d /output ] && printf "0\n" > /output/vm-exec-status 2>/dev/null || true
else
    echo "SMOKE_RESULT=FAIL"
    [ -d /output ] && printf "1\n" > /output/vm-exec-status 2>/dev/null || true
fi
sync 2>/dev/null || true
poweroff 2>/dev/null || true
GUEST_SCRIPT
chmod +x "${GUEST_SMOKE}"

# --------------------------------------------------------------------------
# Step 4: Boot and run
# --------------------------------------------------------------------------

log "Booting tiny-core-linux VM..."
log "Timeout: ${QEMU_TIMEOUT}s | Memory: ${QEMU_MEM}MB | CPUs: ${QEMU_CPUS}"
echo ""

if [ -e /dev/kvm ]; then
    ACCEL="kvm"
else
    log "WARNING: /dev/kvm not present — falling back to TCG (slow)"
    ACCEL="tcg"
fi

# TODO(M41.2): the boot here drops to the TCL prompt; auto-run of the smoke
# script is not yet wired (see note above).  This invocation boots the ISO with
# the app disk and the 9p output share attached so that, once the auto-run
# mechanism is in place, no QEMU-arg changes are needed.
set +e
timeout "${QEMU_TIMEOUT}" qemu-system-x86_64 \
    -machine "q35,accel=${ACCEL}" \
    -m "${QEMU_MEM}" \
    -smp "${QEMU_CPUS}" \
    -cdrom "${BASE_ISO}" \
    -boot d \
    -drive file="${APP_IMG}",if=virtio,format=raw,readonly=on \
    -virtfs "local,path=${OUTPUT_DIR},mount_tag=output,security_model=mapped-xattr,id=output" \
    -nic none \
    -nographic \
    -serial mon:stdio \
    2>&1 | tee "${SMOKE_LOG}"
QEMU_EXIT=$?
set -e

echo ""

# --------------------------------------------------------------------------
# Step 5: Report
# --------------------------------------------------------------------------

vm_check_smoke_result "${SMOKE_LOG}" "${QEMU_EXIT}" "${QEMU_TIMEOUT}" "tiny-core-linux"

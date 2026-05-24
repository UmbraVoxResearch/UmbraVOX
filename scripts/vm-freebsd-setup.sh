#!/usr/bin/env bash
# vm-freebsd-setup.sh — Download FreeBSD cloud image and run UmbraVOX smoke
# inside a QEMU VM.
#
# Runs under nix-shell (qemu_kvm available in the dev environment).
#
# Steps:
#   1. Download FreeBSD 14.2-RELEASE amd64 VM qcow2 (cached in build/vm-cache/).
#   2. Make a disposable overlay so the base image stays pristine.
#   3. Boot VM with serial console.
#   4. In-guest: pkg install ghc cabal-install gmake git, copy source, build, test.
#   5. Report SMOKE_RESULT=PASS or SMOKE_RESULT=FAIL.
#
# The in-guest commands are injected via a cloud-init NoCloud ISO or, for
# simplicity here, via a second virtio disk containing a shell script that
# FreeBSD's rc.local equivalent runs on first boot.
#
# Usage:
#   nix-shell --run 'bash scripts/vm-freebsd-setup.sh'
#   ./uv vm smoke freebsd

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export VM_LOG_PREFIX="VM-FREEBSD"
source "${SCRIPT_DIR}/lib-vm.sh"

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

FREEBSD_VERSION="14.2-RELEASE"
FREEBSD_ARCH="amd64"
IMAGE_FILE="FreeBSD-${FREEBSD_VERSION}-${FREEBSD_ARCH}.qcow2.xz"
IMAGE_URL="https://download.freebsd.org/releases/VM-IMAGES/${FREEBSD_VERSION}/${FREEBSD_ARCH}/Latest/${IMAGE_FILE}"

CACHE_DIR="${UMBRAVOX_ROOT:-$(pwd)}/build/vm-cache/freebsd"
BASE_IMAGE="${CACHE_DIR}/freebsd-${FREEBSD_VERSION}.qcow2"
OVERLAY="${CACHE_DIR}/freebsd-smoke-overlay.qcow2"
INIT_ISO="${CACHE_DIR}/freebsd-init.iso"
SRC_DIR="$(pwd)"

QEMU_MEM="2048"
QEMU_CPUS="2"
QEMU_TIMEOUT=1800   # 30 minutes

# Aliases for backward compatibility within this script
log()  { vm_log "$@"; }
ok()   { vm_ok "$@"; }
fail() { vm_fail "$@"; }

# --------------------------------------------------------------------------
# Step 1: Ensure base image is cached
# --------------------------------------------------------------------------

mkdir -p "${CACHE_DIR}"

if [ ! -f "${BASE_IMAGE}" ]; then
    log "Downloading FreeBSD ${FREEBSD_VERSION} VM image..."
    log "URL: ${IMAGE_URL}"
    curl -L --progress-bar -o "${CACHE_DIR}/${IMAGE_FILE}" "${IMAGE_URL}"

    # M27.5.5: Verify downloaded image integrity before use.
    # TODO: Replace placeholder hash with actual SHA-256 of the release image.
    FREEBSD_IMAGE_SHA256="TODO_INSERT_ACTUAL_SHA256_FOR_FreeBSD-${FREEBSD_VERSION}-${FREEBSD_ARCH}.qcow2.xz"
    echo "${FREEBSD_IMAGE_SHA256}  ${CACHE_DIR}/${IMAGE_FILE}" | sha256sum -c - || {
        fail "SHA-256 verification failed for ${IMAGE_FILE} — aborting."
        rm -f "${CACHE_DIR}/${IMAGE_FILE}"
        exit 1
    }

    log "Decompressing..."
    xz --decompress --keep --stdout "${CACHE_DIR}/${IMAGE_FILE}" > "${BASE_IMAGE}"
    rm -f "${CACHE_DIR}/${IMAGE_FILE}"
    ok "Base image ready: ${BASE_IMAGE}"
else
    ok "Base image already cached: ${BASE_IMAGE}"
fi

# --------------------------------------------------------------------------
# Step 2: Create disposable overlay
# --------------------------------------------------------------------------

vm_create_overlay "${BASE_IMAGE}" "${OVERLAY}"

# --------------------------------------------------------------------------
# Step 3: Build an init ISO with the in-guest smoke script
# --------------------------------------------------------------------------

log "Building init ISO with smoke script..."

TMPDIR_INIT=$(mktemp -d)
trap 'rm -rf "${TMPDIR_INIT}"' EXIT

# Write the in-guest smoke script
cat > "${TMPDIR_INIT}/umbravox-smoke.sh" << 'GUEST_SCRIPT'
#!/bin/sh
set -e

echo "========================================"
echo "  UmbraVOX FreeBSD Smoke Pipeline"
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

# Install build dependencies via pkg
step "pkg-update"    pkg update -f
step "pkg-install"   pkg install -y ghc cabal-hs gmake git curl ca_root_nss

# Mount source disk (/dev/vtbd1 is the second virtio-blk device on FreeBSD)
mkdir -p /mnt/src
step "mount-src"     mount -t msdosfs /dev/vtbd1 /mnt/src

# Copy source to writable workspace
mkdir -p /work/umbravox
cp -R /mnt/src/. /work/umbravox/
cd /work/umbravox

export HOME=/root
export CABAL_DIR=/root/.cabal
mkdir -p "${CABAL_DIR}"

# Update cabal index (network is available during VM boot)
step "cabal-update"  cabal update

# Build
step "cabal-build"   cabal build all --enable-tests

# Test
step "cabal-test"    cabal test umbravox-test --test-options="required"

echo ""
echo "========================================"
echo "  SMOKE SUMMARY: ${PASS} passed, ${FAIL} failed"
echo "========================================"

if [ "${FAIL}" -eq 0 ]; then
    echo "SMOKE_RESULT=PASS"
    poweroff
else
    echo "SMOKE_RESULT=FAIL"
    poweroff
fi
GUEST_SCRIPT

chmod +x "${TMPDIR_INIT}/umbravox-smoke.sh"

# Write rc.local equivalent for FreeBSD — /etc/rc.local runs as root on boot
cat > "${TMPDIR_INIT}/rc.local" << 'RC_LOCAL'
#!/bin/sh
# Run UmbraVOX smoke on first boot
if [ -f /mnt/init/umbravox-smoke.sh ]; then
    mkdir -p /mnt/init
    mount -t cd9660 /dev/cd1 /mnt/init 2>/dev/null || true
    sh /mnt/init/umbravox-smoke.sh 2>&1 | tee /root/umbravox-smoke.log
fi
RC_LOCAL

# Build the ISO using shared helper
vm_build_init_iso "${TMPDIR_INIT}" "${INIT_ISO}"

# --------------------------------------------------------------------------
# Step 4: Build a source disk image from the current working tree
# --------------------------------------------------------------------------

SRC_IMG="${CACHE_DIR}/freebsd-src.img"
vm_build_source_fat "${SRC_DIR}" "${SRC_IMG}" 256

# --------------------------------------------------------------------------
# Step 5: Boot and run
# --------------------------------------------------------------------------

log "Booting FreeBSD VM..."
log "Timeout: ${QEMU_TIMEOUT}s | Memory: ${QEMU_MEM}MB | CPUs: ${QEMU_CPUS}"
log "Press Ctrl-C to abort."
echo ""

# Capture output to a log file and the terminal simultaneously
SMOKE_LOG="${CACHE_DIR}/freebsd-smoke.log"

set +e
timeout "${QEMU_TIMEOUT}" qemu-system-x86_64 \
    -enable-kvm \
    -m "${QEMU_MEM}" \
    -smp "${QEMU_CPUS}" \
    -drive file="${OVERLAY}",if=virtio,format=qcow2 \
    -drive file="${SRC_IMG}",if=virtio,format=raw,readonly=on \
    -drive file="${INIT_ISO}",if=none,id=cd1,format=raw,readonly=on \
    -device ide-cd,drive=cd1,bus=ide.1 \
    -netdev user,id=net0 \
    -device virtio-net-pci,netdev=net0 \
    -nographic \
    -serial mon:stdio \
    2>&1 | tee "${SMOKE_LOG}"
QEMU_EXIT=$?
set -e

echo ""

# --------------------------------------------------------------------------
# Step 6: Report
# --------------------------------------------------------------------------

vm_check_smoke_result "${SMOKE_LOG}" "${QEMU_EXIT}" "${QEMU_TIMEOUT}" "FreeBSD"

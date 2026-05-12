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
#   make vm-smoke-freebsd

set -euo pipefail

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

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

log()  { echo -e "${BLUE}[VM-FREEBSD]${NC} $*"; }
ok()   { echo -e "${GREEN}[VM-FREEBSD]${NC} $*"; }
fail() { echo -e "${RED}[VM-FREEBSD]${NC} $*"; }

# --------------------------------------------------------------------------
# Step 1: Ensure base image is cached
# --------------------------------------------------------------------------

mkdir -p "${CACHE_DIR}"

if [ ! -f "${BASE_IMAGE}" ]; then
    log "Downloading FreeBSD ${FREEBSD_VERSION} VM image..."
    log "URL: ${IMAGE_URL}"
    curl -L --progress-bar -o "${CACHE_DIR}/${IMAGE_FILE}" "${IMAGE_URL}"
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

log "Creating disposable overlay..."
rm -f "${OVERLAY}"
qemu-img create -f qcow2 -b "${BASE_IMAGE}" -F qcow2 "${OVERLAY}"

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

# Build the ISO (requires genisoimage or mkisofs; fall back to xorriso)
if command -v genisoimage >/dev/null 2>&1; then
    genisoimage -o "${INIT_ISO}" -R -J "${TMPDIR_INIT}"
elif command -v mkisofs >/dev/null 2>&1; then
    mkisofs -o "${INIT_ISO}" -R -J "${TMPDIR_INIT}"
elif command -v xorriso >/dev/null 2>&1; then
    xorriso -as mkisofs -o "${INIT_ISO}" -R -J "${TMPDIR_INIT}"
else
    fail "No ISO builder found (genisoimage / mkisofs / xorriso). Install one in nix-shell."
    exit 1
fi

ok "Init ISO built: ${INIT_ISO}"

# --------------------------------------------------------------------------
# Step 4: Build a source disk image from the current working tree
# --------------------------------------------------------------------------

log "Packaging source tree into FAT disk image for in-guest access..."

SRC_IMG="${CACHE_DIR}/freebsd-src.img"
rm -f "${SRC_IMG}"

# Rough size estimate: 256 MB is plenty for the source tree
SRC_SIZE_MB=256
dd if=/dev/zero of="${SRC_IMG}" bs=1M count="${SRC_SIZE_MB}" 2>/dev/null
mkfs.fat -F 32 "${SRC_IMG}"

TMPDIR_MNT=$(mktemp -d)
# Use mcopy (mtools) to avoid needing root for FAT mounts
if command -v mcopy >/dev/null 2>&1; then
    (
        cd "${SRC_DIR}"
        # Copy tracked source files only (skip build artifacts)
        git ls-files | while IFS= read -r f; do
            dir=$(dirname "MTOOLS_SKIP_CHECK=1 ${SRC_IMG}::/${f}")
            mmd -i "${SRC_IMG}" -D s "::$(dirname "${f}")" 2>/dev/null || true
            mcopy -i "${SRC_IMG}" -D o "${f}" "::${f}"
        done
    )
else
    fail "mtools (mcopy) not found. Install mtools in nix-shell for FAT image creation."
    exit 1
fi

ok "Source disk image: ${SRC_IMG}"

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

if [ "${QEMU_EXIT}" -eq 124 ]; then
    fail "VM timed out after ${QEMU_TIMEOUT}s."
    fail "Log: ${SMOKE_LOG}"
    exit 1
fi

if grep -q "SMOKE_RESULT=PASS" "${SMOKE_LOG}" 2>/dev/null; then
    ok "FreeBSD smoke: PASS"
    ok "Log: ${SMOKE_LOG}"
    exit 0
else
    fail "FreeBSD smoke: FAIL"
    fail "Log: ${SMOKE_LOG}"
    exit 1
fi

#!/usr/bin/env bash
# vm-illumos-setup.sh — Download OmniOS cloud image and run UmbraVOX smoke
# inside a QEMU VM.
#
# Runs under nix-shell (qemu_kvm available in the dev environment).
#
# Steps:
#   1. Download OmniOS CE r151052 cloud VMDK (cached in build/vm-cache/).
#   2. Convert VMDK to qcow2; create disposable overlay.
#   3. Boot VM with serial console.
#   4. In-guest: install GHC via OmniOS ooce packages (pkg install ooce/lang/ghc),
#      copy source, cabal build, cabal test.
#   5. Report SMOKE_RESULT=PASS or SMOKE_RESULT=FAIL.
#
# Usage:
#   nix-shell --run 'bash scripts/vm-illumos-setup.sh'
#   make vm-smoke-illumos

set -euo pipefail

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

OMNIOS_RELEASE="r151052"
IMAGE_FILE="omnios-${OMNIOS_RELEASE}.cloud.vmdk.bz2"
IMAGE_URL="https://downloads.omnios.org/media/${OMNIOS_RELEASE}/${IMAGE_FILE}"

CACHE_DIR="${UMBRAVOX_ROOT:-$(pwd)}/build/vm-cache/illumos"
VMDK_FILE="${CACHE_DIR}/omnios-${OMNIOS_RELEASE}.vmdk"
BASE_IMAGE="${CACHE_DIR}/omnios-${OMNIOS_RELEASE}.qcow2"
OVERLAY="${CACHE_DIR}/omnios-smoke-overlay.qcow2"
INIT_ISO="${CACHE_DIR}/omnios-init.iso"
SRC_DIR="$(pwd)"

QEMU_MEM="2048"
QEMU_CPUS="2"
QEMU_TIMEOUT=2400   # 40 minutes (OmniOS boot + pkg install takes longer)

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

log()  { echo -e "${BLUE}[VM-ILLUMOS]${NC} $*"; }
ok()   { echo -e "${GREEN}[VM-ILLUMOS]${NC} $*"; }
fail() { echo -e "${RED}[VM-ILLUMOS]${NC} $*"; }

# --------------------------------------------------------------------------
# Step 1: Ensure base image is cached
# --------------------------------------------------------------------------

mkdir -p "${CACHE_DIR}"

if [ ! -f "${BASE_IMAGE}" ]; then
    if [ ! -f "${VMDK_FILE}" ]; then
        log "Downloading OmniOS CE ${OMNIOS_RELEASE} cloud image..."
        log "URL: ${IMAGE_URL}"
        curl -L --progress-bar -o "${CACHE_DIR}/${IMAGE_FILE}" "${IMAGE_URL}"

        # M27.5.5: Verify downloaded image integrity before use.
        # TODO: Replace placeholder hash with actual SHA-256 of the release image.
        OMNIOS_IMAGE_SHA256="TODO_INSERT_ACTUAL_SHA256_FOR_omnios-${OMNIOS_RELEASE}.cloud.vmdk.bz2"
        echo "${OMNIOS_IMAGE_SHA256}  ${CACHE_DIR}/${IMAGE_FILE}" | sha256sum -c - || {
            fail "SHA-256 verification failed for ${IMAGE_FILE} — aborting."
            rm -f "${CACHE_DIR}/${IMAGE_FILE}"
            exit 1
        }

        log "Decompressing bzip2..."
        bzcat "${CACHE_DIR}/${IMAGE_FILE}" > "${VMDK_FILE}"
        rm -f "${CACHE_DIR}/${IMAGE_FILE}"
        ok "VMDK ready: ${VMDK_FILE}"
    else
        ok "VMDK already cached: ${VMDK_FILE}"
    fi

    log "Converting VMDK to qcow2..."
    qemu-img convert -f vmdk -O qcow2 "${VMDK_FILE}" "${BASE_IMAGE}"
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

cat > "${TMPDIR_INIT}/umbravox-smoke.sh" << 'GUEST_SCRIPT'
#!/bin/bash
set -e

echo "========================================"
echo "  UmbraVOX OmniOS/illumos Smoke Pipeline"
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

# OmniOS uses the IPS package system. The ooce publisher provides GHC.
# Add the ooce publisher if not already present.
step "pkg-publisher-ooce" pkg set-publisher \
    -g https://pkg.ooce.omnios.org/omnios/r151052 ooce 2>/dev/null || \
    pkg set-publisher --no-sticky ooce

# Install dependencies
step "pkg-install" pkg install -q \
    ooce/lang/ghc \
    ooce/developer/cabal \
    developer/gnu-binutils \
    developer/gcc14 \
    system/header \
    developer/build/gnu-make \
    scm/git \
    web/curl

# Mount source disk (/dev/vdb in illumos — second virtio disk)
mkdir -p /mnt/src
RMMNT=""
if [ -b /dev/vdb ]; then
    step "mount-src" mount -F pcfs /dev/vdb /mnt/src && RMMNT="yes"
elif [ -b /dev/dsk/c1d0 ]; then
    step "mount-src" mount -F pcfs /dev/dsk/c1d0 /mnt/src && RMMNT="yes"
fi

# Copy source to writable workspace
mkdir -p /work/umbravox
cp -R /mnt/src/. /work/umbravox/
cd /work/umbravox

export HOME=/root
export CABAL_DIR=/root/.cabal
export PATH="/opt/ooce/bin:/opt/ooce/sbin:${PATH}"
mkdir -p "${CABAL_DIR}"

# Update cabal index
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
    /usr/sbin/shutdown -y -g 0 -i 5
else
    echo "SMOKE_RESULT=FAIL"
    /usr/sbin/shutdown -y -g 0 -i 5
fi
GUEST_SCRIPT

chmod +x "${TMPDIR_INIT}/umbravox-smoke.sh"

# OmniOS uses SMF for service management. We inject a simple SMF manifest so
# the smoke script runs after the system reaches multi-user milestone.
cat > "${TMPDIR_INIT}/umbravox-smoke-smf.xml" << 'SMF_XML'
<?xml version="1.0"?>
<!DOCTYPE service_bundle SYSTEM "/usr/share/lib/xml/dtd/service_bundle.dtd.1">
<service_bundle type="manifest" name="umbravox-smoke">
  <service name="site/umbravox-smoke" type="service" version="1">
    <create_default_instance enabled="true"/>
    <single_instance/>
    <dependency name="multi-user" grouping="require_all" restart_on="none" type="service">
      <service_fmri value="svc:/milestone/multi-user:default"/>
    </dependency>
    <exec_method type="method" name="start"
      exec="/bin/sh -c 'mount -F cd9660 /dev/dsk/c2d0 /mnt/init 2>/dev/null; sh /mnt/init/umbravox-smoke.sh > /root/umbravox-smoke.log 2>&1 &'"
      timeout_seconds="0"/>
    <exec_method type="method" name="stop" exec=":kill" timeout_seconds="60"/>
    <property_group name="startd" type="framework">
      <propval name="duration" type="astring" value="transient"/>
    </property_group>
  </service>
</service_bundle>
SMF_XML

# Build the ISO
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
# Step 4: Build a source FAT disk image
# --------------------------------------------------------------------------

log "Packaging source tree into FAT disk image for in-guest access..."

SRC_IMG="${CACHE_DIR}/illumos-src.img"
rm -f "${SRC_IMG}"

SRC_SIZE_MB=256
dd if=/dev/zero of="${SRC_IMG}" bs=1M count="${SRC_SIZE_MB}" 2>/dev/null
mkfs.fat -F 32 "${SRC_IMG}"

if command -v mcopy >/dev/null 2>&1; then
    (
        cd "${SRC_DIR}"
        git ls-files | while IFS= read -r f; do
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

log "Booting OmniOS VM..."
log "Timeout: ${QEMU_TIMEOUT}s | Memory: ${QEMU_MEM}MB | CPUs: ${QEMU_CPUS}"
log "Press Ctrl-C to abort."
echo ""

SMOKE_LOG="${CACHE_DIR}/illumos-smoke.log"

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
    ok "illumos smoke: PASS"
    ok "Log: ${SMOKE_LOG}"
    exit 0
else
    fail "illumos smoke: FAIL"
    fail "Log: ${SMOKE_LOG}"
    exit 1
fi

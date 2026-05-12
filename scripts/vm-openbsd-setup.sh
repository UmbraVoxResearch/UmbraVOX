#!/usr/bin/env bash
# scripts/vm-openbsd-setup.sh — OpenBSD QEMU VM smoke for UmbraVOX
#
# Boots an OpenBSD 7.x amd64 cloud image under QEMU, installs GHC and
# cabal-install via pkg_add, copies the source tree via SSH/9p, then
# runs `cabal build && cabal test`.
#
# All commands must run inside nix-shell (nix/vm-openbsd.nix provides
# qemu_kvm, wget, openssh, expect, etc.).
#
# Image cache: build/vm/
#   openbsd-amd64.qcow2   — working copy of the cloud image (writable)
#   openbsd-amd64.qcow2.base — original download, kept read-only
#
# Environment overrides:
#   OPENBSD_VERSION   — e.g. "7.5"  (default: 7.5)
#   OPENBSD_MIRROR    — base URL for cloud image download
#   VM_MEM            — QEMU -m value  (default: 2G)
#   VM_SMP            — QEMU -smp value (default: 2)
#   VM_SSH_PORT       — host port forwarded to guest :22 (default: 10222)
#   VM_BOOT_TIMEOUT   — seconds to wait for SSH to become reachable (default: 300)
#   VM_WORK_DIR       — image cache directory (default: build/vm)
set -euo pipefail

# ── Configuration ─────────────────────────────────────────────────────────────

OPENBSD_VERSION="${OPENBSD_VERSION:-7.5}"
# Official OpenBSD cloud images (miniroot) — amd64, gzip-compressed raw disk
OPENBSD_MIRROR="${OPENBSD_MIRROR:-https://cdn.openbsd.org/pub/OpenBSD}"
VM_MEM="${VM_MEM:-2G}"
VM_SMP="${VM_SMP:-2}"
VM_SSH_PORT="${VM_SSH_PORT:-10222}"
VM_BOOT_TIMEOUT="${VM_BOOT_TIMEOUT:-300}"
VM_WORK_DIR="${VM_WORK_DIR:-build/vm}"

# Derive the short version tag used in OpenBSD paths (e.g. "7.5" -> "75")
_ver_tag="$(printf '%s' "$OPENBSD_VERSION" | tr -d '.')"

# Cloud image filename and download URL
# OpenBSD ships miniroot (cloud) images as miniroot<VER>-<REV>.img.gz
# The stable release image follows the naming: miniroot<TAG>.img.gz
IMAGE_BASE="miniroot${_ver_tag}.img"
IMAGE_GZ="${IMAGE_BASE}.gz"
IMAGE_URL="${OPENBSD_MIRROR}/${OPENBSD_VERSION}/amd64/${IMAGE_GZ}"

IMAGE_ORIG="${VM_WORK_DIR}/openbsd-amd64.qcow2.base"
IMAGE_WORK="${VM_WORK_DIR}/openbsd-amd64.qcow2"
SSH_KEY="${VM_WORK_DIR}/openbsd-vm-key"

UMBRAVOX_ROOT="${UMBRAVOX_ROOT:-$(pwd)}"

# ── Helpers ───────────────────────────────────────────────────────────────────

die() { echo "error: $*" >&2; exit 1; }

log() { echo "[vm-openbsd] $*"; }

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "$1 not found — run inside nix-shell nix/vm-openbsd.nix"
}

wait_for_ssh() {
  local port="$1" timeout="$2" elapsed=0
  log "waiting for SSH on localhost:$port (timeout ${timeout}s)..."
  while ! ssh -o StrictHostKeyChecking=no \
              -o ConnectTimeout=3 \
              -o BatchMode=yes \
              -i "$SSH_KEY" \
              -p "$port" root@127.0.0.1 true 2>/dev/null; do
    sleep 5
    elapsed=$((elapsed + 5))
    if ((elapsed >= timeout)); then
      die "timed out waiting for guest SSH after ${timeout}s"
    fi
    log "  still waiting... (${elapsed}s)"
  done
  log "SSH ready after ${elapsed}s"
}

guest_cmd() {
  # Run a command in the guest over SSH.
  ssh -o StrictHostKeyChecking=no \
      -o BatchMode=yes \
      -i "$SSH_KEY" \
      -p "$VM_SSH_PORT" \
      root@127.0.0.1 "$@"
}

# ── Prerequisite checks ───────────────────────────────────────────────────────

require_cmd qemu-system-x86_64
require_cmd qemu-img
require_cmd wget
require_cmd ssh
require_cmd ssh-keygen

# KVM is strongly preferred; warn and continue with TCG if absent.
if [[ ! -e /dev/kvm ]]; then
  log "WARNING: /dev/kvm not present — falling back to TCG (slow)"
  ACCEL="tcg"
else
  ACCEL="kvm"
fi

# ── Image cache ───────────────────────────────────────────────────────────────

mkdir -p "$VM_WORK_DIR"

if [[ ! -f "$IMAGE_ORIG" ]]; then
  log "downloading OpenBSD ${OPENBSD_VERSION} cloud image..."
  log "  URL: ${IMAGE_URL}"
  tmp_gz="${VM_WORK_DIR}/${IMAGE_GZ}"
  wget -q --show-progress -O "$tmp_gz" "$IMAGE_URL" || die "download failed: $IMAGE_URL"

  log "decompressing image..."
  # gunzip the .img.gz to a raw image, then convert to qcow2 for snapshotting
  tmp_raw="${VM_WORK_DIR}/openbsd-amd64.raw"
  gunzip -c "$tmp_gz" > "$tmp_raw"
  rm -f "$tmp_gz"

  log "converting raw image to qcow2..."
  qemu-img convert -f raw -O qcow2 "$tmp_raw" "$IMAGE_ORIG"
  rm -f "$tmp_raw"

  # Grow the image so there is room for GHC + build artifacts (~8 GB headroom)
  qemu-img resize "$IMAGE_ORIG" +8G
  log "base image ready: $IMAGE_ORIG ($(du -sh "$IMAGE_ORIG" | cut -f1))"
else
  log "base image already cached: $IMAGE_ORIG"
fi

# Create a fresh working copy from the base image for this run.
# Using qcow2 backing file keeps the base image clean.
log "creating working image from base..."
rm -f "$IMAGE_WORK"
qemu-img create -f qcow2 -b "$(realpath "$IMAGE_ORIG")" -F qcow2 "$IMAGE_WORK"

# ── SSH key ───────────────────────────────────────────────────────────────────

if [[ ! -f "$SSH_KEY" ]]; then
  log "generating ephemeral SSH key pair..."
  ssh-keygen -t ed25519 -N "" -f "$SSH_KEY" -C "umbravox-openbsd-vm"
fi
PUB_KEY="$(cat "${SSH_KEY}.pub")"

# ── Boot guest ────────────────────────────────────────────────────────────────

log "booting OpenBSD guest (mem=${VM_MEM}, smp=${VM_SMP}, accel=${ACCEL})..."

# OpenBSD cloud images are configured via cloud-init or via a user-data disk.
# The miniroot images accept a simple cloud-init NoCloud seed disk; we embed
# the SSH public key so the setup script can log in without a password.

SEED_DIR="${VM_WORK_DIR}/openbsd-seed"
SEED_IMG="${VM_WORK_DIR}/openbsd-seed.img"
mkdir -p "$SEED_DIR"

# cloud-init meta-data
cat > "${SEED_DIR}/meta-data" <<EOF
instance-id: umbravox-openbsd-1
local-hostname: umbravox-openbsd
EOF

# cloud-init user-data: root SSH key + disable password auth
cat > "${SEED_DIR}/user-data" <<EOF
#cloud-config
users:
  - name: root
    ssh_authorized_keys:
      - ${PUB_KEY}
disable_root: false
ssh_pwauth: false
EOF

# Build a FAT seed disk image (cloud-init NoCloud label)
# Use genext2fs-style approach: create a 1MB FAT image via dd + mkdosfs
SEED_TMP="${VM_WORK_DIR}/openbsd-seed-files"
mkdir -p "$SEED_TMP"
cp "${SEED_DIR}/meta-data"  "$SEED_TMP/"
cp "${SEED_DIR}/user-data"  "$SEED_TMP/"

# Create 1MB raw image, format as FAT, copy seed files
dd if=/dev/zero of="$SEED_IMG" bs=1M count=1 2>/dev/null
if command -v mkdosfs >/dev/null 2>&1; then
  mkdosfs -n cidata "$SEED_IMG"
elif command -v mkfs.vfat >/dev/null 2>&1; then
  mkfs.vfat -n CIDATA "$SEED_IMG"
else
  log "WARNING: neither mkdosfs nor mkfs.vfat available — cloud-init seed disk skipped"
  log "         SSH key injection will not work; manual first-boot setup required"
  SEED_IMG=""
fi

if [[ -n "$SEED_IMG" ]] && [[ -f "$SEED_IMG" ]]; then
  if command -v mcopy >/dev/null 2>&1; then
    mcopy -i "$SEED_IMG" "${SEED_TMP}/meta-data" ::/meta-data
    mcopy -i "$SEED_IMG" "${SEED_TMP}/user-data"  ::/user-data
  else
    log "WARNING: mcopy not available — seed files not injected into FAT image"
    log "         Install mtools if cloud-init key injection is required"
    SEED_IMG=""
  fi
fi

# Build qemu drive arguments
DRIVES=(
  "-drive" "if=virtio,format=qcow2,file=${IMAGE_WORK}"
)
if [[ -n "${SEED_IMG:-}" ]] && [[ -f "$SEED_IMG" ]]; then
  DRIVES+=("-drive" "if=virtio,format=raw,file=${SEED_IMG},readonly=on")
fi

# Start QEMU in the background
qemu-system-x86_64 \
  -machine q35,accel="${ACCEL}" \
  -cpu max \
  -m "${VM_MEM}" \
  -smp "${VM_SMP}" \
  -nographic \
  -nodefaults \
  -serial mon:stdio \
  "${DRIVES[@]}" \
  -netdev "user,id=net0,hostfwd=tcp::${VM_SSH_PORT}-:22" \
  -device virtio-net-pci,netdev=net0 \
  &
QEMU_PID=$!
trap 'log "shutting down guest..."; kill "$QEMU_PID" 2>/dev/null || true; wait "$QEMU_PID" 2>/dev/null || true' EXIT

# ── Wait for SSH ──────────────────────────────────────────────────────────────

wait_for_ssh "$VM_SSH_PORT" "$VM_BOOT_TIMEOUT"

# ── Install GHC + cabal-install ───────────────────────────────────────────────

log "installing GHC and cabal-install via pkg_add..."
# pkg_add -I disables interactive prompts; PKGPATH is not needed for binary packages.
# The package name on OpenBSD 7.x is "ghc" (tracks the GHC release for that OpenBSD version).
guest_cmd "pkg_add -I ghc cabal-install" || die "pkg_add ghc cabal-install failed"

log "checking installed versions..."
guest_cmd "ghc --version && cabal --version"

# ── Copy source tree ──────────────────────────────────────────────────────────

log "copying source tree to guest /work/umbravox ..."
guest_cmd "mkdir -p /work/umbravox"
# rsync is preferred; fall back to scp -r if not available on the host.
if command -v rsync >/dev/null 2>&1; then
  rsync -az --exclude='.git' --exclude='dist-newstyle' --exclude='build' \
    -e "ssh -o StrictHostKeyChecking=no -i '${SSH_KEY}' -p ${VM_SSH_PORT}" \
    "${UMBRAVOX_ROOT}/" \
    "root@127.0.0.1:/work/umbravox/"
else
  # tar | ssh approach avoids rsync dependency
  tar -C "$UMBRAVOX_ROOT" \
    --exclude='.git' --exclude='dist-newstyle' --exclude='build' \
    -czf - . \
  | guest_cmd "tar -C /work/umbravox -xzf -"
fi
log "source tree copied."

# ── Build + test ──────────────────────────────────────────────────────────────

log "running cabal build inside guest..."
guest_cmd bash -lc '
  set -euo pipefail
  cd /work/umbravox
  export HOME=/root
  mkdir -p /root/.cabal
  cabal update
  cabal build all
'

log "running cabal test inside guest..."
guest_cmd bash -lc '
  set -euo pipefail
  cd /work/umbravox
  export HOME=/root
  cabal test umbravox-test --test-options="required"
'

# ── Done ──────────────────────────────────────────────────────────────────────

log "OpenBSD VM smoke: PASS"
log "shutting down guest gracefully..."
guest_cmd "shutdown -h now" 2>/dev/null || true
# Give QEMU a moment to exit cleanly before the EXIT trap fires.
sleep 5

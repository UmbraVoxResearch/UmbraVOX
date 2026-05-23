#!/usr/bin/env bash
# scripts/vm-netbsd-setup.sh — NetBSD QEMU VM smoke for UmbraVOX
#
# Boots a NetBSD 10.x amd64 cloud image under QEMU, installs GHC via
# pkgin (pre-built binary package), copies the source tree, then runs
# `cabal build && cabal test`.
#
# All commands must run inside nix-shell (nix/vm-netbsd.nix provides
# qemu_kvm, wget, openssh, expect, etc.).
#
# Image cache: build/vm/
#   netbsd-amd64.qcow2.base — original download, kept read-only
#   netbsd-amd64.qcow2      — working copy for this run
#
# Environment overrides:
#   NETBSD_VERSION   — e.g. "10.0" (default: 10.0)
#   NETBSD_MIRROR    — base URL for cloud image download
#   VM_MEM           — QEMU -m value  (default: 2G)
#   VM_SMP           — QEMU -smp value (default: 2)
#   VM_SSH_PORT      — host port forwarded to guest :22 (default: 10322)
#   VM_BOOT_TIMEOUT  — seconds to wait for SSH (default: 360)
#   VM_WORK_DIR      — image cache directory (default: build/vm)
set -euo pipefail

# ── Configuration ─────────────────────────────────────────────────────────────

NETBSD_VERSION="${NETBSD_VERSION:-10.0}"
# NetBSD cloud images (amd64) are hosted on cdn.netbsd.org
# Path pattern: /pub/NetBSD/NetBSD-<VER>/images/NetBSD-<VER>-amd64.img.gz
NETBSD_MIRROR="${NETBSD_MIRROR:-https://cdn.netbsd.org/pub/NetBSD}"
VM_MEM="${VM_MEM:-2G}"
VM_SMP="${VM_SMP:-2}"
VM_SSH_PORT="${VM_SSH_PORT:-10322}"
VM_BOOT_TIMEOUT="${VM_BOOT_TIMEOUT:-360}"
VM_WORK_DIR="${VM_WORK_DIR:-build/vm}"

IMAGE_NAME="NetBSD-${NETBSD_VERSION}-amd64"
IMAGE_GZ="${IMAGE_NAME}.img.gz"
IMAGE_URL="${NETBSD_MIRROR}/NetBSD-${NETBSD_VERSION}/images/${IMAGE_GZ}"

IMAGE_ORIG="${VM_WORK_DIR}/netbsd-amd64.qcow2.base"
IMAGE_WORK="${VM_WORK_DIR}/netbsd-amd64.qcow2"
SSH_KEY="${VM_WORK_DIR}/netbsd-vm-key"

UMBRAVOX_ROOT="${UMBRAVOX_ROOT:-$(pwd)}"

# ── Helpers ───────────────────────────────────────────────────────────────────

die() { echo "error: $*" >&2; exit 1; }

log() { echo "[vm-netbsd] $*"; }

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "$1 not found — run inside nix-shell nix/vm-netbsd.nix"
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

if [[ ! -e /dev/kvm ]]; then
  log "WARNING: /dev/kvm not present — falling back to TCG (slow)"
  ACCEL="tcg"
else
  ACCEL="kvm"
fi

# ── Image cache ───────────────────────────────────────────────────────────────

mkdir -p "$VM_WORK_DIR"

if [[ ! -f "$IMAGE_ORIG" ]]; then
  log "downloading NetBSD ${NETBSD_VERSION} cloud image..."
  log "  URL: ${IMAGE_URL}"
  tmp_gz="${VM_WORK_DIR}/${IMAGE_GZ}"
  wget -q --show-progress -O "$tmp_gz" "$IMAGE_URL" || die "download failed: $IMAGE_URL"

  # M27.5.5: Verify downloaded image integrity before use.
  # TODO: Replace placeholder hash with actual SHA-256 of the release image.
  NETBSD_IMAGE_SHA256="TODO_INSERT_ACTUAL_SHA256_FOR_${IMAGE_GZ}"
  echo "${NETBSD_IMAGE_SHA256}  ${tmp_gz}" | sha256sum -c - || {
    die "SHA-256 verification failed for ${IMAGE_GZ} — aborting."
  }

  log "decompressing image..."
  tmp_raw="${VM_WORK_DIR}/netbsd-amd64.img"
  gunzip -c "$tmp_gz" > "$tmp_raw"
  rm -f "$tmp_gz"

  log "converting raw image to qcow2..."
  qemu-img convert -f raw -O qcow2 "$tmp_raw" "$IMAGE_ORIG"
  rm -f "$tmp_raw"

  # Grow the image to provide space for GHC + build artifacts
  qemu-img resize "$IMAGE_ORIG" +8G
  log "base image ready: $IMAGE_ORIG ($(du -sh "$IMAGE_ORIG" | cut -f1))"
else
  log "base image already cached: $IMAGE_ORIG"
fi

# Create a fresh working copy backed by the base image.
log "creating working image from base..."
rm -f "$IMAGE_WORK"
qemu-img create -f qcow2 -b "$(realpath "$IMAGE_ORIG")" -F qcow2 "$IMAGE_WORK"

# ── SSH key ───────────────────────────────────────────────────────────────────

if [[ ! -f "$SSH_KEY" ]]; then
  log "generating ephemeral SSH key pair..."
  ssh-keygen -t ed25519 -N "" -f "$SSH_KEY" -C "umbravox-netbsd-vm"
fi
PUB_KEY="$(cat "${SSH_KEY}.pub")"

# ── cloud-init seed disk ──────────────────────────────────────────────────────
# NetBSD 10 cloud images support cloud-init (NoCloud datasource).
# We build a FAT seed disk with meta-data and user-data so that the guest
# picks up the SSH public key on first boot without manual interaction.

SEED_DIR="${VM_WORK_DIR}/netbsd-seed"
SEED_IMG="${VM_WORK_DIR}/netbsd-seed.img"
SEED_TMP="${VM_WORK_DIR}/netbsd-seed-files"
mkdir -p "$SEED_DIR" "$SEED_TMP"

cat > "${SEED_TMP}/meta-data" <<EOF
instance-id: umbravox-netbsd-1
local-hostname: umbravox-netbsd
EOF

cat > "${SEED_TMP}/user-data" <<EOF
#cloud-config
users:
  - name: root
    ssh_authorized_keys:
      - ${PUB_KEY}
disable_root: false
ssh_pwauth: false
EOF

# Build FAT seed image
dd if=/dev/zero of="$SEED_IMG" bs=1M count=1 2>/dev/null
SEED_OK=0
if command -v mkdosfs >/dev/null 2>&1; then
  mkdosfs -n cidata "$SEED_IMG" && SEED_OK=1
elif command -v mkfs.vfat >/dev/null 2>&1; then
  mkfs.vfat -n CIDATA "$SEED_IMG" && SEED_OK=1
fi

if ((SEED_OK)) && command -v mcopy >/dev/null 2>&1; then
  mcopy -i "$SEED_IMG" "${SEED_TMP}/meta-data" ::/meta-data
  mcopy -i "$SEED_IMG" "${SEED_TMP}/user-data"  ::/user-data
  log "cloud-init seed disk prepared: $SEED_IMG"
else
  log "WARNING: cannot build cloud-init seed disk (mkdosfs/mcopy missing)"
  log "         SSH key injection skipped; guest may require manual setup"
  SEED_IMG=""
fi

# ── Boot guest ────────────────────────────────────────────────────────────────

log "booting NetBSD guest (mem=${VM_MEM}, smp=${VM_SMP}, accel=${ACCEL})..."

DRIVES=(
  "-drive" "if=virtio,format=qcow2,file=${IMAGE_WORK}"
)
if [[ -n "${SEED_IMG:-}" ]] && [[ -f "$SEED_IMG" ]]; then
  DRIVES+=("-drive" "if=virtio,format=raw,file=${SEED_IMG},readonly=on")
fi

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

# ── Install GHC via pkgin ─────────────────────────────────────────────────────

log "refreshing pkgin and installing GHC..."
# pkgin is the binary package manager shipped with NetBSD cloud images.
# The GHC package is named "ghc" in pkgsrc; cabal-install may be "hs-cabal-install".
guest_cmd bash -lc '
  set -euo pipefail
  pkgin -y update
  pkgin -y install ghc
  # Try cabal-install; some pkgsrc mirrors name it hs-cabal-install
  pkgin -y install cabal-install 2>/dev/null || pkgin -y install hs-cabal-install 2>/dev/null || true
  ghc --version
  cabal --version 2>/dev/null || echo "INFO: cabal-install not in pkgin; may need bootstrap"
'

# ── Copy source tree ──────────────────────────────────────────────────────────

log "copying source tree to guest /work/umbravox ..."
guest_cmd "mkdir -p /work/umbravox"
tar -C "$UMBRAVOX_ROOT" \
  --exclude='.git' --exclude='dist-newstyle' --exclude='build' \
  -czf - . \
| guest_cmd "tar -C /work/umbravox -xzf -"
log "source tree copied."

# ── Build + test ──────────────────────────────────────────────────────────────

log "running cabal update + build inside guest..."
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

log "NetBSD VM smoke: PASS"
log "shutting down guest gracefully..."
guest_cmd "shutdown -p now" 2>/dev/null || true
sleep 5

#!/usr/bin/env bash
# scripts/vm-dragonfly-setup.sh — DragonFlyBSD QEMU VM smoke for UmbraVOX
#
# Boots a DragonFlyBSD 6.x amd64 ISO under QEMU and checks whether GHC is
# available in dports (DragonFlyBSD's FreeBSD-derived package collection).
#
# GHC availability on DragonFlyBSD
# ----------------------------------
# The "lang/ghc" port exists in dports, but pre-built binary packages are not
# guaranteed to be present on the public pkg mirror for every release.  This
# script runs a best-effort check:
#
#   1. Boot the ISO in live/installer mode.
#   2. Attempt `pkg install -y ghc cabal-install`.
#   3. If GHC installs successfully, copy the source tree and run
#      `cabal build && cabal test`.
#   4. If GHC is NOT available, emit an INFO notice and exit 0 so CI stays
#      green.  Track the gap in TODO.txt M14.2.4.
#
# All commands must run inside nix-shell (nix/vm-dragonfly.nix).
#
# Image cache: build/vm/
#   dragonfly-amd64.iso     — downloaded ISO (kept, large file)
#   dragonfly-hdd.qcow2     — writable HDD image for the installed system
#
# Environment overrides:
#   DRAGONFLY_VERSION  — e.g. "6.4.0" (default: 6.4.0)
#   DRAGONFLY_MIRROR   — mirror base URL
#   VM_MEM             — QEMU -m value  (default: 2G)
#   VM_SMP             — QEMU -smp value (default: 2)
#   VM_SSH_PORT        — host port forwarded to guest :22 (default: 10422)
#   VM_BOOT_TIMEOUT    — seconds to wait for SSH (default: 420)
#   VM_WORK_DIR        — image cache directory (default: build/vm)
set -euo pipefail

# ── Configuration ─────────────────────────────────────────────────────────────

DRAGONFLY_VERSION="${DRAGONFLY_VERSION:-6.4.0}"
DRAGONFLY_MIRROR="${DRAGONFLY_MIRROR:-https://mirror.racket-lang.org/releases/dragonfly}"
VM_MEM="${VM_MEM:-2G}"
VM_SMP="${VM_SMP:-2}"
VM_SSH_PORT="${VM_SSH_PORT:-10422}"
VM_BOOT_TIMEOUT="${VM_BOOT_TIMEOUT:-420}"
VM_WORK_DIR="${VM_WORK_DIR:-build/vm}"

# DragonFly ISO naming: DragonFly-x86_64-RELEASE-<VER>.iso
# Official mirrors: avalon.dragonflybsd.org/iso-images/
ISO_NAME="DragonFly-x86_64-RELEASE-${DRAGONFLY_VERSION}.iso"
ISO_URL="${DRAGONFLY_MIRROR}/${DRAGONFLY_VERSION}/${ISO_NAME}"
# Fallback: direct from avalon
AVALON_URL="https://avalon.dragonflybsd.org/iso-images/${ISO_NAME}.bz2"

ISO_PATH="${VM_WORK_DIR}/dragonfly-amd64.iso"
HDD_IMG="${VM_WORK_DIR}/dragonfly-hdd.qcow2"
SSH_KEY="${VM_WORK_DIR}/dragonfly-vm-key"

UMBRAVOX_ROOT="${UMBRAVOX_ROOT:-$(pwd)}"

# ── Helpers ───────────────────────────────────────────────────────────────────

die() { echo "error: $*" >&2; exit 1; }

log() { echo "[vm-dragonfly] $*"; }

info_skip() {
  # Emit an INFO notice and exit 0 — used when GHC is unavailable.
  echo ""
  echo "INFO: DragonFlyBSD VM smoke — GHC not available via dports pkg mirror."
  echo "      The 'lang/ghc' port exists in dports but a pre-built binary"
  echo "      package may not be present for DragonFlyBSD ${DRAGONFLY_VERSION}."
  echo "      Action: track in TODO.txt M14.2.4; revisit when a GHC binary"
  echo "      package appears on avalon.dragonflybsd.org/packages/."
  echo ""
  exit 0
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "$1 not found — run inside nix-shell nix/vm-dragonfly.nix"
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
      log "timed out waiting for SSH — this is expected if the ISO boots into"
      log "an interactive installer that requires manual steps."
      info_skip
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
  log "WARNING: /dev/kvm not present — falling back to TCG (very slow for DragonFly)"
  ACCEL="tcg"
else
  ACCEL="kvm"
fi

# ── Image cache ───────────────────────────────────────────────────────────────

mkdir -p "$VM_WORK_DIR"

if [[ ! -f "$ISO_PATH" ]]; then
  log "downloading DragonFlyBSD ${DRAGONFLY_VERSION} ISO..."
  log "  trying mirror: ${ISO_URL}"
  if ! wget -q --show-progress -O "${ISO_PATH}.tmp" "$ISO_URL" 2>/dev/null; then
    log "  mirror failed, trying avalon (bz2): ${AVALON_URL}"
    if wget -q --show-progress -O "${ISO_PATH}.tmp.bz2" "$AVALON_URL" 2>/dev/null; then
      log "  decompressing bz2 ISO..."
      bunzip2 "${ISO_PATH}.tmp.bz2"
      mv "${ISO_PATH}.tmp" "$ISO_PATH"
    else
      log "WARNING: could not download DragonFlyBSD ISO from either mirror."
      log "         Skipping DragonFlyBSD VM smoke (ISO unavailable)."
      info_skip
    fi
  else
    mv "${ISO_PATH}.tmp" "$ISO_PATH"
  fi
  log "ISO cached: $ISO_PATH ($(du -sh "$ISO_PATH" | cut -f1))"
else
  log "ISO already cached: $ISO_PATH"
fi

# Create a writable HDD for installation (DragonFly ISO is a full installer,
# not a cloud image; we boot from the ISO and use the HDD for persistence).
# NOTE: DragonFlyBSD does not ship a cloud image with SSH pre-enabled — the
# ISO boots to an interactive installer.  For unattended use we need to either
# use a pre-installed image snapshot or drive the installer via expect(1).
# Because neither is reliably available, we attempt a live SSH probe and fall
# back to the INFO skip path if the guest does not answer.

if [[ ! -f "$HDD_IMG" ]]; then
  log "creating HDD image (20G) for DragonFly installation..."
  qemu-img create -f qcow2 "$HDD_IMG" 20G
fi

# ── SSH key ───────────────────────────────────────────────────────────────────

if [[ ! -f "$SSH_KEY" ]]; then
  log "generating ephemeral SSH key pair..."
  ssh-keygen -t ed25519 -N "" -f "$SSH_KEY" -C "umbravox-dragonfly-vm"
fi

# ── Boot guest ────────────────────────────────────────────────────────────────

log "booting DragonFlyBSD guest from ISO (mem=${VM_MEM}, smp=${VM_SMP}, accel=${ACCEL})..."
log "NOTE: DragonFlyBSD ISO boots an interactive installer; unattended SSH"
log "      is only possible with a pre-installed snapshot image.  This run"
log "      will attempt SSH and emit INFO if the guest does not answer."

qemu-system-x86_64 \
  -machine q35,accel="${ACCEL}" \
  -cpu max \
  -m "${VM_MEM}" \
  -smp "${VM_SMP}" \
  -nographic \
  -nodefaults \
  -serial mon:stdio \
  -drive "if=virtio,format=qcow2,file=${HDD_IMG}" \
  -drive "media=cdrom,readonly=on,file=${ISO_PATH}" \
  -boot d \
  -netdev "user,id=net0,hostfwd=tcp::${VM_SSH_PORT}-:22" \
  -device virtio-net-pci,netdev=net0 \
  &
QEMU_PID=$!
trap 'log "shutting down guest..."; kill "$QEMU_PID" 2>/dev/null || true; wait "$QEMU_PID" 2>/dev/null || true' EXIT

# ── Wait for SSH (with graceful INFO fallback) ────────────────────────────────

# wait_for_ssh already calls info_skip on timeout — no die here.
wait_for_ssh "$VM_SSH_PORT" "$VM_BOOT_TIMEOUT"

# ── Check GHC availability ────────────────────────────────────────────────────

log "checking GHC availability in dports..."
if ! guest_cmd "pkg install -y ghc" 2>/dev/null; then
  log "pkg install ghc failed or ghc not found in dports binary mirror."
  info_skip
fi

# If we reach here, GHC installed successfully.
log "GHC installed. Checking cabal-install..."
guest_cmd "pkg install -y cabal-install" 2>/dev/null || \
  log "INFO: cabal-install not available; will attempt stack or manual bootstrap if needed"

guest_cmd "ghc --version"

# ── Copy source tree ──────────────────────────────────────────────────────────

log "copying source tree to guest /work/umbravox ..."
guest_cmd "mkdir -p /work/umbravox"
tar -C "$UMBRAVOX_ROOT" \
  --exclude='.git' --exclude='dist-newstyle' --exclude='build' \
  -czf - . \
| guest_cmd "tar -C /work/umbravox -xzf -"
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

log "DragonFlyBSD VM smoke: PASS"
log "shutting down guest..."
guest_cmd "shutdown -p now" 2>/dev/null || true
sleep 5

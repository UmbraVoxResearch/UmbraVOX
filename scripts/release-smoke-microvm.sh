#!/usr/bin/env bash
set -euo pipefail

ROOT="${UMBRAVOX_ROOT:-$(pwd)}"
mode="${1:-qemu}"

die() {
  echo "error: $*" >&2
  exit 1
}

require_dir() {
  local path="$1"
  local label="$2"
  [[ -d "$path" ]] || die "$label not found: $path"
}

require_file() {
  local path="$1"
  local label="$2"
  [[ -f "$path" ]] || die "$label not found: $path"
}

require_dir "$ROOT" "UMBRAVOX_ROOT"
cd "$ROOT"

check_artifact() {
  local latest=""
  local latest_mtime=-1
  local mtime
  local artifact
  local artifacts=()

  shopt -s nullglob
  artifacts=(build/releases/umbravox-*-linux-x86_64.tar.gz)
  shopt -u nullglob

  ((${#artifacts[@]} > 0)) || die "no linux release artifact found under build/releases; run make release-linux first"

  for artifact in "${artifacts[@]}"; do
    mtime="$(stat -c '%Y' "$artifact")" || die "unable to stat release artifact: $artifact"
    if ((mtime > latest_mtime)); then
      latest="$artifact"
      latest_mtime="$mtime"
    fi
  done

  [[ -n "$latest" ]] || die "no linux release artifact found under build/releases; run make release-linux first"
  echo "artifact: $latest"
}

if (($# > 1)); then
  echo "usage: $0 <qemu|firecracker>" >&2
  exit 2
fi

qemu_boot_smoke() {
  : "${UMBRAVOX_QEMU_KERNEL:?set UMBRAVOX_QEMU_KERNEL to a Linux kernel image path}"
  : "${UMBRAVOX_QEMU_INITRD:?set UMBRAVOX_QEMU_INITRD to an initrd path}"
  : "${UMBRAVOX_QEMU_ROOTFS:?set UMBRAVOX_QEMU_ROOTFS to a rootfs image path}"
  require_file "$UMBRAVOX_QEMU_KERNEL" "QEMU kernel image"
  require_file "$UMBRAVOX_QEMU_INITRD" "QEMU initrd"
  require_file "$UMBRAVOX_QEMU_ROOTFS" "QEMU rootfs image"
  if [[ -z "${UMBRAVOX_QEMU_APPEND:-}" ]]; then
    if [[ -n "${UMBRAVOX_QEMU_PROFILE:-}" ]]; then
      require_file "$ROOT/scripts/release-smoke-qemu-profile.sh" "QEMU profile helper"
      UMBRAVOX_QEMU_APPEND="$(bash "$ROOT/scripts/release-smoke-qemu-profile.sh" "$UMBRAVOX_QEMU_PROFILE")"
    else
      : "${UMBRAVOX_QEMU_APPEND:?set UMBRAVOX_QEMU_APPEND, or set UMBRAVOX_QEMU_PROFILE to a deterministic profile}"
    fi
  fi
  [[ -n "$UMBRAVOX_QEMU_APPEND" ]] || die "QEMU kernel command line is empty"

  qemu-system-x86_64 \
    -machine q35,accel=kvm \
    -cpu max \
    -m "${UMBRAVOX_QEMU_MEM_MB:-1024}" \
    -smp "${UMBRAVOX_QEMU_CPUS:-2}" \
    -nographic \
    -nodefaults \
    -no-reboot \
    -kernel "$UMBRAVOX_QEMU_KERNEL" \
    -initrd "$UMBRAVOX_QEMU_INITRD" \
    -append "$UMBRAVOX_QEMU_APPEND" \
    -drive "if=virtio,format=raw,file=${UMBRAVOX_QEMU_ROOTFS},readonly=on"
}

firecracker_boot_smoke() {
  : "${UMBRAVOX_FIRECRACKER_KERNEL:?set UMBRAVOX_FIRECRACKER_KERNEL to a Linux kernel image path}"
  : "${UMBRAVOX_FIRECRACKER_ROOTFS:?set UMBRAVOX_FIRECRACKER_ROOTFS to a rootfs image path}"
  : "${UMBRAVOX_FIRECRACKER_CONFIG:?set UMBRAVOX_FIRECRACKER_CONFIG to a Firecracker config JSON path}"
  require_file "$UMBRAVOX_FIRECRACKER_KERNEL" "Firecracker kernel image"
  require_file "$UMBRAVOX_FIRECRACKER_ROOTFS" "Firecracker rootfs image"
  require_file "$UMBRAVOX_FIRECRACKER_CONFIG" "Firecracker config"

  firecracker --config-file "$UMBRAVOX_FIRECRACKER_CONFIG"
}

case "$mode" in
  qemu)
    if ! command -v qemu-system-x86_64 >/dev/null 2>&1; then
      die "qemu-system-x86_64 not available; install QEMU for microVM smoke lane"
    fi
    if [[ ! -e /dev/kvm ]]; then
      die "/dev/kvm not present; QEMU smoke lane requires KVM-capable host"
    fi
    check_artifact
    if [[ -n "${UMBRAVOX_QEMU_SMOKE_RUNNER:-}" ]]; then
      echo "running QEMU smoke runner command from UMBRAVOX_QEMU_SMOKE_RUNNER"
      # Intended usage: provide a host-specific command that boots a prepared
      # guest image and performs in-guest bundle checks.
      bash -lc "$UMBRAVOX_QEMU_SMOKE_RUNNER"
      exit 0
    fi
    if [[ -n "${UMBRAVOX_QEMU_KERNEL:-}" ]] || [[ -n "${UMBRAVOX_QEMU_INITRD:-}" ]] || [[ -n "${UMBRAVOX_QEMU_ROOTFS:-}" ]] || [[ -n "${UMBRAVOX_QEMU_APPEND:-}" ]] || [[ -n "${UMBRAVOX_QEMU_PROFILE:-}" ]]; then
      echo "running QEMU pinned-boot smoke path from UMBRAVOX_QEMU_* inputs"
      qemu_boot_smoke
      exit 0
    fi
    cat <<'EOF'
QEMU microVM smoke scaffold
- prerequisites satisfied
- to execute in-guest checks now, set UMBRAVOX_QEMU_SMOKE_RUNNER to a host-specific boot-and-check command
- or set UMBRAVOX_QEMU_KERNEL, UMBRAVOX_QEMU_INITRD, UMBRAVOX_QEMU_ROOTFS, and UMBRAVOX_QEMU_APPEND for pinned-boot execution
- optional deterministic profile path: set UMBRAVOX_QEMU_PROFILE (uses scripts/release-smoke-qemu-profile.sh)
- default behavior remains scaffold-only until pinned guest boot wiring is configured
EOF
    ;;
  firecracker)
    if ! command -v firecracker >/dev/null 2>&1; then
      die "firecracker not available; install Firecracker for microVM smoke lane"
    fi
    if [[ ! -e /dev/kvm ]]; then
      die "/dev/kvm not present; Firecracker smoke lane requires KVM-capable host"
    fi
    check_artifact
    if [[ -n "${UMBRAVOX_FIRECRACKER_SMOKE_RUNNER:-}" ]]; then
      echo "running Firecracker smoke runner command from UMBRAVOX_FIRECRACKER_SMOKE_RUNNER"
      bash -lc "$UMBRAVOX_FIRECRACKER_SMOKE_RUNNER"
      exit 0
    fi
    if [[ -n "${UMBRAVOX_FIRECRACKER_KERNEL:-}" ]] || [[ -n "${UMBRAVOX_FIRECRACKER_ROOTFS:-}" ]] || [[ -n "${UMBRAVOX_FIRECRACKER_CONFIG:-}" ]]; then
      echo "running Firecracker pinned-boot smoke path from UMBRAVOX_FIRECRACKER_* inputs"
      firecracker_boot_smoke
      exit 0
    fi
    cat <<'EOF'
Firecracker microVM smoke scaffold
- prerequisites satisfied
- to execute in-guest checks now, set UMBRAVOX_FIRECRACKER_SMOKE_RUNNER to a host-specific boot-and-check command
- or set UMBRAVOX_FIRECRACKER_KERNEL, UMBRAVOX_FIRECRACKER_ROOTFS, and UMBRAVOX_FIRECRACKER_CONFIG for pinned-boot execution
- default behavior remains scaffold-only until pinned microVM boot wiring is configured
EOF
    ;;
  *)
    echo "usage: $0 <qemu|firecracker>" >&2
    exit 2
    ;;
esac

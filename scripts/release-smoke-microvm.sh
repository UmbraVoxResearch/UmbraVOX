#!/usr/bin/env bash
set -euo pipefail

ROOT="${UMBRAVOX_ROOT:-$(pwd)}"
cd "$ROOT"

mode="${1:-qemu}"

check_artifact() {
  local latest
  latest="$(ls -1t build/releases/umbravox-*-linux-x86_64.tar.gz 2>/dev/null | head -n1 || true)"
  if [[ -z "${latest:-}" ]]; then
    echo "no linux release artifact found under build/releases; run make release-linux first" >&2
    exit 1
  fi
  echo "artifact: $latest"
}

qemu_boot_smoke() {
  : "${UMBRAVOX_QEMU_KERNEL:?set UMBRAVOX_QEMU_KERNEL to a Linux kernel image path}"
  : "${UMBRAVOX_QEMU_INITRD:?set UMBRAVOX_QEMU_INITRD to an initrd path}"
  : "${UMBRAVOX_QEMU_ROOTFS:?set UMBRAVOX_QEMU_ROOTFS to a rootfs image path}"
  : "${UMBRAVOX_QEMU_APPEND:?set UMBRAVOX_QEMU_APPEND to kernel cmdline (include console=ttyS0 and one-shot smoke command)}"

  local accel="tcg"
  if [[ -e /dev/kvm ]]; then
    accel="kvm"
  fi

  qemu-system-x86_64 \
    -machine q35,accel="${accel}" \
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

case "$mode" in
  qemu)
    if ! command -v qemu-system-x86_64 >/dev/null 2>&1; then
      echo "qemu-system-x86_64 not available; install QEMU for microVM smoke lane" >&2
      exit 1
    fi
    if [[ ! -e /dev/kvm ]]; then
      echo "/dev/kvm not present; QEMU smoke lane requires KVM-capable host" >&2
      exit 1
    fi
    check_artifact
    if [[ -n "${UMBRAVOX_QEMU_SMOKE_RUNNER:-}" ]]; then
      echo "running QEMU smoke runner command from UMBRAVOX_QEMU_SMOKE_RUNNER"
      # Intended usage: provide a host-specific command that boots a prepared
      # guest image and performs in-guest bundle checks.
      bash -lc "$UMBRAVOX_QEMU_SMOKE_RUNNER"
      exit 0
    fi
    if [[ -n "${UMBRAVOX_QEMU_KERNEL:-}" ]] || [[ -n "${UMBRAVOX_QEMU_INITRD:-}" ]] || [[ -n "${UMBRAVOX_QEMU_ROOTFS:-}" ]] || [[ -n "${UMBRAVOX_QEMU_APPEND:-}" ]]; then
      echo "running QEMU pinned-boot smoke path from UMBRAVOX_QEMU_* inputs"
      qemu_boot_smoke
      exit 0
    fi
    cat <<'EOF'
QEMU microVM smoke scaffold
- prerequisites satisfied
- to execute in-guest checks now, set UMBRAVOX_QEMU_SMOKE_RUNNER to a host-specific boot-and-check command
- or set UMBRAVOX_QEMU_KERNEL, UMBRAVOX_QEMU_INITRD, UMBRAVOX_QEMU_ROOTFS, and UMBRAVOX_QEMU_APPEND for pinned-boot execution
- default behavior remains scaffold-only until pinned guest boot wiring is configured
EOF
    ;;
  firecracker)
    if ! command -v firecracker >/dev/null 2>&1; then
      echo "firecracker not available; install Firecracker for microVM smoke lane" >&2
      exit 1
    fi
    if [[ ! -e /dev/kvm ]]; then
      echo "/dev/kvm not present; Firecracker smoke lane requires KVM-capable host" >&2
      exit 1
    fi
    check_artifact
    if [[ -n "${UMBRAVOX_FIRECRACKER_SMOKE_RUNNER:-}" ]]; then
      echo "running Firecracker smoke runner command from UMBRAVOX_FIRECRACKER_SMOKE_RUNNER"
      bash -lc "$UMBRAVOX_FIRECRACKER_SMOKE_RUNNER"
      exit 0
    fi
    cat <<'EOF'
Firecracker microVM smoke scaffold
- prerequisites satisfied
- to execute in-guest checks now, set UMBRAVOX_FIRECRACKER_SMOKE_RUNNER to a host-specific boot-and-check command
- default behavior remains scaffold-only until pinned microVM boot wiring is configured
EOF
    ;;
  *)
    echo "usage: $0 <qemu|firecracker>" >&2
    exit 2
    ;;
esac

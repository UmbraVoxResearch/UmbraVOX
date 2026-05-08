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
    cat <<'EOF'
QEMU microVM smoke scaffold
- prerequisites satisfied
- next step: boot smoke VM image and run bundle launch/manifest checks in-guest
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
    cat <<'EOF'
Firecracker microVM smoke scaffold
- prerequisites satisfied
- next step: boot smoke microVM and run bundle launch/manifest checks in-guest
EOF
    ;;
  *)
    echo "usage: $0 <qemu|firecracker>" >&2
    exit 2
    ;;
esac

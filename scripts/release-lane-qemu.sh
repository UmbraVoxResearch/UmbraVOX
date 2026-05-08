#!/usr/bin/env bash
set -euo pipefail

if ! command -v qemu-system-x86_64 >/dev/null 2>&1; then
  echo "qemu-system-x86_64 not available; install QEMU for integration lane execution" >&2
  exit 1
fi

if [[ ! -e /dev/kvm ]]; then
  echo "/dev/kvm not present; QEMU lane requires KVM-capable host for parity with planned CI" >&2
  exit 1
fi

cat <<'EOF'
QEMU release lane scaffold
- host supports qemu-system-x86_64 and /dev/kvm
- next implementation step: attach reproducible VM image rootfs and run release workflow in-guest
EOF

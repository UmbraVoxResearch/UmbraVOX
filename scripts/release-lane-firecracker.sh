#!/usr/bin/env bash
set -euo pipefail

if ! command -v firecracker >/dev/null 2>&1; then
  echo "firecracker not available; install Firecracker for authoritative release lane execution" >&2
  exit 1
fi

if [[ ! -e /dev/kvm ]]; then
  echo "/dev/kvm not present; Firecracker lane requires KVM-capable host" >&2
  exit 1
fi

cat <<'EOF'
Firecracker release lane scaffold
- host supports firecracker and /dev/kvm
- next implementation step: boot pinned builder microVM and execute release graph inside guest
EOF

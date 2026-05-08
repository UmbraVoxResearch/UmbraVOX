#!/usr/bin/env bash
set -euo pipefail

profile="${1:-bundle-basic}"

case "$profile" in
  bundle-basic)
    cat <<'EOF'
console=ttyS0 panic=1 rdinit=/bin/sh UMBRAVOX_SMOKE=1 UMBRAVOX_SMOKE_MODE=bundle-basic
EOF
    ;;
  *)
    echo "unknown profile: $profile" >&2
    echo "supported: bundle-basic" >&2
    exit 2
    ;;
esac

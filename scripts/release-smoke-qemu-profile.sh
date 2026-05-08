#!/usr/bin/env bash
set -euo pipefail

die() {
  echo "error: $*" >&2
  exit 1
}

if (($# > 1)); then
  die "usage: $0 [bundle-basic]"
fi

profile="${1:-bundle-basic}"

case "$profile" in
  bundle-basic)
    printf '%s\n' 'console=ttyS0 panic=1 rdinit=/bin/sh UMBRAVOX_SMOKE=1 UMBRAVOX_SMOKE_MODE=bundle-basic'
    ;;
  *)
    echo "error: unknown profile: $profile" >&2
    echo "supported: bundle-basic" >&2
    exit 2
    ;;
esac

#!/usr/bin/env bash
set -euo pipefail

ROOT="${UMBRAVOX_ROOT:-$(pwd)}"
cd "$ROOT"

latest="$(ls -1t build/releases/umbravox-*-linux-x86_64.tar.gz 2>/dev/null | head -n1 || true)"
if [[ -z "${latest:-}" ]]; then
  echo "no linux release artifact found under build/releases; run make release-linux first" >&2
  exit 1
fi

runner=""
if command -v podman >/dev/null 2>&1; then
  runner="podman"
elif command -v docker >/dev/null 2>&1; then
  runner="docker"
fi

if [[ -z "$runner" ]]; then
  echo "SKIP: neither podman nor docker is available for isolated smoke run"
  exit 0
fi

echo "Using ${runner} for isolated smoke run"
echo "Artifact: ${latest}"

"$runner" run --rm -i \
  -v "$ROOT:/work:ro" \
  docker.io/library/ubuntu:24.04 \
  bash -lc "
    set -euo pipefail
    tmp=\$(mktemp -d)
    tar -xzf /work/${latest#"$ROOT/"} -C \"\$tmp\"
    dir=\$(find \"\$tmp\" -maxdepth 1 -mindepth 1 -type d | head -n1)
    test -x \"\$dir/run-umbravox.sh\"
    \"\$dir/run-umbravox.sh\" --help >/dev/null 2>&1 || true
    test -f \"\$dir/LINKAGE.txt\"
    test -f \"\$dir/FILE.txt\"
    echo 'smoke ok'
  "

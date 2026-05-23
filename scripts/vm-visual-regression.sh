#!/usr/bin/env bash
set -euo pipefail
echo "=== UmbraVOX Visual Regression ==="

# When launched via vm-dev-run.sh exec, /work/umbravox already exists
# with build cache symlinks intact. Only copy fresh if missing.
if [ ! -d /work/umbravox ]; then
    mount /dev/vdb /mnt/src 2>/dev/null || mount -o ro /dev/vdb /mnt/src
    cp -a /mnt/src /work/umbravox
fi
cd /work/umbravox
bash scripts/vm-tui-scenario.sh /work/fresh
REFDIR="test/evidence/visual-reference"
RESULT="PASS"
if [ ! -d "$REFDIR" ] || [ -z "$(ls -A "$REFDIR" 2>/dev/null)" ]; then
  echo "No reference screenshots found — skipping regression check"
  echo "Visual reference update is now handled by ./uv"
  echo "VISUAL_REGRESSION=SKIP"
  exit 0
fi
strip_ansi() { sed 's/\x1b\[[0-9;]*[a-zA-Z]//g' "$1"; }
for ref in "$REFDIR"/*.ansi; do
  name="$(basename "$ref")"
  fresh="/work/fresh/$name"
  if [ ! -f "$fresh" ]; then
    echo "MISSING: $name (no fresh capture)"
    RESULT="FAIL"
    continue
  fi
  if ! diff -u <(strip_ansi "$ref") <(strip_ansi "$fresh") > "/work/diff-$name.txt" 2>&1; then
    echo "REGRESSION: $name"
    cat "/work/diff-$name.txt"
    RESULT="FAIL"
  else
    echo "OK: $name"
  fi
done
echo "VISUAL_REGRESSION=$RESULT"

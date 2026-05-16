#!/usr/bin/env bash
set -euo pipefail
echo "=== UmbraVOX Screenshot Capture ==="
mount /dev/vdb /mnt/src 2>/dev/null || mount -o ro /dev/vdb /mnt/src
cp -a /mnt/src /work/umbravox
cd /work/umbravox
bash scripts/vm-tui-scenario.sh /work/screenshots
# Convert ANSI to HTML if aha is available
if command -v aha >/dev/null 2>&1; then
  for f in /work/screenshots/*.ansi; do
    aha -f "$f" > "${f%.ansi}.html" 2>/dev/null || true
  done
fi
ls -la /work/screenshots/
echo "SCREENSHOT_RESULT=PASS"

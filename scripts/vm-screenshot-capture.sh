#!/usr/bin/env bash
set -euo pipefail
echo "=== UmbraVOX Screenshot Capture ==="

# When launched via vm-dev-run.sh exec, /work/umbravox already exists
# with build cache symlinks intact. Only copy fresh if missing.
if [ ! -d /work/umbravox ]; then
    mount /dev/vdb /mnt/src 2>/dev/null || mount -o ro /dev/vdb /mnt/src
    cp -a /mnt/src /work/umbravox
fi
cd /work/umbravox
bash scripts/vm-tui-scenario.sh /work/screenshots
# Convert ANSI to HTML if aha is available
if command -v aha >/dev/null 2>&1; then
  for f in /work/screenshots/*.ansi; do
    aha -f "$f" > "${f%.ansi}.html" 2>/dev/null || true
  done
fi
ls -la /work/screenshots/
# Copy to shared output directory (9p mount) so host can access results
if [ -d /output ]; then
  mkdir -p /output/screenshots
  cp -a /work/screenshots/* /output/screenshots/ 2>/dev/null || true
fi
echo "SCREENSHOT_RESULT=PASS"

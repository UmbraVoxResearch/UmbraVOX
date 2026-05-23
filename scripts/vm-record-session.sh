#!/usr/bin/env bash
set -euo pipefail
echo "=== UmbraVOX Session Recording ==="

# When launched via ./uv exec --, /work/umbravox already exists
# with build cache symlinks intact. Only copy fresh if missing.
if [ ! -d /work/umbravox ]; then
    mount /dev/vdb /mnt/src 2>/dev/null || mount -o ro /dev/vdb /mnt/src
    cp -a /mnt/src /work/umbravox
fi
cd /work/umbravox
export TERM=xterm-256color
asciinema rec --cols 120 --rows 40 --overwrite /work/recording.cast \
  -c "bash scripts/vm-tui-scenario.sh /work/screenshots"
# Copy to shared output directory (9p mount) so host can access results
if [ -d /output ]; then
  mkdir -p /output/recordings
  cp -a /work/recording.cast /output/recordings/ 2>/dev/null || true
  cp -a /work/screenshots/* /output/recordings/ 2>/dev/null || true
fi
echo "RECORD_RESULT=PASS"

#!/usr/bin/env bash
set -euo pipefail
echo "=== UmbraVOX Session Recording ==="
mount /dev/vdb /mnt/src 2>/dev/null || mount -o ro /dev/vdb /mnt/src
cp -a /mnt/src /work/umbravox
cd /work/umbravox
export TERM=xterm-256color
asciinema rec --cols 120 --rows 40 --overwrite /work/recording.cast \
  -c "bash scripts/vm-tui-scenario.sh /work/screenshots"
echo "RECORD_RESULT=PASS"

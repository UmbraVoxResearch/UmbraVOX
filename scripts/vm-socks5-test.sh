#!/usr/bin/env bash
set -euo pipefail
echo "=== UmbraVOX SOCKS5 Transport Test ==="
# Start microsocks proxy on loopback
microsocks -p 1080 &
PROXY_PID=$!
sleep 0.5
# Start echo server
(while true; do nc -l -p 9999 -e cat 2>/dev/null || nc -l 9999; done) &
ECHO_PID=$!
sleep 0.5
# Run SOCKS5 probe (send "hello" through proxy to echo server, verify reply)
REPLY=$(echo "hello" | nc -X 5 -x 127.0.0.1:1080 127.0.0.1 9999 2>/dev/null || echo "FAIL")
kill "$PROXY_PID" "$ECHO_PID" 2>/dev/null || true
if [ "$REPLY" = "hello" ]; then
  echo "SOCKS5_TEST=PASS"
else
  echo "SOCKS5_TEST=FAIL (got: $REPLY)"
fi

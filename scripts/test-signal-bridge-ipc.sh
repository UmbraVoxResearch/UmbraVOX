#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# Signal bridge IPC subprocess smoke test (M19.6.3)
#
# Starts umbravox-signal-bridge as a child process, sends IPC commands
# over stdin, and validates each response line.
set -euo pipefail

ROOT="${UMBRAVOX_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
cd "$ROOT"

# -- locate binary -----------------------------------------------------------
BRIDGE=""
if [[ -n "${SIGNAL_BRIDGE_BIN:-}" ]]; then
    BRIDGE="$SIGNAL_BRIDGE_BIN"
else
    BRIDGE="$(find dist-newstyle -path '*/build/umbravox-signal-bridge/umbravox-signal-bridge' -type f 2>/dev/null | sort -r | head -1)"
fi

if [[ -z "$BRIDGE" || ! -x "$BRIDGE" ]]; then
    echo "FAIL: umbravox-signal-bridge binary not found. Run: ./uv build --signal-bridge" >&2
    exit 1
fi
echo "Binary: $BRIDGE"

# -- helpers ------------------------------------------------------------------
PASS=0
FAIL=0
TOTAL=0

check() {
    local label="$1" expected_prefix="$2" actual="$3"
    TOTAL=$((TOTAL + 1))
    if [[ "$actual" == "$expected_prefix"* ]]; then
        echo "  PASS  $label  (got: ${actual:0:72})"
        PASS=$((PASS + 1))
    else
        echo "  FAIL  $label  (expected prefix: '$expected_prefix', got: '${actual:0:72}')"
        FAIL=$((FAIL + 1))
    fi
}

# -- start bridge subprocess --------------------------------------------------
# We use a coprocess so we can send commands one at a time and read responses.
coproc BRIDGE_PROC ("$BRIDGE" 2>/dev/null)
BRIDGE_PID=$BRIDGE_PROC_PID
# File descriptors for stdin/stdout of the bridge.
BRIDGE_IN=${BRIDGE_PROC[1]}
BRIDGE_OUT=${BRIDGE_PROC[0]}

cleanup() {
    kill "$BRIDGE_PID" 2>/dev/null || true
    wait "$BRIDGE_PID" 2>/dev/null || true
}
trap cleanup EXIT

send_cmd() {
    echo "$1" >&"$BRIDGE_IN"
}

read_resp() {
    local line=""
    read -r -t 5 line <&"$BRIDGE_OUT" || true
    echo "$line"
}

echo ""
echo "=== Signal Bridge IPC Smoke Test ==="
echo ""

# -- 1. PING -> PONG ----------------------------------------------------------
send_cmd "PING"
resp=$(read_resp)
check "PING -> PONG" "PONG" "$resp"

# -- 2. STATUS before AUTH -> STATUS (session:false) --------------------------
send_cmd "STATUS"
resp=$(read_resp)
check "STATUS (pre-auth)" "STATUS " "$resp"

# -- 3. AUTH 0 -> AUTH_OK -----------------------------------------------------
send_cmd "AUTH 0"
resp=$(read_resp)
check "AUTH 0 -> AUTH_OK" "AUTH_OK" "$resp"

# -- 4. STATUS after AUTH -> STATUS (session:true) ----------------------------
send_cmd "STATUS"
resp=$(read_resp)
check "STATUS (post-auth)" "STATUS " "$resp"

# -- 5. CONTACTS -> CONTACTS <hex> -------------------------------------------
send_cmd "CONTACTS"
resp=$(read_resp)
check "CONTACTS" "CONTACTS " "$resp"

# -- 6. SEND <hex> -> OK or ERR -----------------------------------------------
# Encode a minimal JSON envelope: {"to":"alice","body":"hi","timestamp":1}
HEX_ENVELOPE="7b22746f223a22616c696365222c22626f6479223a226869222c2274696d657374616d70223a317d"
send_cmd "SEND $HEX_ENVELOPE"
resp=$(read_resp)
check "SEND <hex>" "OK " "$resp"

# -- 7. SEND with bad hex -> ERR -----------------------------------------------
send_cmd "SEND zzzz"
resp=$(read_resp)
check "SEND bad hex -> ERR" "ERR " "$resp"

# -- 8. INFO (unknown) -> ERR unknown command ---------------------------------
send_cmd "INFO"
resp=$(read_resp)
check "INFO -> ERR unknown" "ERR " "$resp"

# -- 9. CLOSE -> OK, process exits -------------------------------------------
send_cmd "CLOSE"
resp=$(read_resp)
check "CLOSE -> OK" "OK" "$resp"

# Wait for the bridge to exit.
wait "$BRIDGE_PID" 2>/dev/null || true
trap - EXIT

# -- summary ------------------------------------------------------------------
echo ""
echo "=== Results: $PASS/$TOTAL passed, $FAIL failed ==="
echo ""

if [[ "$FAIL" -gt 0 ]]; then
    exit 1
fi
exit 0

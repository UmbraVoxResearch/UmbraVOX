#!/usr/bin/env bash
# ── UmbraVOX Integration Test Agent ─────────────────────────────────
# Runs INSIDE a minimal NixOS test VM. Not invoked directly by users.
#
# The release bundle is on /dev/vdb (ext2, read-only) along with
# agent.env configuration. The agent extracts the bundle, configures
# networking, runs scenario-based tests, and reports results.
#
# Scenarios:
#   readiness  - verify bundle integrity and network config (default)
#   exchange   - start listener, discover peers, verify connectivity
#   flood      - send many messages to connected peers (future/stub)
#
# Networking:
#   M5.3.3 TAP+bridge networking is future work (requires host-side
#   bridge setup and per-VM TAP devices). Current integration tests
#   use dgram multicast (M5.3.4) for peer discovery when available.
#
# Default port: 7853 (from UmbraVox.Protocol.Encoding.defaultPorts)
set -euo pipefail

echo "========================================"
echo "  UmbraVOX Integration Test Agent"
echo "========================================"

# ── Load agent configuration ────────────────────────────────────────
if [ -f /mnt/bundle/agent.env ]; then
    source /mnt/bundle/agent.env
else
    echo "AGENT_RESULT=FAIL (no agent.env found)"
    exit 1
fi

echo "agent_id:    ${AGENT_ID:-?}"
echo "agent_ip:    ${AGENT_IP:-?}"
echo "port:        ${AGENT_PORT:-7853}"
echo "peers:       ${AGENT_PEERS:-none}"
echo "scenario:    ${AGENT_SCENARIO:-readiness}"
echo "timeout:     ${AGENT_TIMEOUT:-60}s"
echo ""

PASS=0
FAIL=0

check() {
    local label="$1"
    shift
    if "$@" 2>/dev/null; then
        echo "  PASS: $label"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $label"
        FAIL=$((FAIL + 1))
    fi
}

# ── Configure network ──────────────────────────────────────────────
if [ -n "${AGENT_IP:-}" ]; then
    ip addr add "${AGENT_IP}/24" dev eth0 2>/dev/null || true
    ip link set eth0 up 2>/dev/null || true
    # Add default route via gateway (.1)
    GATEWAY=$(echo "$AGENT_IP" | sed 's/\.[0-9]*$/.1/')
    ip route add default via "$GATEWAY" 2>/dev/null || true
fi

# ── Extract release bundle ─────────────────────────────────────────
BUNDLE=""
for f in /mnt/bundle/umbravox-*-linux-x86_64.tar.gz; do
    [ -f "$f" ] && BUNDLE="$f" && break
done

if [ -z "$BUNDLE" ]; then
    echo "AGENT_RESULT=FAIL (no release bundle found)"
    exit 1
fi

mkdir -p /work/app
cd /work/app
tar xzf "$BUNDLE" 2>/dev/null

APPDIR=""
for d in umbravox-*; do
    [ -d "$d" ] && APPDIR="$d" && break
done

if [ -z "$APPDIR" ]; then
    echo "AGENT_RESULT=FAIL (bundle extraction failed)"
    exit 1
fi

cd "$APPDIR"

# ── Readiness checks (always run) ──────────────────────────────────
echo ""
echo "── readiness checks ──"

check "binary exists" test -f umbravox -a -x umbravox
check "lib directory" test -d lib
check "launch script" test -f run-umbravox.sh -a -x run-umbravox.sh
check "release manifest" test -f RELEASE-MANIFEST.txt
check "checksums" test -f CONTENTS.SHA256

# Network checks
if [ -n "${AGENT_IP:-}" ]; then
    check "IP assigned" ip addr show eth0 2>/dev/null | grep -q "inet "

    # Can we bind our port?
    check "port bindable" bash -c "
        exec 3<>/dev/tcp/0.0.0.0/${AGENT_PORT:-7853} 2>/dev/null && exec 3>&- || true
        # Fallback: use ss to check port is free
        ! ss -tlnp 2>/dev/null | grep -q ':${AGENT_PORT:-7853} '
    "
fi

# ── Scenario-specific checks ──────────────────────────────────────
case "${AGENT_SCENARIO:-readiness}" in
    readiness)
        echo ""
        echo "── readiness scenario complete ──"
        ;;
    exchange)
        echo ""
        echo "── exchange scenario ──"
        # Wait for network to stabilize
        sleep 2

        # Try to reach seed peers
        if [ -n "${AGENT_PEERS:-}" ]; then
            for peer in $(echo "$AGENT_PEERS" | tr ',' ' '); do
                peer_ip="${peer%%:*}"
                peer_port="${peer##*:}"
                # Try TCP connect to peer
                check "reach peer $peer" bash -c "
                    timeout 5 bash -c 'echo > /dev/tcp/$peer_ip/$peer_port' 2>/dev/null
                " || true
            done
        fi

        # Check mDNS multicast group joined (dgram multicast, M5.3.4)
        if ip maddr show eth0 2>/dev/null | grep -q "224.0.0.251"; then
            echo "  INFO: mDNS multicast group joined"
        fi

        # NOTE: M5.3.3 TAP+bridge networking is future work.
        # Current exchange tests rely on dgram multicast (M5.3.4)
        # which is already wired in the Haskell orchestrator. Full
        # TAP+bridge support requires host-side bridge setup and
        # per-VM TAP device allocation before VM boot.
        ;;
    flood)
        echo ""
        echo "── flood scenario (stub) ──"
        echo "  INFO: flood scenario not yet implemented"
        echo "  INFO: will send many messages to connected peers once"
        echo "  INFO: headless mode is available"
        ;;
    *)
        echo ""
        echo "── unknown scenario: ${AGENT_SCENARIO} ──"
        ;;
esac

# ── Summary ─────────────────────────────────────────────────────────
echo ""
echo "========================================"
echo "  AGENT SUMMARY: $PASS passed, $FAIL failed"
echo "========================================"

if [ "$FAIL" -eq 0 ]; then
    echo "AGENT_RESULT=PASS"
else
    echo "AGENT_RESULT=FAIL"
fi

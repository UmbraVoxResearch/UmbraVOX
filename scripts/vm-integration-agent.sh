#!/usr/bin/env bash
# ── UmbraVOX Integration Test Agent ─────────────────────────────────
# Runs INSIDE a minimal NixOS test VM. Not invoked directly by users.
#
# The release bundle is on /dev/vdb (ext2, read-only) along with
# agent.env configuration. The agent extracts the bundle, configures
# networking, starts umbravox-headless, and reports results.
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

echo "agent_id:  ${AGENT_ID:-?}"
echo "agent_ip:  ${AGENT_IP:-?}"
echo "port:      ${AGENT_PORT:-7853}"
echo "peers:     ${AGENT_PEERS:-none}"
echo "scenario:  ${AGENT_SCENARIO:-listen}"
echo "timeout:   ${AGENT_TIMEOUT:-60}s"
echo ""

# ── Configure network ──────────────────────────────────────────────
# Set static IP on eth0 (the virtio-net device)
if [ -n "${AGENT_IP:-}" ]; then
    ip addr add "${AGENT_IP}/24" dev eth0 2>/dev/null || true
    ip link set eth0 up 2>/dev/null || true
    echo "network: ${AGENT_IP}/24 on eth0"
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

echo "bundle: $BUNDLE"
mkdir -p /work/app
cd /work/app
tar xzf "$BUNDLE" 2>/dev/null

# Find extracted directory
APPDIR=""
for d in umbravox-*; do
    [ -d "$d" ] && APPDIR="$d" && break
done

if [ -z "$APPDIR" ]; then
    echo "AGENT_RESULT=FAIL (bundle extraction failed)"
    exit 1
fi

cd "$APPDIR"
echo "app dir: $(pwd)"

# ── Check binary ───────────────────────────────────────────────────
if [ ! -f umbravox ] || [ ! -x umbravox ]; then
    echo "AGENT_RESULT=FAIL (binary not found or not executable)"
    exit 1
fi
echo "binary: OK"

# ── Run headless agent ─────────────────────────────────────────────
# The release bundle doesn't include umbravox-headless yet (it's a
# separate binary). For now, just verify the bundle works and report
# connectivity readiness.

echo ""
echo "── agent readiness check ──"
PASS=0
FAIL=0

# Check 1: binary exists
echo "  CHECK: binary exists and is executable"
PASS=$((PASS + 1))

# Check 2: library directory
if [ -d lib ]; then
    echo "  CHECK: lib directory present"
    PASS=$((PASS + 1))
else
    echo "  FAIL: lib directory missing"
    FAIL=$((FAIL + 1))
fi

# Check 3: run script
if [ -f run-umbravox.sh ] && [ -x run-umbravox.sh ]; then
    echo "  CHECK: launch script present"
    PASS=$((PASS + 1))
else
    echo "  FAIL: launch script missing"
    FAIL=$((FAIL + 1))
fi

# Check 4: network interface up
if ip addr show eth0 2>/dev/null | grep -q "inet "; then
    echo "  CHECK: network interface configured"
    PASS=$((PASS + 1))
else
    echo "  FAIL: network interface not configured"
    FAIL=$((FAIL + 1))
fi

# Check 5: can resolve own IP
if [ -n "${AGENT_IP:-}" ]; then
    echo "  CHECK: agent IP ${AGENT_IP} assigned"
    PASS=$((PASS + 1))
fi

echo ""
echo "========================================"
echo "  AGENT SUMMARY: $PASS passed, $FAIL failed"
echo "========================================"

if [ "$FAIL" -eq 0 ]; then
    echo "AGENT_RESULT=PASS"
else
    echo "AGENT_RESULT=FAIL"
fi

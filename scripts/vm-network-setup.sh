#!/usr/bin/env bash
# ── UmbraVOX VM Network Setup ──────────────────────────────────────
# Creates or tears down TAP+bridge infrastructure for dual-LAN
# integration testing. Requires CAP_NET_ADMIN (sudo).
#
# Usage:
#   sudo scripts/vm-network-setup.sh setup <agent-count>
#   sudo scripts/vm-network-setup.sh teardown
#   scripts/vm-network-setup.sh status
set -euo pipefail

BRIDGE_A="br-umbravox-a"
BRIDGE_B="br-umbravox-b"
SUBNET_A="10.0.42"
SUBNET_B="10.0.43"

setup() {
    local agent_count="${1:-6}"
    local half=$((agent_count / 2))

    echo "[NET] creating dual-LAN bridge infrastructure..."

    # Create bridges
    ip link add "$BRIDGE_A" type bridge 2>/dev/null || true
    ip link add "$BRIDGE_B" type bridge 2>/dev/null || true
    ip link set "$BRIDGE_A" up
    ip link set "$BRIDGE_B" up
    ip addr add "${SUBNET_A}.1/24" dev "$BRIDGE_A" 2>/dev/null || true
    ip addr add "${SUBNET_B}.1/24" dev "$BRIDGE_B" 2>/dev/null || true

    # Enable IP forwarding between bridges
    sysctl -w net.ipv4.ip_forward=1 > /dev/null

    # Create TAP devices for LAN A agents
    for i in $(seq 0 $((half - 1))); do
        local tap="tap-a${i}"
        ip tuntap add "$tap" mode tap user "$(logname 2>/dev/null || echo $SUDO_USER)" 2>/dev/null || true
        ip link set "$tap" up
        ip link set "$tap" master "$BRIDGE_A"
        echo "  LAN A: $tap -> $BRIDGE_A (agent $i: ${SUBNET_A}.$((10 + i)))"
    done

    # Create TAP devices for LAN B agents
    for i in $(seq 0 $((half - 1))); do
        local j=$((half + i))
        local tap="tap-b${i}"
        ip tuntap add "$tap" mode tap user "$(logname 2>/dev/null || echo $SUDO_USER)" 2>/dev/null || true
        ip link set "$tap" up
        ip link set "$tap" master "$BRIDGE_B"
        echo "  LAN B: $tap -> $BRIDGE_B (agent $j: ${SUBNET_B}.$((10 + i)))"
    done

    echo "[NET] dual-LAN bridges ready"
    echo "  LAN A: $BRIDGE_A (${SUBNET_A}.0/24)"
    echo "  LAN B: $BRIDGE_B (${SUBNET_B}.0/24)"
    echo "  IP forwarding: enabled"
}

teardown() {
    echo "[NET] tearing down bridge infrastructure..."

    # Remove TAP devices
    for tap in $(ip -o link show type tun 2>/dev/null | grep -oP 'tap-[ab]\d+' || true); do
        ip link del "$tap" 2>/dev/null || true
        echo "  removed: $tap"
    done

    # Remove bridges
    for br in "$BRIDGE_A" "$BRIDGE_B"; do
        ip link set "$br" down 2>/dev/null || true
        ip link del "$br" 2>/dev/null || true
        echo "  removed: $br"
    done

    echo "[NET] teardown complete"
}

status() {
    echo "[NET] bridge status:"
    for br in "$BRIDGE_A" "$BRIDGE_B"; do
        if ip link show "$br" > /dev/null 2>&1; then
            local addr=$(ip -4 addr show "$br" 2>/dev/null | grep -oP 'inet \K[\d./]+' || echo "no IP")
            local state=$(ip -o link show "$br" 2>/dev/null | grep -oP 'state \K\w+' || echo "unknown")
            echo "  $br: $state ($addr)"
            bridge link show master "$br" 2>/dev/null | while read line; do
                echo "    $line"
            done
        else
            echo "  $br: not found"
        fi
    done
    echo ""
    echo "  IP forwarding: $(cat /proc/sys/net/ipv4/ip_forward)"
}

case "${1:-status}" in
    setup) setup "${2:-6}" ;;
    teardown) teardown ;;
    status) status ;;
    *) echo "usage: $0 {setup|teardown|status} [agent-count]"; exit 1 ;;
esac

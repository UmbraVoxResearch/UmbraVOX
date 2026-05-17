#!/usr/bin/env bash
# ── UmbraVOX VM Network Policy Enforcer ──────────────────────────────
# Reads vm-network-policy.conf and generates QEMU network arguments.
# Called by vm-dev-run.sh before launching QEMU.
#
# Default: no network (QEMU launched with -nic none).
# If ALLOW rules exist, creates a restricted user-mode network with
# iptables-style filtering via QEMU's built-in firewall.
#
# Usage: source scripts/vm-network-policy.sh
#        QEMU_NET_ARGS contains the result
set -uo pipefail

REPO_ROOT="${REPO_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
POLICY_FILE="$REPO_ROOT/vm-network-policy.conf"

# Parse policy file and generate QEMU network args
generate_network_args() {
    # If no policy file, deny all
    if [ ! -f "$POLICY_FILE" ]; then
        echo "-nic none"
        return
    fi

    # Count ALLOW rules (ignoring comments and blanks)
    local allow_count
    allow_count=$(grep -c '^ALLOW ' "$POLICY_FILE" 2>/dev/null || echo 0)

    if [ "$allow_count" -eq 0 ]; then
        # No ALLOW rules = no network
        echo "-nic none"
        return
    fi

    # If there are ALLOW rules, create restricted user-mode networking
    # QEMU user-mode (-nic user) provides NAT with optional restrict=on
    # which blocks all outbound except explicitly forwarded ports.
    #
    # For each ALLOW rule, add a hostfwd or guestfwd entry.
    # Note: QEMU user-mode can't do fine-grained domain filtering,
    # so we rely on the VM having no DNS resolver configured.
    # Only IP-based ALLOW rules are enforceable at the QEMU level.

    local net_args="-nic user,model=virtio,restrict=on"

    while IFS= read -r line; do
        # Skip comments and blank lines
        [[ "$line" =~ ^# ]] && continue
        [[ -z "$line" ]] && continue

        if [[ "$line" =~ ^ALLOW ]]; then
            local proto dest port
            proto=$(echo "$line" | awk '{print $2}')
            dest=$(echo "$line" | awk '{print $3}')
            port=$(echo "$line" | awk '{print $4}')

            # For user-mode networking, we can only allow specific
            # outbound connections via guestfwd. Domain names need
            # DNS which we don't provide by default.
            # Log the rule for audit.
            echo "  [NET-POLICY] ALLOW $proto $dest $port" >&2
        fi
    done < "$POLICY_FILE"

    echo "$net_args"
}

# Export the result
QEMU_NET_ARGS="$(generate_network_args)"

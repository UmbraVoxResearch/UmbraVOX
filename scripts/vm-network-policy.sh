#!/usr/bin/env bash
# ── UmbraVOX VM Network Policy Enforcer ──────────────────────────────
# Reads vm-network-policy.conf and generates QEMU network arguments.
# Called by vm-dev-run.sh before launching QEMU.
#
# Default: no network (QEMU launched with -nic none).
# ALLOW rules are currently fail-closed until enforceable translation is
# implemented.
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
    allow_count=$(grep -c '^ALLOW ' "$POLICY_FILE" 2>/dev/null || true)
    allow_count=${allow_count:-0}

    if [ "$allow_count" -eq 0 ]; then
        # No ALLOW rules = no network
        echo "-nic none"
        return
    fi

    # ALLOW rules are currently declarative only. Until each rule is translated
    # into enforceable QEMU arguments, fail closed instead of silently allowing
    # broader connectivity than requested.
    echo "[NET-POLICY] ERROR: ALLOW rules are present but enforcement is not implemented." >&2
    echo "[NET-POLICY] ERROR: Remove ALLOW rules or implement translation before booting the VM." >&2

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

    return 1
}

# Export the result
if ! QEMU_NET_ARGS="$(generate_network_args)"; then
    return 1 2>/dev/null || exit 1
fi

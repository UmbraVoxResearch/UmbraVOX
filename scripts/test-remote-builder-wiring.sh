#!/usr/bin/env bash
# ── UmbraVOX Remote Builder Wiring Tests ─────────────────────────────
# Verifies make target wiring uses remote builder flags + fail-closed messaging.
#
# Usage: bash scripts/test-remote-builder-wiring.sh
set -uo pipefail

PASS=0
FAIL=0
SKIP=0

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

check() {
    local label="$1"
    local result="$2"
    if [ "$result" = "PASS" ]; then
        echo -e "  ${GREEN}PASS${NC}: $label"
        ((PASS++))
    elif [ "$result" = "SKIP" ]; then
        echo -e "  ${YELLOW}SKIP${NC}: $label"
        ((SKIP++))
    else
        echo -e "  ${RED}FAIL${NC}: $label"
        ((FAIL++))
    fi
}

require_pattern() {
    local label="$1"
    local text="$2"
    local pattern="$3"
    if printf '%s\n' "$text" | grep -Fq -- "$pattern"; then
        check "$label" "PASS"
    else
        check "$label" "FAIL"
    fi
}

echo "=== Remote Builder Wiring Tests ==="
echo ""

if [ ! -f Makefile ]; then
    check "Makefile exists" "FAIL"
    exit 1
fi

vm_image_dryrun="$(make -n vm-image-build 2>&1 || true)"
vm_signal_dryrun="$(make -n vm-signal-server-build 2>&1 || true)"
isolation_dryrun="$(make -n check-isolation 2>&1 || true)"

require_pattern "vm-image-build loads remote config script" \
    "$vm_image_dryrun" \
    "scripts/nix-remote-builder-config.sh"
require_pattern "vm-image-build passes --builders" \
    "$vm_image_dryrun" \
    "--builders"
require_pattern "vm-image-build passes builders-use-substitutes option" \
    "$vm_image_dryrun" \
    "builders-use-substitutes"
require_pattern "vm-image-build prints fail-closed message" \
    "$vm_image_dryrun" \
    "Fail-closed policy is enabled: no local fallback will be attempted."

require_pattern "vm-signal-server-build loads remote config script" \
    "$vm_signal_dryrun" \
    "scripts/nix-remote-builder-config.sh"
require_pattern "vm-signal-server-build passes --builders" \
    "$vm_signal_dryrun" \
    "--builders"
require_pattern "vm-signal-server-build prints fail-closed message" \
    "$vm_signal_dryrun" \
    "Remote nix-build failed (fail-closed; no local fallback)."

require_pattern "check-isolation includes remote builder check section" \
    "$isolation_dryrun" \
    "Remote builder check:"

echo ""
echo "Results: $PASS passed, $FAIL failed, $SKIP skipped"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi

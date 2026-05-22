#!/usr/bin/env bash
# ── UmbraVOX Make Target Coverage Test ───────────────────────────────
# Verifies every declared .PHONY target can be dry-run invoked.
#
# Usage: bash scripts/test-make-options.sh
# Or:    make test-make-options
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

echo "=== Make Target Coverage Test ==="
echo ""

if [ ! -f Makefile ]; then
    check "Makefile exists in current directory" "FAIL"
    exit 1
fi

if ! command -v timeout >/dev/null 2>&1; then
    check "timeout command available" "FAIL"
    exit 1
fi

mapfile -t TARGETS < <(
    awk '
        /^\.PHONY:/ {
            for (i = 2; i <= NF; i++) {
                if ($i != "\\") {
                    print $i
                }
            }
        }
    ' Makefile | sort -u
)

if [ "${#TARGETS[@]}" -eq 0 ]; then
    check "Discovered .PHONY targets" "FAIL"
    exit 1
fi

echo "Discovered ${#TARGETS[@]} .PHONY targets"
echo ""

FAIL_DIR="$(mktemp -d)"
trap 'rm -rf "$FAIL_DIR"' EXIT

for target in "${TARGETS[@]}"; do
    failure_log="$FAIL_DIR/$target.log"
    if env UMBRAVOX_LOCAL=1 timeout 20 make -n "$target" >"$failure_log" 2>&1; then
        check "make -n $target" "PASS"
    else
        check "make -n $target (UMBRAVOX_LOCAL=1)" "FAIL"
        echo "    log: $failure_log"
    fi
done

echo ""
echo "Results: $PASS passed, $FAIL failed, $SKIP skipped"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi

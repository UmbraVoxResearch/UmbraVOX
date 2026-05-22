#!/usr/bin/env bash
# ── UmbraVOX VM-local Nix Config Tests ──────────────────────────────
# Verifies config loader behavior:
# - file defaults
# - env overrides
# - env-only mode
# - fail-closed enforcement
#
# Usage: bash scripts/test-vm-build-config.sh
set -uo pipefail

PASS=0
FAIL=0
SKIP=0

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

SCRIPT_PATH="./scripts/nix-vm-build-config.sh"

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

run_case_success() {
    local label="$1"
    local cmd="$2"
    if eval "$cmd" >/dev/null 2>&1; then
        check "$label" "PASS"
    else
        check "$label" "FAIL"
    fi
}

run_case_failure() {
    local label="$1"
    local cmd="$2"
    if eval "$cmd" >/dev/null 2>&1; then
        check "$label" "FAIL"
    else
        check "$label" "PASS"
    fi
}

echo "=== VM-local Nix Config Tests ==="
echo ""

if [ ! -x "$SCRIPT_PATH" ]; then
    check "$SCRIPT_PATH is executable" "FAIL"
    exit 1
fi
check "$SCRIPT_PATH is executable" "PASS"

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

CFG_FILE="$TMP_DIR/vm-build.env"
cat > "$CFG_FILE" <<'EOF'
UMBRAVOX_NIX_BUILD_DIR=build/vm/tmp-file
UMBRAVOX_NIX_SANDBOX_BUILD_DIR=/build
UMBRAVOX_NIX_LOCAL_ONLY=1
UMBRAVOX_NIX_REQUIRE_CONFIG=1
EOF

# 1) File-only config succeeds.
run_case_success \
    "file-only config resolves" \
    "env -i PATH=\"$PATH\" UMBRAVOX_NIX_CONFIG_FILE=\"$CFG_FILE\" \"$SCRIPT_PATH\" shell | grep -q 'UMBRAVOX_NIX_CONFIG_SOURCE=.*file'"
run_case_success \
    "file-only values exported" \
    "env -i PATH=\"$PATH\" UMBRAVOX_NIX_CONFIG_FILE=\"$CFG_FILE\" \"$SCRIPT_PATH\" shell | grep -q 'UMBRAVOX_NIX_BUILD_DIR=.*tmp-file'"

# 2) Env overrides file values.
run_case_success \
    "env overrides file values" \
    "env -i PATH=\"$PATH\" UMBRAVOX_NIX_CONFIG_FILE=\"$CFG_FILE\" UMBRAVOX_NIX_BUILD_DIR='build/vm/tmp-env' \"$SCRIPT_PATH\" shell | grep -q 'UMBRAVOX_NIX_BUILD_DIR=.*tmp-env'"
run_case_success \
    "file+env source marker" \
    "env -i PATH=\"$PATH\" UMBRAVOX_NIX_CONFIG_FILE=\"$CFG_FILE\" UMBRAVOX_NIX_BUILD_DIR='build/vm/tmp-env' \"$SCRIPT_PATH\" shell | grep -q 'UMBRAVOX_NIX_CONFIG_SOURCE=.*file+env'"

# 3) Env-only mode works when config file is absent.
run_case_success \
    "env-only mode works without config file" \
    "env -i PATH=\"$PATH\" UMBRAVOX_NIX_CONFIG_FILE=\"$TMP_DIR/does-not-exist.env\" UMBRAVOX_NIX_REQUIRE_CONFIG=0 UMBRAVOX_NIX_BUILD_DIR='build/vm/tmp-env-only' UMBRAVOX_NIX_SANDBOX_BUILD_DIR='/build' UMBRAVOX_NIX_LOCAL_ONLY=1 \"$SCRIPT_PATH\" shell | grep -q 'UMBRAVOX_NIX_CONFIG_SOURCE=.*env-only'"

# 4) Missing config file fails when required.
run_case_failure \
    "required config file missing fails" \
    "env -i PATH=\"$PATH\" UMBRAVOX_NIX_CONFIG_FILE=\"$TMP_DIR/does-not-exist.env\" UMBRAVOX_NIX_REQUIRE_CONFIG=1 \"$SCRIPT_PATH\" shell"

# 5) Local-only guard is enforced.
run_case_failure \
    "local-only cannot be disabled" \
    "env -i PATH=\"$PATH\" UMBRAVOX_NIX_CONFIG_FILE=\"$CFG_FILE\" UMBRAVOX_NIX_LOCAL_ONLY=0 \"$SCRIPT_PATH\" shell"

# 6) Remote-builder env is rejected.
run_case_failure \
    "remote-builder env is rejected" \
    "env -i PATH=\"$PATH\" UMBRAVOX_NIX_CONFIG_FILE=\"$CFG_FILE\" UMBRAVOX_NIX_BUILDER='ssh-ng://builder x86_64-linux - 4' \"$SCRIPT_PATH\" shell"

# 7) Empty build dir is rejected.
run_case_failure \
    "empty build dir fails" \
    "env -i PATH=\"$PATH\" UMBRAVOX_NIX_CONFIG_FILE=\"$CFG_FILE\" UMBRAVOX_NIX_BUILD_DIR='' \"$SCRIPT_PATH\" shell"

echo ""
echo "Results: $PASS passed, $FAIL failed, $SKIP skipped"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi

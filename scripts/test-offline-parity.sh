#!/usr/bin/env bash
# Verify that core-crypto suite behavior is consistent between online and
# offline runner paths.
set -euo pipefail

TEST_ARTIFACT_DIR="${1:-build/test-artifacts}"
mkdir -p "$TEST_ARTIFACT_DIR"

echo -e "\033[0;34m[PARITY]\033[0m Running core-crypto in normal (online) mode..."
cabal test umbravox-test --test-options='core-crypto' > "$TEST_ARTIFACT_DIR/parity-online.log" 2>&1
online_exit=$?

echo -e "\033[0;34m[PARITY]\033[0m Running core-crypto in offline mode..."
UMBRAVOX_OFFLINE=1 ./uv test core-crypto > "$TEST_ARTIFACT_DIR/parity-offline.log" 2>&1
offline_exit=$?

online_pass="$(grep -c 'PASS:' "$TEST_ARTIFACT_DIR/parity-online.log" 2>/dev/null || echo 0)"
offline_pass="$(grep -c 'PASS:' "$TEST_ARTIFACT_DIR/parity-offline.log" 2>/dev/null || echo 0)"

if [ "$online_exit" != "$offline_exit" ]; then
    echo -e "\033[0;31m[PARITY]\033[0m Exit code mismatch: online=$online_exit offline=$offline_exit"
    exit 1
fi

if [ "$online_pass" != "$offline_pass" ]; then
    echo -e "\033[0;31m[PARITY]\033[0m Pass count mismatch: online=$online_pass offline=$offline_pass"
    exit 1
fi

echo -e "\033[0;32m[PARITY]\033[0m Both modes: exit=$online_exit, $online_pass assertions passed."

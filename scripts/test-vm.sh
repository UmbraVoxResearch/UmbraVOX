#!/usr/bin/env bash
# ── UmbraVOX VM Environment Tests ────────────────────────────────────
# Verifies that the NixOS development VM boots, has the required tools,
# and can run basic commands.
#
# Usage: bash scripts/test-vm.sh
# Requires: nix-shell shell-minimal.nix (for QEMU and genext2fs)
#           build/vm/image (run './uv vm build-image' first)
set -Eeuo pipefail

PASS=0; FAIL=0; SKIP=0
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; NC='\033[0m'

check() {
    local label="$1" result="$2"
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

echo "=== VM Environment Tests ==="
echo ""

# ── Preflight ─────────────────────────────────────────────────────────
echo "[1/4] Preflight checks..."

if [ -e /dev/kvm ]; then
    check "KVM available" "PASS"
else
    check "KVM available" "FAIL"
    echo "  Cannot run VM tests without KVM. Exiting."
    exit 1
fi

if command -v qemu-system-x86_64 >/dev/null 2>&1; then
    check "qemu-system-x86_64 on PATH" "PASS"
else
    check "qemu-system-x86_64 on PATH" "FAIL"
    echo "  Run inside nix-shell shell-minimal.nix"
    exit 1
fi

if command -v genext2fs >/dev/null 2>&1; then
    check "genext2fs on PATH" "PASS"
else
    check "genext2fs on PATH" "FAIL"
    echo "  Run inside nix-shell shell-minimal.nix"
    exit 1
fi

if [ -L build/vm/image ] && [ -e build/vm/image ]; then
    check "VM image cached" "PASS"
else
    check "VM image cached (run ./uv vm build-image)" "FAIL"
    exit 1
fi

echo ""

# ── Test 2: VM boot and tool availability ─────────────────────────────
echo "[2/4] Testing VM boot and toolchain..."

# Create a minimal test script that checks tools and exits
TEST_SCRIPT=$(mktemp /tmp/umbravox-vm-test.XXXXXX.sh)
cat > "$TEST_SCRIPT" << 'EOF'
#!/usr/bin/env bash
echo "VM_BOOT=OK"
echo "KERNEL=$(uname -r)"

for tool in ghc cabal fstar.exe z3 coqc gcc make git tmux; do
    if command -v $tool >/dev/null 2>&1; then
        echo "TOOL_${tool}=OK"
    else
        echo "TOOL_${tool}=MISSING"
    fi
done

echo "GHC_VERSION=$(ghc --numeric-version 2>/dev/null || echo N/A)"
echo "CABAL_VERSION=$(cabal --numeric-version 2>/dev/null || echo N/A)"

echo "VM_TEST_COMPLETE=YES"
EOF

# Run via vm-dev-run.sh in exec mode with a short timeout
RESULT=$(timeout 120 bash scripts/vm-dev-run.sh exec "bash -c '$(cat $TEST_SCRIPT)'" 2>&1 || true)
rm -f "$TEST_SCRIPT"

if echo "$RESULT" | grep -q "VM_BOOT=OK"; then
    check "VM boots successfully" "PASS"
else
    check "VM boots successfully" "FAIL"
fi

for tool in ghc cabal fstar.exe z3 gcc make git tmux; do
    if echo "$RESULT" | grep -q "TOOL_${tool}=OK"; then
        check "VM provides $tool" "PASS"
    elif echo "$RESULT" | grep -q "TOOL_${tool}=MISSING"; then
        check "VM provides $tool" "FAIL"
    else
        check "VM provides $tool (no response)" "SKIP"
    fi
done

if echo "$RESULT" | grep -q "VM_TEST_COMPLETE=YES"; then
    check "VM test script completed" "PASS"
else
    check "VM test script completed (may have timed out)" "FAIL"
fi

echo ""

# ── Test 3: VM can build the project ──────────────────────────────────
echo "[3/4] Testing VM can parse cabal file..."

RESULT2=$(timeout 120 bash scripts/vm-dev-run.sh exec "cd /work/umbravox && cabal check 2>&1; echo CABAL_CHECK=\$?" 2>&1 || true)

if echo "$RESULT2" | grep -q "CABAL_CHECK=0"; then
    check "VM cabal check passes" "PASS"
elif echo "$RESULT2" | grep -q "CABAL_CHECK="; then
    check "VM cabal check" "FAIL"
else
    check "VM cabal check (no response)" "SKIP"
fi

echo ""

# ── Test 4: VM source mount ───────────────────────────────────────────
echo "[4/4] Testing VM source mounting..."

RESULT3=$(timeout 120 bash scripts/vm-dev-run.sh exec "ls /work/umbravox/UmbraVox.cabal && echo SOURCE_MOUNT=OK || echo SOURCE_MOUNT=FAIL" 2>&1 || true)

if echo "$RESULT3" | grep -q "SOURCE_MOUNT=OK"; then
    check "VM source mounted at /work/umbravox" "PASS"
else
    check "VM source mounted at /work/umbravox" "FAIL"
fi

echo ""
echo "Results: $PASS passed, $FAIL failed, $SKIP skipped"
if [ "$FAIL" -gt 0 ]; then exit 1; fi

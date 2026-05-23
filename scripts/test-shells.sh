#!/usr/bin/env bash
# ── UmbraVOX Shell Environment Tests ─────────────────────────────────
# Verifies that each nix-shell environment provides the expected tools
# and displays the correct banner identity.
#
# Usage: bash scripts/test-shells.sh
# Requires: nix-shell on PATH
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

echo "=== Shell Environment Tests ==="
echo ""

# ── Test 1: shell.nix (full local) ───────────────────────────────────
echo "[1/3] Testing shell.nix (full local shell)..."

if command -v nix-shell >/dev/null 2>&1; then
    # Check banner identity
    banner=$(nix-shell shell.nix --run "echo SHELL_OK" 2>&1)
    if echo "$banner" | grep -q "FULL LOCAL SHELL"; then
        check "shell.nix banner shows [ FULL LOCAL SHELL ]" "PASS"
    else
        check "shell.nix banner shows [ FULL LOCAL SHELL ]" "FAIL"
    fi

    # Check required tools
    for tool in ghc cabal fstar.exe z3 coqc gcc make git; do
        if nix-shell shell.nix --run "command -v $tool" >/dev/null 2>&1; then
            check "shell.nix provides $tool" "PASS"
        else
            check "shell.nix provides $tool" "FAIL"
        fi
    done
else
    check "nix-shell available" "SKIP"
fi

echo ""

# ── Test 2: shell-minimal.nix (VM orchestration) ─────────────────────
echo "[2/3] Testing shell-minimal.nix (VM-first shell)..."

if command -v nix-shell >/dev/null 2>&1; then
    banner=$(nix-shell shell-minimal.nix --run "echo SHELL_OK" 2>&1)
    if echo "$banner" | grep -q "VM-FIRST SHELL"; then
        check "shell-minimal.nix banner shows [ VM-FIRST SHELL ]" "PASS"
    else
        check "shell-minimal.nix banner shows [ VM-FIRST SHELL ]" "FAIL"
    fi

    # Check orchestration tools (should be present)
    for tool in qemu-system-x86_64 genext2fs git make; do
        if nix-shell shell-minimal.nix --run "command -v $tool" >/dev/null 2>&1; then
            check "shell-minimal.nix provides $tool" "PASS"
        else
            check "shell-minimal.nix provides $tool" "FAIL"
        fi
    done

    # Check dev tools (should NOT be present)
    for tool in ghc cabal fstar.exe; do
        if nix-shell shell-minimal.nix --run "command -v $tool" >/dev/null 2>&1; then
            check "shell-minimal.nix does NOT provide $tool (should be VM-only)" "FAIL"
        else
            check "shell-minimal.nix does NOT provide $tool (VM-only)" "PASS"
        fi
    done
else
    check "nix-shell available" "SKIP"
fi

echo ""

# ── Test 3: VM image ─────────────────────────────────────────────────
echo "[3/3] Testing VM image availability..."

if [ -L build/vm/image ] && [ -e build/vm/image ]; then
    check "VM image cached at build/vm/image" "PASS"

    if [ -f build/vm/image/nixos.img ]; then
        check "VM image contains nixos.img" "PASS"
        size=$(stat -c%s build/vm/image/nixos.img 2>/dev/null || echo 0)
        if [ "$size" -gt 1000000000 ]; then
            check "VM image size > 1GB ($(( size / 1048576 ))MB)" "PASS"
        else
            check "VM image size > 1GB ($(( size / 1048576 ))MB — may be too small)" "FAIL"
        fi
    else
        check "VM image contains nixos.img" "FAIL"
    fi

    if [ -e /dev/kvm ]; then
        check "KVM available (/dev/kvm exists)" "PASS"
    else
        check "KVM available" "FAIL"
    fi
else
    check "VM image cached (run './uv vm build-image' first)" "SKIP"
fi

echo ""
echo "Results: $PASS passed, $FAIL failed, $SKIP skipped"
if [ "$FAIL" -gt 0 ]; then exit 1; fi

#!/usr/bin/env bash
# ── UmbraVOX Infrastructure Regression Test Suite ────────────────────
# Comprehensive tests for build system, VM pipeline, shell environments,
# ./uv commands, and evidence harness. Run after any infrastructure
# change to catch regressions.
#
# Usage: bash scripts/test-infrastructure.sh
# Or:    ./uv test-infra
#
# Requires: nix-shell (or shell-minimal.nix for VM tests)
set -uo pipefail

PASS=0; FAIL=0; SKIP=0; TOTAL=0
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; BLUE='\033[0;34m'; NC='\033[0m'

check() {
    local label="$1" result="$2"
    ((TOTAL++))
    if [ "$result" = "PASS" ]; then
        echo -e "  ${GREEN}PASS${NC}: $label"
        ((PASS++))
    elif [ "$result" = "SKIP" ]; then
        echo -e "  ${YELLOW}SKIP${NC}: $label"
        ((SKIP++))
    else
        echo -e "  ${RED}FAIL${NC}: $label — $result"
        ((FAIL++))
    fi
}

echo -e "${BLUE}=== UmbraVOX Infrastructure Regression Suite ===${NC}"
echo ""

# ── Section 1: Build system structure ────────────────────────────────
echo -e "${BLUE}[1/8] Build system structure tests${NC}"

# Check the uv bootstrap wrapper exists and is executable
if [ -f ./uv ]; then
    check "./uv bootstrap wrapper exists" "PASS"
    if [ -x ./uv ]; then
        check "./uv is executable" "PASS"
    else
        check "./uv is executable" "FAIL"
    fi
else
    check "./uv bootstrap wrapper exists" "FAIL"
fi

# Check the Go binary builds successfully
if ./uv help >/dev/null 2>&1; then
    check "./uv help runs without error" "PASS"
else
    check "./uv help runs without error" "FAIL"
fi

# Check critical commands are listed in ./uv help output
help_output=$(./uv help 2>&1 || true)
for cmd in build test verify dev check coverage release vm evidence fuzz clean exec help; do
    if echo "$help_output" | grep -qiw "$cmd"; then
        check "./uv help lists '$cmd' command" "PASS"
    else
        check "./uv help lists '$cmd' command" "FAIL"
    fi
done

# Check ./uv test --list shows expected suites
if ./uv test --list >/dev/null 2>&1; then
    check "./uv test --list runs without error" "PASS"
else
    check "./uv test --list runs without error" "FAIL"
fi

# Check ./uv vm info runs
if ./uv vm info >/dev/null 2>&1; then
    check "./uv vm info runs without error" "PASS"
else
    check "./uv vm info runs without error" "FAIL"
fi

if bash scripts/test-vm-build-config.sh >/dev/null 2>&1; then
    check "scripts/test-vm-build-config.sh passes" "PASS"
else
    check "scripts/test-vm-build-config.sh passes" "FAIL"
fi

if bash scripts/test-vm-bootstrap.sh >/dev/null 2>&1; then
    check "scripts/test-vm-bootstrap.sh passes" "PASS"
else
    check "scripts/test-vm-bootstrap.sh passes" "FAIL"
fi

echo ""

# ── Section 2: Shell environments ─────────────────────────────────────
echo -e "${BLUE}[2/8] Shell environment tests${NC}"

if command -v nix-shell >/dev/null 2>&1; then
    # shell.nix banner
    banner=$(nix-shell shell.nix --run "echo OK" 2>&1)
    if echo "$banner" | grep -q "FULL LOCAL SHELL"; then
        check "shell.nix shows [ FULL LOCAL SHELL ] banner" "PASS"
    else
        check "shell.nix shows [ FULL LOCAL SHELL ] banner" "FAIL"
    fi

    # shell-minimal.nix banner
    banner=$(nix-shell shell-minimal.nix --run "echo OK" 2>&1)
    if echo "$banner" | grep -q "VM-FIRST SHELL"; then
        check "shell-minimal.nix shows [ VM-FIRST SHELL ] banner" "PASS"
    else
        check "shell-minimal.nix shows [ VM-FIRST SHELL ] banner" "FAIL"
    fi

    # shell-minimal.nix has QEMU but not GHC
    if nix-shell shell-minimal.nix --run "command -v qemu-system-x86_64" >/dev/null 2>&1; then
        check "shell-minimal.nix provides qemu-system-x86_64" "PASS"
    else
        check "shell-minimal.nix provides qemu-system-x86_64" "FAIL"
    fi
    if nix-shell --pure shell-minimal.nix --run "command -v ghc" >/dev/null 2>&1; then
        check "shell-minimal.nix does NOT provide ghc" "FAIL (ghc found)"
    else
        check "shell-minimal.nix does NOT provide ghc" "PASS"
    fi
else
    check "nix-shell available" "SKIP"
fi

echo ""

# ── Section 3: VM image ───────────────────────────────────────────────
echo -e "${BLUE}[3/8] VM image tests${NC}"

if [ -L build/vm/image ] && [ -e build/vm/image ]; then
    check "VM image symlink exists" "PASS"
    if [ -f build/vm/image/nixos.img ]; then
        check "VM image contains nixos.img" "PASS"
        # Check it's bootable (has MBR)
        header=$(hexdump -C build/vm/image/nixos.img -n 2 | head -1)
        if echo "$header" | grep -q 'eb 63'; then
            check "VM image has GRUB boot sector" "PASS"
        else
            check "VM image has GRUB boot sector" "FAIL (header: $header)"
        fi
    else
        check "VM image contains nixos.img" "FAIL"
    fi
else
    check "VM image cached (run ./uv vm image-build)" "SKIP"
fi

if [ -e /dev/kvm ]; then
    check "KVM available" "PASS"
else
    check "KVM available" "SKIP"
fi

# Network policy is handled by the Go tool
if [ -f tools/pkg/netpol/policy.go ]; then
    check "tools/pkg/netpol/policy.go exists (network policy)" "PASS"
else
    check "tools/pkg/netpol/policy.go exists (network policy)" "FAIL"
fi

echo ""

# ── Section 4: Network policy ────────────────────────────────────────
echo -e "${BLUE}[4/8] Network policy tests${NC}"

if [ -f vm-network-policy.conf ]; then
    check "vm-network-policy.conf exists" "PASS"
else
    check "vm-network-policy.conf exists" "FAIL"
fi

# Default policy must deny all — no uncommented ALLOW rules
if [ -f vm-network-policy.conf ]; then
    allow_count=$(grep -cE '^\s*ALLOW' vm-network-policy.conf 2>/dev/null || true)
    if [ "${allow_count:-0}" -eq 0 ]; then
        check "Default policy has no uncommented ALLOW rules (deny-all)" "PASS"
    else
        check "Default policy has no uncommented ALLOW rules (deny-all)" "FAIL ($allow_count ALLOW rules found)"
    fi
else
    check "Default policy has no uncommented ALLOW rules" "SKIP"
fi

# Policy file must not be world-writable (host-only control)
if [ -f vm-network-policy.conf ]; then
    perms=$(stat -c '%a' vm-network-policy.conf 2>/dev/null || stat -f '%Lp' vm-network-policy.conf 2>/dev/null)
    if echo "$perms" | grep -qE '[2367]$'; then
        check "vm-network-policy.conf is not world-writable" "FAIL (perms: $perms)"
    else
        check "vm-network-policy.conf is not world-writable" "PASS"
    fi
else
    check "vm-network-policy.conf is not world-writable" "SKIP"
fi

echo ""

# ── Section 5: F* proof invariants ────────────────────────────────────
echo -e "${BLUE}[5/8] F* proof invariants${NC}"

admit_count=$(grep -RIn '\badmit\b\|admit()' test/evidence/formal-proofs/fstar --include='*.fst' | grep -v '\*)\|(\*\|//' | grep -v 'admit_smt' | wc -l)
if [ "$admit_count" -eq 0 ]; then
    check "F* specs: 0 admit() calls" "PASS"
else
    check "F* specs: 0 admit() calls" "FAIL ($admit_count found)"
fi

assume_count=$(grep -RIn '^assume val' test/evidence/formal-proofs/fstar | wc -l)
check "F* specs: $assume_count assume val declarations" "PASS"

if [ -f test/evidence/formal-proofs/ASSUMPTIONS.md ]; then
    check "Assumption ledger exists" "PASS"
else
    check "Assumption ledger exists" "FAIL"
fi

echo ""

# ── Section 6: Evidence harness ───────────────────────────────────────
echo -e "${BLUE}[6/8] Evidence harness tests${NC}"

if [ -x test/evidence/formal-proofs/check-external-evidence.sh ]; then
    check "check-external-evidence.sh is executable" "PASS"
else
    check "check-external-evidence.sh is executable" "FAIL"
fi

if [ -f test/evidence/formal-proofs/coq/Makefile ]; then
    check "Coq project Makefile exists" "PASS"
else
    check "Coq project Makefile exists" "FAIL"
fi

if [ -f test/evidence/formal-proofs/coq/_CoqProject ]; then
    check "Coq _CoqProject exists" "PASS"
else
    check "Coq _CoqProject exists" "FAIL"
fi

if [ -f scripts/primality-certificate.hs ]; then
    check "Primality certificate script exists" "PASS"
else
    check "Primality certificate script exists" "FAIL"
fi

# Inventory logs
if [ -f test/evidence/formal-proofs/logs/assume-val-inventory.txt ]; then
    inv_count=$(wc -l < test/evidence/formal-proofs/logs/assume-val-inventory.txt)
    live_count=$(grep -RIn '^assume val' test/evidence/formal-proofs/fstar/ | wc -l)
    if [ "$inv_count" -eq "$live_count" ]; then
        check "Assume-val inventory matches live count ($live_count)" "PASS"
    else
        check "Assume-val inventory matches live count" "FAIL (log=$inv_count live=$live_count)"
    fi
else
    check "Assume-val inventory log exists" "FAIL"
fi

if [ -f test/evidence/formal-proofs/logs/verification-summary.txt ]; then
    check "Verification summary exists" "PASS"
else
    check "Verification summary exists" "FAIL"
fi

echo ""

# ── Section 7: Scripts ────────────────────────────────────────────────
echo -e "${BLUE}[7/8] Script tests${NC}"

for script in scripts/vm-tui-scenario.sh scripts/vm-screenshot-capture.sh \
              scripts/vm-record-session.sh scripts/vm-visual-regression.sh \
              scripts/vm-socks5-test.sh \
              scripts/nix-vm-build-config.sh \
              scripts/test-vm-build-config.sh \
              scripts/test-shells.sh scripts/test-vm.sh \
              scripts/test-vm-bootstrap.sh \
              scripts/test-infrastructure.sh; do
    if [ -f "$script" ]; then
        if [ -x "$script" ]; then
            check "$script exists and is executable" "PASS"
        else
            check "$script exists but NOT executable" "FAIL"
        fi
    else
        check "$script exists" "SKIP"
    fi
done

# Check ./uv bootstrap wrapper and Go tool sources
for src in ./uv \
           tools/cmd/umbravox-vm/main.go \
           tools/pkg/repo/repo.go \
           tools/pkg/log/log.go \
           tools/pkg/download/download.go \
           tools/pkg/disk/source.go; do
    if [ -f "$src" ]; then
        check "$src exists" "PASS"
    else
        check "$src exists" "FAIL"
    fi
done

echo ""

# ── Section 8: Documentation ──────────────────────────────────────────
echo -e "${BLUE}[8/8] Documentation completeness${NC}"

for doc in README.md doc/ARCHITECTURE.md doc/RELEASES.md doc/CRYPTO-SAFETY.md \
           doc/DO-178C-COVERAGE.md doc/VM-DEVELOPMENT.md doc/runner-classes.md \
           doc/FSTAR-AXIOM-REGISTRY.md doc/QUICKSTART.md \
           test/evidence/formal-proofs/ASSUMPTIONS.md; do
    if [ -f "$doc" ]; then
        check "$doc exists" "PASS"
    else
        check "$doc exists" "FAIL"
    fi
done

# Check VM-first is documented
if grep -q "VM-First\|VM-first\|vm-first" doc/VM-DEVELOPMENT.md 2>/dev/null; then
    check "VM-first development documented" "PASS"
else
    check "VM-first development documented" "FAIL"
fi

echo ""
echo -e "${BLUE}=== Results ===${NC}"
echo -e "  Total: $TOTAL  ${GREEN}Pass: $PASS${NC}  ${RED}Fail: $FAIL${NC}  ${YELLOW}Skip: $SKIP${NC}"
echo ""
if [ "$FAIL" -gt 0 ]; then
    echo -e "${RED}INFRASTRUCTURE REGRESSION DETECTED${NC}"
    exit 1
else
    echo -e "${GREEN}All infrastructure tests passed.${NC}"
fi

#!/usr/bin/env bash
# ── UmbraVOX Infrastructure Regression Test Suite ────────────────────
# Comprehensive tests for build system, VM pipeline, shell environments,
# Makefile targets, and evidence harness. Run after any infrastructure
# change to catch regressions.
#
# Usage: bash scripts/test-infrastructure.sh
# Or:    make test-infra
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

# ── Section 1: Makefile structure ─────────────────────────────────────
echo -e "${BLUE}[1/7] Makefile structure tests${NC}"

# Check critical targets exist
for target in build test verify quality vm-dev vm-build vm-test vm-verify \
              vm-image-build vm-image-clean vm-cache-clean vm-extract \
              check-evidence test-shells test-vm help clean; do
    if make -n "$target" >/dev/null 2>&1; then
        check "Makefile target '$target' exists" "PASS"
    else
        check "Makefile target '$target' exists" "FAIL"
    fi
done

# Check VM routing is default (UMBRAVOX_LOCAL=0)
out=$(make -n build 2>&1 | head -1)
if echo "$out" | grep -q 'vm-build'; then
    check "make build routes to VM by default" "PASS"
else
    check "make build routes to VM by default" "FAIL ($out)"
fi

# Check local override works
out=$(UMBRAVOX_LOCAL=1 make -n build 2>&1 | head -3)
if echo "$out" | grep -q 'cabal build'; then
    check "UMBRAVOX_LOCAL=1 make build runs cabal locally" "PASS"
else
    check "UMBRAVOX_LOCAL=1 make build runs cabal locally" "FAIL"
fi

echo ""

# ── Section 2: Shell environments ─────────────────────────────────────
echo -e "${BLUE}[2/7] Shell environment tests${NC}"

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
    if nix-shell shell-minimal.nix --run "command -v ghc" >/dev/null 2>&1; then
        check "shell-minimal.nix does NOT provide ghc" "FAIL (ghc found)"
    else
        check "shell-minimal.nix does NOT provide ghc" "PASS"
    fi
else
    check "nix-shell available" "SKIP"
fi

echo ""

# ── Section 3: VM image ───────────────────────────────────────────────
echo -e "${BLUE}[3/7] VM image tests${NC}"

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
    check "VM image cached (run make vm-image-build)" "SKIP"
fi

if [ -e /dev/kvm ]; then
    check "KVM available" "PASS"
else
    check "KVM available" "FAIL"
fi

echo ""

# ── Section 4: F* proof invariants ────────────────────────────────────
echo -e "${BLUE}[4/7] F* proof invariants${NC}"

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

# ── Section 5: Evidence harness ───────────────────────────────────────
echo -e "${BLUE}[5/7] Evidence harness tests${NC}"

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

echo ""

# ── Section 6: Scripts ────────────────────────────────────────────────
echo -e "${BLUE}[6/7] Script tests${NC}"

for script in scripts/vm-dev-run.sh scripts/vm-smoke-run.sh \
              scripts/vm-tui-scenario.sh scripts/vm-screenshot-capture.sh \
              scripts/vm-record-session.sh scripts/vm-visual-regression.sh \
              scripts/vm-socks5-test.sh scripts/vm-build-test.sh \
              scripts/test-shells.sh scripts/test-vm.sh \
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

echo ""

# ── Section 7: Documentation ──────────────────────────────────────────
echo -e "${BLUE}[7/7] Documentation completeness${NC}"

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

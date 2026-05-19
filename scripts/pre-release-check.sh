#!/usr/bin/env bash
# ── UmbraVOX Pre-Release Check ──────────────────────────────────────
# Run before tagging a release. Verifies all assurance gates pass.
#
# Usage: bash scripts/pre-release-check.sh
set -uo pipefail

RED='\033[0;31m'; GREEN='\033[0;32m'; BLUE='\033[0;34m'; NC='\033[0m'
PASS=0; FAIL=0

run_check() {
    local label="$1"; shift
    echo -e "${BLUE}[CHECK]${NC} $label"
    if "$@" > /dev/null 2>&1; then
        echo -e "  ${GREEN}PASS${NC}"
        ((PASS++))
    else
        echo -e "  ${RED}FAIL${NC}"
        ((FAIL++))
    fi
}

echo -e "${BLUE}=== UmbraVOX Pre-Release Check ===${NC}"
echo ""

# 1. F* admit check
echo -e "${BLUE}[1/8]${NC} F* admit check"
admit_count=$(grep -RIn '\badmit\b\|admit()' test/evidence/formal-proofs/fstar --include='*.fst' | grep -v '\*)\|(\*\|//' | grep -v 'admit_smt' | wc -l)
if [ "$admit_count" -eq 0 ]; then
    echo -e "  ${GREEN}PASS${NC} (0 admit)"
    ((PASS++))
else
    echo -e "  ${RED}FAIL${NC} ($admit_count admit found)"
    ((FAIL++))
fi

# 2. assume val inventory
echo -e "${BLUE}[2/8]${NC} Assume val inventory"
assume_count=$(grep -RIn '^assume val' test/evidence/formal-proofs/fstar/ | wc -l)
echo -e "  ${GREEN}INFO${NC} $assume_count assume val declarations"
grep -RIn '^assume val' test/evidence/formal-proofs/fstar/ | sort > test/evidence/formal-proofs/logs/assume-val-inventory.txt
((PASS++))

# 3. Assumption ledger consistency
run_check "Assumption ledger" bash test/evidence/formal-proofs/check-assumption-ledger.sh

# 4. Proof hygiene
run_check "Proof hygiene" bash test/evidence/formal-proofs/check-proof-hygiene.sh

# 5. Coq build
echo -e "${BLUE}[5/8]${NC} Coq build"
if command -v coqc > /dev/null 2>&1; then
    if make -C test/evidence/formal-proofs/coq clean > /dev/null 2>&1 && \
       make -C test/evidence/formal-proofs/coq > /dev/null 2>&1; then
        echo -e "  ${GREEN}PASS${NC}"
        ((PASS++))
    else
        echo -e "  ${RED}FAIL${NC}"
        ((FAIL++))
    fi
else
    echo -e "  SKIP (coqc not available — run in nix-shell)"
fi

# 6. Infrastructure tests
echo -e "${BLUE}[6/8]${NC} Infrastructure tests"
if bash scripts/test-infrastructure.sh > /dev/null 2>&1; then
    echo -e "  ${GREEN}PASS${NC}"
    ((PASS++))
else
    echo -e "  ${RED}FAIL${NC}"
    ((FAIL++))
fi

# 7. Differential tests
echo -e "${BLUE}[7/8]${NC} Differential tests"
if cabal test umbravox-test --test-options='differential-oracle' > /dev/null 2>&1; then
    echo -e "  ${GREEN}PASS${NC}"
    ((PASS++))
else
    echo -e "  ${RED}FAIL${NC}"
    ((FAIL++))
fi

# 8. ASSURANCE-MATRIX freshness
echo -e "${BLUE}[8/8]${NC} ASSURANCE-MATRIX freshness"
matrix_assume=$(grep 'assume val total' test/evidence/formal-proofs/ASSURANCE-MATRIX.md | grep -o '[0-9]*' | head -1)
if [ "$matrix_assume" = "$assume_count" ]; then
    echo -e "  ${GREEN}PASS${NC} (matrix=$matrix_assume, live=$assume_count)"
    ((PASS++))
else
    echo -e "  ${RED}FAIL${NC} (matrix=$matrix_assume, live=$assume_count)"
    ((FAIL++))
fi

echo ""
echo -e "${BLUE}=== Results ===${NC}"
echo -e "  Pass: ${GREEN}$PASS${NC}  Fail: ${RED}$FAIL${NC}"
echo ""
if [ "$FAIL" -gt 0 ]; then
    echo -e "${RED}PRE-RELEASE CHECK FAILED${NC}"
    exit 1
else
    echo -e "${GREEN}All pre-release checks passed.${NC}"
fi

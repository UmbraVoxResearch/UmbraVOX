#!/usr/bin/env bash
# ── UmbraVOX CI Overnight Verification Job ──────────────────────────
# Runs all heavy verification that's too slow for per-push CI.
# Designed for nightly/weekly CI runs.
#
# Usage: nix-shell --run "bash scripts/ci-overnight-verify.sh"
# Expected runtime: 30-60 minutes depending on hardware.
set -uo pipefail

RED='\033[0;31m'; GREEN='\033[0;32m'; BLUE='\033[0;34m'; NC='\033[0m'
PASS=0; FAIL=0; SKIP=0

echo -e "${BLUE}=== UmbraVOX CI Overnight Verification ===${NC}"
echo "  Started: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo ""

# 1. Pre-release checks (8 gates)
echo -e "${BLUE}[1/6]${NC} Pre-release checks..."
if bash scripts/pre-release-check.sh > /dev/null 2>&1; then
    echo -e "  ${GREEN}PASS${NC} (8/8 gates)"
    ((PASS++))
else
    echo -e "  ${RED}FAIL${NC}"
    ((FAIL++))
fi

# 2. Full differential test suite
echo -e "${BLUE}[2/6]${NC} Differential testing (34 suites)..."
if cabal test umbravox-test --test-options='differential-oracle' > /dev/null 2>&1; then
    echo -e "  ${GREEN}PASS${NC}"
    ((PASS++))
else
    echo -e "  ${RED}FAIL${NC}"
    ((FAIL++))
fi

# 3. Full Haskell test suite
echo -e "${BLUE}[3/6]${NC} Full Haskell test suite..."
if timeout 1800 cabal test umbravox-test --test-options='required' > /dev/null 2>&1; then
    echo -e "  ${GREEN}PASS${NC}"
    ((PASS++))
else
    echo -e "  ${RED}FAIL${NC} (or timeout)"
    ((FAIL++))
fi

# 4. Coq verification (all files)
echo -e "${BLUE}[4/6]${NC} Coq verification (6 files)..."
if make -C test/evidence/formal-proofs/coq clean > /dev/null 2>&1 && \
   make -C test/evidence/formal-proofs/coq > /dev/null 2>&1; then
    echo -e "  ${GREEN}PASS${NC}"
    ((PASS++))
else
    echo -e "  ${RED}FAIL${NC}"
    ((FAIL++))
fi

# 5. F* verification (optional — requires high z3rlimit, may take 30+ min)
echo -e "${BLUE}[5/6]${NC} F* verification..."
if command -v fstar.exe > /dev/null 2>&1; then
    fstar_bin=$(find dist-newstyle -name fstar-verify -type f -path '*/build/*' 2>/dev/null | head -1)
    if [ -n "$fstar_bin" ] && [ -x "$fstar_bin" ]; then
        if timeout 3600 "$fstar_bin" > /dev/null 2>&1; then
            echo -e "  ${GREEN}PASS${NC}"
            ((PASS++))
        else
            echo -e "  ${RED}FAIL${NC} (or timeout)"
            ((FAIL++))
        fi
    else
        echo -e "  SKIP (fstar-verify binary not built)"
        ((SKIP++))
    fi
else
    echo -e "  SKIP (fstar.exe not available)"
    ((SKIP++))
fi

# 6. Evidence integrity
echo -e "${BLUE}[6/6]${NC} Evidence integrity..."
if bash test/evidence/formal-proofs/check-external-evidence.sh > /dev/null 2>&1; then
    echo -e "  ${GREEN}PASS${NC}"
    ((PASS++))
else
    echo -e "  ${RED}FAIL${NC}"
    ((FAIL++))
fi

echo ""
echo -e "${BLUE}=== Results ===${NC}"
echo -e "  Pass: ${GREEN}$PASS${NC}  Fail: ${RED}$FAIL${NC}  Skip: $SKIP"
echo "  Finished: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo ""

if [ "$FAIL" -gt 0 ]; then
    echo -e "${RED}CI OVERNIGHT VERIFICATION FAILED${NC}"
    exit 1
else
    echo -e "${GREEN}All overnight verification checks passed.${NC}"
fi

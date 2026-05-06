#!/usr/bin/env bash
# ============================================================================
# UmbraVOX F* Formal Verification Runner
#
# Verifies all cryptographic primitive specifications against their
# NIST/RFC standards using the F* proof assistant.
#
# Usage:
#   ./verify.sh              # Verify all modules
#   ./verify.sh Spec.SHA256  # Verify a single module
#
# Prerequisites:
#   - F* (fstar.exe) must be on PATH
#   - z3 (Z3 SMT solver) must be on PATH
#
# Exit codes:
#   0 - All modules verified successfully
#   1 - One or more modules failed verification
# ============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

FSTAR="${FSTAR_EXE:-fstar.exe}"
Z3="${Z3_EXE:-z3}"

# Modules in dependency order
ALL_MODULES=(
    "Spec.SHA256"
    "Spec.SHA512"
    "Spec.HMAC"
    "Spec.HKDF"
    "Spec.AES256"
    "Spec.GaloisField"
    "Spec.GCM"
)

# F* flags
FSTAR_FLAGS=(
    --cache_checked_modules
    --already_cached "Prims,FStar"
    --odir _output
    --cache_dir _cache
)

# --------------------------------------------------------------------------
# Color output
# --------------------------------------------------------------------------

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

log_info()  { echo -e "${BLUE}[INFO]${NC}  $*"; }
log_ok()    { echo -e "${GREEN}[PASS]${NC}  $*"; }
log_fail()  { echo -e "${RED}[FAIL]${NC}  $*"; }
log_warn()  { echo -e "${YELLOW}[WARN]${NC}  $*"; }

check_tool() {
    local tool="$1"
    local name="$2"
    if ! command -v "$tool" &>/dev/null; then
        log_fail "$name not found on PATH (looked for: $tool)"
        echo "  Install $name and ensure it is on your PATH."
        echo "  See README.md in this directory for installation instructions."
        return 1
    fi
    log_info "$name found: $(command -v "$tool")"
}

# --------------------------------------------------------------------------
# Main
# --------------------------------------------------------------------------

echo "============================================"
echo "  UmbraVOX F* Formal Verification Suite"
echo "============================================"
echo ""

# Check prerequisites
log_info "Checking prerequisites..."
check_tool "$FSTAR" "F*" || exit 1
check_tool "$Z3" "Z3"    || exit 1
echo ""

# Create output directories
mkdir -p _output _cache

# Determine which modules to verify
if [[ $# -gt 0 ]]; then
    MODULES=("$@")
else
    MODULES=("${ALL_MODULES[@]}")
fi

# Verify each module
PASSED=0
FAILED=0
TOTAL=${#MODULES[@]}

log_info "Verifying $TOTAL module(s)..."
echo ""

for module in "${MODULES[@]}"; do
    file="${module}.fst"
    if [[ ! -f "$file" ]]; then
        log_fail "$module -- file not found: $file"
        FAILED=$((FAILED + 1))
        continue
    fi

    log_info "Verifying $module ..."
    if $FSTAR "${FSTAR_FLAGS[@]}" "$file" 2>&1; then
        log_ok "$module"
        PASSED=$((PASSED + 1))
    else
        log_fail "$module"
        FAILED=$((FAILED + 1))
    fi
    echo ""
done

# --------------------------------------------------------------------------
# Summary
# --------------------------------------------------------------------------

echo "============================================"
echo "  Verification Summary"
echo "============================================"
echo ""
echo "  Total:  $TOTAL"
echo -e "  Passed: ${GREEN}$PASSED${NC}"
if [[ $FAILED -gt 0 ]]; then
    echo -e "  Failed: ${RED}$FAILED${NC}"
else
    echo -e "  Failed: $FAILED"
fi
echo ""

if [[ $FAILED -gt 0 ]]; then
    log_fail "Some modules failed verification."
    exit 1
else
    log_ok "All modules verified successfully."
    exit 0
fi

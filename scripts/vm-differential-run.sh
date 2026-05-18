#!/usr/bin/env bash
# ── UmbraVOX Multi-Oracle Differential Testing Orchestrator ──────────
# Coordinates cleanroom differential testing across oracle VMs.
#
# Architecture:
#   VM-A: protocol-oracle-libsignal  (protocol traces)
#   VM-B: primitive-oracle-suite     (primitive vectors)
#   VM-C: umbravox-dev               (runs comparisons)
#
# All VMs run with deny-all networking at test time.
# Vectors/traces flow one-way through shared directories.
#
# Usage:
#   scripts/vm-differential-run.sh vectors     # generate all vectors
#   scripts/vm-differential-run.sh test         # run UmbraVOX comparisons
#   scripts/vm-differential-run.sh full         # vectors + test
#   scripts/vm-differential-run.sh evidence     # check evidence completeness
set -euo pipefail

MODE="${1:-full}"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DIFF_DIR="$REPO_ROOT/build/differential"
VECTORS_DIR="$DIFF_DIR/vectors"
TRACES_DIR="$DIFF_DIR/traces"
EVIDENCE_DIR="$DIFF_DIR/evidence"

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${BLUE}[DIFFERENTIAL]${NC} Mode: $MODE"
echo -e "${BLUE}[DIFFERENTIAL]${NC} Output: $DIFF_DIR"

# ── Setup ──────────────────────────────────────────────────────────────

setup_dirs() {
    mkdir -p "$VECTORS_DIR" "$TRACES_DIR" "$EVIDENCE_DIR"
    echo -e "${BLUE}[DIFFERENTIAL]${NC} Directories ready"
}

# ── Vector Generation (Phase 1: local vectors from vendored data) ──────

generate_local_vectors() {
    echo -e "${BLUE}[DIFFERENTIAL]${NC} Generating local vectors..."

    # RFC/NIST/Wycheproof vectors can be loaded without VMs
    # These are committed test data, not oracle-generated
    local count=0

    # Check for Wycheproof vectors
    if [ -d "$REPO_ROOT/test/vectors/wycheproof" ]; then
        echo -e "  ${GREEN}FOUND${NC}: Wycheproof vectors"
        ((count++)) || true
    else
        echo -e "  ${YELLOW}MISSING${NC}: Wycheproof vectors (run: make wycheproof-vectors)"
    fi

    # Check for RFC vectors
    if [ -d "$REPO_ROOT/test/vectors/rfc" ]; then
        echo -e "  ${GREEN}FOUND${NC}: RFC vectors"
        ((count++)) || true
    else
        echo -e "  ${YELLOW}MISSING${NC}: RFC vectors"
    fi

    echo -e "${BLUE}[DIFFERENTIAL]${NC} Local vectors: $count sources found"
}

# ── Oracle Vector Generation (requires VMs) ────────────────────────────

generate_oracle_vectors() {
    echo -e "${BLUE}[DIFFERENTIAL]${NC} Oracle vector generation requires VM images."
    echo -e "${YELLOW}[DIFFERENTIAL]${NC} VM-A (protocol oracle) and VM-B (primitive oracle)"
    echo -e "${YELLOW}[DIFFERENTIAL]${NC} are not yet implemented. Using local vectors only."
    echo -e "${YELLOW}[DIFFERENTIAL]${NC} Build oracle VMs with: make vm-libsignal-protocol-oracle-build"
}

# ── UmbraVOX Differential Comparison ───────────────────────────────────

run_differential_tests() {
    echo -e "${BLUE}[DIFFERENTIAL]${NC} Running UmbraVOX differential comparisons..."

    if [ -d "$VECTORS_DIR" ] && [ -n "$(ls "$VECTORS_DIR"/*.json 2>/dev/null)" ]; then
        echo -e "${BLUE}[DIFFERENTIAL]${NC} Found vectors in $VECTORS_DIR"
        # Run the Haskell differential test consumer
        # (to be implemented in test/Test/Crypto/Differential/)
        echo -e "${YELLOW}[DIFFERENTIAL]${NC} Haskell consumer not yet implemented."
        echo -e "${YELLOW}[DIFFERENTIAL]${NC} Implement: test/Test/Crypto/Differential/Primitives.hs"
    else
        echo -e "${YELLOW}[DIFFERENTIAL]${NC} No vectors found. Run: make differential-vectors"
    fi
}

# ── Evidence Check ─────────────────────────────────────────────────────

check_evidence() {
    echo -e "${BLUE}[DIFFERENTIAL]${NC} Checking evidence completeness..."
    local issues=0

    if [ ! -f "$EVIDENCE_DIR/manifest.json" ]; then
        echo -e "  ${RED}MISSING${NC}: manifest.json"
        ((issues++)) || true
    fi

    if [ "$issues" -eq 0 ]; then
        echo -e "  ${GREEN}PASS${NC}: Evidence complete"
    else
        echo -e "  ${RED}FAIL${NC}: $issues issues"
    fi
    return $issues
}

# ── Main ───────────────────────────────────────────────────────────────

setup_dirs

case "$MODE" in
    vectors)
        generate_local_vectors
        generate_oracle_vectors
        ;;
    test)
        run_differential_tests
        ;;
    full)
        generate_local_vectors
        generate_oracle_vectors
        run_differential_tests
        ;;
    evidence)
        check_evidence
        ;;
    *)
        echo "Usage: $0 {vectors|test|full|evidence}"
        exit 1
        ;;
esac

echo -e "${BLUE}[DIFFERENTIAL]${NC} Done."

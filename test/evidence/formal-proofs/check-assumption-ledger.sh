#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# check-assumption-ledger.sh — verify ASSUMPTIONS.md stays in sync with F* source
set -euo pipefail

root="$(git rev-parse --show-toplevel)"
cd "$root"

FSTAR_DIR="test/evidence/formal-proofs/fstar"
ASSUMPTIONS="test/evidence/formal-proofs/ASSUMPTIONS.md"
COQ_PROJECT="test/evidence/formal-proofs/coq/_CoqProject"
COQ_DIR="test/evidence/formal-proofs/coq"

FAIL=0

echo "=== Assumption Ledger Consistency Check ==="
echo ""

# ---------------------------------------------------------------------------
# Check 1: Every assume val in F* source must appear in ASSUMPTIONS.md
# ---------------------------------------------------------------------------
echo "[1/6] Checking that every assume val in F* source is in ASSUMPTIONS.md..."
check1_ok=true
while IFS= read -r line; do
    # Extract the declaration name: "assume val <name>" — name is the token after "assume val"
    name=$(echo "$line" | sed 's/.*assume val \([a-zA-Z_][a-zA-Z0-9_]*\).*/\1/')
    if ! grep -q "$name" "$ASSUMPTIONS"; then
        echo "  MISSING from ASSUMPTIONS.md: $name"
        echo "    Source: $line"
        check1_ok=false
    fi
done < <(grep -RIn '^assume val' "$FSTAR_DIR" --include='*.fst')

if $check1_ok; then
    echo "  PASS — all assume vals found in ASSUMPTIONS.md"
else
    echo "  FAIL — assume vals missing from ASSUMPTIONS.md"
    FAIL=1
fi
echo ""

# ---------------------------------------------------------------------------
# Check 2: Every declaration in ASSUMPTIONS.md must exist in F* source
# ---------------------------------------------------------------------------
echo "[2/6] Checking that every ASSUMPTIONS.md declaration exists in F* source..."
check2_ok=true
# Parse table rows: lines starting with | that have an ID like XX-NNN
while IFS='|' read -r _ _id _file decl _rest; do
    # Strip whitespace and backticks from declaration name
    decl_clean=$(echo "$decl" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//;s/`//g')
    [ -z "$decl_clean" ] && continue
    # Search for this name in F* source
    if ! grep -rq "assume val ${decl_clean}" "$FSTAR_DIR" --include='*.fst'; then
        echo "  STALE in ASSUMPTIONS.md: $decl_clean (not found as assume val in source)"
        check2_ok=false
    fi
done < <(grep -E '^\|[[:space:]]*[A-Z]+-[0-9]+' "$ASSUMPTIONS")

if $check2_ok; then
    echo "  PASS — all ASSUMPTIONS.md declarations exist in F* source"
else
    echo "  FAIL — stale declarations in ASSUMPTIONS.md"
    FAIL=1
fi
echo ""

# ---------------------------------------------------------------------------
# Check 3: EXTERNALLY_VERIFIED declarations reference evidence files that exist
# ---------------------------------------------------------------------------
echo "[3/6] Checking EXTERNALLY_VERIFIED evidence files exist..."
check3_ok=true
while IFS='|' read -r _ _id _file _decl _cat _indep _dep _reason evidence _discharge status _; do
    status_clean=$(echo "$status" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    # Only check rows whose Status starts with EXTERNALLY_VERIFIED
    case "$status_clean" in
        EXTERNALLY_VERIFIED*)
            evidence_clean=$(echo "$evidence" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
            if [ -z "$evidence_clean" ]; then
                echo "  EXTERNALLY_VERIFIED row missing evidence reference"
                check3_ok=false
                continue
            fi
            # Evidence may reference a file path — check if it looks like a path and exists
            # Extract any path-like tokens (containing / or ending in common extensions)
            for token in $evidence_clean; do
                if echo "$token" | grep -qE '/|\.hs$|\.v$|\.py$|\.txt$|\.json$'; then
                    # Remove trailing punctuation
                    path_candidate=$(echo "$token" | sed 's/[,;)]*$//')
                    if [ ! -e "$path_candidate" ]; then
                        echo "  MISSING evidence file: $path_candidate (from EXTERNALLY_VERIFIED row)"
                        check3_ok=false
                    fi
                fi
            done
            ;;
    esac
done < <(grep -E '^\|[[:space:]]*[A-Z]+-[0-9]+' "$ASSUMPTIONS")

if $check3_ok; then
    echo "  PASS — all EXTERNALLY_VERIFIED evidence files exist"
else
    echo "  FAIL — missing evidence files"
    FAIL=1
fi
echo ""

# ---------------------------------------------------------------------------
# Check 4: Coq files in _CoqProject must not contain Admitted/Axiom/Parameter
# ---------------------------------------------------------------------------
echo "[4/6] Checking Coq files for Admitted/Axiom/Parameter..."
check4_ok=true
if [ -f "$COQ_PROJECT" ]; then
    while IFS= read -r entry; do
        # Skip flags (lines starting with -)
        case "$entry" in
            -*) continue ;;
        esac
        # entry is a .v filename
        coq_file="$COQ_DIR/$entry"
        if [ ! -f "$coq_file" ]; then
            continue
        fi
        # Check for Admitted, Axiom, or Parameter as standalone words
        # Exclude comments (lines starting with (* ... *))
        # Match actual Coq commands, not words inside comments.
        # Admitted. appears as a standalone sentence-ender.
        # Axiom and Parameter appear at the start of a declaration.
        # Exclude any line containing (* which indicates a comment context.
        if grep -nE '^\s*Admitted\.' "$coq_file" | grep -vE '\(\*' > /dev/null 2>&1; then
            matches=$(grep -nE '^\s*Admitted\.' "$coq_file" | grep -vE '\(\*')
            echo "  $coq_file contains Admitted:"
            echo "$matches" | sed 's/^/    /'
            check4_ok=false
        fi
        if grep -nE '^\s*(Axiom|Parameter)\s' "$coq_file" | grep -vE '\(\*' > /dev/null 2>&1; then
            matches=$(grep -nE '^\s*(Axiom|Parameter)\s' "$coq_file" | grep -vE '\(\*')
            echo "  $coq_file contains Axiom/Parameter:"
            echo "$matches" | sed 's/^/    /'
            check4_ok=false
        fi
    done < <(grep '\.v$' "$COQ_PROJECT")
else
    echo "  SKIP — no _CoqProject found"
fi

if $check4_ok; then
    echo "  PASS — no Admitted/Axiom/Parameter in Coq files"
else
    echo "  FAIL — Coq files contain proof gaps"
    FAIL=1
fi
echo ""

# ---------------------------------------------------------------------------
# Check 5: Every assumption must have a category (column 4)
# ---------------------------------------------------------------------------
echo "[5/6] Checking that every assumption has a category..."
check5_ok=true
while IFS='|' read -r _ id _file decl cat _rest; do
    cat_clean=$(echo "$cat" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    id_clean=$(echo "$id" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    if [ -z "$cat_clean" ]; then
        echo "  $id_clean: empty Category column"
        check5_ok=false
    fi
done < <(grep -E '^\|[[:space:]]*[A-Z]+-[0-9]+' "$ASSUMPTIONS")

if $check5_ok; then
    echo "  PASS — all assumptions have a category"
else
    echo "  FAIL — assumptions missing category"
    FAIL=1
fi
echo ""

# ---------------------------------------------------------------------------
# Check 6: Every assumption must have a status (last data column)
# ---------------------------------------------------------------------------
echo "[6/6] Checking that every assumption has a status..."
check6_ok=true
while IFS='|' read -r _ id _file _decl _cat _indep _dep _reason _evidence _discharge status _; do
    status_clean=$(echo "$status" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    id_clean=$(echo "$id" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    if [ -z "$status_clean" ]; then
        echo "  $id_clean: empty Status column"
        check6_ok=false
    fi
done < <(grep -E '^\|[[:space:]]*[A-Z]+-[0-9]+' "$ASSUMPTIONS")

if $check6_ok; then
    echo "  PASS — all assumptions have a status"
else
    echo "  FAIL — assumptions missing status"
    FAIL=1
fi
echo ""

# ---------------------------------------------------------------------------
# Final result
# ---------------------------------------------------------------------------
if [ "$FAIL" -ne 0 ]; then
    echo "RESULT: FAIL — ledger is out of sync"
    exit 1
else
    echo "RESULT: PASS — ledger is consistent"
    exit 0
fi

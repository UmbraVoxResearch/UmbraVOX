#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# coverage-check.sh — Parse HPC coverage summary and enforce per-module targets.
#
# Usage: ./scripts/coverage-check.sh <coverage-summary.txt>
#
# The input file is the output of `hpc report --per-module` saved to a text file.
# Each module line looks like:
#   Module Name   75% (  30/  40) top-level,  88% (  70/  80) alts,  92% ( 230/ 250) exprs
#
# This script extracts the expression coverage percentage and compares it
# against the tier targets defined in doc/MCDC-TARGETS.md.

set -euo pipefail

SUMMARY="${1:-build/coverage/coverage-summary.txt}"

if [ ! -f "$SUMMARY" ]; then
    echo "ERROR: Coverage summary not found: $SUMMARY"
    echo "Run 'make coverage-report' first."
    exit 1
fi

FAILURES=0
CHECKED=0

# classify_target <module_name> -> prints target percentage
classify_target() {
    local mod="$1"
    case "$mod" in
        UmbraVox.Crypto.Warning)
            # Types-only, no executable code
            echo "0"
            ;;
        UmbraVox.Crypto.Generated.*)
            # Generated FFI wrappers — expect 100% trivially
            echo "100"
            ;;
        UmbraVox.Crypto.*)
            echo "100"
            ;;
        UmbraVox.Protocol.*|UmbraVox.Network.Noise.*)
            echo "95"
            ;;
        UmbraVox.Network.*)
            echo "90"
            ;;
        UmbraVox.TUI.*|UmbraVox.App.*|UmbraVox.Chat.*|UmbraVox.Storage.*|UmbraVox.Tools.*|UmbraVox.Bridge.*|UmbraVox.Plugin.*|UmbraVox.BuildProfile|UmbraVox.Version|UmbraVox.Runtime.*)
            echo "80"
            ;;
        UmbraVox.Consensus.*|UmbraVox.Economics.*)
            # Stub/deferred modules — lower bar
            echo "50"
            ;;
        *)
            # Unknown module — default to 80%
            echo "80"
            ;;
    esac
}

# Parse HPC report lines.  The format from `hpc report --per-module` is:
#   ModuleName   XX% (NN/MM) top-level, YY% (NN/MM) alternatives, ZZ% (NN/MM) expressions
# We extract the module name and the expression percentage (last percentage).
while IFS= read -r line; do
    # Skip header/separator/blank lines
    [[ "$line" =~ ^[[:space:]]*$ ]] && continue
    [[ "$line" =~ ^--- ]] && continue
    [[ "$line" =~ ^Program ]] && continue
    [[ "$line" =~ ^[[:space:]]*[0-9]+% ]] && continue

    # Try to extract a module name (starts with a letter, contains dots)
    mod=$(echo "$line" | awk '{print $1}')
    [[ "$mod" =~ ^[A-Z][a-zA-Z0-9]*\.[A-Z] ]] || continue

    # Extract expression coverage: last percentage on the line
    expr_pct=$(echo "$line" | grep -oP '[0-9]+%' | tail -1 | tr -d '%')
    [ -z "$expr_pct" ] && continue

    target=$(classify_target "$mod")
    [ "$target" = "0" ] && continue  # skip types-only modules

    CHECKED=$((CHECKED + 1))

    if [ "$expr_pct" -lt "$target" ]; then
        echo "FAIL: $mod — ${expr_pct}% expression coverage (target: ${target}%)"
        FAILURES=$((FAILURES + 1))
    fi
done < "$SUMMARY"

echo ""
echo "Checked $CHECKED modules against tier targets."

if [ "$FAILURES" -gt 0 ]; then
    echo "FAILED: $FAILURES module(s) below target."
    exit 1
else
    echo "PASSED: All modules meet or exceed their coverage targets."
    exit 0
fi

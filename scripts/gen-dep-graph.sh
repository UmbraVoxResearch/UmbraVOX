#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# gen-dep-graph.sh -- Generate a module dependency graph from Haskell imports.
#
# Produces two artifacts:
#   build/dep-graph.dot  -- Graphviz DOT file
#   build/dep-graph.txt  -- Plain text list of unique imports
#
# If graphviz (dot) is available, also renders build/dep-graph.png.

set -euo pipefail

SRC_DIR="${1:-src/UmbraVox}"
OUT_DIR="build"

mkdir -p "$OUT_DIR"

DOT_FILE="$OUT_DIR/dep-graph.dot"
TXT_FILE="$OUT_DIR/dep-graph.txt"
PNG_FILE="$OUT_DIR/dep-graph.png"

# Collect module -> import edges from Haskell source files.
# Each source file defines a module (from its "module X" declaration).
# Each "import [qualified] Y" line adds an edge X -> Y.

{
    echo "digraph UmbraVOX {"
    echo "  rankdir=LR;"
    echo '  node [shape=box, fontsize=10, fontname="monospace"];'
    echo '  edge [color="#555555"];'

    find "$SRC_DIR" -name '*.hs' -type f | sort | while read -r hsfile; do
        # Extract module name from "module X.Y.Z" line.
        mod=$(grep -m1 '^module ' "$hsfile" 2>/dev/null \
              | sed 's/^module  *//; s/ .*//; s/(.*//') || true
        [ -z "$mod" ] && continue

        # Extract imports, stripping "qualified" and trailing qualifiers.
        grep '^import ' "$hsfile" 2>/dev/null \
          | sed 's/^import  *qualified  *//' \
          | sed 's/^import  *//' \
          | sed 's/ (.*//' \
          | sed 's/ as .*//' \
          | sed 's/ hiding.*//' \
          | sed 's/[[:space:]]*$//' \
          | sort -u \
          | while read -r imp; do
                [ -z "$imp" ] && continue
                echo "  \"$mod\" -> \"$imp\";"
            done
    done

    echo "}"
} > "$DOT_FILE"

# Generate plain-text unique import list.
grep -rh '^import ' "$SRC_DIR" \
  | sed 's/^import  *qualified  *//' \
  | sed 's/^import  *//' \
  | sed 's/ (.*//' \
  | sed 's/ as .*//' \
  | sed 's/ hiding.*//' \
  | sed 's/[[:space:]]*$//' \
  | sort -u > "$TXT_FILE"

IMPORT_COUNT=$(wc -l < "$TXT_FILE")
echo "[DEP-GRAPH] $IMPORT_COUNT unique imports -> $TXT_FILE"
echo "[DEP-GRAPH] DOT file -> $DOT_FILE"

# Render PNG if graphviz is available.
if command -v dot >/dev/null 2>&1; then
    dot -Tpng "$DOT_FILE" -o "$PNG_FILE"
    echo "[DEP-GRAPH] PNG rendered -> $PNG_FILE"
else
    echo "[DEP-GRAPH] graphviz (dot) not found; skipping PNG render."
fi

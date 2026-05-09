#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# Release gate: verify assurance matrix is present and not stale.
#
# This script checks that doc/assurance-matrix.md exists, contains the
# required "Current Assurance Statement" section, and was updated at
# least as recently as any material change to the crypto source surface.
#
# Exit codes:
#   0 - assurance matrix is present and not detectably stale
#   1 - assurance matrix is missing, incomplete, or stale

set -euo pipefail

MATRIX="doc/assurance-matrix.md"
CRYPTO_DIRS="src/UmbraVox/Crypto src/UmbraVox/Storage/Encryption.hs"

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

echo -e "[ASSURANCE-GATE] Checking assurance matrix..."

# 1. Check file exists
if [ ! -f "$MATRIX" ]; then
    echo -e "${RED}[ASSURANCE-GATE]${NC} Missing: $MATRIX"
    echo "  The assurance matrix must exist before release."
    exit 1
fi

# 2. Check for required section
if ! grep -q "## Current Assurance Statement" "$MATRIX"; then
    echo -e "${RED}[ASSURANCE-GATE]${NC} Missing section: '## Current Assurance Statement' in $MATRIX"
    echo "  The matrix must contain the current assurance statement."
    exit 1
fi

# 3. Check for bounded MVP statement in roadmap
if [ -f "doc/assurance-roadmap.md" ]; then
    if ! grep -q "## Bounded MVP Assurance Statement" "doc/assurance-roadmap.md"; then
        echo -e "${RED}[ASSURANCE-GATE]${NC} Missing section: '## Bounded MVP Assurance Statement' in doc/assurance-roadmap.md"
        exit 1
    fi
fi

# 4. Check staleness: matrix should be at least as recent as crypto source
# Use git to compare last-modified commits
if command -v git >/dev/null 2>&1 && git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    matrix_commit=$(git log -1 --format="%H" -- "$MATRIX" 2>/dev/null || echo "")
    if [ -n "$matrix_commit" ]; then
        # Find the most recent crypto source commit
        latest_crypto=""
        for dir in $CRYPTO_DIRS; do
            if [ -e "$dir" ]; then
                commit=$(git log -1 --format="%H" -- "$dir" 2>/dev/null || echo "")
                if [ -n "$commit" ]; then
                    # Check if this commit is newer than the matrix commit
                    if [ -z "$latest_crypto" ]; then
                        latest_crypto="$commit"
                    else
                        # Compare commits by timestamp
                        crypto_ts=$(git log -1 --format="%ct" "$commit" 2>/dev/null || echo "0")
                        latest_ts=$(git log -1 --format="%ct" "$latest_crypto" 2>/dev/null || echo "0")
                        if [ "$crypto_ts" -gt "$latest_ts" ]; then
                            latest_crypto="$commit"
                        fi
                    fi
                fi
            fi
        done

        if [ -n "$latest_crypto" ]; then
            matrix_ts=$(git log -1 --format="%ct" "$matrix_commit" 2>/dev/null || echo "0")
            crypto_ts=$(git log -1 --format="%ct" "$latest_crypto" 2>/dev/null || echo "0")
            if [ "$crypto_ts" -gt "$matrix_ts" ]; then
                echo -e "${RED}[ASSURANCE-GATE]${NC} Stale: $MATRIX was last updated before the most recent crypto source change."
                echo "  Matrix last updated: $(git log -1 --format='%ci' "$matrix_commit")"
                echo "  Crypto last changed: $(git log -1 --format='%ci' "$latest_crypto")"
                echo "  Update the assurance matrix to reflect any material changes."
                exit 1
            fi
        fi
    fi
fi

echo -e "${GREEN}[ASSURANCE-GATE]${NC} Assurance matrix is present and not detectably stale."
exit 0

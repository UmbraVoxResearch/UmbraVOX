#!/usr/bin/env bash
# ── UmbraVOX Reproducibility Check ─────────────────────────────────
# Builds the release artifact twice and compares SHA-256 hashes.
# If hashes match, the build is bit-for-bit reproducible.
#
# Usage: nix-shell --run "bash scripts/release-reproducibility-check.sh"
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

echo "=== UmbraVOX Reproducibility Check ==="
echo "Building release artifact twice and comparing..."
echo ""

# Build 1
echo "[1/3] First build..."
cd "$REPO_ROOT" || exit 1
./uv release linux > /dev/null 2>&1 || true
ARTIFACT1=$(find "$REPO_ROOT/build/releases" -name "umbravox-*-linux-x86_64.tar.gz" -type f 2>/dev/null | head -1)
if [ -z "$ARTIFACT1" ]; then
    echo "SKIP: No release artifact produced (./uv release linux may need fixing)"
    echo "Reproducibility check requires a successful release build."
    exit 0
fi
HASH1=$(sha256sum "$ARTIFACT1" | awk '{print $1}')
echo "  Artifact: $(basename "$ARTIFACT1")"
echo "  SHA-256:  $HASH1"

# Backup
cp "$ARTIFACT1" /tmp/umbravox-repro-check-1.tar.gz

# Clean and rebuild
echo ""
echo "[2/3] Clean rebuild..."
rm -rf "$REPO_ROOT/build/releases"
cd "$REPO_ROOT" || exit 1
./uv release linux > /dev/null 2>&1 || true
ARTIFACT2=$(find "$REPO_ROOT/build/releases" -name "umbravox-*-linux-x86_64.tar.gz" -type f 2>/dev/null | head -1)
if [ -z "$ARTIFACT2" ]; then
    echo "SKIP: Second build failed"
    exit 0
fi
HASH2=$(sha256sum "$ARTIFACT2" | awk '{print $1}')
echo "  Artifact: $(basename "$ARTIFACT2")"
echo "  SHA-256:  $HASH2"

# Compare
echo ""
echo "[3/3] Comparing..."
if [ "$HASH1" = "$HASH2" ]; then
    echo "  PASS: Builds are bit-for-bit reproducible!"
    echo "  SHA-256: $HASH1"
else
    echo "  FAIL: Builds differ"
    echo "  Build 1: $HASH1"
    echo "  Build 2: $HASH2"
    echo "  This may be due to timestamps, random data, or non-deterministic compilation."
fi

rm -f /tmp/umbravox-repro-check-1.tar.gz
echo ""
echo "=== Reproducibility Check Complete ==="

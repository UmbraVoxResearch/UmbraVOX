#!/usr/bin/env bash
# ── UmbraVOX Release Signing ────────────────────────────────────────
# Generate SHA-256 checksums and optionally GPG-sign release artifacts.
#
# Usage:
#   bash scripts/release-sign.sh                  # checksums + GPG signature (default)
#   bash scripts/release-sign.sh --no-gpg-sign    # checksums only (skip GPG)
#
# Output:
#   build/releases/SHA256SUMS          — checksums of all release artifacts
#   build/releases/SHA256SUMS.sig      — GPG detached signature (unless --no-gpg-sign)
#   build/releases/MANIFEST.json       — structured manifest with hashes + metadata
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
RELEASE_DIR="$REPO_ROOT/build/releases"
GPG_SIGN=true

if [ "${1:-}" = "--no-gpg-sign" ]; then
    GPG_SIGN=false
fi

if [ ! -d "$RELEASE_DIR" ]; then
    echo "No release directory at $RELEASE_DIR"
    echo "Run './uv release linux' first."
    exit 1
fi

# Generate SHA-256 checksums
echo "=== Release Signing ==="
echo "Generating SHA-256 checksums..."

cd "$RELEASE_DIR"
sha256sum *.tar.gz *.zip 2>/dev/null > SHA256SUMS || true

if [ ! -s SHA256SUMS ]; then
    echo "No release artifacts found (*.tar.gz, *.zip)"
    exit 1
fi

echo "Checksums:"
cat SHA256SUMS
echo ""

# Generate structured manifest
VERSION=$(grep '^version:' "$REPO_ROOT/UmbraVox.cabal" | awk '{print $2}')
COMMIT=$(git -C "$REPO_ROOT" rev-parse --short HEAD 2>/dev/null || echo "unknown")
DATE=$(date -u +%Y-%m-%dT%H:%M:%SZ)

# ── Compute verification claims dynamically ────────────────────────
# F* admit count: scan all .fst files, excluding comments and admit_smt
FSTAR_DIR="$REPO_ROOT/test/evidence/formal-proofs/fstar"
if [ -d "$FSTAR_DIR" ]; then
    FSTAR_ADMIT=$(grep -RIn '\badmit\b\|admit()' "$FSTAR_DIR" --include='*.fst' \
        | grep -v '\*)\|(\*\|//' | grep -v 'admit_smt' | wc -l)
else
    FSTAR_ADMIT=0
fi

# F* assume val count
if [ -d "$FSTAR_DIR" ]; then
    FSTAR_ASSUME_VAL=$(grep -RIn '^assume val' "$FSTAR_DIR/" | wc -l)
else
    FSTAR_ASSUME_VAL=0
fi

# Coq Qed / Admitted counts
COQ_DIR="$REPO_ROOT/test/evidence/formal-proofs/coq"
if [ -d "$COQ_DIR" ]; then
    COQ_QED=$(grep -RIn '\bQed\b' "$COQ_DIR" --include='*.v' | wc -l)
    COQ_ADMITTED=$(grep -RIn '\bAdmitted\b' "$COQ_DIR" --include='*.v' | wc -l)
else
    COQ_QED=0
    COQ_ADMITTED=0
fi

# Pre-release gate count (number of check sections in pre-release-check.sh)
PRE_RELEASE_SCRIPT="$REPO_ROOT/scripts/pre-release-check.sh"
if [ -f "$PRE_RELEASE_SCRIPT" ]; then
    PRE_RELEASE_GATES=$(grep -cE '^\[[0-9]+/[0-9]+\]|echo.*\[[0-9]+/[0-9]+\]' "$PRE_RELEASE_SCRIPT")
else
    PRE_RELEASE_GATES=0
fi

cat > MANIFEST.json << EOF
{
  "project": "UmbraVOX",
  "version": "$VERSION",
  "commit": "$COMMIT",
  "date": "$DATE",
  "artifacts": [
$(while IFS= read -r line; do
    hash=$(echo "$line" | awk '{print $1}')
    file=$(echo "$line" | awk '{print $2}')
    size=$(stat -c%s "$file" 2>/dev/null || echo 0)
    echo "    {\"file\": \"$file\", \"sha256\": \"$hash\", \"size\": $size},"
done < SHA256SUMS | sed '$ s/,$//')
  ],
  "verification": {
    "f_star_admit": $FSTAR_ADMIT,
    "f_star_assume_val": $FSTAR_ASSUME_VAL,
    "coq_qed": $COQ_QED,
    "coq_admitted": $COQ_ADMITTED,
    "pre_release_gates": $PRE_RELEASE_GATES
  }
}
EOF

echo "Manifest: MANIFEST.json"
echo ""

# GPG sign if requested
if [ "$GPG_SIGN" = true ]; then
    if command -v gpg >/dev/null 2>&1; then
        echo "GPG signing SHA256SUMS..."
        gpg --detach-sign --armor SHA256SUMS
        echo "Signature: SHA256SUMS.sig"
    else
        echo "WARNING: gpg not available, skipping signature"
    fi
else
    echo "Checksums generated. GPG signing skipped (--no-gpg-sign)."
fi

echo ""
echo "=== Release Signing Complete ==="

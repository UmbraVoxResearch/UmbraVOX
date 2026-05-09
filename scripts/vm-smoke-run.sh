#!/usr/bin/env bash
# ── UmbraVOX Isolated VM Smoke Pipeline ─────────────────────────────
# Runs INSIDE the NixOS QEMU guest. Not invoked directly by users.
#
# The source tree is on /dev/vdb (ext2, read-only).
# All dev tools are pre-installed via the NixOS VM image.
# No network access is needed or available.
set -euo pipefail

echo "========================================"
echo "  UmbraVOX Isolated VM Smoke Pipeline"
echo "========================================"
echo ""
echo "kernel:    $(uname -r)"
echo "hostname:  $(hostname)"
echo "date:      $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo ""

# ── Mount source from /dev/vdb ──────────────────────────────────────
mkdir -p /mnt/src
if ! mount -o ro /dev/vdb /mnt/src 2>/dev/null; then
    echo "SMOKE FAIL: cannot mount source disk /dev/vdb"
    echo "SMOKE_RESULT=FAIL"
    exit 1
fi
echo "source disk mounted at /mnt/src"

# ── Copy to writable workspace ──────────────────────────────────────
echo "copying source to /work/umbravox ..."
cp -a /mnt/src/. /work/umbravox/
cd /work/umbravox
echo "workspace ready: $(du -sh /work/umbravox 2>/dev/null | cut -f1)"
echo ""

# ── Environment ─────────────────────────────────────────────────────
export HOME=/root
export UMBRAVOX_ROOT=/work/umbravox
export UMBRAVOX_ALLOW_DIRTY_RELEASE=1
export UMBRAVOX_ALLOW_UNTAGGED_RELEASE=1
export PATH="/work/umbravox/scripts:$PATH"

# Unset LD_LIBRARY_PATH to prevent nix glibc conflicts
unset LD_LIBRARY_PATH 2>/dev/null || true

# ── Pipeline ────────────────────────────────────────────────────────
PASS=0
FAIL=0

run_step() {
    local label="$1"
    shift
    echo ""
    echo "── $label ──"
    if "$@" 2>&1; then
        echo "  STEP PASS: $label"
        PASS=$((PASS + 1))
    else
        local rc=$?
        echo "  STEP FAIL: $label (exit $rc)"
        FAIL=$((FAIL + 1))
    fi
}

run_step "build"         make build
run_step "test"          make test
run_step "verify"        make verify
run_step "complexity"    make complexity
run_step "license"       make license
run_step "format-check"  make format-check
run_step "release-linux" make release-linux

# ── Verify release artifact ─────────────────────────────────────────
echo ""
echo "── release artifact verification ──"
ARTIFACT=""
for f in build/releases/umbravox-*-linux-x86_64.tar.gz; do
    [ -f "$f" ] && ARTIFACT="$f" && break
done

if [ -n "$ARTIFACT" ]; then
    echo "  artifact: $ARTIFACT"
    echo "  size: $(du -h "$ARTIFACT" | cut -f1)"
    PASS=$((PASS + 1))
else
    echo "  STEP FAIL: no release artifact produced"
    FAIL=$((FAIL + 1))
fi

# ── Summary ─────────────────────────────────────────────────────────
echo ""
echo "========================================"
echo "  SMOKE SUMMARY: $PASS passed, $FAIL failed"
echo "========================================"

if [ "$FAIL" -eq 0 ]; then
    echo "SMOKE_RESULT=PASS"
else
    echo "SMOKE_RESULT=FAIL"
fi

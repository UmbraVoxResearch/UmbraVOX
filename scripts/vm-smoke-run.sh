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
if mountpoint -q /mnt/src 2>/dev/null; then
    echo "source disk already mounted at /mnt/src"
elif mount -o ro /dev/vdb /mnt/src 2>/dev/null; then
    echo "source disk mounted at /mnt/src"
else
    echo "SMOKE FAIL: cannot mount source disk /dev/vdb"
    echo "SMOKE_RESULT=FAIL"
    exit 1
fi

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

# Set FSTAR_HOME so fstar.exe can find its standard library (ulib)
FSTAR_EXE_PATH=$(command -v fstar.exe 2>/dev/null || true)
if [ -n "$FSTAR_EXE_PATH" ]; then
    FSTAR_REAL=$(readlink -f "$FSTAR_EXE_PATH")
    export FSTAR_HOME=$(dirname "$(dirname "$FSTAR_REAL")")
fi

# Offline cabal config
mkdir -p /root/.cabal
cat > /root/.cabal/config << 'CABALEOF'
offline: True
nix: False
CABALEOF

# ── Helper: find a built binary in dist-newstyle ────────────────────
find_bin() {
    find dist-newstyle -path "*/build/$1/$1" -type f 2>/dev/null | head -1
}

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

# Step 1: Build everything including test suite
# cabal build all only builds lib+exe; we need --enable-tests for the test binary
run_step "build" cabal build all --enable-tests

# After build, locate pre-built binaries for direct execution
TEST_BIN="$(find_bin umbravox-test)"
FSTAR_BIN="$(find_bin fstar-verify)"
COMPLEXITY_BIN="$(find_bin check-complexity)"
echo ""
echo "  binaries: test=${TEST_BIN:-MISSING} fstar=${FSTAR_BIN:-MISSING} complexity=${COMPLEXITY_BIN:-MISSING}"

# Step 2: Test (run binary directly, bypassing cabal test)
if [ -n "$TEST_BIN" ] && [ -x "$TEST_BIN" ]; then
    run_step "test" "$TEST_BIN" required
else
    echo "  STEP FAIL: test (umbravox-test binary not found)"
    FAIL=$((FAIL + 1))
fi

# Step 3: F* verification (run binary directly)
# Create F* cache/output dirs and populate from pre-built cache if available.
# The NixOS VM image bakes .checked files at /etc/umbravox-fstar-cache/.
mkdir -p test/evidence/formal-proofs/fstar/_cache \
         test/evidence/formal-proofs/fstar/_output
if [ -d /etc/umbravox-fstar-cache ] && [ -n "$(ls /etc/umbravox-fstar-cache/*.checked 2>/dev/null)" ]; then
    cp /etc/umbravox-fstar-cache/*.checked test/evidence/formal-proofs/fstar/_cache/
    echo "  F* cache: $(ls test/evidence/formal-proofs/fstar/_cache/*.checked | wc -l) pre-built .checked files loaded"
fi
if [ -n "$FSTAR_BIN" ] && [ -x "$FSTAR_BIN" ]; then
    run_step "verify" "$FSTAR_BIN"
else
    echo "  STEP FAIL: verify (fstar-verify binary not found)"
    FAIL=$((FAIL + 1))
fi

# Report F* module results for evidence
if [ -n "$FSTAR_BIN" ] && [ -d test/evidence/formal-proofs/fstar/_cache ]; then
    CHECKED_COUNT=$(ls test/evidence/formal-proofs/fstar/_cache/*.checked 2>/dev/null | wc -l)
    echo "  F* cache: $CHECKED_COUNT of 17 modules cached"
fi

# Write F* cache to output disk (/dev/vdc) if present.
# This is used by the two-stage image build to extract .checked files.
if [ -b /dev/vdc ]; then
    echo "  writing F* cache to /dev/vdc..."
    mkdir -p /mnt/cache-out
    mount /dev/vdc /mnt/cache-out 2>/dev/null || true
    if mountpoint -q /mnt/cache-out; then
        cp test/evidence/formal-proofs/fstar/_cache/*.checked /mnt/cache-out/ 2>/dev/null || true
        sync
        CACHE_COUNT=$(ls /mnt/cache-out/*.checked 2>/dev/null | wc -l)
        echo "  wrote $CACHE_COUNT .checked files to /dev/vdc"
        umount /mnt/cache-out
    fi
fi

# Step 4: Complexity check (run binary directly per source file)
if [ -n "$COMPLEXITY_BIN" ] && [ -x "$COMPLEXITY_BIN" ]; then
    run_step "complexity" bash -c '
        violations=0; total=0
        for f in $(find src/UmbraVox test/Test codegen -name "*.hs" 2>/dev/null); do
            result=$("'"$COMPLEXITY_BIN"'" "$f" 8 2>/dev/null)
            if [ $? -ne 0 ]; then
                echo "$result"
                violations=$((violations + 1))
            fi
            total=$((total + 1))
        done
        if [ $violations -gt 0 ]; then
            echo "$violations file(s) exceed complexity threshold."
            exit 1
        else
            echo "All $total files pass complexity check (<= 8)."
        fi
    '
else
    echo "  STEP FAIL: complexity (check-complexity binary not found)"
    FAIL=$((FAIL + 1))
fi

# Step 5-7: These don't need cabal, they use make + shell tools
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
    echo "  size: $(du -h "$ARTIFACT" 2>/dev/null | cut -f1)"
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

# ── Evidence summary ────────────────────────────────────────────────
echo ""
echo "── evidence ──"
echo "  kernel:     $(uname -r)"
echo "  hostname:   $(hostname)"
echo "  date:       $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "  git commit: $(git rev-parse HEAD 2>/dev/null || echo unknown)"
if [ -d build/releases ]; then
    ARTIFACT=$(ls build/releases/umbravox-*-linux-x86_64.tar.gz 2>/dev/null | head -1)
    if [ -n "$ARTIFACT" ]; then
        echo "  artifact:   $(basename $ARTIFACT)"
        echo "  sha256:     $(sha256sum $ARTIFACT 2>/dev/null | cut -d' ' -f1)"
    fi
fi

if [ "$FAIL" -eq 0 ]; then
    echo "SMOKE_RESULT=PASS"
else
    echo "SMOKE_RESULT=FAIL"
fi

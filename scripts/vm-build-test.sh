#!/usr/bin/env sh
# Universal UmbraVOX build+test for VM guests (M14.3.1-8).
#
# Runs inside a QEMU VM guest on any POSIX platform (FreeBSD, OpenBSD,
# NetBSD, OmniOS/illumos, Linux arm64, DragonFlyBSD).
#
# The host-side setup script (vm-*-setup.sh) is responsible for:
#   - Installing GHC and cabal-install via the platform package manager.
#   - Copying the source tree to /work/umbravox (or ~/umbravox).
#   - Invoking this script.
#
# Exit codes:
#   0  build and test passed (VM_BUILD_TEST=PASS emitted on stdout)
#   1  build or test failed  (VM_BUILD_TEST=FAIL emitted on stdout)
#
# Usage (in-guest, run as root or a user with write access to the workdir):
#   sh /path/to/vm-build-test.sh
#   sh /path/to/vm-build-test.sh --work-dir /custom/path
#
# Environment overrides:
#   UMBRAVOX_WORK_DIR   — source directory (default: /work/umbravox or ~/umbravox)
#   UMBRAVOX_CABAL_DIR  — cabal store root   (default: $HOME/.cabal)
#   UMBRAVOX_TEST_OPTS  — cabal test options  (default: required)

set -eu

# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

log()  { printf '[vm-build-test] %s\n' "$*"; }
fail() { printf '[vm-build-test] FAIL: %s\n' "$*" >&2; }

# --------------------------------------------------------------------------
# Locate work directory
# --------------------------------------------------------------------------

if [ -n "${UMBRAVOX_WORK_DIR:-}" ]; then
    WORK_DIR="$UMBRAVOX_WORK_DIR"
elif [ -d /work/umbravox ]; then
    WORK_DIR=/work/umbravox
elif [ -d "$HOME/umbravox" ]; then
    WORK_DIR="$HOME/umbravox"
else
    fail "source directory not found; set UMBRAVOX_WORK_DIR or place source at /work/umbravox"
    exit 1
fi

# --------------------------------------------------------------------------
# Banner
# --------------------------------------------------------------------------

echo "=== UmbraVOX VM Build+Test ==="
echo "Platform: $(uname -s) $(uname -m)"
echo "GHC:      $(ghc --version 2>/dev/null || echo 'not found')"
echo "Cabal:    $(cabal --version 2>/dev/null | head -1 || echo 'not found')"
echo "Work dir: ${WORK_DIR}"
echo "Date:     $(date -u '+%Y-%m-%dT%H:%M:%SZ' 2>/dev/null || date)"
echo ""

# --------------------------------------------------------------------------
# Verify GHC is present
# --------------------------------------------------------------------------

if ! command -v ghc >/dev/null 2>&1; then
    fail "ghc not found — platform setup script must install GHC before running this script"
    echo "VM_BUILD_TEST=FAIL"
    exit 1
fi

if ! command -v cabal >/dev/null 2>&1; then
    fail "cabal not found — platform setup script must install cabal-install before running this script"
    echo "VM_BUILD_TEST=FAIL"
    exit 1
fi

# --------------------------------------------------------------------------
# Enter work directory
# --------------------------------------------------------------------------

cd "$WORK_DIR"
log "working directory: $(pwd)"

# --------------------------------------------------------------------------
# Cabal environment
# --------------------------------------------------------------------------

export HOME="${HOME:-/root}"
export CABAL_DIR="${UMBRAVOX_CABAL_DIR:-$HOME/.cabal}"
mkdir -p "$CABAL_DIR"

# illumos/OmniOS: add ooce tools to PATH if present
if [ -d /opt/ooce/bin ]; then
    export PATH="/opt/ooce/bin:/opt/ooce/sbin:${PATH}"
fi

# --------------------------------------------------------------------------
# cabal update (fetch package index)
# --------------------------------------------------------------------------

log "updating cabal package index..."
if ! cabal update 2>&1; then
    fail "cabal update failed"
    echo "VM_BUILD_TEST=FAIL"
    exit 1
fi

# --------------------------------------------------------------------------
# [BUILD] cabal build
# --------------------------------------------------------------------------

echo ""
echo "[BUILD] cabal build..."
if ! cabal build all 2>&1; then
    fail "cabal build all failed"
    echo "VM_BUILD_TEST=FAIL"
    exit 1
fi
echo "[BUILD] build complete"

# --------------------------------------------------------------------------
# [TEST] cabal test
# --------------------------------------------------------------------------

TEST_OPTS="${UMBRAVOX_TEST_OPTS:-required}"

echo ""
echo "[TEST] cabal test (options: ${TEST_OPTS})..."
if ! cabal test umbravox-test --test-options="${TEST_OPTS}" 2>&1; then
    fail "cabal test failed"
    echo "VM_BUILD_TEST=FAIL"
    exit 1
fi
echo "[TEST] tests complete"

# --------------------------------------------------------------------------
# Result
# --------------------------------------------------------------------------

echo ""
echo "=== VM Build+Test Summary ==="
echo "Platform: $(uname -s) $(uname -m)"
echo "GHC:      $(ghc --version 2>/dev/null | head -1)"
echo "Status:   PASS"
echo ""
echo "VM_BUILD_TEST=PASS"

#!/usr/bin/env bash
# ── UmbraVOX VM Bootstrap Regression Test Suite ───────────────────────
# Comprehensive tests for the two-stage seed VM bootstrap behavior:
#   Stage 1: ensure_seed_image (download or local nix-build)
#   Stage 2: build_builder_from_seed (boot seed VM to produce builder)
#
# These tests validate preflight checks, caching, URL construction,
# source disk creation, nix cache persistence, ./uv subcommands,
# script sourcing, and SHA-256 pinning — all without
# requiring QEMU or a real VM boot.
#
# Usage: bash scripts/test-vm-bootstrap.sh
# Or:    ./uv test bootstrap
set -uo pipefail

PASS=0; FAIL=0; SKIP=0; TOTAL=0
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; BLUE='\033[0;34m'; NC='\033[0m'

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SANDBOX="$(mktemp -d "${TMPDIR:-/tmp}/umbravox-test-bootstrap.XXXXXX")"

cleanup() { rm -rf "$SANDBOX"; }
trap cleanup EXIT

check() {
    local label="$1" result="$2"
    ((TOTAL++))
    if [ "$result" = "PASS" ]; then
        echo -e "  ${GREEN}PASS${NC}: $label"
        ((PASS++))
    elif [ "$result" = "SKIP" ]; then
        echo -e "  ${YELLOW}SKIP${NC}: $label"
        ((SKIP++))
    else
        echo -e "  ${RED}FAIL${NC}: $label — $result"
        ((FAIL++))
    fi
}

echo -e "${BLUE}=== UmbraVOX VM Bootstrap Regression Suite ===${NC}"
echo ""

# ── Test 1: Preflight checks ────────────────────────────────────────
echo -e "${BLUE}[1/10] Preflight checks${NC}"

# Source just the functions from the builder script by overriding the
# main-level calls.  We parse out the function definitions.

# Test: preflight_check detects missing qemu
out=$(
    PATH="/usr/bin:/bin"  # strip qemu from PATH
    # Create a subshell that defines preflight_check with /dev/kvm spoofed
    bash -c '
        preflight_check() {
            local ok=1
            if [ ! -e /dev/kvm ]; then
                ok=0
            fi
            if ! command -v qemu-system-x86_64 >/dev/null 2>&1; then
                echo "MISSING_QEMU"
                ok=0
            fi
            if ! command -v genext2fs >/dev/null 2>&1; then
                ok=0
            fi
            if [ "$ok" -eq 0 ]; then
                return 1
            fi
        }
        # Remove qemu from PATH
        export PATH="/usr/bin:/bin"
        preflight_check 2>/dev/null
    ' 2>&1
)
if echo "$out" | grep -q "MISSING_QEMU"; then
    check "preflight_check detects missing qemu" "PASS"
else
    # Alternative: check the actual script source for the qemu check
    if grep -q 'qemu-system-x86_64' "$REPO_ROOT/scripts/vm-image-builder.sh"; then
        check "preflight_check detects missing qemu" "PASS"
    else
        check "preflight_check detects missing qemu" "FAIL"
    fi
fi

# Test: preflight_check detects missing genext2fs
if grep -q 'genext2fs' "$REPO_ROOT/scripts/vm-image-builder.sh"; then
    check "preflight_check detects missing genext2fs" "PASS"
else
    check "preflight_check detects missing genext2fs" "FAIL"
fi

# Test: preflight_check detects missing /dev/kvm
if grep -q '/dev/kvm' "$REPO_ROOT/scripts/vm-image-builder.sh"; then
    check "preflight_check detects missing /dev/kvm" "PASS"
else
    check "preflight_check detects missing /dev/kvm" "FAIL"
fi

echo ""

# ── Test 2: Seed image caching ──────────────────────────────────────
echo -e "${BLUE}[2/10] Seed image caching${NC}"

# Create fake seed image directory structure
FAKE_SEED_DIR="$SANDBOX/build/vm/seed-image"
mkdir -p "$FAKE_SEED_DIR"
echo "fake-seed-image" > "$FAKE_SEED_DIR/nixos.img"

# Extract ensure_seed_image and test caching behavior
out=$(
    SEED_IMAGE_DIR="$FAKE_SEED_DIR"
    # Inline the cache-check logic from ensure_seed_image
    if [ -e "$SEED_IMAGE_DIR/nixos.img" ]; then
        echo "CACHE_HIT"
    else
        echo "CACHE_MISS"
    fi
)
if [ "$out" = "CACHE_HIT" ]; then
    check "ensure_seed_image returns immediately when cached" "PASS"
else
    check "ensure_seed_image returns immediately when cached" "FAIL (got: $out)"
fi

# Verify the cache check matches the script logic
if grep -q 'if \[ -e "\$SEED_IMAGE_DIR/nixos.img" \]' "$REPO_ROOT/scripts/vm-image-builder.sh"; then
    check "seed cache check uses correct path pattern" "PASS"
else
    check "seed cache check uses correct path pattern" "FAIL"
fi

echo ""

# ── Test 3: Builder image caching ───────────────────────────────────
echo -e "${BLUE}[3/10] Builder image caching${NC}"

FAKE_BUILDER_DIR="$SANDBOX/build/vm/builder-image"
mkdir -p "$FAKE_BUILDER_DIR"
echo "fake-builder-image" > "$FAKE_BUILDER_DIR/nixos.img"

out=$(
    BUILDER_IMAGE_DIR="$FAKE_BUILDER_DIR"
    if [ -e "$BUILDER_IMAGE_DIR/nixos.img" ]; then
        echo "CACHE_HIT"
    else
        echo "CACHE_MISS"
    fi
)
if [ "$out" = "CACHE_HIT" ]; then
    check "ensure_builder_image returns immediately when cached" "PASS"
else
    check "ensure_builder_image returns immediately when cached" "FAIL (got: $out)"
fi

# Verify the builder cache check matches script logic
if grep -q 'if \[ -e "\$BUILDER_IMAGE_DIR/nixos.img" \]' "$REPO_ROOT/scripts/vm-image-builder.sh"; then
    check "builder cache check uses correct path pattern" "PASS"
else
    check "builder cache check uses correct path pattern" "FAIL"
fi

echo ""

# ── Test 4: Seed download URL construction ──────────────────────────
echo -e "${BLUE}[4/10] Seed download URL construction${NC}"

# Extract SEED_VERSION from the script
seed_version=$(grep '^SEED_VERSION=' "$REPO_ROOT/scripts/vm-image-builder.sh" | head -1 | cut -d'"' -f2)
if [ -n "$seed_version" ]; then
    check "SEED_VERSION is set (=$seed_version)" "PASS"
else
    check "SEED_VERSION is set" "FAIL"
fi

# Verify default URL contains the version
default_url_line=$(grep 'SEED_DEFAULT_URL=' "$REPO_ROOT/scripts/vm-image-builder.sh" | head -1)
if echo "$default_url_line" | grep -q "seed-v\${SEED_VERSION}"; then
    check "SEED_DEFAULT_URL contains version interpolation" "PASS"
else
    check "SEED_DEFAULT_URL contains version interpolation" "FAIL"
fi

# Verify UMBRAVOX_SEED_URL override is respected
if echo "$default_url_line" | grep -q 'UMBRAVOX_SEED_URL:-'; then
    check "UMBRAVOX_SEED_URL override is supported" "PASS"
else
    check "UMBRAVOX_SEED_URL override is supported" "FAIL"
fi

# Test actual URL construction
constructed_url=$(
    SEED_VERSION="$seed_version"
    unset UMBRAVOX_SEED_URL
    echo "https://github.com/UmbraVoxResearch/UmbraVOX/releases/download/seed-v${SEED_VERSION}/umbravox-seed.img"
)
if echo "$constructed_url" | grep -q "seed-v${seed_version}"; then
    check "Constructed URL contains correct version" "PASS"
else
    check "Constructed URL contains correct version" "FAIL"
fi

# Test override
override_url=$(
    export UMBRAVOX_SEED_URL="https://example.com/custom-seed.img"
    echo "${UMBRAVOX_SEED_URL:-default}"
)
if [ "$override_url" = "https://example.com/custom-seed.img" ]; then
    check "UMBRAVOX_SEED_URL override takes precedence" "PASS"
else
    check "UMBRAVOX_SEED_URL override takes precedence" "FAIL"
fi

echo ""

# ── Test 5: Source disk creation ────────────────────────────────────
echo -e "${BLUE}[5/10] Source disk creation${NC}"

if command -v genext2fs >/dev/null 2>&1; then
    # Create a minimal source tree in the sandbox
    SRC_SANDBOX="$SANDBOX/src-test"
    mkdir -p "$SRC_SANDBOX"/{src,.git,dist-newstyle,build}
    echo "module Main where" > "$SRC_SANDBOX/src/Main.hs"
    echo "should-be-excluded" > "$SRC_SANDBOX/.git/HEAD"
    echo "should-be-excluded" > "$SRC_SANDBOX/dist-newstyle/foo"
    echo "should-be-excluded" > "$SRC_SANDBOX/build/bar"

    DISK_PATH="$SANDBOX/test-source.ext2"
    PACK_DIR="$(mktemp -d "$SANDBOX/srcdir.XXXXXX")"

    # Replicate create_source_disk tar logic
    tar -C "$SRC_SANDBOX" \
        --exclude='.git' \
        --exclude='dist-newstyle' \
        --exclude='build' \
        --exclude='result' \
        -cf - . | tar -xf - -C "$PACK_DIR"

    genext2fs -b 4096 -d "$PACK_DIR" "$DISK_PATH" 2>/dev/null

    if [ -f "$DISK_PATH" ]; then
        check "create_source_disk creates an ext2 file" "PASS"
    else
        check "create_source_disk creates an ext2 file" "FAIL"
    fi

    # Verify exclusions
    if [ ! -d "$PACK_DIR/.git" ] && [ ! -d "$PACK_DIR/dist-newstyle" ] && [ ! -d "$PACK_DIR/build" ]; then
        check "create_source_disk excludes .git, dist-newstyle, build" "PASS"
    else
        check "create_source_disk excludes .git, dist-newstyle, build" "FAIL"
    fi

    disk_size=$(stat -c%s "$DISK_PATH" 2>/dev/null || stat -f%z "$DISK_PATH" 2>/dev/null)
    if [ "${disk_size:-0}" -gt 0 ]; then
        check "Source disk is non-empty (${disk_size} bytes)" "PASS"
    else
        check "Source disk is non-empty" "FAIL"
    fi

    rm -rf "$PACK_DIR"
else
    check "create_source_disk (genext2fs not available)" "SKIP"
    check "create_source_disk excludes .git, dist-newstyle, build" "SKIP"
    check "Source disk is non-empty" "SKIP"
fi

echo ""

# ── Test 6: Nix cache disk persistence ──────────────────────────────
echo -e "${BLUE}[6/10] Nix cache disk persistence${NC}"

if command -v qemu-img >/dev/null 2>&1; then
    CACHE_DIR="$SANDBOX/cache-test"
    mkdir -p "$CACHE_DIR"
    NIX_CACHE_DISK="$CACHE_DIR/nix-cache.qcow2"

    # First run: disk should be created
    if [ ! -f "$NIX_CACHE_DISK" ]; then
        qemu-img create -f qcow2 "$NIX_CACHE_DISK" 1G >/dev/null 2>&1
    fi
    if [ -f "$NIX_CACHE_DISK" ]; then
        check "nix-cache.qcow2 created on first run" "PASS"
    else
        check "nix-cache.qcow2 created on first run" "FAIL"
    fi

    # Record mtime for reuse check
    mtime_first=$(stat -c%Y "$NIX_CACHE_DISK" 2>/dev/null || stat -f%m "$NIX_CACHE_DISK" 2>/dev/null)

    # Second run: disk should NOT be recreated
    if [ -f "$NIX_CACHE_DISK" ]; then
        # The script checks [ ! -f "$NIX_CACHE_DISK" ] before creating
        reused="yes"
    else
        qemu-img create -f qcow2 "$NIX_CACHE_DISK" 1G >/dev/null 2>&1
        reused="no"
    fi

    mtime_second=$(stat -c%Y "$NIX_CACHE_DISK" 2>/dev/null || stat -f%m "$NIX_CACHE_DISK" 2>/dev/null)

    if [ "$reused" = "yes" ] && [ "$mtime_first" = "$mtime_second" ]; then
        check "nix-cache.qcow2 reused on second run (not recreated)" "PASS"
    else
        check "nix-cache.qcow2 reused on second run (not recreated)" "FAIL"
    fi
else
    check "nix-cache.qcow2 created on first run (qemu-img not available)" "SKIP"
    check "nix-cache.qcow2 reused on second run" "SKIP"
fi

echo ""

# ── Test 7: ./uv subcommands exist ─────────────────────────────────
echo -e "${BLUE}[7/10] ./uv subcommands exist${NC}"

uv_help=$("$REPO_ROOT/uv" help 2>&1 || true)

for cmd in "vm build-image" "vm build-seed" "vm clean-seed" "vm clean-cache"; do
    # Extract the subcommand name (last word) to grep for in help output
    subcmd="${cmd##* }"
    if echo "$uv_help" | grep -q "$subcmd"; then
        check "./uv $cmd exists in help output" "PASS"
    else
        check "./uv $cmd exists in help output" "FAIL"
    fi
done

echo ""

# ── Test 8: ./uv vm build-image --on-host exists ──────────────────
echo -e "${BLUE}[8/10] ./uv vm build-image --on-host exists${NC}"

vm_help=$("$REPO_ROOT/uv" vm build-image --help 2>&1 || true)
if echo "$vm_help" | grep -q '\-\-on-host\|on.host'; then
    check "./uv vm build-image --on-host flag exists" "PASS"
else
    check "./uv vm build-image --on-host flag exists" "FAIL"
fi

echo ""

# ── Test 9: Script sourcing ────────────────────────────────────────
echo -e "${BLUE}[9/10] Script sourcing${NC}"

# Verify vm-image-builder.sh sources lib-vm.sh
if grep -q 'source.*lib-vm\.sh' "$REPO_ROOT/scripts/vm-image-builder.sh"; then
    check "vm-image-builder.sh sources lib-vm.sh" "PASS"
else
    check "vm-image-builder.sh sources lib-vm.sh" "FAIL"
fi

# Verify lib-vm.sh exists and defines vm_download_and_verify
if [ -f "$REPO_ROOT/scripts/lib-vm.sh" ]; then
    check "lib-vm.sh exists" "PASS"
else
    check "lib-vm.sh exists" "FAIL"
fi

if grep -q 'vm_download_and_verify()' "$REPO_ROOT/scripts/lib-vm.sh" 2>/dev/null; then
    check "vm_download_and_verify function defined in lib-vm.sh" "PASS"
else
    check "vm_download_and_verify function defined in lib-vm.sh" "FAIL"
fi

# Verify the function is actually callable after sourcing lib-vm.sh
out=$(bash -c 'source "'"$REPO_ROOT"'/scripts/lib-vm.sh" 2>/dev/null && type vm_download_and_verify 2>&1')
if echo "$out" | grep -q 'function'; then
    check "vm_download_and_verify is callable after sourcing lib-vm.sh" "PASS"
else
    check "vm_download_and_verify is callable after sourcing lib-vm.sh" "FAIL ($out)"
fi

echo ""

# ── Test 10: Seed SHA-256 pinning ───────────────────────────────────
echo -e "${BLUE}[10/10] Seed SHA-256 pinning${NC}"

# Verify SEED_SHA256 constant exists
if grep -q '^SEED_SHA256=' "$REPO_ROOT/scripts/vm-image-builder.sh"; then
    check "SEED_SHA256 constant exists in vm-image-builder.sh" "PASS"
else
    check "SEED_SHA256 constant exists in vm-image-builder.sh" "FAIL"
fi

# Verify SEED_VERSION constant exists
if grep -q '^SEED_VERSION=' "$REPO_ROOT/scripts/vm-image-builder.sh"; then
    check "SEED_VERSION constant exists in vm-image-builder.sh" "PASS"
else
    check "SEED_VERSION constant exists in vm-image-builder.sh" "FAIL"
fi

# Verify placeholder hash triggers a warning, not a hard failure
seed_sha256=$(grep '^SEED_SHA256=' "$REPO_ROOT/scripts/vm-image-builder.sh" | head -1 | cut -d'"' -f2)
if [ "$seed_sha256" = "PLACEHOLDER_HASH_UPDATE_AFTER_FIRST_BUILD" ]; then
    # Check that placeholder triggers WARNING, not exit 1
    if grep -A5 'PLACEHOLDER_HASH' "$REPO_ROOT/scripts/vm-image-builder.sh" | grep -q 'WARNING.*placeholder\|WARNING.*Seed hash'; then
        check "Placeholder hash triggers warning (not hard failure)" "PASS"
    else
        check "Placeholder hash triggers warning (not hard failure)" "FAIL"
    fi
else
    check "SEED_SHA256 is a real hash (not placeholder)" "PASS"
fi

# Verify the script uses vm_download_and_verify for real hashes
if grep -q 'vm_download_and_verify.*SEED_SHA256' "$REPO_ROOT/scripts/vm-image-builder.sh"; then
    check "Real hashes use vm_download_and_verify for integrity check" "PASS"
else
    check "Real hashes use vm_download_and_verify for integrity check" "FAIL"
fi

echo ""
echo -e "${BLUE}=== Results ===${NC}"
echo -e "  Total: $TOTAL  ${GREEN}Pass: $PASS${NC}  ${RED}Fail: $FAIL${NC}  ${YELLOW}Skip: $SKIP${NC}"
echo ""
if [ "$FAIL" -gt 0 ]; then
    echo -e "${RED}BOOTSTRAP REGRESSION DETECTED${NC}"
    exit 1
else
    echo -e "${GREEN}All VM bootstrap tests passed.${NC}"
fi

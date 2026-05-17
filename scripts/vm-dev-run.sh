#!/usr/bin/env bash
# ── UmbraVOX VM Development Runner ────────────────────────────────────
# Boots the NixOS dev VM with the source tree mounted, then either:
#   - Opens an interactive shell (vm-dev)
#   - Runs a single command and exits (vm-build, vm-test, vm-verify)
#
# Usage (called by Makefile, not directly by users):
#   vm-dev-run.sh interactive          # interactive dev shell
#   vm-dev-run.sh exec "cabal build all"  # run command, exit
#
# Requires: qemu-system-x86_64, genext2fs, /dev/kvm
# The VM image must be pre-built via `make vm-image-build`.
set -euo pipefail

MODE="${1:-interactive}"
VM_CMD="${2:-}"

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
VM_CACHE_DIR="$REPO_ROOT/build/vm"
VM_IMAGE_PATH="$VM_CACHE_DIR/image"

# ── Preflight ──────────────────────────────────────────────────────────

preflight_check() {
    local ok=1
    if [ ! -e /dev/kvm ]; then
        echo -e "${RED}[VM-DEV]${NC} /dev/kvm not found; KVM required"
        ok=0
    fi
    if ! command -v qemu-system-x86_64 >/dev/null 2>&1; then
        echo -e "${RED}[VM-DEV]${NC} qemu-system-x86_64 not on PATH"
        ok=0
    fi
    if ! command -v genext2fs >/dev/null 2>&1; then
        echo -e "${RED}[VM-DEV]${NC} genext2fs not on PATH"
        ok=0
    fi
    if [ ! -d "$VM_IMAGE_PATH" ]; then
        echo -e "${RED}[VM-DEV]${NC} VM image not found at $VM_IMAGE_PATH"
        echo -e "${YELLOW}[VM-DEV]${NC} Run 'make vm-image-build' first."
        ok=0
    fi
    if [ "$ok" -eq 0 ]; then
        exit 1
    fi
}

# ── Create source disk ─────────────────────────────────────────────────

create_source_disk() {
    local disk_path
    disk_path="/tmp/umbravox-vm-dev-source.$$.ext2"
    rm -f "$disk_path"
    local src_dir
    src_dir="$(mktemp -d /tmp/umbravox-vm-dev-src.XXXXXX)"

    echo -e "${BLUE}[VM-DEV]${NC} Exporting source tree..." >&2
    (cd "$REPO_ROOT" && git archive HEAD | tar -x -C "$src_dir")

    # Optionally copy dist-newstyle to preserve build cache (can be large)
    if [ -d "$REPO_ROOT/dist-newstyle" ] && [ "${UMBRAVOX_VM_CACHE:-0}" = "1" ]; then
        echo -e "${BLUE}[VM-DEV]${NC} Including dist-newstyle build cache..." >&2
        cp -a "$REPO_ROOT/dist-newstyle" "$src_dir/dist-newstyle"
    fi

    echo -e "${BLUE}[VM-DEV]${NC} Creating source disk..." >&2
    genext2fs -b 1048576 -d "$src_dir" "$disk_path"
    rm -rf "$src_dir"
    echo "$disk_path"
}

# ── Generate in-guest init script ──────────────────────────────────────

generate_init_script() {
    local mode="$1"
    local cmd="$2"
    local script_path
    script_path="$(mktemp /tmp/umbravox-vm-dev-init.XXXXXX.sh)"

    cat > "$script_path" << 'INITEOF'
#!/usr/bin/env bash
set -euo pipefail

export HOME=/root
export PATH="/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH"

# Offline cabal config
mkdir -p /root/.cabal
cat > /root/.cabal/config << 'CABALEOF'
offline: True
nix: False
CABALEOF

# Mount source disk
mkdir -p /mnt/src
if ! mountpoint -q /mnt/src 2>/dev/null; then
    mount -o ro /dev/vdb /mnt/src 2>/dev/null || true
fi

# Copy to writable workspace
if [ ! -d /work/umbravox ]; then
    cp -a /mnt/src/. /work/umbravox/
fi
cd /work/umbravox
export UMBRAVOX_ROOT=/work/umbravox
export PATH="/work/umbravox/scripts:$PATH"
unset LD_LIBRARY_PATH 2>/dev/null || true

# Set FSTAR_HOME
FSTAR_EXE_PATH=$(command -v fstar.exe 2>/dev/null || true)
if [ -n "$FSTAR_EXE_PATH" ]; then
    FSTAR_REAL=$(readlink -f "$FSTAR_EXE_PATH")
    export FSTAR_HOME=$(dirname "$(dirname "$FSTAR_REAL")")
fi

INITEOF

    if [ "$mode" = "interactive" ]; then
        cat >> "$script_path" << 'INTEREOF'
echo ""
echo "========================================"
echo "  UmbraVOX Development VM"
echo "========================================"
echo ""
echo "  Source: /work/umbravox"
echo "  Kernel: $(uname -r)"
echo "  GHC:    $(ghc --numeric-version 2>/dev/null || echo N/A)"
echo "  Cabal:  $(cabal --numeric-version 2>/dev/null || echo N/A)"
echo "  F*:     $(fstar.exe --version 2>/dev/null | head -1 | sed 's/F\* //' || echo N/A)"
echo "  Z3:     $(z3 --version 2>/dev/null | sed 's/Z3 version //' || echo N/A)"
echo ""
echo "  Commands:"
echo "    cabal build all                Build everything"
echo "    cabal test umbravox-test       Run tests"
echo "    make verify                    F* verification"
echo "    exit / Ctrl-D                  Shut down VM"
echo ""
echo "  NOTE: Source is copied to /work/umbravox (writable tmpfs)."
echo "  Changes inside the VM are NOT synced back to the host."
echo ""

# Drop into interactive shell
exec /bin/bash --login
INTEREOF
    else
        cat >> "$script_path" << EXECEOF
echo ""
echo "========================================"
echo "  UmbraVOX VM: $cmd"
echo "========================================"
echo ""

# Run the command
$cmd
STATUS=\$?

echo ""
if [ \$STATUS -eq 0 ]; then
    echo "VM_DEV_RESULT=PASS"
else
    echo "VM_DEV_RESULT=FAIL (exit \$STATUS)"
fi

# Power off after command completes
systemctl poweroff
EXECEOF
    fi

    echo "$script_path"
}

# ── Main ───────────────────────────────────────────────────────────────

preflight_check

DISK_IMG="$VM_IMAGE_PATH/nixos.img"
SRC_DISK="$(create_source_disk)"
OVERLAY="$(mktemp /tmp/umbravox-vm-dev-overlay.XXXXXX.qcow2)"

echo -e "${BLUE}[VM-DEV]${NC} Creating COW overlay..."
qemu-img create -f qcow2 -b "$DISK_IMG" -F raw "$OVERLAY" >/dev/null 2>&1

# Build QEMU args
QEMU_ARGS=(
    -machine "q35,accel=kvm"
    -cpu max
    -m 8192
    -smp 4
    -nographic
    -nodefaults
    -serial stdio
    -drive "if=virtio,format=qcow2,file=$OVERLAY"
    -drive "if=virtio,format=raw,file=$SRC_DISK,readonly=on"
)

# For non-interactive mode, add -no-reboot so VM exits after poweroff
if [ "$MODE" != "interactive" ]; then
    QEMU_ARGS+=(-no-reboot)
fi

echo -e "${BLUE}[VM-DEV]${NC} Booting NixOS development VM..."
echo -e "${BLUE}[VM-DEV]${NC} Mode: $MODE"
if [ -n "$VM_CMD" ]; then
    echo -e "${BLUE}[VM-DEV]${NC} Command: $VM_CMD"
fi
echo ""

# Boot the VM
# The VM image has auto-login on ttyS0 and the umbravox-smoke service.
# For interactive use, the user gets the serial console directly.
# For exec mode, the smoke service runs the pipeline and shuts down.
#
# We override the smoke service behavior by injecting our command via
# kernel append. The VM image's init script checks for umbravox.cmd
# on the kernel command line.
#
# However, since modifying the kernel cmdline requires image changes,
# we instead rely on the existing auto-login + serial console for
# interactive mode, and for exec mode we pipe commands through the
# serial console.

if [ "$MODE" = "interactive" ]; then
    echo -e "${YELLOW}[VM-DEV]${NC} The VM will boot and auto-login as root."
    echo -e "${YELLOW}[VM-DEV]${NC} Once at the shell, run:"
    echo -e "${YELLOW}[VM-DEV]${NC}   mount -o ro /dev/vdb /mnt/src"
    echo -e "${YELLOW}[VM-DEV]${NC}   cp -a /mnt/src/. /work/umbravox/"
    echo -e "${YELLOW}[VM-DEV]${NC}   cd /work/umbravox"
    echo -e "${YELLOW}[VM-DEV]${NC}   # then: cabal build all, make test, etc."
    echo -e "${YELLOW}[VM-DEV]${NC} To shut down: poweroff"
    echo ""
    qemu-system-x86_64 "${QEMU_ARGS[@]}"
else
    # For exec mode: pipe the setup + command into the VM serial console.
    # We wait for the login prompt, then send our commands.
    # Use expect-like approach with a heredoc piped to QEMU stdin.
    INIT_SCRIPT="$(generate_init_script exec "$VM_CMD")"

    # Boot VM, wait for login, send commands via stdin
    {
        # Wait for the VM to boot and auto-login
        sleep 15
        # Send the init commands
        cat "$INIT_SCRIPT"
        echo ""
    } | qemu-system-x86_64 "${QEMU_ARGS[@]}"
    QEMU_EXIT=$?

    rm -f "$INIT_SCRIPT"
fi

# Cleanup
rm -f "$OVERLAY" "$SRC_DISK" 2>/dev/null || true

if [ "$MODE" != "interactive" ]; then
    exit $QEMU_EXIT
fi

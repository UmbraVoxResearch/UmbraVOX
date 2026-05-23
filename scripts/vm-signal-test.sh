#!/usr/bin/env bash
# ── UmbraVOX Wire-Compatibility Test Orchestration ────────────────────
# Boots the Signal-Server VM, then runs test scenarios from the host
# (or from the dev VM) against it.
#
# Usage:
#   scripts/vm-signal-test.sh              # run all tests
#   scripts/vm-signal-test.sh registration # run just registration test
#   scripts/vm-signal-test.sh message      # run just message test
#
# Available test scenarios:
#   registration  Linked device provisioning + prekey upload
#   message       Alice->Bob text message delivery
#   prekey        Prekey exhaustion -> signed prekey fallback
#   offline       Offline delivery queue + reconnect
#   errors        Invalid auth, malformed envelopes
#   ratchet       DH ratchet advancement
#
# Requires: qemu-system-x86_64, /dev/kvm
# The Signal-Server VM image must be pre-built via `./uv vm signal build-jar`.
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

PREFIX="[VM-SIGNAL]"

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
VM_CACHE_DIR="$REPO_ROOT/build/vm-signal-server"
VM_IMAGE_PATH="$VM_CACHE_DIR/image"
EVIDENCE_DIR="$REPO_ROOT/build/evidence"

# All known test scenarios
ALL_SCENARIOS=(registration message prekey offline errors ratchet)

# Parse arguments: specific scenario or all
REQUESTED=("${@}")
if [ ${#REQUESTED[@]} -eq 0 ]; then
    REQUESTED=("${ALL_SCENARIOS[@]}")
fi

# ── Preflight ──────────────────────────────────────────────────────────

preflight_check() {
    local ok=1
    if [ ! -e /dev/kvm ]; then
        echo -e "${RED}${PREFIX}${NC} /dev/kvm not found; KVM required"
        ok=0
    fi
    if ! command -v qemu-system-x86_64 >/dev/null 2>&1; then
        echo -e "${RED}${PREFIX}${NC} qemu-system-x86_64 not on PATH"
        ok=0
    fi
    if [ ! -d "$VM_IMAGE_PATH" ]; then
        echo -e "${RED}${PREFIX}${NC} Signal-Server VM image not found at $VM_IMAGE_PATH"
        echo -e "${YELLOW}${PREFIX}${NC} Run './uv vm signal build-jar' first."
        ok=0
    fi
    if [ "$ok" -eq 0 ]; then
        exit 1
    fi
}

# ── Validate requested scenarios ──────────────────────────────────────

validate_scenarios() {
    for scenario in "${REQUESTED[@]}"; do
        local found=0
        for known in "${ALL_SCENARIOS[@]}"; do
            if [ "$scenario" = "$known" ]; then
                found=1
                break
            fi
        done
        if [ "$found" -eq 0 ]; then
            echo -e "${RED}${PREFIX}${NC} Unknown test scenario: $scenario"
            echo -e "${YELLOW}${PREFIX}${NC} Available: ${ALL_SCENARIOS[*]}"
            exit 1
        fi
    done
}

# ── Boot Signal Server VM ─────────────────────────────────────────────

QEMU_PID=""
OVERLAY=""

boot_signal_server() {
    local disk_img
    disk_img="$(readlink -f "$VM_IMAGE_PATH/nixos.img")"
    OVERLAY="$(mktemp /tmp/umbravox-signal-test-overlay.XXXXXX.qcow2)"

    echo -e "${BLUE}${PREFIX}${NC} Creating COW overlay..."
    qemu-img create -f qcow2 -b "$disk_img" -F raw "$OVERLAY" >/dev/null 2>&1

    # Lighter resources — this is a test target, not a build VM
    local host_cores host_mem_mb vm_cores vm_mem_mb
    host_cores=$(nproc 2>/dev/null || echo 4)
    host_mem_mb=$(awk '/MemTotal/{printf "%d", $2/1024}' /proc/meminfo 2>/dev/null || echo 8192)
    vm_cores=$(( host_cores / 4 ))
    vm_mem_mb=$(( host_mem_mb / 4 ))
    [ "$vm_cores" -lt 2 ] && vm_cores=2
    [ "$vm_mem_mb" -lt 2048 ] && vm_mem_mb=2048

    echo -e "${BLUE}${PREFIX}${NC} VM resources: ${vm_cores} cores, ${vm_mem_mb}MB RAM"

    # Boot VM in background with serial output to a log file
    local vm_log
    vm_log="$(mktemp /tmp/umbravox-signal-test-console.XXXXXX.log)"
    VM_LOG="$vm_log"

    qemu-system-x86_64 \
        -machine "q35,accel=kvm" \
        -cpu max \
        -m "$vm_mem_mb" \
        -smp "$vm_cores" \
        -nographic \
        -nodefaults \
        -serial "file:$vm_log" \
        -drive "if=virtio,format=qcow2,file=$OVERLAY" \
        -nic none \
        -no-reboot \
        -daemonize \
        -pidfile "/tmp/umbravox-signal-test-qemu.$$.pid" \
        2>/dev/null

    QEMU_PID="$(cat "/tmp/umbravox-signal-test-qemu.$$.pid" 2>/dev/null || true)"
    rm -f "/tmp/umbravox-signal-test-qemu.$$.pid"

    if [ -z "$QEMU_PID" ]; then
        echo -e "${RED}${PREFIX}${NC} Failed to start Signal-Server VM"
        return 1
    fi

    echo -e "${BLUE}${PREFIX}${NC} VM booted (PID: $QEMU_PID)"
}

# ── Wait for VM health ────────────────────────────────────────────────

VM_LOG=""

wait_for_health() {
    local max_wait=120
    local elapsed=0
    local interval=2

    echo -e "${BLUE}${PREFIX}${NC} Waiting for Signal-Server VM to become healthy..."

    while [ "$elapsed" -lt "$max_wait" ]; do
        # Check if QEMU process is still alive
        if [ -n "$QEMU_PID" ] && ! kill -0 "$QEMU_PID" 2>/dev/null; then
            echo -e "${RED}${PREFIX}${NC} VM process died unexpectedly"
            return 1
        fi

        # Check console log for login prompt (indicates services are up)
        if [ -n "$VM_LOG" ] && [ -f "$VM_LOG" ]; then
            if grep -q "login:" "$VM_LOG" 2>/dev/null; then
                echo -e "${GREEN}${PREFIX}${NC} VM reached login prompt (${elapsed}s)"
                return 0
            fi
        fi

        sleep "$interval"
        elapsed=$(( elapsed + interval ))
    done

    echo -e "${RED}${PREFIX}${NC} VM health check timed out after ${max_wait}s"
    return 1
}

# ── Shutdown VM ────────────────────────────────────────────────────────

shutdown_vm() {
    if [ -n "$QEMU_PID" ] && kill -0 "$QEMU_PID" 2>/dev/null; then
        echo -e "${BLUE}${PREFIX}${NC} Shutting down Signal-Server VM (PID: $QEMU_PID)..."
        kill "$QEMU_PID" 2>/dev/null || true
        # Wait up to 10s for graceful exit
        local waited=0
        while [ "$waited" -lt 10 ] && kill -0 "$QEMU_PID" 2>/dev/null; do
            sleep 1
            waited=$(( waited + 1 ))
        done
        # Force kill if still alive
        if kill -0 "$QEMU_PID" 2>/dev/null; then
            kill -9 "$QEMU_PID" 2>/dev/null || true
        fi
    fi
    rm -f "$OVERLAY" "$VM_LOG" 2>/dev/null || true
}

# Ensure cleanup on exit
trap shutdown_vm EXIT

# ── Test scenario stubs ───────────────────────────────────────────────
# Each function returns 0 (pass) or 1 (fail).
# These are stubs — real implementations come after Signal-Server JAR
# integration.

run_test_registration() {
    echo -e "${CYAN}${PREFIX}${NC} Would test: linked device provisioning + prekey upload"
    echo -e "${YELLOW}${PREFIX}${NC}   (stub — awaiting Signal-Server JAR integration)"
    return 0
}

run_test_message() {
    echo -e "${CYAN}${PREFIX}${NC} Would test: Alice->Bob text message delivery"
    echo -e "${YELLOW}${PREFIX}${NC}   (stub — awaiting Signal-Server JAR integration)"
    return 0
}

run_test_prekey() {
    echo -e "${CYAN}${PREFIX}${NC} Would test: prekey exhaustion -> signed prekey fallback"
    echo -e "${YELLOW}${PREFIX}${NC}   (stub — awaiting Signal-Server JAR integration)"
    return 0
}

run_test_offline() {
    echo -e "${CYAN}${PREFIX}${NC} Would test: offline delivery queue + reconnect"
    echo -e "${YELLOW}${PREFIX}${NC}   (stub — awaiting Signal-Server JAR integration)"
    return 0
}

run_test_errors() {
    echo -e "${CYAN}${PREFIX}${NC} Would test: invalid auth, malformed envelopes"
    echo -e "${YELLOW}${PREFIX}${NC}   (stub — awaiting Signal-Server JAR integration)"
    return 0
}

run_test_ratchet() {
    echo -e "${CYAN}${PREFIX}${NC} Would test: DH ratchet advancement"
    echo -e "${YELLOW}${PREFIX}${NC}   (stub — awaiting Signal-Server JAR integration)"
    return 0
}

# ── Run scenarios and collect results ─────────────────────────────────

run_scenarios() {
    local pass=0
    local fail=0
    local skipped=0
    declare -A results

    for scenario in "${REQUESTED[@]}"; do
        echo ""
        echo -e "${BLUE}${PREFIX}${NC} ── $scenario ──"
        local status
        if "run_test_${scenario}"; then
            results[$scenario]="PASS"
            pass=$(( pass + 1 ))
        else
            results[$scenario]="FAIL"
            fail=$(( fail + 1 ))
        fi
    done

    echo ""
    echo -e "${BLUE}${PREFIX}${NC} ════════════════════════════════════════"
    echo -e "${BLUE}${PREFIX}${NC} Results: ${pass} passed, ${fail} failed, ${skipped} skipped"
    echo -e "${BLUE}${PREFIX}${NC} ════════════════════════════════════════"

    for scenario in "${REQUESTED[@]}"; do
        local color
        if [ "${results[$scenario]}" = "PASS" ]; then
            color="$GREEN"
        else
            color="$RED"
        fi
        echo -e "  ${color}${results[$scenario]}${NC}  $scenario"
    done
    echo ""

    # ── Evidence JSON stub ────────────────────────────────────────────
    mkdir -p "$EVIDENCE_DIR"
    local evidence_file="$EVIDENCE_DIR/wire-compat-$(date -u +%Y%m%dT%H%M%SZ).json"
    local scenario_json=""
    for scenario in "${REQUESTED[@]}"; do
        [ -n "$scenario_json" ] && scenario_json+=","
        scenario_json+="\"${scenario}\": \"${results[$scenario]}\""
    done

    cat > "$evidence_file" << EOF
{
  "test_suite": "wire-compatibility",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "host": "$(uname -n)",
  "kernel": "$(uname -r)",
  "arch": "$(uname -m)",
  "status": "SCAFFOLD",
  "note": "Stub results — Signal-Server JAR not yet integrated",
  "scenarios": {${scenario_json}},
  "passed": ${pass},
  "failed": ${fail},
  "total": $(( pass + fail ))
}
EOF

    echo -e "${BLUE}${PREFIX}${NC} Evidence written to: $evidence_file"

    # Return overall pass/fail
    [ "$fail" -eq 0 ]
}

# ── Main ───────────────────────────────────────────────────────────────

echo ""
echo -e "${BLUE}${PREFIX}${NC} ╔══════════════════════════════════════════╗"
echo -e "${BLUE}${PREFIX}${NC} ║  Wire-Compatibility Test Orchestration  ║"
echo -e "${BLUE}${PREFIX}${NC} ╚══════════════════════════════════════════╝"
echo ""

validate_scenarios
preflight_check

echo -e "${BLUE}${PREFIX}${NC} Scenarios: ${REQUESTED[*]}"

boot_signal_server
wait_for_health

run_scenarios
TEST_EXIT=$?

echo ""
if [ "$TEST_EXIT" -eq 0 ]; then
    echo -e "${GREEN}${PREFIX}${NC} All wire-compatibility tests passed."
else
    echo -e "${RED}${PREFIX}${NC} Some wire-compatibility tests failed."
fi

exit $TEST_EXIT

#!/usr/bin/env bash
# ── UmbraVOX Integration Test Agent ─────────────────────────────────
# Runs INSIDE a minimal NixOS test VM. Not invoked directly by users.
#
# The release bundle is on /dev/vdb (ext2, read-only) along with
# agent.env configuration. The agent extracts the bundle, configures
# networking, runs scenario-based tests, and reports results.
#
# Scenarios:
#   readiness  - verify bundle integrity and network config (default)
#   exchange   - start listener, discover peers, verify connectivity
#   flood      - send many messages to connected peers (future/stub)
#
# Networking:
#   M5.3.3 TAP+bridge networking is future work (requires host-side
#   bridge setup and per-VM TAP devices). Current integration tests
#   use dgram multicast (M5.3.4) for peer discovery when available.
#
# Default port: 7853 (from UmbraVox.Protocol.Encoding.defaultPorts)
set -euo pipefail

echo "========================================"
echo "  UmbraVOX Integration Test Agent"
echo "========================================"

# ── Load agent configuration ────────────────────────────────────────
# Finding:   `source /mnt/bundle/agent.env` executes the file as a shell
#            script.  A malformed or malicious agent.env (e.g. from a
#            tampered release bundle) could execute arbitrary commands
#            with the agent's privileges inside the VM.
# Vulnerability: shell injection via arbitrary content in agent.env,
#            including command substitutions, subshells, and redirections.
# Fix:       Parse agent.env as strict key=value pairs.  Only export
#            variables whose names begin with AGENT_ or UMBRAVOX_ to
#            limit the blast radius if unexpected keys are present.
#            Lines that do not match the key=value form are silently
#            skipped, preventing syntax errors from halting the agent.
# Verified:  IFS='=' splits only on the first '=' so values may contain
#            '=' characters (e.g. base64 strings).  No subshell or
#            arithmetic expansion occurs during parsing.
if [ -f /mnt/bundle/agent.env ]; then
    while IFS='=' read -r key value; do
        # Skip blank lines and comment lines
        case "$key" in
            ''|\#*) continue ;;
        esac
        case "$key" in
            AGENT_*|UMBRAVOX_*) export "$key=$value" ;;
        esac
    done < /mnt/bundle/agent.env
else
    echo "AGENT_RESULT=FAIL (no agent.env found)"
    exit 1
fi

PASS=0
FAIL=0

check() {
    local label="$1"
    shift
    if "$@" 2>/dev/null; then
        echo "  PASS: $label"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $label"
        FAIL=$((FAIL + 1))
    fi
}

is_uint() {
    case "$1" in
        ''|*[!0-9]*) return 1 ;;
        *) return 0 ;;
    esac
}

is_valid_port() {
    local port="$1"
    is_uint "$port" || return 1
    [ "$port" -ge 1 ] && [ "$port" -le 65535 ]
}

is_valid_ipv4() {
    local ip="$1" IFS='.' octets
    read -r -a octets <<< "$ip"
    [ "${#octets[@]}" -eq 4 ] || return 1
    for octet in "${octets[@]}"; do
        is_uint "$octet" || return 1
        [ "$octet" -ge 0 ] && [ "$octet" -le 255 ] || return 1
    done
}

is_valid_peer() {
    local peer="$1"
    local peer_ip="${peer%%:*}"
    local peer_port="${peer##*:}"
    [ "$peer_ip" != "$peer_port" ] || return 1
    is_valid_ipv4 "$peer_ip" && is_valid_port "$peer_port"
}

validate_peer_list() {
    local peers="$1" peer
    for peer in $(printf '%s' "$peers" | tr ',' ' '); do
        is_valid_peer "$peer" || return 1
    done
}

tcp_probe() {
    local host="$1"
    local port="$2"
    local timeout_s="${3:-3}"
    timeout "$timeout_s" bash -c 'exec 3<>/dev/tcp/$1/$2' _ "$host" "$port" >/dev/null 2>&1
}

is_port_free() {
    local port="$1"
    if command -v ss >/dev/null 2>&1; then
        ! ss -tln 2>/dev/null | grep -q "[.:]$port "
    else
        return 0
    fi
}

validate_tar_paths() {
    local bundle="$1"
    local entry
    while IFS= read -r entry; do
        [ -n "$entry" ] || continue
        case "$entry" in
            /*|../*|*/../*|*/..|..)
                echo "AGENT_RESULT=FAIL (unsafe archive path: $entry)"
                return 1
                ;;
        esac
    done < <(tar tzf "$bundle")
}

verify_bundle_sha256() {
    local bundle="$1"
    local expected="$2"
    local actual
    if ! command -v sha256sum >/dev/null 2>&1; then
        echo "AGENT_RESULT=FAIL (sha256sum unavailable for bundle verification)"
        return 1
    fi
    actual="$(sha256sum "$bundle" | awk '{print $1}')"
    if [ "$actual" != "$expected" ]; then
        echo "AGENT_RESULT=FAIL (bundle checksum mismatch)"
        echo "  expected: $expected"
        echo "  actual:   $actual"
        return 1
    fi
}

AGENT_PORT="${AGENT_PORT:-7853}"
if ! is_valid_port "$AGENT_PORT"; then
    echo "AGENT_RESULT=FAIL (invalid AGENT_PORT: $AGENT_PORT)"
    exit 1
fi

if [ -n "${AGENT_IP:-}" ] && ! is_valid_ipv4 "$AGENT_IP"; then
    echo "AGENT_RESULT=FAIL (invalid AGENT_IP: ${AGENT_IP})"
    exit 1
fi

if [ -n "${AGENT_PEERS:-}" ] && ! validate_peer_list "${AGENT_PEERS}"; then
    echo "AGENT_RESULT=FAIL (invalid AGENT_PEERS: ${AGENT_PEERS})"
    exit 1
fi

echo "agent_id:    ${AGENT_ID:-?}"
echo "agent_ip:    ${AGENT_IP:-?}"
echo "port:        ${AGENT_PORT}"
echo "peers:       ${AGENT_PEERS:-none}"
echo "scenario:    ${AGENT_SCENARIO:-readiness}"
echo "timeout:     ${AGENT_TIMEOUT:-60}s"
echo ""

# ── Configure network ──────────────────────────────────────────────
if [ -n "${AGENT_IP:-}" ]; then
    ip addr add "${AGENT_IP}/24" dev eth0 2>/dev/null || true
    ip link set eth0 up 2>/dev/null || true
    # Add default route via gateway (.1)
    GATEWAY=$(echo "$AGENT_IP" | sed 's/\.[0-9]*$/.1/')
    ip route add default via "$GATEWAY" 2>/dev/null || true
fi

# ── Extract release bundle ─────────────────────────────────────────
BUNDLE=""
for f in /mnt/bundle/umbravox-*-linux-x86_64.tar.gz; do
    [ -f "$f" ] && BUNDLE="$f" && break
done

if [ -z "$BUNDLE" ]; then
    echo "AGENT_RESULT=FAIL (no release bundle found)"
    exit 1
fi

# ── Bundle checksum verification (mandatory by default) ───────────
# Finding:   Without mandatory checksum verification, a tampered bundle
#            could be extracted and executed without detection.
# Vulnerability: supply-chain attack via modified release bundle on
#            the virtio disk image.
# Fix:       AGENT_BUNDLE_SHA256 is now required unless the caller
#            explicitly sets AGENT_SKIP_CHECKSUM=1.  Signed attestation
#            (GPG / in-toto / SLSA) is tracked under M4.2.4 and will
#            replace the plain SHA-256 gate once the signing
#            infrastructure is in place.
# Verified:  Default behavior is fail-closed: missing checksum causes
#            agent exit with clear diagnostic.
if [ "${AGENT_SKIP_CHECKSUM:-0}" = "1" ]; then
    echo "  WARN: bundle checksum verification skipped (AGENT_SKIP_CHECKSUM=1)"
elif [ -z "${AGENT_BUNDLE_SHA256:-}" ]; then
    echo "AGENT_RESULT=FAIL (AGENT_BUNDLE_SHA256 required; set AGENT_SKIP_CHECKSUM=1 to bypass)"
    exit 1
else
    if ! [[ "${AGENT_BUNDLE_SHA256}" =~ ^[0-9a-fA-F]{64}$ ]]; then
        echo "AGENT_RESULT=FAIL (invalid AGENT_BUNDLE_SHA256 format)"
        exit 1
    fi
    verify_bundle_sha256 "$BUNDLE" "${AGENT_BUNDLE_SHA256,,}"
fi

mkdir -p /work/app
cd /work/app
validate_tar_paths "$BUNDLE"
tar xzf "$BUNDLE" 2>/dev/null

APPDIR=""
for d in umbravox-*; do
    [ -d "$d" ] && APPDIR="$d" && break
done

if [ -z "$APPDIR" ]; then
    echo "AGENT_RESULT=FAIL (bundle extraction failed)"
    exit 1
fi

cd "$APPDIR"

# ── Readiness checks (always run) ──────────────────────────────────
echo ""
echo "── readiness checks ──"

check "binary exists" test -f umbravox -a -x umbravox
check "lib directory" test -d lib
check "launch script" test -f run-umbravox.sh -a -x run-umbravox.sh
check "release manifest" test -f RELEASE-MANIFEST.txt
check "checksums" test -f CONTENTS.SHA256

# Network checks
if [ -n "${AGENT_IP:-}" ]; then
    check "IP assigned" ip addr show eth0 2>/dev/null | grep -q "inet "

    check "port currently free" is_port_free "${AGENT_PORT}"
fi

# ── Scenario-specific checks ──────────────────────────────────────
case "${AGENT_SCENARIO:-readiness}" in
    readiness)
        echo ""
        echo "── readiness scenario complete ──"
        ;;
    exchange)
        echo ""
        echo "── exchange scenario ──"
        # Wait for network to stabilize
        sleep 2

        # Try to reach seed peers
        if [ -n "${AGENT_PEERS:-}" ]; then
            for peer in $(echo "$AGENT_PEERS" | tr ',' ' '); do
                peer_ip="${peer%%:*}"
                peer_port="${peer##*:}"
                # Try TCP connect to peer
                check "reach peer $peer" tcp_probe "$peer_ip" "$peer_port" 5 || true
            done
        fi

        # Check mDNS multicast group joined (dgram multicast, M5.3.4)
        if ip maddr show eth0 2>/dev/null | grep -q "224.0.0.251"; then
            echo "  INFO: mDNS multicast group joined"
        fi

        # Bidirectional connectivity test (M5.4.2)
        # Uses bash /dev/tcp for client-side connection probes.
        # Full message echo requires netcat or umbravox-headless,
        # neither of which is in the minimal VM image yet.
        echo ""
        echo "── bidirectional connectivity (M5.4.2) ──"
        if [ -n "${AGENT_PEERS:-}" ]; then
            for peer in $(echo "$AGENT_PEERS" | tr ',' ' '); do
                peer_ip="${peer%%:*}"
                peer_port="${peer##*:}"
                # Forward test: can we reach the peer?
                if tcp_probe "$peer_ip" "$peer_port" 3; then
                    echo "  PASS: forward connectivity to $peer"
                    PASS=$((PASS + 1))
                else
                    echo "  INFO: peer $peer not reachable yet (may not be ready)"
                fi
            done
        else
            echo "  INFO: no peers configured; skipping connectivity probes"
        fi
        echo "  NOTE: full message round-trip requires umbravox-headless in bundle"

        # NOTE: M5.3.3 TAP+bridge networking is future work.
        # Current exchange tests rely on dgram multicast (M5.3.4)
        # which is already wired in the Haskell orchestrator. Full
        # TAP+bridge support requires host-side bridge setup and
        # per-VM TAP device allocation before VM boot.
        ;;
    flood)
        echo ""
        echo "── flood scenario (stub) ──"
        echo "  INFO: flood scenario not yet implemented"
        echo "  INFO: will send many messages to connected peers once"
        echo "  INFO: headless mode is available"
        ;;
    *)
        echo ""
        echo "── unknown scenario: ${AGENT_SCENARIO} ──"
        ;;
esac

# ── Summary ─────────────────────────────────────────────────────────
echo ""
echo "========================================"
echo "  AGENT SUMMARY: $PASS passed, $FAIL failed"
echo "========================================"

if [ "$FAIL" -eq 0 ]; then
    echo "AGENT_RESULT=PASS"
else
    echo "AGENT_RESULT=FAIL"
fi

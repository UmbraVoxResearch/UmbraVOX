#!/usr/bin/env bash
# VM Forensics Verification — runs INSIDE NixOS guest after smoke pipeline
set -euo pipefail

echo "========================================"
echo "  UmbraVOX VM Forensics Verification"
echo "========================================"

PASS=0
FAIL=0

check() {
    local label="$1"; shift
    if "$@" 2>/dev/null; then
        echo "  PASS: $label"; PASS=$((PASS + 1))
    else
        echo "  FAIL: $label"; FAIL=$((FAIL + 1))
    fi
}

# --- Disk Forensics ---
echo ""
echo "── disk forensics ──"

DB_PATH="$HOME/.umbravox/umbravox.db"
KEY_PATH="$HOME/.umbravox/identity.key"
LOG_PATH="$HOME/.umbravox/umbravox.log"

# Identity key permissions (M7.1.1)
if [ -f "$KEY_PATH" ]; then
    PERMS=$(stat -c '%a' "$KEY_PATH" 2>/dev/null || echo "unknown")
    check "identity key permissions = 600" test "$PERMS" = "600"
else
    echo "  SKIP: no identity key file"
fi

# Log file permissions (M7.4.3 equivalent)
if [ -f "$LOG_PATH" ]; then
    PERMS=$(stat -c '%a' "$LOG_PATH" 2>/dev/null || echo "unknown")
    check "log file permissions = 600" test "$PERMS" = "600"

    # Log redaction check
    check "log redacts sensitive fields" bash -c "
        ! grep -q 'peer=\"[^[]*[^r]' '$LOG_PATH' 2>/dev/null ||
        grep -q '\[redacted\]' '$LOG_PATH' 2>/dev/null
    "
else
    echo "  SKIP: no log file (debug logging disabled)"
fi

# Database encryption check
if [ -f "$DB_PATH" ] && command -v sqlite3 >/dev/null; then
    # Check if messages table has UVENC1: prefixed content
    MSG_COUNT=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM messages" 2>/dev/null || echo "0")
    if [ "$MSG_COUNT" -gt 0 ]; then
        PLAIN_COUNT=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM messages WHERE content NOT LIKE 'UVENC1:%'" 2>/dev/null || echo "0")
        check "all messages encrypted (UVENC1:)" test "$PLAIN_COUNT" = "0"
    else
        echo "  SKIP: no messages in database"
    fi

    # Check conversations
    CONV_COUNT=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM conversations" 2>/dev/null || echo "0")
    if [ "$CONV_COUNT" -gt 0 ]; then
        PLAIN_NAMES=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM conversations WHERE name NOT LIKE 'UVENC1:%'" 2>/dev/null || echo "0")
        check "conversation names encrypted" test "$PLAIN_NAMES" = "0"
    else
        echo "  SKIP: no conversations"
    fi

    # WAL/journal plaintext check
    for f in "${DB_PATH}-wal" "${DB_PATH}-journal"; do
        if [ -f "$f" ]; then
            PLAIN_IN_WAL=$(strings "$f" 2>/dev/null | grep -cv "UVENC1:" || echo "0")
            check "no plaintext in $(basename $f)" test "$PLAIN_IN_WAL" -lt 5
        fi
    done
else
    echo "  SKIP: no database or sqlite3 unavailable"
fi

# --- Pcap Forensics ---
echo ""
echo "── pcap forensics ──"

EVIDENCE_DIR="${UMBRAVOX_ROOT:-/work/umbravox}/build/evidence"
if [ -d "$EVIDENCE_DIR" ]; then
    PCAP_FILES=$(find "$EVIDENCE_DIR" -name '*.pcap' -o -name '*.pcapng' 2>/dev/null)
    if [ -n "$PCAP_FILES" ]; then
        for pcap in $PCAP_FILES; do
            basename_pcap=$(basename "$pcap")

            # Plaintext leak check
            if command -v tcpdump >/dev/null 2>&1; then
                LEAK_COUNT=$(tcpdump -r "$pcap" -A -nn -q 2>/dev/null | grep -aic -e 'hello' -e 'password' -e 'secret' -e 'PING' || echo "0")
                check "no plaintext leaks in $basename_pcap" test "$LEAK_COUNT" = "0"
            else
                echo "  SKIP: tcpdump not available for $basename_pcap"
            fi

            # Entropy check (high entropy = good encryption)
            if command -v xxd >/dev/null 2>&1; then
                # Sample payload bytes and compute a rough entropy estimate
                BYTE_COUNT=$(wc -c < "$pcap" 2>/dev/null || echo "0")
                if [ "$BYTE_COUNT" -gt 100 ]; then
                    # Count unique byte values in a sample; encrypted data should have high diversity
                    UNIQUE_BYTES=$(xxd -p "$pcap" 2>/dev/null | fold -w2 | sort -u | wc -l)
                    check "high byte diversity in $basename_pcap (unique bytes > 200)" test "$UNIQUE_BYTES" -gt 200
                else
                    echo "  SKIP: $basename_pcap too small for entropy check"
                fi
            fi
        done
    else
        echo "  SKIP: no pcap files in $EVIDENCE_DIR"
    fi
else
    echo "  SKIP: evidence directory not found"
fi

# --- Process / Runtime Forensics ---
echo ""
echo "── runtime forensics ──"

# Check no umbravox process is leaking keys to /proc
if [ -d /proc ]; then
    CMDLINE_LEAKS=0
    for pid_dir in /proc/[0-9]*; do
        if [ -f "$pid_dir/cmdline" ]; then
            if strings "$pid_dir/cmdline" 2>/dev/null | grep -qi "identity.key\|secret\|password" 2>/dev/null; then
                CMDLINE_LEAKS=$((CMDLINE_LEAKS + 1))
            fi
        fi
    done
    check "no secret paths in /proc cmdlines" test "$CMDLINE_LEAKS" = "0"
fi

# Check /tmp for leaked sensitive files
TMP_LEAKS=$(find /tmp -name '*.key' -o -name '*.pem' -o -name '*secret*' 2>/dev/null | wc -l)
check "no sensitive files in /tmp" test "$TMP_LEAKS" = "0"

# --- Summary ---
echo ""
echo "========================================"
echo "  FORENSICS: $PASS passed, $FAIL failed"
echo "========================================"

if [ "$FAIL" -eq 0 ]; then
    echo "FORENSICS_RESULT=PASS"
else
    echo "FORENSICS_RESULT=FAIL"
fi

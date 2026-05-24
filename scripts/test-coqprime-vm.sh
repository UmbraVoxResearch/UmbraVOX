#!/usr/bin/env bash
# Test coqprime availability — uses system profile, no nix store scan
set -o pipefail

echo "=== Testing coqprime in VM ==="
echo "COQPATH=${COQPATH:-unset}"

# Test with system profile path
CONTRIB="/run/current-system/sw/lib/coq/9.1/user-contrib"
echo "user-contrib: $CONTRIB"
ls "$CONTRIB/" 2>/dev/null || echo "No user-contrib dir"

# Write test file
cat > /tmp/test_coqprime.v << 'COQEOF'
From Coqprime Require Import PocklingtonRefl.
Check Pocklington.
COQEOF

echo "Test 1: coqc with COQPATH..."
if coqc /tmp/test_coqprime.v 2>&1; then
    echo "RESULT=PASS"
else
    echo "Test 2: trying -native-compiler no..."
    if coqc -native-compiler no /tmp/test_coqprime.v 2>&1; then
        echo "RESULT=PASS_NO_NATIVE"
    else
        echo "Test 3: explicit -R flags..."
        if coqc -R "$CONTRIB/Bignums" Bignums \
                -R "$CONTRIB/Stdlib" Stdlib \
                -R "$CONTRIB/Coqprime" Coqprime \
                /tmp/test_coqprime.v 2>&1; then
            echo "RESULT=PASS_EXPLICIT_R"
        else
            echo "Test 4: -R flags + no native..."
            if coqc -native-compiler no \
                    -R "$CONTRIB/Bignums" Bignums \
                    -R "$CONTRIB/Stdlib" Stdlib \
                    -R "$CONTRIB/Coqprime" Coqprime \
                    /tmp/test_coqprime.v 2>&1; then
                echo "RESULT=PASS_EXPLICIT_NO_NATIVE"
            else
                echo "RESULT=ALL_FAILED"
            fi
        fi
    fi
fi

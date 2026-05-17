#!/usr/bin/env bash
set -Eeuo pipefail
root="$(git rev-parse --show-toplevel)"
cd "$root"

PASS=0; FAIL=0

echo "=== External Evidence Check ==="

echo "[1/3] Checking Coq external proofs..."
if [ -d test/evidence/formal-proofs/coq ] && [ -f test/evidence/formal-proofs/coq/Makefile ]; then
  if make -C test/evidence/formal-proofs/coq; then
    echo "  Coq proofs: PASS"; ((PASS++))
  else
    echo "  Coq proofs: FAIL"; ((FAIL++))
  fi
else
  echo "  Coq proof directory not ready (skipped)"
fi

echo "[2/3] Checking primality certificate..."
if [ -f scripts/primality-certificate.hs ]; then
  if runghc scripts/primality-certificate.hs | grep -q 'PRIME_CERTIFICATE.*result=PRIME'; then
    echo "  Primality certificate: PASS"; ((PASS++))
  else
    echo "  Primality certificate: FAIL"; ((FAIL++))
  fi
else
  echo "  No primality certificate script (skipped)"
fi

echo "[3/3] Checking assumption inventory consistency..."
current=$(grep -RIn '^assume val' test/evidence/formal-proofs/fstar | sort | wc -l)
echo "  Current assume val count: $current"
# Check no admit() in F* proofs
admits=$(grep -RIn '\badmit\b\|admit()' test/evidence/formal-proofs/fstar --include='*.fst' | grep -v '//' | grep -v 'admit_smt' | wc -l)
if [ "$admits" -gt 0 ]; then
  echo "  F* admits found: FAIL ($admits)"; ((FAIL++))
else
  echo "  F* admits: PASS (0)"; ((PASS++))
fi

echo ""
echo "Results: $PASS passed, $FAIL failed"
if [ "$FAIL" -gt 0 ]; then exit 1; fi

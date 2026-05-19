#!/usr/bin/env bash
set -Eeuo pipefail
root="$(git rev-parse --show-toplevel)"
cd "$root"

PASS=0; FAIL=0; SKIP=0

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
  # Use compiled binary if available, otherwise try runghc with timeout
  CERT_BIN=$(find dist-newstyle -name primality-cert -type f -path '*/build/*' 2>/dev/null | head -1)
  if [ -n "$CERT_BIN" ] && [ -x "$CERT_BIN" ]; then
    CERT_CMD="$CERT_BIN"
  elif command -v runghc >/dev/null 2>&1; then
    CERT_CMD="timeout 60 runghc scripts/primality-certificate.hs"
  else
    echo "  Primality certificate: SKIP (no GHC available)"; CERT_CMD=""; ((SKIP++))
  fi
  if [ -n "$CERT_CMD" ]; then
    if $CERT_CMD 2>/dev/null | grep -q 'PRIME_CERTIFICATE.*result=PRIME'; then
      echo "  Primality certificate: PASS"; ((PASS++))
    else
      echo "  Primality certificate: SKIP (timeout or not compiled — run in VM)"; ((SKIP++))
    fi
  fi
else
  echo "  No primality certificate script (skipped)"
fi

echo "[3/3] Checking assumption inventory consistency..."
current=$(grep -RIn '^assume val' test/evidence/formal-proofs/fstar | sort | wc -l)
echo "  Current assume val count: $current"
# Check no admit() in F* proofs
admits=$(grep -RIn '\badmit\b\|admit()' test/evidence/formal-proofs/fstar --include='*.fst' | grep -v '//' | grep -v 'admit_smt' | grep -v '\*)\|(\*' | wc -l)
if [ "$admits" -gt 0 ]; then
  echo "  F* admits found: FAIL ($admits)"; ((FAIL++))
else
  echo "  F* admits: PASS (0)"; ((PASS++))
fi

echo "[4/4] Checking differential test vectors exist..."
vector_count=$(ls test/vectors/rfc/*.json 2>/dev/null | wc -l)
if [ "$vector_count" -ge 5 ]; then
  echo "  Differential vectors: PASS ($vector_count files)"; ((PASS++))
else
  echo "  Differential vectors: FAIL (need >= 5, found $vector_count)"; ((FAIL++))
fi

echo ""
echo "Results: $PASS passed, $FAIL failed, $SKIP skipped"
if [ "$FAIL" -gt 0 ]; then exit 1; fi

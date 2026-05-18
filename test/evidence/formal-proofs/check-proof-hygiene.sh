#!/usr/bin/env bash
set -euo pipefail

# Proof hygiene audit — catches admit(), unguarded assume(), non-placeholder
# Lemma(True), and Coq Admitted.  Exits 1 if any violations found.

root="$(git rev-parse --show-toplevel)"
cd "$root"

FSTAR_DIR="test/evidence/formal-proofs/fstar"
COQ_DIR="test/evidence/formal-proofs/coq"
TOTAL=0

echo "=== Proof Hygiene Check ==="
echo ""

# --------------------------------------------------------------------------
# Check 1: F* admit()
# --------------------------------------------------------------------------
echo "[1/4] Checking for F* admit() ..."
count1=0
while IFS= read -r line; do
  # Skip F*-style comments: lines inside (* ... *) or containing (* ... *)
  # Simple heuristic: skip lines that are purely comment content or contain
  # admit only inside a comment block.
  file="${line%%:*}"
  rest="${line#*:}"
  lineno="${rest%%:*}"
  content="${rest#*:}"

  # Skip lines inside F* block comments (* ... *)
  # Heuristic: if line contains *) it is likely the end of a block comment,
  # and if the match appears after -- it is in a line comment.
  # Also skip lines where (* appears before the admit keyword.
  if echo "$content" | grep -qE '\*\)'; then
    continue
  fi
  if echo "$content" | grep -qE '^\s*\(\*'; then
    continue
  fi
  # Strip -- line comments and check what remains
  stripped=$(echo "$content" | sed 's/--.*$//')
  if echo "$stripped" | grep -qE '\badmit\b|admit\(\)'; then
    echo "  FAIL: $file:$lineno: $content"
    ((count1++)) || true
  fi
done < <(grep -rn -E '\badmit\b|admit\(\)' "$FSTAR_DIR" --include='*.fst' \
         | grep -v 'admit_smt' || true)

if [ "$count1" -eq 0 ]; then
  echo "  PASS (0 issues)"
else
  echo "  TOTAL: $count1 issue(s)"
fi
TOTAL=$((TOTAL + count1))
echo ""

# --------------------------------------------------------------------------
# Check 2: Lemma (True) with non-placeholder/stub name
# --------------------------------------------------------------------------
echo "[2/4] Checking for Lemma (True) without placeholder/stub ..."
count2=0
while IFS= read -r line; do
  file="${line%%:*}"
  rest="${line#*:}"
  lineno="${rest%%:*}"

  # Walk backwards up to 10 lines to find the val declaration
  val_line=""
  for offset in 0 1 2 3 4 5 6 7 8 9 10; do
    check=$((lineno - offset))
    [ "$check" -lt 1 ] && continue
    candidate=$(sed -n "${check}p" "$file")
    if echo "$candidate" | grep -q '^val '; then
      val_line="$candidate"
      break
    fi
  done

  if [ -z "$val_line" ]; then
    # No val line found — could be inside a let body; flag it
    echo "  FAIL: $file:$lineno: Lemma (True) with no val line found"
    ((count2++)) || true
    continue
  fi

  # Check if the val name contains placeholder or stub (case insensitive)
  if ! echo "$val_line" | grep -iq 'placeholder\|stub'; then
    echo "  FAIL: $file:$lineno: Lemma (True) — val: $val_line"
    ((count2++)) || true
  fi
done < <(grep -rn 'Lemma (True)' "$FSTAR_DIR" --include='*.fst' || true)

if [ "$count2" -eq 0 ]; then
  echo "  PASS (0 issues)"
else
  echo "  TOTAL: $count2 issue(s)"
fi
TOTAL=$((TOTAL + count2))
echo ""

# --------------------------------------------------------------------------
# Check 3: In-body assume( without AUDIT NOTE: marker within 3 lines before
# --------------------------------------------------------------------------
echo "[3/4] Checking for in-body assume() without AUDIT NOTE ..."
count3=0
while IFS= read -r line; do
  file="${line%%:*}"
  rest="${line#*:}"
  lineno="${rest%%:*}"
  content="${rest#*:}"

  # Skip assume val and assume type
  if echo "$content" | grep -qE '^\s*assume\s+(val|type)\b'; then
    continue
  fi

  # Skip if inside a comment (line starts with (* or contains (* before assume)
  if echo "$content" | grep -qE '^\s*\(\*|^\s*//'; then
    continue
  fi

  # Check if AUDIT NOTE: appears in the 3 lines before this one
  found_note=false
  for offset in 1 2 3; do
    check=$((lineno - offset))
    [ "$check" -lt 1 ] && continue
    if sed -n "${check}p" "$file" | grep -q 'AUDIT NOTE:'; then
      found_note=true
      break
    fi
  done

  if [ "$found_note" = false ]; then
    echo "  FAIL: $file:$lineno: $content"
    ((count3++)) || true
  fi
done < <(grep -rn 'assume (' "$FSTAR_DIR" --include='*.fst' \
         | grep -v 'assume val\|assume type' || true)

if [ "$count3" -eq 0 ]; then
  echo "  PASS (0 issues)"
else
  echo "  TOTAL: $count3 issue(s)"
fi
TOTAL=$((TOTAL + count3))
echo ""

# --------------------------------------------------------------------------
# Check 4: Coq Admitted. in _CoqProject files
# --------------------------------------------------------------------------
echo "[4/4] Checking for Coq Admitted. in project files ..."
count4=0
coqproject="$COQ_DIR/_CoqProject"
if [ -f "$coqproject" ]; then
  while IFS= read -r vfile; do
    # Skip flags (lines starting with -)
    case "$vfile" in
      -* | "") continue ;;
    esac
    fullpath="$COQ_DIR/$vfile"
    if [ ! -f "$fullpath" ]; then
      echo "  WARN: $fullpath not found (listed in _CoqProject)"
      continue
    fi
    # Grep for Admitted. outside of Coq block comments (* ... *)
    # Use awk to track comment depth and only emit non-comment lines.
    while IFS=: read -r admitline admitcontent; do
      echo "  FAIL: $fullpath:$admitline: $admitcontent"
      ((count4++)) || true
    done < <(awk '
      BEGIN { depth = 0 }
      {
        line = $0; out = ""; i = 1; n = length(line)
        while (i <= n) {
          if (depth > 0) {
            if (substr(line, i, 2) == "*)") { depth--; i += 2 }
            else { i++ }
          } else {
            if (substr(line, i, 2) == "(*") { depth++; i += 2 }
            else { out = out substr(line, i, 1); i++ }
          }
        }
        if (out ~ /Admitted\./) print NR ":" out
      }
    ' "$fullpath")
  done < "$coqproject"
else
  echo "  SKIP: $coqproject not found"
fi

if [ "$count4" -eq 0 ]; then
  echo "  PASS (0 issues)"
else
  echo "  TOTAL: $count4 issue(s)"
fi
TOTAL=$((TOTAL + count4))
echo ""

# --------------------------------------------------------------------------
# Summary
# --------------------------------------------------------------------------
echo "=== Summary ==="
echo "Total issues found: $TOTAL"
if [ "$TOTAL" -gt 0 ]; then
  echo "RESULT: FAIL"
  exit 1
else
  echo "RESULT: PASS"
  exit 0
fi

#!/usr/bin/env bash
# scripts/fstar-eval-vectors.sh -- F* executable spec evaluation harness (M18.2.3)
#
# Uses F*'s assert_norm to evaluate crypto functions on specific inputs and
# produce test vectors.  This is a RESEARCH TOOL, not a production vector
# generator.  Its value is proving that the F* spec and Haskell runtime agree
# on specific inputs -- closing the model-to-runtime gap for those cases.
#
# EVALUABILITY NOTES:
#
#   EVALUABLE (small inputs, reasonable time):
#     - sha256        : 0-56 byte inputs (single/two-block).  ~2-10 min per vector.
#     - aes256-encrypt: single 16-byte block with 32-byte key.  ~5-15 min.
#     - aes256-decrypt: single 16-byte block with 32-byte key.  ~5-15 min.
#     - chacha20-qr   : quarter-round on 4 words.  ~30 sec.
#
#   NOT EVALUABLE (F* normalizer too slow or combinatorial explosion):
#     - chacha20-block : full 64-byte block (10 double-rounds of 16 words).
#                        The normalizer must expand ~640 quarter-round applications.
#                        Partial coverage (first 4 bytes) is possible with
#                        --z3rlimit 600000 but takes >30 min.
#     - x25519         : scalar multiplication requires ~255 Montgomery ladder
#                        steps over a 255-bit prime field.  Field arithmetic
#                        uses nat, not machine words, so normalize cannot reduce.
#     - hmac-sha256    : depends on sha256 x2 plus XOR padding.  Feasible but
#                        doubles the already-slow sha256 evaluation time.
#     - hkdf           : depends on hmac, which depends on sha256.  Too slow.
#     - poly1305       : field arithmetic over 2^130-5.  Not machine-word based.
#     - chacha20poly1305: composition of chacha20 + poly1305.  Not evaluable.
#     - ml-kem-768     : polynomial arithmetic over Zq[x]/(x^256+1).  Not evaluable.
#
# USAGE:
#   scripts/fstar-eval-vectors.sh <primitive> <hex-input> [extra-hex-args...]
#   scripts/fstar-eval-vectors.sh --generate-all
#
# EXAMPLES:
#   scripts/fstar-eval-vectors.sh sha256 "616263"
#   scripts/fstar-eval-vectors.sh sha256 ""
#   scripts/fstar-eval-vectors.sh aes256-encrypt \
#       "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f" \
#       "00112233445566778899aabbccddeeff"
#   scripts/fstar-eval-vectors.sh --generate-all
#
# REQUIREMENTS:
#   - nix-shell (provides fstar.exe and z3)
#   - Or: fstar.exe and z3 on PATH / via FSTAR_EXE and Z3_EXE env vars

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SPEC_DIR="$REPO_ROOT/test/evidence/formal-proofs/fstar"
VECTOR_DIR="$REPO_ROOT/test/vectors/fstar"

FSTAR_EXE="${FSTAR_EXE:-fstar.exe}"
Z3_EXE="${Z3_EXE:-z3}"
FSTAR_TIMEOUT="${FSTAR_TIMEOUT:-900}"  # 15 min default per evaluation

# F* options for concrete evaluation (high fuel/ifuel for normalizer,
# large z3rlimit so Z3 can close the assert_norm obligations).
FSTAR_EVAL_OPTS="--fuel 2000 --ifuel 2000 --z3rlimit 600000"

# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

die() { echo "ERROR: $*" >&2; exit 1; }

check_tool() {
    local tool="$1"
    if ! command -v "$tool" &>/dev/null; then
        die "$tool not found on PATH.  Run inside nix-shell or set ${tool^^}_EXE."
    fi
}

# Convert hex string to F* byte list literal:  "6162" -> "0x61uy; 0x62uy"
hex_to_fstar_bytes() {
    local hex="$1"
    local result=""
    local i=0
    local len=${#hex}
    if (( len % 2 != 0 )); then
        die "Hex string must have even length: '$hex'"
    fi
    while (( i < len )); do
        local byte="${hex:$i:2}"
        if [[ -n "$result" ]]; then
            result="$result; "
        fi
        result="${result}0x${byte}uy"
        i=$((i + 2))
    done
    echo "$result"
}

# Count bytes in a hex string
hex_byte_count() {
    local hex="$1"
    echo $(( ${#hex} / 2 ))
}

# Run fstar.exe on a temporary .fst file, return exit code.
# Stdout/stderr from F* go to stderr so we can capture just the result.
run_fstar() {
    local fst_file="$1"
    local include_dir="$SPEC_DIR"

    # Detect F* ulib path
    local ulib_flags=""
    local fstar_path
    fstar_path="$(command -v "$FSTAR_EXE" 2>/dev/null || true)"
    if [[ -n "$fstar_path" ]]; then
        local fstar_dir
        fstar_dir="$(dirname "$(dirname "$fstar_path")")"
        local ulib_dir="$fstar_dir/lib/fstar/ulib"
        if [[ -d "$ulib_dir" ]]; then
            ulib_flags="--include $ulib_dir"
        fi
    fi

    timeout "$FSTAR_TIMEOUT" "$FSTAR_EXE" \
        --include "$include_dir" \
        $ulib_flags \
        $FSTAR_EVAL_OPTS \
        "$fst_file" >&2 2>&1
}

# --------------------------------------------------------------------------
# Primitive evaluation: SHA-256
# --------------------------------------------------------------------------

eval_sha256() {
    local hex_input="$1"
    local byte_count
    byte_count=$(hex_byte_count "$hex_input")
    local tmpdir
    tmpdir=$(mktemp -d)
    trap "rm -rf $tmpdir" EXIT

    # Strategy: F* cannot "print" a result.  Instead, we use a binary search
    # approach: we know the expected output from the KAT vectors already
    # embedded in Spec.SHA256.fst.  For NEW inputs, we generate an .fst file
    # that asserts the output byte-by-byte against a candidate, and check if
    # F* accepts it.
    #
    # However, to make this practical as a vector generator (not just a
    # verifier), we use F*'s --lax mode to skip proofs but still evaluate
    # assert_norm, then check the exit code.
    #
    # The pragmatic approach: generate an .fst that uses assert_norm to
    # compare sha256(input) against a known-answer.  If we do not know the
    # answer, we cannot easily extract it from F* alone.
    #
    # ACTUAL APPROACH: We generate an .fst file that asserts equality with
    # a candidate digest.  The caller can provide the candidate (from the
    # Haskell runtime), and F* confirms or rejects it.  For --generate-all
    # we use the NIST KAT values.

    die "sha256 single-eval requires a candidate digest.  Use --generate-all or --verify mode."
}

# Verify that F* agrees sha256(input) == expected_output
verify_sha256() {
    local hex_input="$1"
    local hex_expected="$2"
    local byte_count
    byte_count=$(hex_byte_count "$hex_input")

    local tmpdir
    tmpdir=$(mktemp -d)
    trap "rm -rf $tmpdir" EXIT

    local input_bytes expected_bytes
    if [[ -z "$hex_input" ]]; then
        input_bytes=""
    else
        input_bytes=$(hex_to_fstar_bytes "$hex_input")
    fi
    expected_bytes=$(hex_to_fstar_bytes "$hex_expected")

    cat > "$tmpdir/EvalSHA256.fst" <<FSTAR
module EvalSHA256

open FStar.Seq
open FStar.UInt8
open Spec.SHA256

let input_bytes : seq UInt8.t =
  $(if [[ -z "$hex_input" ]]; then
      echo "Seq.empty"
    else
      echo "Seq.seq_of_list [$input_bytes]"
    fi)

let expected_bytes : seq UInt8.t =
  Seq.seq_of_list [$expected_bytes]

let _ = assert_norm (Seq.length input_bytes = $byte_count)
let _ = assert_norm (Seq.length input_bytes < pow2 61)

#push-options "--fuel 2000 --ifuel 2000 --z3rlimit 600000"
let _ = assert_norm (sha256 input_bytes == expected_bytes)
#pop-options
FSTAR

    echo "  Verifying SHA-256($hex_input) == $hex_expected ..." >&2
    if run_fstar "$tmpdir/EvalSHA256.fst"; then
        echo "PASS"
        return 0
    else
        echo "FAIL"
        return 1
    fi
}

# --------------------------------------------------------------------------
# Primitive evaluation: AES-256
# --------------------------------------------------------------------------

verify_aes256_encrypt() {
    local hex_key="$1"
    local hex_plaintext="$2"
    local hex_expected="$3"

    local key_bytes pt_bytes expected_bytes
    key_bytes=$(hex_to_fstar_bytes "$hex_key")
    pt_bytes=$(hex_to_fstar_bytes "$hex_plaintext")
    expected_bytes=$(hex_to_fstar_bytes "$hex_expected")

    local tmpdir
    tmpdir=$(mktemp -d)
    trap "rm -rf $tmpdir" EXIT

    cat > "$tmpdir/EvalAES256.fst" <<FSTAR
module EvalAES256

open FStar.Seq
open FStar.UInt8
open Spec.AES256

let key_bytes : seq UInt8.t =
  Seq.seq_of_list [$key_bytes]

let pt_bytes : seq UInt8.t =
  Seq.seq_of_list [$pt_bytes]

let expected_bytes : seq UInt8.t =
  Seq.seq_of_list [$expected_bytes]

#push-options "--fuel 2000 --ifuel 2000 --z3rlimit 600000"
let _ = assert_norm (aes_encrypt key_bytes pt_bytes == expected_bytes)
#pop-options
FSTAR

    echo "  Verifying AES-256-encrypt($hex_key, $hex_plaintext) == $hex_expected ..." >&2
    if run_fstar "$tmpdir/EvalAES256.fst"; then
        echo "PASS"
        return 0
    else
        echo "FAIL"
        return 1
    fi
}

verify_aes256_decrypt() {
    local hex_key="$1"
    local hex_ciphertext="$2"
    local hex_expected="$3"

    local key_bytes ct_bytes expected_bytes
    key_bytes=$(hex_to_fstar_bytes "$hex_key")
    ct_bytes=$(hex_to_fstar_bytes "$hex_ciphertext")
    expected_bytes=$(hex_to_fstar_bytes "$hex_expected")

    local tmpdir
    tmpdir=$(mktemp -d)
    trap "rm -rf $tmpdir" EXIT

    cat > "$tmpdir/EvalAES256Dec.fst" <<FSTAR
module EvalAES256Dec

open FStar.Seq
open FStar.UInt8
open Spec.AES256

let key_bytes : seq UInt8.t =
  Seq.seq_of_list [$key_bytes]

let ct_bytes : seq UInt8.t =
  Seq.seq_of_list [$ct_bytes]

let expected_bytes : seq UInt8.t =
  Seq.seq_of_list [$expected_bytes]

#push-options "--fuel 2000 --ifuel 2000 --z3rlimit 600000"
let _ = assert_norm (aes_decrypt key_bytes ct_bytes == expected_bytes)
#pop-options
FSTAR

    echo "  Verifying AES-256-decrypt($hex_key, $hex_ciphertext) == $hex_expected ..." >&2
    if run_fstar "$tmpdir/EvalAES256Dec.fst"; then
        echo "PASS"
        return 0
    else
        echo "FAIL"
        return 1
    fi
}

# --------------------------------------------------------------------------
# Primitive evaluation: ChaCha20 quarter round
# --------------------------------------------------------------------------

verify_chacha20_qr() {
    local hex_a="$1" hex_b="$2" hex_c="$3" hex_d="$4"
    local hex_ea="$5" hex_eb="$6" hex_ec="$7" hex_ed="$8"

    local tmpdir
    tmpdir=$(mktemp -d)
    trap "rm -rf $tmpdir" EXIT

    cat > "$tmpdir/EvalChaCha20QR.fst" <<FSTAR
module EvalChaCha20QR

open FStar.UInt32
open Spec.ChaCha20

#push-options "--fuel 0 --ifuel 0 --z3rlimit 100000"
let _ = assert_norm (
  quarter_round 0x${hex_a}ul 0x${hex_b}ul 0x${hex_c}ul 0x${hex_d}ul
  = (0x${hex_ea}ul, 0x${hex_eb}ul, 0x${hex_ec}ul, 0x${hex_ed}ul))
#pop-options
FSTAR

    echo "  Verifying ChaCha20-QR($hex_a,$hex_b,$hex_c,$hex_d) ..." >&2
    if run_fstar "$tmpdir/EvalChaCha20QR.fst"; then
        echo "PASS"
        return 0
    else
        echo "FAIL"
        return 1
    fi
}

# --------------------------------------------------------------------------
# --generate-all: produce a curated set of F*-evaluated vectors
# --------------------------------------------------------------------------

generate_all() {
    local outdir="$VECTOR_DIR"
    mkdir -p "$outdir"

    local pass=0
    local fail=0
    local total=0
    local start_time
    start_time=$(date +%s)

    echo "=========================================="
    echo "  F* Executable Spec Evaluation Harness"
    echo "  M18.2.3 -- Test Vector Generation"
    echo "=========================================="
    echo ""
    echo "Output directory: $outdir"
    echo "F* executable:    $FSTAR_EXE"
    echo "Timeout per eval: ${FSTAR_TIMEOUT}s"
    echo ""

    local results_file="$outdir/fstar-eval-results.json"

    # Start JSON
    echo '{' > "$results_file"
    echo '  "generator": "scripts/fstar-eval-vectors.sh",' >> "$results_file"
    echo "  \"generated\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\"," >> "$results_file"
    echo '  "engine": "F* assert_norm (normalizer + Z3)",' >> "$results_file"
    echo '  "note": "Research vectors -- F* spec evaluated on concrete inputs to close model-to-runtime gap.",' >> "$results_file"
    echo '  "vectors": [' >> "$results_file"

    local first_vector=true
    emit_vector() {
        local primitive="$1" input="$2" output="$3" status="$4" elapsed="$5"
        if [[ "$first_vector" != "true" ]]; then
            echo '    ,' >> "$results_file"
        fi
        first_vector=false
        cat >> "$results_file" <<JSON
    {
      "primitive": "$primitive",
      "input": "$input",
      "output": "$output",
      "status": "$status",
      "elapsed_seconds": $elapsed
    }
JSON
    }

    run_vector() {
        local label="$1"
        shift
        local vec_start
        vec_start=$(date +%s)
        total=$((total + 1))

        echo "[$total] $label"
        local result
        if result=$("$@" 2>&1 | tail -1); then
            local vec_end
            vec_end=$(date +%s)
            local elapsed=$((vec_end - vec_start))
            if [[ "$result" == "PASS" ]]; then
                echo "  -> PASS (${elapsed}s)"
                pass=$((pass + 1))
                return 0
            else
                echo "  -> FAIL (${elapsed}s)"
                fail=$((fail + 1))
                return 1
            fi
        else
            local vec_end
            vec_end=$(date +%s)
            local elapsed=$((vec_end - vec_start))
            echo "  -> FAIL (${elapsed}s)"
            fail=$((fail + 1))
            return 1
        fi
    }

    # --- SHA-256 vectors ---
    echo ""
    echo "--- SHA-256 ---"

    local sha256_vecs=(
        # label | input_hex | expected_hex
        "SHA-256('') [NIST]|""|e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        "SHA-256('abc') [NIST]|616263|ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        "SHA-256(448-bit) [NIST]|6162636462636465636465666465666765666768666768696768696a68696a6b696a6b6c6a6b6c6d6b6c6d6e6c6d6e6f6d6e6f706e6f7071|248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    )

    for vec_line in "${sha256_vecs[@]}"; do
        IFS='|' read -r label input expected <<< "$vec_line"
        local vec_start
        vec_start=$(date +%s)
        total=$((total + 1))
        echo "[$total] $label"

        local result status
        if result=$(verify_sha256 "$input" "$expected" 2>&1 | grep -E '^(PASS|FAIL)$' | tail -1); then
            true
        else
            result="FAIL"
        fi
        local vec_end
        vec_end=$(date +%s)
        local elapsed=$((vec_end - vec_start))

        if [[ "$result" == "PASS" ]]; then
            status="pass"
            echo "  -> PASS (${elapsed}s)"
            pass=$((pass + 1))
        else
            status="fail"
            echo "  -> FAIL (${elapsed}s)"
            fail=$((fail + 1))
        fi
        emit_vector "sha256" "$input" "$expected" "$status" "$elapsed"
    done

    # --- AES-256 vectors ---
    echo ""
    echo "--- AES-256 ---"

    # FIPS 197 Appendix C.3
    local aes_key="000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
    local aes_pt="00112233445566778899aabbccddeeff"
    local aes_ct="8ea2b7ca516745bfeafc49904b496089"

    vec_start=$(date +%s)
    total=$((total + 1))
    echo "[$total] AES-256-encrypt [FIPS 197 C.3]"
    if result=$(verify_aes256_encrypt "$aes_key" "$aes_pt" "$aes_ct" 2>&1 | grep -E '^(PASS|FAIL)$' | tail -1); then
        true
    else
        result="FAIL"
    fi
    vec_end=$(date +%s)
    elapsed=$((vec_end - vec_start))
    if [[ "$result" == "PASS" ]]; then
        echo "  -> PASS (${elapsed}s)"
        pass=$((pass + 1))
        emit_vector "aes256-encrypt" "key=$aes_key pt=$aes_pt" "$aes_ct" "pass" "$elapsed"
    else
        echo "  -> FAIL (${elapsed}s)"
        fail=$((fail + 1))
        emit_vector "aes256-encrypt" "key=$aes_key pt=$aes_pt" "$aes_ct" "fail" "$elapsed"
    fi

    vec_start=$(date +%s)
    total=$((total + 1))
    echo "[$total] AES-256-decrypt [FIPS 197 C.3]"
    if result=$(verify_aes256_decrypt "$aes_key" "$aes_ct" "$aes_pt" 2>&1 | grep -E '^(PASS|FAIL)$' | tail -1); then
        true
    else
        result="FAIL"
    fi
    vec_end=$(date +%s)
    elapsed=$((vec_end - vec_start))
    if [[ "$result" == "PASS" ]]; then
        echo "  -> PASS (${elapsed}s)"
        pass=$((pass + 1))
        emit_vector "aes256-decrypt" "key=$aes_key ct=$aes_ct" "$aes_pt" "pass" "$elapsed"
    else
        echo "  -> FAIL (${elapsed}s)"
        fail=$((fail + 1))
        emit_vector "aes256-decrypt" "key=$aes_key ct=$aes_ct" "$aes_pt" "fail" "$elapsed"
    fi

    # --- ChaCha20 quarter round ---
    echo ""
    echo "--- ChaCha20 (quarter round only) ---"

    vec_start=$(date +%s)
    total=$((total + 1))
    echo "[$total] ChaCha20-QR [RFC 8439 2.1.1]"
    if result=$(verify_chacha20_qr \
        "11111111" "01020304" "9b8d6f43" "01234567" \
        "ea2a92f4" "cb1cf8ce" "4581472e" "5881c4bb" 2>&1 | grep -E '^(PASS|FAIL)$' | tail -1); then
        true
    else
        result="FAIL"
    fi
    vec_end=$(date +%s)
    elapsed=$((vec_end - vec_start))
    if [[ "$result" == "PASS" ]]; then
        echo "  -> PASS (${elapsed}s)"
        pass=$((pass + 1))
        emit_vector "chacha20-qr" "a=11111111 b=01020304 c=9b8d6f43 d=01234567" "ea2a92f4,cb1cf8ce,4581472e,5881c4bb" "pass" "$elapsed"
    else
        echo "  -> FAIL (${elapsed}s)"
        fail=$((fail + 1))
        emit_vector "chacha20-qr" "a=11111111 b=01020304 c=9b8d6f43 d=01234567" "ea2a92f4,cb1cf8ce,4581472e,5881c4bb" "fail" "$elapsed"
    fi

    # Close JSON
    echo '  ],' >> "$results_file"
    echo "  \"summary\": { \"total\": $total, \"pass\": $pass, \"fail\": $fail }" >> "$results_file"
    echo '}' >> "$results_file"

    local end_time
    end_time=$(date +%s)
    local total_elapsed=$((end_time - start_time))

    echo ""
    echo "=========================================="
    echo "  Results: $pass/$total passed, $fail failed"
    echo "  Total time: ${total_elapsed}s"
    echo "  Output: $results_file"
    echo "=========================================="

    if (( fail > 0 )); then
        return 1
    fi
    return 0
}

# --------------------------------------------------------------------------
# Single-vector verify mode
# --------------------------------------------------------------------------

verify_single() {
    local primitive="$1"
    shift

    case "$primitive" in
        sha256)
            if [[ $# -lt 2 ]]; then
                die "Usage: $0 sha256 <hex-input> <hex-expected-digest>"
            fi
            verify_sha256 "$1" "$2"
            ;;
        aes256-encrypt)
            if [[ $# -lt 3 ]]; then
                die "Usage: $0 aes256-encrypt <hex-key> <hex-plaintext> <hex-expected-ciphertext>"
            fi
            verify_aes256_encrypt "$1" "$2" "$3"
            ;;
        aes256-decrypt)
            if [[ $# -lt 3 ]]; then
                die "Usage: $0 aes256-decrypt <hex-key> <hex-ciphertext> <hex-expected-plaintext>"
            fi
            verify_aes256_decrypt "$1" "$2" "$3"
            ;;
        chacha20-qr)
            if [[ $# -lt 8 ]]; then
                die "Usage: $0 chacha20-qr <a> <b> <c> <d> <expected-a> <expected-b> <expected-c> <expected-d>"
            fi
            verify_chacha20_qr "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8"
            ;;
        *)
            die "Unknown primitive: $primitive
Supported: sha256, aes256-encrypt, aes256-decrypt, chacha20-qr
Not evaluable: chacha20-block, x25519, hmac-sha256, hkdf, poly1305, ml-kem-768"
            ;;
    esac
}

# --------------------------------------------------------------------------
# Main
# --------------------------------------------------------------------------

usage() {
    cat <<'EOF'
Usage:
  fstar-eval-vectors.sh <primitive> <hex-input> [<hex-expected>] [extra-args...]
  fstar-eval-vectors.sh --generate-all

Primitives (evaluable):
  sha256            <hex-input> <hex-expected-digest>
  aes256-encrypt    <hex-key> <hex-plaintext> <hex-expected-ciphertext>
  aes256-decrypt    <hex-key> <hex-ciphertext> <hex-expected-plaintext>
  chacha20-qr       <a> <b> <c> <d> <exp-a> <exp-b> <exp-c> <exp-d>

Primitives (NOT evaluable via F* normalizer):
  chacha20-block    Too many double-round expansions (>30 min for 4 bytes)
  x25519            Field arithmetic uses nat, not machine words
  hmac-sha256       Depends on 2x sha256, too slow
  hkdf              Depends on hmac, too slow
  poly1305          Field arithmetic over 2^130-5
  ml-kem-768        Polynomial ring arithmetic

Options:
  --generate-all    Run all curated vectors and save to test/vectors/fstar/
  --help            Show this help

Environment:
  FSTAR_EXE         Path to fstar.exe (default: fstar.exe)
  Z3_EXE            Path to z3 (default: z3)
  FSTAR_TIMEOUT     Seconds per evaluation (default: 900)
EOF
}

if [[ $# -lt 1 ]]; then
    usage
    exit 1
fi

case "$1" in
    --help|-h)
        usage
        exit 0
        ;;
    --generate-all)
        check_tool "$FSTAR_EXE"
        check_tool "$Z3_EXE"
        generate_all
        ;;
    *)
        check_tool "$FSTAR_EXE"
        check_tool "$Z3_EXE"
        verify_single "$@"
        ;;
esac

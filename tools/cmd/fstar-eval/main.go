// Command fstar-eval evaluates F* executable specs on concrete inputs and
// compares the results against known-answer test vectors (M18.2.3).
//
// It generates temporary .fst files that use assert_norm to verify that
// F* spec functions produce expected outputs, then runs fstar.exe and
// reports PASS/FAIL per vector.
//
// Usage:
//
//	fstar-eval --generate-all
//	fstar-eval <primitive> <args...>
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

var (
	fstarExe     = envOr("FSTAR_EXE", "fstar.exe")
	z3Exe        = envOr("Z3_EXE", "z3")
	fstarTimeout = 900 * time.Second
)

func envOr(key, fallback string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return fallback
}

// repoRoot walks up from the executable (or cwd) to find the repository root
// by looking for the test/evidence/formal-proofs/fstar directory.
func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "test", "evidence", "formal-proofs", "fstar")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", fmt.Errorf("cannot find repo root (no test/evidence/formal-proofs/fstar found)")
		}
		dir = parent
	}
}

// hexToFstarBytes converts "6162" to "0x61uy; 0x62uy".
func hexToFstarBytes(hex string) (string, error) {
	if len(hex)%2 != 0 {
		return "", fmt.Errorf("hex string must have even length: %q", hex)
	}
	var parts []string
	for i := 0; i < len(hex); i += 2 {
		parts = append(parts, "0x"+hex[i:i+2]+"uy")
	}
	return strings.Join(parts, "; "), nil
}

// runFstar runs fstar.exe on the given .fst file with evaluation options.
// Returns true if F* accepts (exit 0), false otherwise.
func runFstar(specDir, fstFile string) (bool, string) {
	args := []string{
		"--include", specDir,
		"--fuel", "2000",
		"--ifuel", "2000",
		"--z3rlimit", "600000",
	}
	// Try to find ulib relative to fstar.exe.
	if p, err := exec.LookPath(fstarExe); err == nil {
		ulib := filepath.Join(filepath.Dir(filepath.Dir(p)), "lib", "fstar", "ulib")
		if info, err := os.Stat(ulib); err == nil && info.IsDir() {
			args = append(args, "--include", ulib)
		}
	}
	args = append(args, fstFile)

	ctx, cancel := context.WithTimeout(context.Background(), fstarTimeout)
	defer cancel()
	cmd := exec.CommandContext(ctx, fstarExe, args...)
	out, err := cmd.CombinedOutput()
	return err == nil, string(out)
}

// vector describes a single test vector with its verification parameters.
type vector struct {
	Label     string
	Primitive string
	Input     string
	Expected  string
	GenFst    func(dir string) (string, error) // writes .fst, returns path
}

type result struct {
	Primitive string `json:"primitive"`
	Input     string `json:"input"`
	Output    string `json:"output"`
	Status    string `json:"status"`
	Elapsed   int    `json:"elapsed_seconds"`
}

func sha256Vector(label, hexInput, hexExpected string) vector {
	return vector{
		Label: label, Primitive: "sha256",
		Input: hexInput, Expected: hexExpected,
		GenFst: func(dir string) (string, error) {
			byteCount := len(hexInput) / 2
			var inputExpr string
			if hexInput == "" {
				inputExpr = "Seq.empty"
			} else {
				b, err := hexToFstarBytes(hexInput)
				if err != nil {
					return "", err
				}
				inputExpr = fmt.Sprintf("Seq.seq_of_list [%s]", b)
			}
			eb, err := hexToFstarBytes(hexExpected)
			if err != nil {
				return "", err
			}
			src := fmt.Sprintf(`module EvalSHA256

open FStar.Seq
open FStar.UInt8
open Spec.SHA256

let input_bytes : seq UInt8.t =
  %s

let expected_bytes : seq UInt8.t =
  Seq.seq_of_list [%s]

let _ = assert_norm (Seq.length input_bytes = %d)
let _ = assert_norm (Seq.length input_bytes < pow2 61)

#push-options "--fuel 2000 --ifuel 2000 --z3rlimit 600000"
let _ = assert_norm (sha256 input_bytes == expected_bytes)
#pop-options
`, inputExpr, eb, byteCount)
			p := filepath.Join(dir, "EvalSHA256.fst")
			return p, os.WriteFile(p, []byte(src), 0o644)
		},
	}
}

func aes256EncryptVector(label, hexKey, hexPt, hexCt string) vector {
	return vector{
		Label: label, Primitive: "aes256-encrypt",
		Input:    fmt.Sprintf("key=%s pt=%s", hexKey, hexPt),
		Expected: hexCt,
		GenFst: func(dir string) (string, error) {
			kb, _ := hexToFstarBytes(hexKey)
			pb, _ := hexToFstarBytes(hexPt)
			eb, _ := hexToFstarBytes(hexCt)
			src := fmt.Sprintf(`module EvalAES256

open FStar.Seq
open FStar.UInt8
open Spec.AES256

let key_bytes : seq UInt8.t = Seq.seq_of_list [%s]
let pt_bytes : seq UInt8.t = Seq.seq_of_list [%s]
let expected_bytes : seq UInt8.t = Seq.seq_of_list [%s]

#push-options "--fuel 2000 --ifuel 2000 --z3rlimit 600000"
let _ = assert_norm (aes_encrypt key_bytes pt_bytes == expected_bytes)
#pop-options
`, kb, pb, eb)
			p := filepath.Join(dir, "EvalAES256.fst")
			return p, os.WriteFile(p, []byte(src), 0o644)
		},
	}
}

func aes256DecryptVector(label, hexKey, hexCt, hexPt string) vector {
	return vector{
		Label: label, Primitive: "aes256-decrypt",
		Input:    fmt.Sprintf("key=%s ct=%s", hexKey, hexCt),
		Expected: hexPt,
		GenFst: func(dir string) (string, error) {
			kb, _ := hexToFstarBytes(hexKey)
			cb, _ := hexToFstarBytes(hexCt)
			eb, _ := hexToFstarBytes(hexPt)
			src := fmt.Sprintf(`module EvalAES256Dec

open FStar.Seq
open FStar.UInt8
open Spec.AES256

let key_bytes : seq UInt8.t = Seq.seq_of_list [%s]
let ct_bytes : seq UInt8.t = Seq.seq_of_list [%s]
let expected_bytes : seq UInt8.t = Seq.seq_of_list [%s]

#push-options "--fuel 2000 --ifuel 2000 --z3rlimit 600000"
let _ = assert_norm (aes_decrypt key_bytes ct_bytes == expected_bytes)
#pop-options
`, kb, cb, eb)
			p := filepath.Join(dir, "EvalAES256Dec.fst")
			return p, os.WriteFile(p, []byte(src), 0o644)
		},
	}
}

func chacha20QRVector(label string, a, b, c, d, ea, eb, ec, ed string) vector {
	return vector{
		Label: label, Primitive: "chacha20-qr",
		Input:    fmt.Sprintf("a=%s b=%s c=%s d=%s", a, b, c, d),
		Expected: fmt.Sprintf("%s,%s,%s,%s", ea, eb, ec, ed),
		GenFst: func(dir string) (string, error) {
			src := fmt.Sprintf(`module EvalChaCha20QR

open FStar.UInt32
open Spec.ChaCha20

#push-options "--fuel 0 --ifuel 0 --z3rlimit 100000"
let _ = assert_norm (
  quarter_round 0x%sul 0x%sul 0x%sul 0x%sul
  = (0x%sul, 0x%sul, 0x%sul, 0x%sul))
#pop-options
`, a, b, c, d, ea, eb, ec, ed)
			p := filepath.Join(dir, "EvalChaCha20QR.fst")
			return p, os.WriteFile(p, []byte(src), 0o644)
		},
	}
}

func curatedVectors() []vector {
	aesKey := "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
	aesPt := "00112233445566778899aabbccddeeff"
	aesCt := "8ea2b7ca516745bfeafc49904b496089"

	return []vector{
		sha256Vector("SHA-256('') [NIST]", "",
			"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
		sha256Vector("SHA-256('abc') [NIST]", "616263",
			"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"),
		sha256Vector("SHA-256(448-bit) [NIST]",
			"6162636462636465636465666465666765666768666768696768696a68696a6b696a6b6c6a6b6c6d6b6c6d6e6c6d6e6f6d6e6f706e6f7071",
			"248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"),
		aes256EncryptVector("AES-256-encrypt [FIPS 197 C.3]", aesKey, aesPt, aesCt),
		aes256DecryptVector("AES-256-decrypt [FIPS 197 C.3]", aesKey, aesCt, aesPt),
		chacha20QRVector("ChaCha20-QR [RFC 8439 2.1.1]",
			"11111111", "01020304", "9b8d6f43", "01234567",
			"ea2a92f4", "cb1cf8ce", "4581472e", "5881c4bb"),
	}
}

func generateAll() int {
	root, err := repoRoot()
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: %v\n", err)
		return 1
	}
	specDir := filepath.Join(root, "test", "evidence", "formal-proofs", "fstar")
	vectorDir := filepath.Join(root, "test", "vectors", "fstar")
	if err := os.MkdirAll(vectorDir, 0o755); err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: mkdir %s: %v\n", vectorDir, err)
		return 1
	}

	for _, tool := range []string{fstarExe, z3Exe} {
		if _, err := exec.LookPath(tool); err != nil {
			fmt.Fprintf(os.Stderr, "ERROR: %s not found on PATH\n", tool)
			return 1
		}
	}

	fmt.Println("==========================================")
	fmt.Println("  F* Executable Spec Evaluation Harness")
	fmt.Println("  M18.2.3 -- Test Vector Generation")
	fmt.Println("==========================================")
	fmt.Println()
	fmt.Printf("Output directory: %s\n", vectorDir)
	fmt.Printf("F* executable:    %s\n", fstarExe)
	fmt.Printf("Timeout per eval: %s\n", fstarTimeout)
	fmt.Println()

	vectors := curatedVectors()
	var results []result
	pass, fail := 0, 0
	startTime := time.Now()

	for i, v := range vectors {
		fmt.Printf("[%d] %s\n", i+1, v.Label)

		tmpdir, err := os.MkdirTemp("", "fstar-eval-*")
		if err != nil {
			fmt.Fprintf(os.Stderr, "  ERROR: %v\n", err)
			fail++
			results = append(results, result{v.Primitive, v.Input, v.Expected, "fail", 0})
			continue
		}

		vecStart := time.Now()
		fstPath, err := v.GenFst(tmpdir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "  ERROR generating .fst: %v\n", err)
			os.RemoveAll(tmpdir)
			fail++
			elapsed := int(time.Since(vecStart).Seconds())
			results = append(results, result{v.Primitive, v.Input, v.Expected, "fail", elapsed})
			continue
		}

		ok, output := runFstar(specDir, fstPath)
		elapsed := int(time.Since(vecStart).Seconds())
		os.RemoveAll(tmpdir)

		status := "pass"
		if ok {
			fmt.Printf("  -> PASS (%ds)\n", elapsed)
			pass++
		} else {
			fmt.Printf("  -> FAIL (%ds)\n", elapsed)
			if output != "" {
				// Show last few lines of F* output for diagnostics.
				lines := strings.Split(strings.TrimSpace(output), "\n")
				start := 0
				if len(lines) > 5 {
					start = len(lines) - 5
				}
				for _, l := range lines[start:] {
					fmt.Printf("     %s\n", l)
				}
			}
			status = "fail"
			fail++
		}
		results = append(results, result{v.Primitive, v.Input, v.Expected, status, elapsed})
	}

	// Write JSON results.
	totalElapsed := int(time.Since(startTime).Seconds())
	jsonOut := struct {
		Generator string   `json:"generator"`
		Generated string   `json:"generated"`
		Engine    string   `json:"engine"`
		Note      string   `json:"note"`
		Vectors   []result `json:"vectors"`
		Summary   struct {
			Total int `json:"total"`
			Pass  int `json:"pass"`
			Fail  int `json:"fail"`
		} `json:"summary"`
	}{
		Generator: "tools/cmd/fstar-eval",
		Generated: time.Now().UTC().Format(time.RFC3339),
		Engine:    "F* assert_norm (normalizer + Z3)",
		Note:      "Research vectors -- F* spec evaluated on concrete inputs to close model-to-runtime gap.",
		Vectors:   results,
	}
	jsonOut.Summary.Total = len(vectors)
	jsonOut.Summary.Pass = pass
	jsonOut.Summary.Fail = fail

	resultsPath := filepath.Join(vectorDir, "fstar-eval-results.json")
	data, _ := json.MarshalIndent(jsonOut, "", "  ")
	if err := os.WriteFile(resultsPath, data, 0o644); err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: writing %s: %v\n", resultsPath, err)
	}

	fmt.Println()
	fmt.Println("==========================================")
	fmt.Printf("  Results: %d/%d passed, %d failed\n", pass, len(vectors), fail)
	fmt.Printf("  Total time: %ds\n", totalElapsed)
	fmt.Printf("  Output: %s\n", resultsPath)
	fmt.Println("==========================================")

	if fail > 0 {
		return 1
	}
	return 0
}

func verifySingle(args []string) int {
	if len(args) < 1 {
		usage()
		return 1
	}
	root, err := repoRoot()
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: %v\n", err)
		return 1
	}
	specDir := filepath.Join(root, "test", "evidence", "formal-proofs", "fstar")

	for _, tool := range []string{fstarExe, z3Exe} {
		if _, err := exec.LookPath(tool); err != nil {
			fmt.Fprintf(os.Stderr, "ERROR: %s not found on PATH\n", tool)
			return 1
		}
	}

	primitive := args[0]
	rest := args[1:]
	var v vector

	switch primitive {
	case "sha256":
		if len(rest) < 2 {
			fmt.Fprintln(os.Stderr, "Usage: fstar-eval sha256 <hex-input> <hex-expected>")
			return 1
		}
		v = sha256Vector("sha256", rest[0], rest[1])
	case "aes256-encrypt":
		if len(rest) < 3 {
			fmt.Fprintln(os.Stderr, "Usage: fstar-eval aes256-encrypt <hex-key> <hex-pt> <hex-ct>")
			return 1
		}
		v = aes256EncryptVector("aes256-encrypt", rest[0], rest[1], rest[2])
	case "aes256-decrypt":
		if len(rest) < 3 {
			fmt.Fprintln(os.Stderr, "Usage: fstar-eval aes256-decrypt <hex-key> <hex-ct> <hex-pt>")
			return 1
		}
		v = aes256DecryptVector("aes256-decrypt", rest[0], rest[1], rest[2])
	case "chacha20-qr":
		if len(rest) < 8 {
			fmt.Fprintln(os.Stderr, "Usage: fstar-eval chacha20-qr <a> <b> <c> <d> <ea> <eb> <ec> <ed>")
			return 1
		}
		v = chacha20QRVector("chacha20-qr", rest[0], rest[1], rest[2], rest[3],
			rest[4], rest[5], rest[6], rest[7])
	default:
		fmt.Fprintf(os.Stderr, "Unknown primitive: %s\nSupported: sha256, aes256-encrypt, aes256-decrypt, chacha20-qr\n", primitive)
		return 1
	}

	tmpdir, err := os.MkdirTemp("", "fstar-eval-*")
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: %v\n", err)
		return 1
	}
	defer os.RemoveAll(tmpdir)

	fstPath, err := v.GenFst(tmpdir)
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: %v\n", err)
		return 1
	}

	fmt.Fprintf(os.Stderr, "  Verifying %s ...\n", v.Label)
	ok, _ := runFstar(specDir, fstPath)
	if ok {
		fmt.Println("PASS")
		return 0
	}
	fmt.Println("FAIL")
	return 1
}

func usage() {
	fmt.Print(`Usage:
  fstar-eval --generate-all
  fstar-eval <primitive> <hex-input> [<hex-expected>] [extra-args...]

Primitives (evaluable):
  sha256            <hex-input> <hex-expected-digest>
  aes256-encrypt    <hex-key> <hex-plaintext> <hex-expected-ciphertext>
  aes256-decrypt    <hex-key> <hex-ciphertext> <hex-expected-plaintext>
  chacha20-qr       <a> <b> <c> <d> <exp-a> <exp-b> <exp-c> <exp-d>

Options:
  --generate-all    Run all curated vectors and save to test/vectors/fstar/
  --help            Show this help

Environment:
  FSTAR_EXE         Path to fstar.exe (default: fstar.exe)
  Z3_EXE            Path to z3 (default: z3)
  FSTAR_TIMEOUT     Seconds per evaluation (default: 900)
`)
}

func main() {
	if len(os.Args) < 2 {
		usage()
		os.Exit(1)
	}

	switch os.Args[1] {
	case "--help", "-h":
		usage()
	case "--generate-all":
		os.Exit(generateAll())
	default:
		os.Exit(verifySingle(os.Args[1:]))
	}
}

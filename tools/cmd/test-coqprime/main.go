// Command test-coqprime checks that coqprime is usable inside the VM.
//
// It writes a small Coq test file and tries coqc with progressively more
// flags until one variant succeeds.  Exit 0 if any variant passes, 1 if
// all fail.
package main

import (
	"fmt"
	"os"
	"os/exec"
)

const testFile = "/tmp/test_coqprime.v"

const testSource = `From Coqprime Require Import PocklingtonRefl.
Check Pocklington.
`

const contrib = "/run/current-system/sw/lib/coq/9.1/user-contrib"

// variant describes one coqc invocation strategy.
type variant struct {
	label string
	args  []string
}

func main() {
	fmt.Println("=== Testing coqprime in VM ===")
	fmt.Printf("COQPATH=%s\n", envOrUnset("COQPATH"))
	fmt.Printf("user-contrib: %s\n", contrib)

	if entries, err := os.ReadDir(contrib); err == nil {
		for _, e := range entries {
			fmt.Printf("  %s\n", e.Name())
		}
	} else {
		fmt.Println("No user-contrib dir")
	}

	if err := os.WriteFile(testFile, []byte(testSource), 0o644); err != nil {
		fmt.Fprintf(os.Stderr, "cannot write %s: %v\n", testFile, err)
		os.Exit(1)
	}

	variants := []variant{
		{"coqc with COQPATH", []string{testFile}},
		{"-native-compiler no", []string{"-native-compiler", "no", testFile}},
		{"explicit -R flags", []string{
			"-R", contrib + "/Bignums", "Bignums",
			"-R", contrib + "/Stdlib", "Stdlib",
			"-R", contrib + "/Coqprime", "Coqprime",
			testFile,
		}},
		{"-R flags + no native", []string{
			"-native-compiler", "no",
			"-R", contrib + "/Bignums", "Bignums",
			"-R", contrib + "/Stdlib", "Stdlib",
			"-R", contrib + "/Coqprime", "Coqprime",
			testFile,
		}},
	}

	results := []string{
		"PASS", "PASS_NO_NATIVE", "PASS_EXPLICIT_R", "PASS_EXPLICIT_NO_NATIVE",
	}

	for i, v := range variants {
		fmt.Printf("Test %d: %s...\n", i+1, v.label)
		cmd := exec.Command("coqc", v.args...)
		out, err := cmd.CombinedOutput()
		if len(out) > 0 {
			fmt.Print(string(out))
		}
		if err == nil {
			fmt.Printf("RESULT=%s\n", results[i])
			os.Exit(0)
		}
	}

	fmt.Println("RESULT=ALL_FAILED")
	os.Exit(1)
}

func envOrUnset(key string) string {
	if v, ok := os.LookupEnv(key); ok {
		return v
	}
	return "unset"
}

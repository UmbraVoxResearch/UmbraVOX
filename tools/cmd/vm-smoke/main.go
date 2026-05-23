// Command vm-smoke runs the UmbraVOX isolated VM smoke pipeline.
//
// This is the Go replacement for scripts/vm-smoke-run.sh. The smoke
// pipeline runs INSIDE the NixOS QEMU guest and exercises build, test,
// verification, complexity checks, and release artifact generation.
//
// Usage:
//
//	vm-smoke run             Run the full smoke pipeline in-guest
//	vm-smoke status          Show last smoke run results
//
// Requires: runs inside a NixOS QEMU guest with dev tools pre-installed.
package main

import (
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

// ANSI color helpers.
const (
	red    = "\033[0;31m"
	green  = "\033[0;32m"
	blue   = "\033[0;34m"
	yellow = "\033[1;33m"
	nc     = "\033[0m"
)

const usageText = `vm-smoke — UmbraVOX isolated VM smoke pipeline

Usage:
  vm-smoke <subcommand> [flags]

Subcommands:
  run       Run the full smoke pipeline (build, test, verify, complexity,
            license, format-check, release-linux)
  status    Show results from the last smoke run

Flags for 'run':
  --work-dir <path>      Source directory (default: /work/umbravox)
  --output-dir <path>    Results output directory (default: /output)
  --skip <stages>        Comma-separated stages to skip
                         (build,test,verify,complexity,license,format,release)
  --only <stages>        Run only these stages (comma-separated)
  --verbose              Verbose output for each stage

Flags for 'status':
  --output-dir <path>    Results directory (default: /output)
  --json                 Output results as JSON

This tool runs INSIDE the NixOS QEMU guest. The source tree is expected
on /dev/vdb (ext2, read-only). All dev tools are pre-installed via the
NixOS VM image. No network access is needed or available.

Smoke pipeline stages:
  1. build       cabal build all --enable-tests
  2. test        cabal test umbravox-test (required gate)
  3. verify      F* verification
  4. complexity  Complexity metrics check
  5. license     License header check
  6. format      Source formatting check
  7. release     Release artifact generation (linux bundle)
`

func logMsg(color, msg string) {
	fmt.Fprintf(os.Stderr, "%s[vm-smoke]%s %s\n", color, nc, msg)
}

func main() {
	flag.Usage = func() { fmt.Fprint(os.Stderr, usageText) }
	flag.Parse()

	args := flag.Args()
	if len(args) == 0 {
		flag.Usage()
		os.Exit(2)
	}

	os.Exit(dispatch(args))
}

func dispatch(args []string) int {
	subcmd := args[0]
	switch subcmd {
	case "run":
		fs := flag.NewFlagSet("run", flag.ExitOnError)
		workDir := fs.String("work-dir", "/work/umbravox", "source directory")
		outputDir := fs.String("output-dir", "/output", "results output directory")
		skip := fs.String("skip", "", "comma-separated stages to skip")
		only := fs.String("only", "", "run only these stages (comma-separated)")
		verbose := fs.Bool("verbose", false, "verbose output for each stage")
		fs.Parse(args[1:])
		return runSmoke(*workDir, *outputDir, *skip, *only, *verbose)

	case "status":
		fs := flag.NewFlagSet("status", flag.ExitOnError)
		outputDir := fs.String("output-dir", "/output", "results directory")
		jsonOut := fs.Bool("json", false, "output results as JSON")
		fs.Parse(args[1:])
		return runStatus(*outputDir, *jsonOut)

	case "help", "-h", "--help":
		flag.Usage()
		return 0

	default:
		fmt.Fprintf(os.Stderr, "vm-smoke: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		return 2
	}
}

// allStages lists pipeline stages in order.
var allStages = []string{"build", "test", "verify", "complexity", "license", "format", "release"}

// shouldRun returns true if the stage should be executed given skip/only filters.
func shouldRun(stage, skip, only string) bool {
	if only != "" {
		for _, s := range strings.Split(only, ",") {
			if strings.TrimSpace(s) == stage {
				return true
			}
		}
		return false
	}
	if skip != "" {
		for _, s := range strings.Split(skip, ",") {
			if strings.TrimSpace(s) == stage {
				return false
			}
		}
	}
	return true
}

// runStep executes a command, prints pass/fail, and returns true on success.
func runStep(label string, verbose bool, name string, args ...string) bool {
	fmt.Printf("\n-- %s --\n", label)
	cmd := exec.Command(name, args...)
	cmd.Dir = "" // inherit working directory
	if verbose {
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
	} else {
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
	}

	err := cmd.Run()
	if err == nil {
		fmt.Printf("  STEP PASS: %s\n", label)
		return true
	}
	fmt.Printf("  STEP FAIL: %s (%v)\n", label, err)
	return false
}

// findBin locates a built binary in dist-newstyle.
func findBin(workDir, name string) string {
	// Walk dist-newstyle looking for build/<name>/<name>
	distDir := filepath.Join(workDir, "dist-newstyle")
	var result string
	filepath.Walk(distDir, func(path string, info os.FileInfo, err error) error {
		if err != nil || result != "" {
			return nil
		}
		if info.IsDir() {
			return nil
		}
		if filepath.Base(path) == name {
			dir := filepath.Dir(path)
			if filepath.Base(dir) == name || filepath.Base(filepath.Dir(dir)) == "build" {
				result = path
			}
		}
		return nil
	})
	return result
}

func runSmoke(workDir, outputDir, skip, only string, verbose bool) int {
	fmt.Println("========================================")
	fmt.Println("  UmbraVOX Isolated VM Smoke Pipeline")
	fmt.Println("========================================")
	fmt.Println()

	// Print system info
	hostname, _ := os.Hostname()
	uname, _ := exec.Command("uname", "-r").Output()
	fmt.Printf("kernel:    %s", string(uname))
	fmt.Printf("hostname:  %s\n", hostname)
	fmt.Printf("date:      %s\n", time.Now().UTC().Format("2006-01-02T15:04:05Z"))
	fmt.Println()

	// Mount source disk
	os.MkdirAll("/mnt/src", 0o755)
	exec.Command("mount", "-o", "ro", "/dev/vdb", "/mnt/src").Run()

	// Copy to writable workspace
	markerPath := filepath.Join(workDir, ".vm-source-ready")
	if _, err := os.Stat(markerPath); os.IsNotExist(err) {
		fmt.Println("copying source to", workDir, "...")
		os.MkdirAll(workDir, 0o755)
		exec.Command("cp", "-a", "/mnt/src/.", workDir+"/").Run()
		os.WriteFile(markerPath, []byte("ready\n"), 0o644)
	} else {
		fmt.Println("workspace already prepared at", workDir, "(marker present)")
	}

	if err := os.Chdir(workDir); err != nil {
		logMsg(red, fmt.Sprintf("Cannot chdir to %s: %v", workDir, err))
		return 1
	}
	fmt.Println()

	// Set environment
	os.Setenv("HOME", "/root")
	os.Setenv("UMBRAVOX_ROOT", workDir)
	os.Setenv("UMBRAVOX_ALLOW_DIRTY_RELEASE", "1")
	os.Setenv("UMBRAVOX_ALLOW_UNTAGGED_RELEASE", "1")
	path := os.Getenv("PATH")
	os.Setenv("PATH", filepath.Join(workDir, "scripts")+":"+path)
	os.Unsetenv("LD_LIBRARY_PATH")

	// Set FSTAR_HOME
	if fstarExe, err := exec.LookPath("fstar.exe"); err == nil {
		realPath, err := filepath.EvalSymlinks(fstarExe)
		if err == nil {
			os.Setenv("FSTAR_HOME", filepath.Dir(filepath.Dir(realPath)))
		}
	}

	// Offline cabal config
	os.MkdirAll("/root/.cabal", 0o755)
	os.WriteFile("/root/.cabal/config", []byte("offline: True\nnix: False\n"), 0o644)

	pass := 0
	fail := 0

	// Step 1: Build
	if shouldRun("build", skip, only) {
		if runStep("build", verbose, "cabal", "build", "all", "--enable-tests") {
			pass++
		} else {
			fail++
		}
	}

	// Locate binaries
	testBin := findBin(workDir, "umbravox-test")
	fstarBin := findBin(workDir, "fstar-verify")
	complexityBin := findBin(workDir, "check-complexity")
	fmt.Printf("\n  binaries: test=%s fstar=%s complexity=%s\n",
		orMissing(testBin), orMissing(fstarBin), orMissing(complexityBin))

	// Step 2: Test
	if shouldRun("test", skip, only) {
		if testBin != "" {
			if runStep("test", verbose, testBin, "required") {
				pass++
			} else {
				fail++
			}
		} else {
			fmt.Println("  STEP FAIL: test (umbravox-test binary not found)")
			fail++
		}
	}

	// Step 3: F* verification
	if shouldRun("verify", skip, only) {
		// Prepare F* cache
		cacheDir := filepath.Join(workDir, "test/evidence/formal-proofs/fstar/_cache")
		outputFstar := filepath.Join(workDir, "test/evidence/formal-proofs/fstar/_output")
		os.MkdirAll(cacheDir, 0o755)
		os.MkdirAll(outputFstar, 0o755)

		// Copy pre-built cache if available
		if entries, err := filepath.Glob("/etc/umbravox-fstar-cache/*.checked"); err == nil && len(entries) > 0 {
			for _, e := range entries {
				exec.Command("cp", e, cacheDir+"/").Run()
			}
			fmt.Printf("  F* cache: %d pre-built .checked files loaded\n", len(entries))
		}

		if fstarBin != "" {
			if runStep("verify", verbose, fstarBin) {
				pass++
			} else {
				fail++
			}
		} else {
			fmt.Println("  STEP FAIL: verify (fstar-verify binary not found)")
			fail++
		}
	}

	// Step 4: Complexity
	if shouldRun("complexity", skip, only) {
		if complexityBin != "" {
			ok := runComplexityCheck(workDir, complexityBin)
			if ok {
				fmt.Println("  STEP PASS: complexity")
				pass++
			} else {
				fmt.Println("  STEP FAIL: complexity")
				fail++
			}
		} else {
			fmt.Println("  STEP FAIL: complexity (check-complexity binary not found)")
			fail++
		}
	}

	// Step 5: License
	if shouldRun("license", skip, only) {
		if runStep("license", verbose, "make", "license") {
			pass++
		} else {
			fail++
		}
	}

	// Step 6: Format check
	if shouldRun("format", skip, only) {
		if runStep("format-check", verbose, "make", "format-check") {
			pass++
		} else {
			fail++
		}
	}

	// Step 7: Release
	if shouldRun("release", skip, only) {
		if runStep("release-linux", verbose, "make", "release-linux") {
			pass++
		} else {
			fail++
		}
	}

	// Verify release artifact
	fmt.Println("\n-- release artifact verification --")
	artifact := findArtifact(workDir)
	if artifact != "" {
		fmt.Printf("  artifact: %s\n", artifact)
		if fi, err := os.Stat(artifact); err == nil {
			fmt.Printf("  size: %dK\n", fi.Size()/1024)
		}
		pass++
	} else {
		fmt.Println("  STEP FAIL: no release artifact produced")
		fail++
	}

	// Summary
	fmt.Println()
	fmt.Println("========================================")
	fmt.Printf("  SMOKE SUMMARY: %d passed, %d failed\n", pass, fail)
	fmt.Println("========================================")

	// Evidence
	fmt.Println("\n-- evidence --")
	fmt.Printf("  kernel:     %s", string(uname))
	fmt.Printf("  hostname:   %s\n", hostname)
	fmt.Printf("  date:       %s\n", time.Now().UTC().Format("2006-01-02T15:04:05Z"))
	gitHash, _ := exec.Command("git", "rev-parse", "HEAD").Output()
	fmt.Printf("  git commit: %s\n", strings.TrimSpace(string(gitHash)))

	if artifact != "" {
		fmt.Printf("  artifact:   %s\n", filepath.Base(artifact))
		sha, _ := exec.Command("sha256sum", artifact).Output()
		if len(sha) > 0 {
			fmt.Printf("  sha256:     %s\n", strings.Fields(string(sha))[0])
		}
	}

	// Write result to output directory
	result := "PASS"
	if fail > 0 {
		result = "FAIL"
	}
	fmt.Printf("SMOKE_RESULT=%s\n", result)

	// Persist to output dir
	os.MkdirAll(outputDir, 0o755)
	summary := fmt.Sprintf("SMOKE_RESULT=%s\nPASS=%d\nFAIL=%d\nDATE=%s\n",
		result, pass, fail, time.Now().UTC().Format("2006-01-02T15:04:05Z"))
	os.WriteFile(filepath.Join(outputDir, "smoke-result"), []byte(summary), 0o644)

	if fail > 0 {
		return 1
	}
	return 0
}

func runComplexityCheck(workDir, complexityBin string) bool {
	fmt.Println("\n-- complexity --")
	violations := 0
	total := 0

	dirs := []string{"src/UmbraVox", "test/Test", "codegen"}
	for _, dir := range dirs {
		fullDir := filepath.Join(workDir, dir)
		filepath.Walk(fullDir, func(path string, info os.FileInfo, err error) error {
			if err != nil || info.IsDir() {
				return nil
			}
			if !strings.HasSuffix(path, ".hs") {
				return nil
			}
			total++
			cmd := exec.Command(complexityBin, path, "8")
			out, runErr := cmd.CombinedOutput()
			if runErr != nil {
				fmt.Print(string(out))
				violations++
			}
			return nil
		})
	}

	if violations > 0 {
		fmt.Printf("  %d file(s) exceed complexity threshold.\n", violations)
		return false
	}
	fmt.Printf("  All %d files pass complexity check (<= 8).\n", total)
	return true
}

func findArtifact(workDir string) string {
	pattern := filepath.Join(workDir, "build/releases/umbravox-*-linux-x86_64.tar.gz")
	matches, _ := filepath.Glob(pattern)
	if len(matches) > 0 {
		return matches[0]
	}
	return ""
}

func orMissing(s string) string {
	if s == "" {
		return "MISSING"
	}
	return s
}

func runStatus(outputDir string, jsonOut bool) int {
	resultFile := filepath.Join(outputDir, "smoke-result")
	data, err := os.ReadFile(resultFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "vm-smoke: no smoke results found at %s\n", resultFile)
		return 1
	}

	if jsonOut {
		// Parse key=value pairs into JSON
		fmt.Println("{")
		lines := strings.Split(strings.TrimSpace(string(data)), "\n")
		for i, line := range lines {
			parts := strings.SplitN(line, "=", 2)
			if len(parts) == 2 {
				comma := ","
				if i == len(lines)-1 {
					comma = ""
				}
				fmt.Printf("  %q: %q%s\n", parts[0], parts[1], comma)
			}
		}
		fmt.Println("}")
	} else {
		fmt.Print(string(data))
	}

	if strings.Contains(string(data), "SMOKE_RESULT=FAIL") {
		return 1
	}
	return 0
}

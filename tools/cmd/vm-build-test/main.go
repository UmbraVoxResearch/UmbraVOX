// Command vm-build-test runs the universal UmbraVOX build+test sequence
// inside a VM guest on any POSIX platform.
//
// This is the Go replacement for scripts/vm-build-test.sh. It runs inside
// a QEMU VM guest on FreeBSD, OpenBSD, NetBSD, OmniOS/illumos, Linux arm64,
// or DragonFlyBSD.
//
// Usage:
//
//	vm-build-test run [--work-dir /path]    Run build+test sequence
//	vm-build-test status                    Show last run results
//
// The host-side setup script (vm-*-setup.sh) is responsible for installing
// GHC and cabal-install via the platform package manager.
package main

import (
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"time"
)

const usageText = `vm-build-test — UmbraVOX universal VM build+test runner

Usage:
  vm-build-test <subcommand> [flags]

Subcommands:
  run       Run build+test sequence in the VM guest
  status    Show last run results

Flags for 'run':
  --work-dir <path>       Source directory (default: /work/umbravox or ~/umbravox)
  --output-dir <path>     Results output directory (default: /output)
  --cabal-jobs <n>        Number of parallel cabal build jobs (default: auto)
  --skip-test             Skip test execution (build only)
  --timeout <duration>    Overall timeout (default: 60m)

Flags for 'status':
  --output-dir <path>     Results directory (default: /output)

This runs inside a QEMU VM guest. The platform setup script must install
GHC and cabal-install before invoking this tool.

Supported platforms: FreeBSD, OpenBSD, NetBSD, OmniOS/illumos (SmartOS,
OpenIndiana), Linux arm64, DragonFlyBSD.

Exit codes:
  0  build and test passed (VM_BUILD_TEST=PASS)
  1  build or test failed  (VM_BUILD_TEST=FAIL)
`

func log(msg string)  { fmt.Printf("[vm-build-test] %s\n", msg) }
func fail(msg string) { fmt.Fprintf(os.Stderr, "[vm-build-test] FAIL: %s\n", msg) }

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
		workDir := fs.String("work-dir", "", "source directory (default: auto-detect)")
		outputDir := fs.String("output-dir", "/output", "results output directory")
		cabalJobs := fs.Int("cabal-jobs", runtime.NumCPU(), "parallel cabal build jobs")
		skipTest := fs.Bool("skip-test", false, "skip test execution (build only)")
		_ = fs.String("timeout", "60m", "overall timeout")
		fs.Parse(args[1:])
		return runBuildTest(*workDir, *outputDir, *cabalJobs, *skipTest)

	case "status":
		fs := flag.NewFlagSet("status", flag.ExitOnError)
		outputDir := fs.String("output-dir", "/output", "results directory")
		fs.Parse(args[1:])
		return runStatus(*outputDir)

	case "help", "-h", "--help":
		flag.Usage()
		return 0

	default:
		fmt.Fprintf(os.Stderr, "vm-build-test: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		return 2
	}
}

// detectWorkDir finds the source directory.
func detectWorkDir(explicit string) string {
	if explicit != "" {
		return explicit
	}
	// Check UMBRAVOX_WORK_DIR env
	if envDir := os.Getenv("UMBRAVOX_WORK_DIR"); envDir != "" {
		return envDir
	}
	// Standard locations
	if fi, err := os.Stat("/work/umbravox"); err == nil && fi.IsDir() {
		return "/work/umbravox"
	}
	home := os.Getenv("HOME")
	if home == "" {
		home = "/root"
	}
	if fi, err := os.Stat(filepath.Join(home, "umbravox")); err == nil && fi.IsDir() {
		return filepath.Join(home, "umbravox")
	}
	return ""
}

// runCmd runs a command with stdout/stderr connected, returns success.
func runCmd(label string, name string, args ...string) bool {
	fmt.Printf("\n[%s] %s...\n", strings.ToUpper(label), name+" "+strings.Join(args, " "))
	cmd := exec.Command(name, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		fail(fmt.Sprintf("%s failed", label))
		return false
	}
	fmt.Printf("[%s] %s complete\n", strings.ToUpper(label), label)
	return true
}

func runBuildTest(workDirFlag, outputDir string, cabalJobs int, skipTest bool) int {
	workDir := detectWorkDir(workDirFlag)
	if workDir == "" {
		fail("source directory not found; set --work-dir or UMBRAVOX_WORK_DIR or place source at /work/umbravox")
		fmt.Println("VM_BUILD_TEST=FAIL")
		return 1
	}

	// Banner
	fmt.Println("=== UmbraVOX VM Build+Test ===")
	fmt.Printf("Platform: %s %s\n", runtime.GOOS, runtime.GOARCH)

	ghcVer, _ := exec.Command("ghc", "--version").Output()
	cabalVer, _ := exec.Command("cabal", "--version").Output()
	fmt.Printf("GHC:      %s", orDefault(strings.TrimSpace(string(ghcVer)), "not found"))
	if len(ghcVer) == 0 {
		fmt.Println()
	} else {
		fmt.Println()
	}
	fmt.Printf("Cabal:    %s\n", orDefault(firstLine(string(cabalVer)), "not found"))
	fmt.Printf("Work dir: %s\n", workDir)
	fmt.Printf("Date:     %s\n", time.Now().UTC().Format("2006-01-02T15:04:05Z"))
	fmt.Println()

	// Verify GHC and cabal are present
	if _, err := exec.LookPath("ghc"); err != nil {
		fail("ghc not found -- platform setup script must install GHC before running this script")
		fmt.Println("VM_BUILD_TEST=FAIL")
		return 1
	}
	if _, err := exec.LookPath("cabal"); err != nil {
		fail("cabal not found -- platform setup script must install cabal-install before running this script")
		fmt.Println("VM_BUILD_TEST=FAIL")
		return 1
	}

	// Enter work directory
	if err := os.Chdir(workDir); err != nil {
		fail(fmt.Sprintf("cannot chdir to %s: %v", workDir, err))
		fmt.Println("VM_BUILD_TEST=FAIL")
		return 1
	}
	log(fmt.Sprintf("working directory: %s", workDir))

	// Set up environment
	home := os.Getenv("HOME")
	if home == "" {
		home = "/root"
		os.Setenv("HOME", home)
	}
	cabalDir := os.Getenv("UMBRAVOX_CABAL_DIR")
	if cabalDir == "" {
		cabalDir = filepath.Join(home, ".cabal")
	}
	os.Setenv("CABAL_DIR", cabalDir)
	os.MkdirAll(cabalDir, 0o755)

	// illumos/OmniOS: add ooce tools to PATH
	if _, err := os.Stat("/opt/ooce/bin"); err == nil {
		os.Setenv("PATH", "/opt/ooce/bin:/opt/ooce/sbin:"+os.Getenv("PATH"))
	}

	// cabal update
	log("updating cabal package index...")
	if !runCmd("update", "cabal", "update") {
		fmt.Println("VM_BUILD_TEST=FAIL")
		return 1
	}

	// Build
	buildArgs := []string{"build", "all"}
	if cabalJobs > 0 {
		buildArgs = append(buildArgs, fmt.Sprintf("-j%d", cabalJobs))
	}
	if !runCmd("build", "cabal", buildArgs...) {
		fmt.Println("VM_BUILD_TEST=FAIL")
		return 1
	}

	// Test
	if !skipTest {
		testOpts := os.Getenv("UMBRAVOX_TEST_OPTS")
		if testOpts == "" {
			testOpts = "required"
		}
		if !runCmd("test", "cabal", "test", "umbravox-test", "--test-options="+testOpts) {
			fmt.Println("VM_BUILD_TEST=FAIL")
			return 1
		}
	}

	// Summary
	fmt.Println()
	fmt.Println("=== VM Build+Test Summary ===")
	fmt.Printf("Platform: %s %s\n", runtime.GOOS, runtime.GOARCH)
	ghcVer2, _ := exec.Command("ghc", "--version").Output()
	fmt.Printf("GHC:      %s\n", firstLine(string(ghcVer2)))
	fmt.Println("Status:   PASS")
	fmt.Println()
	fmt.Println("VM_BUILD_TEST=PASS")

	// Write result to output directory
	os.MkdirAll(outputDir, 0o755)
	result := fmt.Sprintf("VM_BUILD_TEST=PASS\nPLATFORM=%s/%s\nDATE=%s\n",
		runtime.GOOS, runtime.GOARCH, time.Now().UTC().Format("2006-01-02T15:04:05Z"))
	os.WriteFile(filepath.Join(outputDir, "build-test-result"), []byte(result), 0o644)

	return 0
}

func runStatus(outputDir string) int {
	resultFile := filepath.Join(outputDir, "build-test-result")
	data, err := os.ReadFile(resultFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "vm-build-test: no results found at %s\n", resultFile)
		return 1
	}
	fmt.Print(string(data))
	if strings.Contains(string(data), "VM_BUILD_TEST=FAIL") {
		return 1
	}
	return 0
}

func firstLine(s string) string {
	if idx := strings.IndexByte(s, '\n'); idx >= 0 {
		return s[:idx]
	}
	return s
}

func orDefault(s, def string) string {
	if s == "" {
		return def
	}
	return s
}

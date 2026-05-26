// SPDX-License-Identifier: Apache-2.0
package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
)

// testSuites maps suite names to cabal test-options arguments.
var testSuites = map[string]string{
	"core":         "core",
	"core-crypto":  "core-crypto",
	"core-network": "core-network",
	"core-chat":    "core-chat",
	"core-tui":     "core-tui",
	"core-tools":   "core-tools",
	"tcp":          "tcp",
	"fault":        "fault",
	"recovery":     "recovery",
	"tui-sim":      "tui-sim",
	"integrity":    "integrity",
	"mdns":         "mdns",
	"deferred":     "deferred",
	"differential": "differential",
	"soak":         "soak",
}

// runBuild handles: uv build [--docs] [--direct]
// Codegen always runs before build to ensure generated files match specs.
// --direct bypasses the VM and runs via nix-shell on the host (for CI).
func runBuild(args []string) int {
	docs := false
	direct := false
	bypassInteractive := false
	for _, a := range args {
		switch a {
		case "--docs":
			docs = true
		case "--direct":
			direct = true
		case "--direct-bypass-interactive":
			direct = true
			bypassInteractive = true
		}
	}

	if direct {
		if !confirmDirect("build", bypassInteractive) {
			return 1
		}
		repoRoot, err := repo.Root()
		if err != nil {
			log.Fail(tag, err.Error())
			return 1
		}
		shellNix := filepath.Join(repoRoot, "shell.nix")
		innerCmd := "cabal run codegen && cabal build all --enable-tests"
		cmd := exec.Command("nix-shell", shellNix, "--pure", "--run", innerCmd)
		cmd.Dir = repoRoot
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			if exitErr, ok := err.(*exec.ExitError); ok {
				return exitErr.ExitCode()
			}
			log.Fail(tag, fmt.Sprintf("nix-shell: %v", err))
			return 1
		}
		return 0
	}

	var cmd string
	if docs {
		cmd = "cabal haddock --haddock-html 2>&1"
	} else {
		// Always run codegen before build to ensure generated C/Haskell/FFI
		// files are in sync with .spec sources. This provides assurance that
		// the generated code matches the specifications.
		cmd = `cabal run codegen && cabal build all --enable-tests 2>&1 | tail -20 && \
  echo "=== Extracting runtime bundle ===" && \
  mkdir -p /output/runtime/bin /output/runtime/lib && \
  BIN=$(cabal list-bin exe:umbravox 2>/dev/null) && \
  if [ -n "$BIN" ] && [ -f "$BIN" ]; then \
    cp "$BIN" /output/runtime/bin/umbravox && \
    chmod +x /output/runtime/bin/umbravox && \
    INTERP=$(patchelf --print-interpreter "$BIN" 2>/dev/null) && \
    [ -n "$INTERP" ] && cp "$INTERP" /output/runtime/lib/ ; \
    ldd "$BIN" 2>/dev/null | awk '/=>/ && !/not found/ {print $3}' | while read lib; do \
      [ -f "$lib" ] && cp "$lib" /output/runtime/lib/ ; \
    done ; \
    strip /output/runtime/bin/umbravox 2>/dev/null ; \
    patchelf --set-interpreter /app/lib/ld-linux-x86-64.so.2 /output/runtime/bin/umbravox && \
    patchelf --set-rpath /app/lib /output/runtime/bin/umbravox && \
    echo "Runtime bundle extracted: $(ls /output/runtime/bin/ /output/runtime/lib/ | wc -l) files" ; \
  else \
    echo "WARNING: could not extract runtime bundle (binary not found)" ; \
  fi`
	}

	code := execInVM(cmd, qemu.ProfileDev, 45*time.Minute)
	if code == 0 && !docs {
		// Move runtime bundle from VM output to build/runtime/
		repoRoot, err := repo.Root()
		if err == nil {
			runtimeSrc := filepath.Join(repoRoot, "build", "vm-output", "runtime")
			runtimeDst := filepath.Join(repoRoot, "build", "runtime")
			if fi, err := os.Stat(runtimeSrc); err == nil && fi.IsDir() {
				os.RemoveAll(runtimeDst)
				os.Rename(runtimeSrc, runtimeDst)
				// Ensure execute permission (9p share may strip it)
				binPath := filepath.Join(runtimeDst, "bin", "umbravox")
				os.Chmod(binPath, 0o755)
				log.Info(tag, "Runtime bundle saved to build/runtime/")
			}
		}
	}
	return code
}

// runTest handles: uv test [SUITE] [--list] [--direct]
// --direct bypasses the VM and runs via nix-shell on the host (for CI).
func runTest(args []string) int {
	if len(args) > 0 && (args[0] == "--list" || args[0] == "-l") {
		fmt.Println("Available test suites:")
		for name := range testSuites {
			fmt.Printf("  %s\n", name)
		}
		fmt.Println("  all        (every suite sequentially)")
		fmt.Println("  ephemeral  (builds fresh image, tests, discards)")
		fmt.Println("  e2e              (end-to-end: build, test, check, runtime, SBOM)")
		fmt.Println("  e2e --bootstrap  (cold start: clean, build image, then full e2e)")
		fmt.Println("\nUsage: ./uv test [SUITE]")
		fmt.Println("No suite argument runs the required fast gate.")
		return 0
	}

	// Check for --direct anywhere in args; collect remaining args without it.
	direct := false
	bypassInteractive := false
	var filtered []string
	for _, a := range args {
		switch a {
		case "--direct":
			direct = true
		case "--direct-bypass-interactive":
			direct = true
			bypassInteractive = true
		default:
			filtered = append(filtered, a)
		}
	}
	args = filtered

	if direct {
		if !confirmDirect("test", bypassInteractive) {
			return 1
		}
		repoRoot, err := repo.Root()
		if err != nil {
			log.Fail(tag, err.Error())
			return 1
		}
		shellNix := filepath.Join(repoRoot, "shell.nix")
		innerCmd := "cabal build all --enable-tests && cabal test umbravox-test --test-options='required'"
		cmd := exec.Command("nix-shell", shellNix, "--pure", "--run", innerCmd)
		cmd.Dir = repoRoot
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			if exitErr, ok := err.(*exec.ExitError); ok {
				return exitErr.ExitCode()
			}
			log.Fail(tag, fmt.Sprintf("nix-shell: %v", err))
			return 1
		}
		return 0
	}

	if len(args) == 0 {
		// Default: required fast gate
		cmd := "cabal build all --enable-tests && cabal test umbravox-test --test-options='required' 2>&1"
		return execInVM(cmd, qemu.ProfileDev, 45*time.Minute)
	}

	suite := args[0]

	if suite == "ephemeral" {
		return runTestEphemeral()
	}

	if suite == "e2e" {
		return runE2E(args[1:])
	}

	if suite == "all" {
		return runTestAll()
	}

	opts, ok := testSuites[suite]
	if !ok {
		fmt.Fprintf(os.Stderr, "Unknown test suite: %s\nRun './uv test --list' for available suites.\n", suite)
		return 2
	}

	timeout := 45 * time.Minute
	if suite == "soak" {
		timeout = 120 * time.Minute
	}

	cmd := fmt.Sprintf("cabal build all --enable-tests && cabal test umbravox-test --test-options='%s' 2>&1", opts)
	return execInVM(cmd, qemu.ProfileDev, timeout)
}

// runTestEphemeral builds a fresh VM image in a temp dir, runs tests, and discards.
func runTestEphemeral() int {
	cmd := `nix build .#vm-image -o /tmp/ephemeral-image 2>&1 && \
qemu-img create -f qcow2 -b /tmp/ephemeral-image/nixos.img -F raw /tmp/ephemeral-overlay.qcow2 && \
echo "Ephemeral image built; running tests..." && \
cabal build all --enable-tests && cabal test umbravox-test --test-options='required' 2>&1`
	return execInVM(cmd, qemu.ProfileBuild, 60*time.Minute)
}

// runTestAll runs every named test suite sequentially in a single VM session.
// This ensures complete coverage of all test paths.
func runTestAll() int {
	log.Info(tag, "Running all test suites sequentially...")

	// Build once, then run each suite
	var parts []string
	parts = append(parts, "cabal build all --enable-tests")
	for name, opts := range testSuites {
		parts = append(parts, fmt.Sprintf(
			"echo '=== Suite: %s ===' && cabal test umbravox-test --test-options='%s' 2>&1",
			name, opts))
	}

	cmd := ""
	for i, p := range parts {
		if i == 0 {
			cmd = p
		} else {
			cmd += " && " + p
		}
	}

	// Soak suite alone is 2h; all suites together need generous timeout
	return execInVM(cmd, qemu.ProfileDev, 180*time.Minute)
}

// runVerify handles: uv verify [vectors [ARGS...]]
func runVerify(args []string) int {
	if len(args) > 0 && args[0] == "vectors" {
		return runFstarEval(args[1:])
	}
	cmd := "cabal build all --enable-tests && cabal run fstar-verify 2>&1"
	return execInVM(cmd, qemu.ProfileDev, 120*time.Minute)
}

// runFstarEval builds and runs the fstar-eval binary (M18.2.3 vector evaluation).
// Invoked via: uv verify vectors [--generate-all | <primitive> <args...>]
func runFstarEval(args []string) int {
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	evalBin := filepath.Join(repoRoot, "build", "tools", "fstar-eval")
	if _, err := os.Stat(evalBin); os.IsNotExist(err) {
		log.Info(tag, "Building fstar-eval tool...")
		buildCmd := exec.Command("go", "build", "-trimpath", "-ldflags=-s -w", "-o", evalBin, "./cmd/fstar-eval/")
		buildCmd.Dir = filepath.Join(repoRoot, "tools")
		buildCmd.Env = append(os.Environ(),
			"GOMODCACHE="+filepath.Join(repoRoot, "build", "go", "mod"),
			"GOCACHE="+filepath.Join(repoRoot, "build", "go", "cache"))
		if out, err := buildCmd.CombinedOutput(); err != nil {
			log.Fail(tag, fmt.Sprintf("Failed to build fstar-eval: %v\n%s", err, out))
			return 1
		}
	}

	if len(args) == 0 {
		args = []string{"--generate-all"}
	}

	cmd := exec.Command(evalBin, args...)
	cmd.Dir = repoRoot
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return exitErr.ExitCode()
		}
		log.Fail(tag, fmt.Sprintf("fstar-eval: %v", err))
		return 1
	}
	return 0
}

// runAll handles: uv (no args) — build + test + check
func runAll() int {
	// Build + test in VM
	cmd := `cabal build all --enable-tests 2>&1 | tail -20 && \
cabal test umbravox-test --test-options='required' 2>&1`
	if code := execInVM(cmd, qemu.ProfileDev, 60*time.Minute); code != 0 {
		return code
	}
	// Host-side checks
	return runCheck(nil)
}

// confirmDirect warns the user about --direct mode and asks for confirmation.
// Returns true if the user confirms (or bypass is set). Always shows warning.
func confirmDirect(action string, bypass bool) bool {
	fmt.Fprintln(os.Stderr)
	log.Warn(tag, "WARNING: --direct bypasses VM isolation.")
	fmt.Fprintf(os.Stderr, "  This will %s directly on the host via nix-shell.\n", action)
	fmt.Fprintf(os.Stderr, "  The primary development path uses VMs (./uv %s without --direct).\n", action)
	fmt.Fprintf(os.Stderr, "  --direct is intended for CI runners without KVM access.\n")
	fmt.Fprintln(os.Stderr)

	if bypass {
		log.Info(tag, "Proceeding (--direct-bypass-interactive)")
		return true
	}

	fmt.Fprintf(os.Stderr, "  Continue with host-direct %s? [y/N] ", action)
	reader := bufio.NewReader(os.Stdin)
	line, _ := reader.ReadString('\n')
	answer := strings.TrimSpace(strings.ToLower(line))
	if answer == "y" || answer == "yes" {
		return true
	}
	log.Info(tag, "Cancelled. Use VM-based ./uv "+action+" instead.")
	return false
}

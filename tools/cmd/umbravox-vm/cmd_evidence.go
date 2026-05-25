// SPDX-License-Identifier: Apache-2.0
package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
)

// runEvidence handles: uv evidence [--fast|--full]
func runEvidence(args []string) int {
	mode := "standard"
	for _, a := range args {
		switch a {
		case "--fast":
			mode = "fast"
		case "--full":
			mode = "full"
		}
	}

	switch mode {
	case "fast":
		return evidenceFast()
	case "full":
		if code := evidenceFast(); code != 0 {
			return code
		}
		return evidenceFull()
	default:
		// Standard: fast checks + quality pipeline
		if code := evidenceFast(); code != 0 {
			return code
		}
		return runAll()
	}
}

func evidenceFast() int {
	log.Info(tag, "Running fast assurance checks (no compilation)...")
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	failed := false

	// Check for F* assume val
	cmd := exec.Command("grep", "-r", "assume val", "--include=*.fst", "--include=*.fsti",
		filepath.Join(repoRoot, "test", "evidence", "formal-proofs", "fstar"))
	if out, _ := cmd.Output(); len(out) > 0 {
		log.Fail(tag, "F* assume val found (should be reduced):")
		fmt.Fprintf(os.Stderr, "%s", out)
		failed = true
	}

	// Check for Coq Admitted
	cmd = exec.Command("grep", "-r", "Admitted", "--include=*.v",
		filepath.Join(repoRoot, "test", "evidence", "formal-proofs", "coq"))
	if out, _ := cmd.Output(); len(out) > 0 {
		log.Fail(tag, "Coq Admitted found:")
		fmt.Fprintf(os.Stderr, "%s", out)
		failed = true
	}

	// Check external evidence
	evidenceScript := filepath.Join(repoRoot, "test", "evidence", "formal-proofs", "check-external-evidence.sh")
	if _, err := os.Stat(evidenceScript); err == nil {
		cmd := exec.Command("bash", evidenceScript)
		cmd.Dir = repoRoot
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			log.Fail(tag, "External evidence check failed")
			failed = true
		}
	}

	if failed {
		return 1
	}
	log.OK(tag, "Fast assurance checks passed.")
	return 0
}

func evidenceFull() int {
	log.Info(tag, "Running full release-grade assurance suite...")
	cmd := `cabal build all --enable-tests && \
cabal test umbravox-test --test-options='required' && \
cabal run fstar-verify 2>&1`
	return execInVM(cmd, qemu.ProfileBuild, 120*time.Minute)
}

// runCoverage handles: uv coverage [--check|--mcdc]
func runCoverage(args []string) int {
	check := false
	mcdc := false
	for _, a := range args {
		switch a {
		case "--check":
			check = true
		case "--mcdc":
			mcdc = true
		}
	}

	if mcdc {
		log.Info(tag, "Generating MC/DC expression-level coverage report...")
		cmd := `cabal configure --enable-coverage && \
cabal build all --enable-tests && \
cabal test umbravox-test 2>&1 && \
hpc report umbravox-test 2>&1`
		return execInVM(cmd, qemu.ProfileDev, 30*time.Minute)
	}

	if check {
		log.Info(tag, "Checking coverage against tier targets...")
		repoRoot, err := repo.Root()
		if err != nil {
			log.Fail(tag, err.Error())
			return 1
		}
		summary := filepath.Join(repoRoot, "build", "coverage", "coverage-summary.txt")
		return checkCoverageTargets(summary)
	}

	// Default: generate HTML coverage report
	log.Info(tag, "Generating HPC coverage report...")
	cmd := `cabal configure --enable-coverage && \
cabal build all --enable-tests && \
cabal test umbravox-test 2>&1 && \
mkdir -p build/coverage && \
hpc markup umbravox-test --destdir=build/coverage 2>&1`
	return execInVM(cmd, qemu.ProfileDev, 30*time.Minute)
}

// coverageTarget returns the expression-coverage target percentage for a
// Haskell module name, following the tier policy in doc/MCDC-TARGETS.md.
// Returns -1 for types-only modules that should be skipped entirely.
func coverageTarget(mod string) int {
	switch {
	case mod == "UmbraVox.Crypto.Warning":
		// Types-only, no executable code
		return -1
	case strings.HasPrefix(mod, "UmbraVox.Crypto.Generated."):
		// Generated FFI wrappers — expect 100% trivially
		return 100
	case strings.HasPrefix(mod, "UmbraVox.Crypto."):
		return 100
	case strings.HasPrefix(mod, "UmbraVox.Protocol."),
		strings.HasPrefix(mod, "UmbraVox.Network.Noise."):
		return 95
	case strings.HasPrefix(mod, "UmbraVox.Network."):
		return 90
	case strings.HasPrefix(mod, "UmbraVox.TUI."),
		strings.HasPrefix(mod, "UmbraVox.App."),
		strings.HasPrefix(mod, "UmbraVox.Chat."),
		strings.HasPrefix(mod, "UmbraVox.Storage."),
		strings.HasPrefix(mod, "UmbraVox.Tools."),
		strings.HasPrefix(mod, "UmbraVox.Bridge."),
		strings.HasPrefix(mod, "UmbraVox.Plugin."),
		mod == "UmbraVox.BuildProfile",
		mod == "UmbraVox.Version",
		strings.HasPrefix(mod, "UmbraVox.Runtime."):
		return 80
	case strings.HasPrefix(mod, "UmbraVox.Consensus."),
		strings.HasPrefix(mod, "UmbraVox.Economics."):
		// Stub/deferred modules — lower bar
		return 50
	default:
		// Unknown module — default to 80%
		return 80
	}
}

// moduleNameRe matches a Haskell module name: starts with an uppercase letter,
// contains dots, with each component starting uppercase (e.g. Foo.Bar.Baz).
var moduleNameRe = regexp.MustCompile(`^[A-Z][a-zA-Z0-9]*\.[A-Z]`)

// pctRe matches one or more percentage values like "75%" on a line.
var pctRe = regexp.MustCompile(`(\d+)%`)

// checkCoverageTargets parses an HPC per-module summary file and enforces the
// tier-based expression coverage targets.  It is the Go equivalent of
// scripts/coverage-check.sh.
//
// Input format (from `hpc report --per-module`):
//
//	ModuleName   75% ( 30/ 40) top-level, 88% ( 70/ 80) alts, 92% (230/250) exprs
func checkCoverageTargets(summaryPath string) int {
	f, err := os.Open(summaryPath)
	if err != nil {
		if os.IsNotExist(err) {
			fmt.Fprintf(os.Stderr, "ERROR: Coverage summary not found: %s\nRun './uv coverage' first.\n", summaryPath)
		} else {
			fmt.Fprintf(os.Stderr, "ERROR: Cannot open coverage summary: %v\n", err)
		}
		return 1
	}
	defer f.Close()

	failures := 0
	checked := 0

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()

		// Skip blank, separator, "Program", and lines that start with a percentage
		trimmed := strings.TrimSpace(line)
		if trimmed == "" || strings.HasPrefix(trimmed, "---") || strings.HasPrefix(trimmed, "Program") {
			continue
		}
		if len(trimmed) > 0 && (trimmed[0] >= '0' && trimmed[0] <= '9') {
			continue
		}

		// First field must look like a Haskell module name (Foo.Bar...)
		fields := strings.Fields(line)
		if len(fields) == 0 {
			continue
		}
		mod := fields[0]
		if !moduleNameRe.MatchString(mod) {
			continue
		}

		// Extract expression coverage: the last percentage on the line
		matches := pctRe.FindAllStringSubmatch(line, -1)
		if len(matches) == 0 {
			continue
		}
		lastMatch := matches[len(matches)-1]
		exprPct, err := strconv.Atoi(lastMatch[1])
		if err != nil {
			continue
		}

		target := coverageTarget(mod)
		if target < 0 {
			// Skip types-only modules
			continue
		}

		checked++
		if exprPct < target {
			fmt.Printf("FAIL: %s — %d%% expression coverage (target: %d%%)\n", mod, exprPct, target)
			failures++
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: Reading coverage summary: %v\n", err)
		return 1
	}

	fmt.Printf("\nChecked %d modules against tier targets.\n", checked)
	if failures > 0 {
		fmt.Printf("FAILED: %d module(s) below target.\n", failures)
		return 1
	}
	fmt.Println("PASSED: All modules meet or exceed their coverage targets.")
	return 0
}

// runFuzz handles: uv fuzz [MODE]
func runFuzz(args []string) int {
	mode := "differential"
	if len(args) > 0 {
		mode = args[0]
	}

	switch mode {
	case "differential":
		log.Info(tag, "Running differential fuzzing...")
		cmd := "cabal build all --enable-tests && cabal test umbravox-test --test-options='differential' 2>&1"
		return execInVM(cmd, qemu.ProfileDev, 60*time.Minute)
	case "afl":
		log.Info(tag, "Building AFL++ fuzz harnesses...")
		cmd := `cabal build all --enable-tests && \
ghc -O2 -o build/fuzz-gcm test/Fuzz/FuzzGCM.hs && \
ghc -O2 -o build/fuzz-ed25519 test/Fuzz/FuzzEd25519.hs && \
ghc -O2 -o build/fuzz-x25519 test/Fuzz/FuzzX25519.hs 2>&1`
		return execInVM(cmd, qemu.ProfileDev, 30*time.Minute)
	default:
		fmt.Fprintf(os.Stderr, "Unknown fuzz mode: %s\nAvailable: differential, afl\n", mode)
		return 2
	}
}

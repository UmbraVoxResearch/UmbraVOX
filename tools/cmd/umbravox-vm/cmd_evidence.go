// SPDX-License-Identifier: Apache-2.0
package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
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
		script := filepath.Join(repoRoot, "scripts", "coverage-check.sh")
		summary := filepath.Join(repoRoot, "build", "coverage", "coverage-summary.txt")
		cmd := exec.Command("bash", script, summary)
		cmd.Dir = repoRoot
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			log.Fail(tag, "Coverage check failed")
			return 1
		}
		return 0
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

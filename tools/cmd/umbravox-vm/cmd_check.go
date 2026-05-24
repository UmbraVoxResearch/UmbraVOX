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

// runCheck handles: uv check [GATE] [--fix]
// Gates: lint, format, license, complexity
// No arg = run all gates.
func runCheck(args []string) int {
	fix := false
	var gate string
	for _, a := range args {
		switch a {
		case "--fix":
			fix = true
		default:
			gate = a
		}
	}

	if gate != "" {
		switch gate {
		case "lint":
			return checkLint()
		case "format":
			return checkFormat()
		case "license":
			return checkLicense(fix)
		case "complexity":
			return checkComplexity()
		default:
			fmt.Fprintf(os.Stderr, "Unknown check gate: %s\nAvailable: lint, format, license, complexity\n", gate)
			return 2
		}
	}

	// Run all gates
	code := 0
	if c := checkLint(); c != 0 {
		code = c
	}
	if c := checkFormat(); c != 0 {
		code = c
	}
	if c := checkLicense(fix); c != 0 {
		code = c
	}
	if c := checkComplexity(); c != 0 {
		code = c
	}
	if code == 0 {
		log.OK(tag, "All quality gates passed.")
	}
	return code
}

func checkLint() int {
	log.Info(tag, "Checking lint (tabs, long lines, trailing newlines)...")
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	srcDirs := []string{"src", "test"}
	failed := false

	for _, dir := range srcDirs {
		d := filepath.Join(repoRoot, dir)
		if _, err := os.Stat(d); err != nil {
			continue
		}

		// Check for tabs
		cmd := exec.Command("grep", "-Prn", "\t", "--include=*.hs", "-r", d)
		if out, err := cmd.Output(); err == nil && len(out) > 0 {
			log.Fail(tag, fmt.Sprintf("Tab characters found in %s/:", dir))
			fmt.Fprintf(os.Stderr, "%s", out)
			failed = true
		}

		// Check for lines > 120 chars
		cmd = exec.Command("grep", "-Pcn", ".{121,}", "--include=*.hs", "-r", d)
		if out, err := cmd.Output(); err == nil && len(out) > 0 {
			log.Fail(tag, fmt.Sprintf("Lines > 120 chars in %s/:", dir))
			fmt.Fprintf(os.Stderr, "%s", out)
			failed = true
		}
	}

	if failed {
		return 1
	}
	log.OK(tag, "Lint: passed")
	return 0
}

func checkFormat() int {
	log.Info(tag, "Checking format (tabs, trailing whitespace)...")
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	failed := false
	for _, dir := range []string{"src", "test"} {
		d := filepath.Join(repoRoot, dir)
		if _, err := os.Stat(d); err != nil {
			continue
		}

		cmd := exec.Command("grep", "-Pn", `\s$`, "--include=*.hs", "-r", d)
		if out, err := cmd.Output(); err == nil && len(out) > 0 {
			log.Fail(tag, fmt.Sprintf("Trailing whitespace in %s/:", dir))
			fmt.Fprintf(os.Stderr, "%s", out)
			failed = true
		}
	}

	if failed {
		return 1
	}
	log.OK(tag, "Format: passed")
	return 0
}

func checkLicense(fix bool) int {
	log.Info(tag, "Checking SPDX license headers...")
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	spdxID := "-- SPDX-License-Identifier: Apache-2.0"
	failed := false

	for _, dir := range []string{"src", "test"} {
		d := filepath.Join(repoRoot, dir)
		if _, err := os.Stat(d); err != nil {
			continue
		}

		// Find .hs files missing SPDX header
		findCmd := exec.Command("find", d, "-name", "*.hs", "-type", "f")
		out, err := findCmd.Output()
		if err != nil {
			continue
		}

		for _, f := range splitLines(string(out)) {
			if f == "" {
				continue
			}
			data, err := os.ReadFile(f)
			if err != nil {
				continue
			}
			content := string(data)
			if len(content) < 5 {
				continue
			}
			// Check first 5 lines for SPDX
			lines := splitLines(content)
			found := false
			limit := 5
			if len(lines) < limit {
				limit = len(lines)
			}
			for i := 0; i < limit; i++ {
				if contains(lines[i], "SPDX-License-Identifier") {
					found = true
					break
				}
			}
			if !found {
				rel, _ := filepath.Rel(repoRoot, f)
				if fix {
					newContent := spdxID + "\n" + content
					if err := os.WriteFile(f, []byte(newContent), 0o644); err != nil {
						log.Fail(tag, fmt.Sprintf("Failed to fix %s: %v", rel, err))
						failed = true
					} else {
						log.Info(tag, fmt.Sprintf("Fixed: %s", rel))
					}
				} else {
					log.Fail(tag, fmt.Sprintf("Missing SPDX header: %s", rel))
					failed = true
				}
			}
		}
	}

	if failed {
		return 1
	}
	log.OK(tag, "License: passed")
	return 0
}

func checkComplexity() int {
	log.Info(tag, "Checking cyclomatic complexity <= 8 (all project code)...")

	// Haskell complexity runs in VM via check-complexity
	cmd := "cabal build all --enable-tests && cabal run check-complexity 2>&1"
	if code := execInVM(cmd, qemu.ProfileDev, 15*time.Minute); code != 0 {
		log.Fail(tag, "Haskell complexity check failed")
		return code
	}

	// Go complexity check (host-side)
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	goDir := filepath.Join(repoRoot, "tools")
	if _, err := os.Stat(goDir); err == nil {
		if _, err := exec.LookPath("gocyclo"); err == nil {
			cmd := exec.Command("gocyclo", "-over", "8", goDir)
			if out, err := cmd.Output(); err == nil && len(out) > 0 {
				log.Fail(tag, "Go functions with complexity > 8:")
				fmt.Fprintf(os.Stderr, "%s", out)
				return 1
			}
		}
	}

	log.OK(tag, "Complexity: passed")
	return 0
}

// helpers

func splitLines(s string) []string {
	var lines []string
	for _, line := range filepath.SplitList(s) {
		lines = append(lines, line)
	}
	// Use manual split since SplitList uses path separator
	lines = nil
	start := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '\n' {
			lines = append(lines, s[start:i])
			start = i + 1
		}
	}
	if start < len(s) {
		lines = append(lines, s[start:])
	}
	return lines
}

func contains(s, sub string) bool {
	return len(s) >= len(sub) && (s == sub || len(sub) == 0 ||
		(len(s) > 0 && findSubstring(s, sub)))
}

func findSubstring(s, sub string) bool {
	for i := 0; i <= len(s)-len(sub); i++ {
		if s[i:i+len(sub)] == sub {
			return true
		}
	}
	return false
}

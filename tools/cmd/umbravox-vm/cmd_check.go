// SPDX-License-Identifier: Apache-2.0
package main

import (
	"encoding/json"
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

// runCheck handles: uv check [GATE] [--fix] [--direct]
// Gates: lint, format, license, complexity
// No arg = run all gates.
// --direct is accepted as a no-op: check already runs on the host.
func runCheck(args []string) int {
	fix := false
	var gate string
	for _, a := range args {
		switch a {
		case "--fix":
			fix = true
		case "--direct":
			// no-op: check runs on host regardless
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
		case "generated-headers":
			return checkGeneratedHeaders()
		case "assurance":
			return checkAssurance()
		case "pre-release":
			return checkPreRelease()
		case "sbom":
			return checkSBOM()
		default:
			fmt.Fprintf(os.Stderr, "Unknown check gate: %s\nAvailable: lint, format, license, complexity, generated-headers, assurance, pre-release, sbom\n", gate)
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
	if c := checkGeneratedHeaders(); c != 0 {
		code = c
	}
	if c := checkAssurance(); c != 0 {
		code = c
	}
	if c := checkSBOM(); c != 0 {
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

	// Generated C branch check (host-side, informational)
	// Constant-time crypto should be straight-line (complexity 1).
	// Branches in generated C indicate the generator is emitting
	// conditionals where it should use ct_select (security concern).
	genDir := filepath.Join(repoRoot, "csrc", "generated")
	if _, err := os.Stat(genDir); err == nil {
		branchCheck := exec.Command("bash", "-c",
			`for f in `+genDir+`/*.c; do
				branches=$(grep -c 'if\s*(' "$f" 2>/dev/null || echo 0)
				if [ "$branches" -gt 0 ]; then
					echo "  $(basename $f): $branches branch(es) — review for constant-time safety"
				fi
			done`)
		if out, _ := branchCheck.Output(); len(out) > 0 {
			log.Fail(tag, "Generated C contains branches (must use ct_select for constant-time):")
			fmt.Fprintf(os.Stderr, "%s", out)
			return 1
		}
		log.OK(tag, "Generated C: no branches (constant-time safe)")
	}

	// Shell script complexity check (host-side)
	// ShellCheck with complexity metrics would be ideal but is not always
	// available. For now, check that no shell function exceeds 100 lines
	// as a rough proxy for complexity.
	scriptsDir := filepath.Join(repoRoot, "scripts")
	if _, err := os.Stat(scriptsDir); err == nil {
		longFuncs := exec.Command("bash", "-c",
			`grep -rl '() {' `+scriptsDir+` --include='*.sh' 2>/dev/null | while read f; do
				awk '/^[a-zA-Z_]+\(\) \{/{name=$1; lines=0; next} /^\}/{if(lines>100) print FILENAME":"name" ("lines" lines)"; lines=0; next} {lines++}' "$f"
			done`)
		if out, _ := longFuncs.Output(); len(out) > 0 {
			log.Warn(tag, "Shell functions > 100 lines (consider refactoring):")
			fmt.Fprintf(os.Stderr, "%s", out)
		}
	}

	log.OK(tag, "Complexity: passed")
	return 0
}

func checkGeneratedHeaders() int {
	log.Info(tag, "Checking generated file headers (CryptoGen marker)...")
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	genDir := filepath.Join(repoRoot, "csrc", "generated")
	entries, err := filepath.Glob(filepath.Join(genDir, "*.c"))
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to glob generated files: %v", err))
		return 1
	}
	if len(entries) == 0 {
		log.Info(tag, "No .c files in csrc/generated/ (run codegen first). Skipping.")
		return 0
	}

	marker := "Auto-generated by CryptoGen"
	failed := false

	for _, f := range entries {
		data, err := os.ReadFile(f)
		if err != nil {
			rel, _ := filepath.Rel(repoRoot, f)
			log.Fail(tag, fmt.Sprintf("Cannot read %s: %v", rel, err))
			failed = true
			continue
		}

		lines := splitLines(string(data))
		limit := 3
		if len(lines) < limit {
			limit = len(lines)
		}

		found := false
		for i := 0; i < limit; i++ {
			if contains(lines[i], marker) {
				found = true
				break
			}
		}

		rel, _ := filepath.Rel(repoRoot, f)
		if found {
			log.OK(tag, fmt.Sprintf("Generated header: PASS  %s", rel))
		} else {
			log.Fail(tag, fmt.Sprintf("Generated header: FAIL  %s (missing \"%s\" in first 3 lines)", rel, marker))
			failed = true
		}
	}

	if failed {
		return 1
	}
	log.OK(tag, "Generated headers: passed")
	return 0
}

func checkAssurance() int {
	log.Info(tag, "Checking assurance matrix (present and not stale)...")
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	matrix := filepath.Join(repoRoot, "doc", "assurance-matrix.md")
	roadmap := filepath.Join(repoRoot, "doc", "assurance-roadmap.md")

	// 1. Check file exists.
	if _, err := os.Stat(matrix); os.IsNotExist(err) {
		log.Fail(tag, fmt.Sprintf("Missing: doc/assurance-matrix.md"))
		fmt.Fprintf(os.Stderr, "  The assurance matrix must exist before release.\n")
		return 1
	}

	// 2. Check for required section.
	matrixData, err := os.ReadFile(matrix)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot read doc/assurance-matrix.md: %v", err))
		return 1
	}
	if !contains(string(matrixData), "## Current Assurance Statement") {
		log.Fail(tag, "Missing section: '## Current Assurance Statement' in doc/assurance-matrix.md")
		fmt.Fprintf(os.Stderr, "  The matrix must contain the current assurance statement.\n")
		return 1
	}

	// 3. Check for bounded MVP statement in roadmap (if roadmap exists).
	if _, err := os.Stat(roadmap); err == nil {
		roadmapData, err := os.ReadFile(roadmap)
		if err != nil {
			log.Fail(tag, fmt.Sprintf("Cannot read doc/assurance-roadmap.md: %v", err))
			return 1
		}
		if !contains(string(roadmapData), "## Bounded MVP Assurance Statement") {
			log.Fail(tag, "Missing section: '## Bounded MVP Assurance Statement' in doc/assurance-roadmap.md")
			return 1
		}
	}

	// 4. Staleness check via git: matrix must be at least as recent as any
	// material change under src/UmbraVox/Crypto or src/UmbraVox/Storage/Encryption.hs.
	cryptoPaths := []string{
		"src/UmbraVox/Crypto",
		"src/UmbraVox/Storage/Encryption.hs",
	}

	if _, err := exec.LookPath("git"); err == nil {
		// Confirm we are inside a work-tree.
		check := exec.Command("git", "-C", repoRoot, "rev-parse", "--is-inside-work-tree")
		if check.Run() == nil {
			matrixHash, matrixTS := gitLastCommit(repoRoot, "doc/assurance-matrix.md")
			if matrixHash != "" {
				var latestHash string
				var latestTS int64
				for _, p := range cryptoPaths {
					full := filepath.Join(repoRoot, filepath.FromSlash(p))
					if _, err := os.Stat(full); err != nil {
						continue
					}
					h, ts := gitLastCommit(repoRoot, p)
					if h != "" && ts > latestTS {
						latestHash = h
						latestTS = ts
					}
				}

				if latestHash != "" && latestTS > matrixTS {
					log.Fail(tag, "Stale: doc/assurance-matrix.md was last updated before the most recent crypto source change.")
					matrixDate := gitCommitDate(repoRoot, matrixHash)
					cryptoDate := gitCommitDate(repoRoot, latestHash)
					fmt.Fprintf(os.Stderr, "  Matrix last updated: %s\n", matrixDate)
					fmt.Fprintf(os.Stderr, "  Crypto last changed: %s\n", cryptoDate)
					fmt.Fprintf(os.Stderr, "  Update the assurance matrix to reflect any material changes.\n")
					return 1
				}
			}
		}
	}

	log.OK(tag, "Assurance: passed")
	return 0
}

// checkSBOM validates build/sbom.cdx.json: existence, JSON validity,
// minimum component count, freshness (<=7 days), and required CycloneDX fields.
// Missing file is informational (warn, don't fail) because the SBOM is generated
// by `./uv sbom`, not by `./uv check`.
func checkSBOM() int {
	log.Info(tag, "Checking SBOM (build/sbom.cdx.json)...")
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	sbomPath := filepath.Join(repoRoot, "build", "sbom.cdx.json")

	data, err := os.ReadFile(sbomPath)
	if os.IsNotExist(err) {
		log.Warn(tag, "SBOM not found: build/sbom.cdx.json (run './uv sbom' to generate)")
		return 0
	}
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot read build/sbom.cdx.json: %v", err))
		return 1
	}

	// 2. Valid JSON
	var doc map[string]interface{}
	if err := json.Unmarshal(data, &doc); err != nil {
		log.Fail(tag, fmt.Sprintf("SBOM is not valid JSON: %v", err))
		return 1
	}

	// 3. Required CycloneDX fields
	bomFormat, _ := doc["bomFormat"].(string)
	specVersion, _ := doc["specVersion"].(string)
	if bomFormat == "" || specVersion == "" {
		log.Fail(tag, "SBOM missing required fields: bomFormat and/or specVersion")
		return 1
	}
	if bomFormat != "CycloneDX" {
		log.Fail(tag, fmt.Sprintf("SBOM bomFormat is %q; expected \"CycloneDX\"", bomFormat))
		return 1
	}

	// 4. Component count >= 10
	components, _ := doc["components"].([]interface{})
	if len(components) < 10 {
		log.Fail(tag, fmt.Sprintf("SBOM contains only %d component(s); expected at least 10 (sanity check)", len(components)))
		return 1
	}

	// 5. Freshness: file mtime <= 7 days ago
	info, err := os.Stat(sbomPath)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot stat build/sbom.cdx.json: %v", err))
		return 1
	}
	age := time.Since(info.ModTime())
	if age > 7*24*time.Hour {
		log.Fail(tag, fmt.Sprintf("SBOM is %.0f days old (max 7); run './uv sbom' to regenerate", age.Hours()/24))
		return 1
	}

	log.OK(tag, fmt.Sprintf("SBOM: passed (bomFormat=%s specVersion=%s components=%d age=%.1fh)",
		bomFormat, specVersion, len(components), age.Hours()))
	return 0
}

// checkPreRelease runs all pre-release assurance gates (Go port of
// scripts/pre-release-check.sh). Invoke with: ./uv check pre-release
func checkPreRelease() int {
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	log.Info(tag, "=== UmbraVOX Pre-Release Check ===")

	pass, fail := 0, 0

	type result struct {
		label string
		code  int
		info  string // optional informational note
	}
	var results []result

	runGate := func(label string, fn func() (int, string)) {
		code, note := fn()
		results = append(results, result{label: label, code: code, info: note})
		if code == 0 {
			pass++
		} else if code > 0 { // code < 0 means skip
			fail++
		}
	}

	fstarDir := filepath.Join(repoRoot, "test", "evidence", "formal-proofs", "fstar")
	proofLogsDir := filepath.Join(repoRoot, "test", "evidence", "formal-proofs", "logs")
	assuranceMatrix := filepath.Join(repoRoot, "test", "evidence", "formal-proofs", "ASSURANCE-MATRIX.md")

	// 1. F* admit check
	runGate("[1/10] F* admit check", func() (int, string) {
		if _, err := os.Stat(fstarDir); err != nil {
			return -1, "skip (fstar dir not found)"
		}
		cmd := exec.Command("grep", "-RIn", `\badmit\b\|admit()`, "--include=*.fst", "-r", fstarDir)
		out, _ := cmd.Output()
		count := 0
		for _, line := range splitLines(string(out)) {
			if line == "" {
				continue
			}
			// Exclude comments and admit_smt
			if contains(line, "*)") || contains(line, "(*") || contains(line, "//") || contains(line, "admit_smt") {
				continue
			}
			count++
		}
		if count == 0 {
			return 0, "0 admit"
		}
		return 1, fmt.Sprintf("%d admit found", count)
	})

	// 2. assume val inventory (always passes; writes inventory file)
	var assumeCount int
	runGate("[2/10] Assume val inventory", func() (int, string) {
		if _, err := os.Stat(fstarDir); err != nil {
			return -1, "skip (fstar dir not found)"
		}
		cmd := exec.Command("grep", "-RIn", "^assume val", "-r", fstarDir)
		out, _ := cmd.Output()
		lines := splitLines(strings.TrimRight(string(out), "\n"))
		for _, l := range lines {
			if l != "" {
				assumeCount++
			}
		}
		// Write inventory file
		if _, err := os.Stat(proofLogsDir); err == nil {
			inventoryLines := make([]string, 0, len(lines))
			for _, l := range lines {
				if l != "" {
					inventoryLines = append(inventoryLines, l)
				}
			}
			// sort for determinism
			sortStrings(inventoryLines)
			inventoryPath := filepath.Join(proofLogsDir, "assume-val-inventory.txt")
			_ = os.WriteFile(inventoryPath, []byte(strings.Join(inventoryLines, "\n")+"\n"), 0o644)
		}
		return 0, fmt.Sprintf("%d assume val declarations", assumeCount)
	})

	// 3. Assumption ledger consistency
	runGate("[3/10] Assumption ledger", func() (int, string) {
		script := filepath.Join(repoRoot, "test", "evidence", "formal-proofs", "check-assumption-ledger.sh")
		if _, err := os.Stat(script); err != nil {
			return -1, "skip (check-assumption-ledger.sh not found)"
		}
		cmd := exec.Command("bash", script)
		cmd.Dir = repoRoot
		if err := cmd.Run(); err != nil {
			return 1, ""
		}
		return 0, ""
	})

	// 4. Proof hygiene
	runGate("[4/10] Proof hygiene", func() (int, string) {
		script := filepath.Join(repoRoot, "test", "evidence", "formal-proofs", "check-proof-hygiene.sh")
		if _, err := os.Stat(script); err != nil {
			return -1, "skip (check-proof-hygiene.sh not found)"
		}
		cmd := exec.Command("bash", script)
		cmd.Dir = repoRoot
		if err := cmd.Run(); err != nil {
			return 1, ""
		}
		return 0, ""
	})

	// 5. Coq build
	runGate("[5/10] Coq build", func() (int, string) {
		if _, err := exec.LookPath("coqc"); err != nil {
			return -1, "skip (coqc not available — run in nix-shell)"
		}
		coqDir := filepath.Join(repoRoot, "test", "evidence", "formal-proofs", "coq")
		if _, err := os.Stat(coqDir); err != nil {
			return -1, "skip (coq dir not found)"
		}
		clean := exec.Command("make", "-C", coqDir, "clean")
		if err := clean.Run(); err != nil {
			return 1, ""
		}
		build := exec.Command("make", "-C", coqDir)
		if err := build.Run(); err != nil {
			return 1, ""
		}
		return 0, ""
	})

	// 6. Infrastructure tests
	runGate("[6/10] Infrastructure tests", func() (int, string) {
		script := filepath.Join(repoRoot, "scripts", "test-infrastructure.sh")
		if _, err := os.Stat(script); err != nil {
			return -1, "skip (test-infrastructure.sh not found)"
		}
		cmd := exec.Command("bash", script)
		cmd.Dir = repoRoot
		if err := cmd.Run(); err != nil {
			return 1, ""
		}
		return 0, ""
	})

	// 7. Differential tests
	runGate("[7/10] Differential tests", func() (int, string) {
		if _, err := exec.LookPath("cabal"); err != nil {
			return -1, "skip (cabal not available)"
		}
		cmd := exec.Command("cabal", "test", "umbravox-test", "--test-options=differential-oracle")
		cmd.Dir = repoRoot
		if err := cmd.Run(); err != nil {
			return 1, ""
		}
		return 0, ""
	})

	// 8. ASSURANCE-MATRIX freshness
	runGate("[8/10] ASSURANCE-MATRIX freshness", func() (int, string) {
		if _, err := os.Stat(assuranceMatrix); err != nil {
			return -1, "skip (ASSURANCE-MATRIX.md not found)"
		}
		data, err := os.ReadFile(assuranceMatrix)
		if err != nil {
			return 1, fmt.Sprintf("cannot read ASSURANCE-MATRIX.md: %v", err)
		}
		// Extract the number from "assume val total N"
		matrixAssume := extractAssumeValTotal(string(data))
		if matrixAssume < 0 {
			return 1, "cannot parse 'assume val total' from ASSURANCE-MATRIX.md"
		}
		if matrixAssume == assumeCount {
			return 0, fmt.Sprintf("matrix=%d, live=%d", matrixAssume, assumeCount)
		}
		return 1, fmt.Sprintf("matrix=%d, live=%d — update ASSURANCE-MATRIX.md", matrixAssume, assumeCount)
	})

	// 9. GPG signing key availability
	runGate("[9/10] GPG signing key availability", func() (int, string) {
		if _, err := exec.LookPath("gpg"); err != nil {
			return 1, "gpg not installed — required for release signing"
		}
		cmd := exec.Command("gpg", "--list-secret-keys", "--keyid-format", "LONG")
		out, _ := cmd.Output()
		count := 0
		for _, line := range splitLines(string(out)) {
			if strings.HasPrefix(line, "sec") {
				count++
			}
		}
		if count > 0 {
			return 0, fmt.Sprintf("%d secret key(s) available", count)
		}
		return 1, "no GPG secret keys found — release signing requires a key"
	})

	// 10. Reproducibility check
	runGate("[10/10] Reproducibility check", func() (int, string) {
		script := filepath.Join(repoRoot, "scripts", "release-reproducibility-check.sh")
		info, err := os.Stat(script)
		if err != nil || info.Mode()&0o111 == 0 {
			return -1, "skip (release-reproducibility-check.sh not found or not executable)"
		}
		cmd := exec.Command("bash", script)
		cmd.Dir = repoRoot
		if err := cmd.Run(); err != nil {
			return 1, ""
		}
		return 0, ""
	})

	// Print results
	fmt.Println()
	for _, r := range results {
		switch {
		case r.code < 0:
			suffix := ""
			if r.info != "" {
				suffix = " (" + r.info + ")"
			}
			log.Info(tag, fmt.Sprintf("%s: SKIP%s", r.label, suffix))
		case r.code == 0:
			suffix := ""
			if r.info != "" {
				suffix = " (" + r.info + ")"
			}
			log.OK(tag, fmt.Sprintf("%s: PASS%s", r.label, suffix))
		default:
			suffix := ""
			if r.info != "" {
				suffix = " — " + r.info
			}
			log.Fail(tag, fmt.Sprintf("%s: FAIL%s", r.label, suffix))
		}
	}

	fmt.Println()
	log.Info(tag, fmt.Sprintf("Results: Pass=%d  Fail=%d", pass, fail))
	if fail > 0 {
		log.Fail(tag, "PRE-RELEASE CHECK FAILED")
		return 1
	}
	log.OK(tag, "All pre-release checks passed.")
	return 0
}

// extractAssumeValTotal parses the count from a line like "assume val total N"
// in ASSURANCE-MATRIX.md. Returns -1 if not found.
func extractAssumeValTotal(content string) int {
	for _, line := range splitLines(content) {
		if !contains(line, "assume val total") {
			continue
		}
		// Extract the first number in the line
		fields := splitFields(line)
		for _, f := range fields {
			var n int
			if _, err := fmt.Sscan(f, &n); err == nil {
				return n
			}
		}
	}
	return -1
}

// sortStrings sorts a string slice in place (avoids importing sort in this file).
func sortStrings(ss []string) {
	// Insertion sort — slice is small (assume val inventory).
	for i := 1; i < len(ss); i++ {
		key := ss[i]
		j := i - 1
		for j >= 0 && ss[j] > key {
			ss[j+1] = ss[j]
			j--
		}
		ss[j+1] = key
	}
}

// gitLastCommit returns the commit hash and Unix timestamp of the most recent
// commit that touched path (relative to repoRoot). Returns ("", 0) on failure.
func gitLastCommit(repoRoot, path string) (string, int64) {
	cmd := exec.Command("git", "-C", repoRoot, "log", "-1", "--format=%H %ct", "--", path)
	out, err := cmd.Output()
	if err != nil || len(out) == 0 {
		return "", 0
	}
	parts := splitFields(strings.TrimSpace(string(out)))
	if len(parts) != 2 {
		return "", 0
	}
	var ts int64
	fmt.Sscan(parts[1], &ts)
	return parts[0], ts
}

// gitCommitDate returns a human-readable date string for a commit hash.
func gitCommitDate(repoRoot, hash string) string {
	cmd := exec.Command("git", "-C", repoRoot, "log", "-1", "--format=%ci", hash)
	out, err := cmd.Output()
	if err != nil {
		return hash
	}
	return strings.TrimSpace(string(out))
}

// splitFields splits s on whitespace, returning non-empty tokens.
func splitFields(s string) []string {
	var fields []string
	start := -1
	for i := 0; i < len(s); i++ {
		isSpace := s[i] == ' ' || s[i] == '\t' || s[i] == '\n' || s[i] == '\r'
		if !isSpace && start == -1 {
			start = i
		} else if isSpace && start != -1 {
			fields = append(fields, s[start:i])
			start = -1
		}
	}
	if start != -1 {
		fields = append(fields, s[start:])
	}
	return fields
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

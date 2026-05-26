// SPDX-License-Identifier: Apache-2.0
package main

import (
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
)

// runE2E handles: uv test e2e [--bootstrap]
//
// Default (no flag): warm pipeline from existing VM image.
//   1. Build + codegen in dev VM
//   2. Run test suite in dev VM
//   3. Quality gates
//   4. Build runtime images
//   5. Verify ./uv run launches app
//
// --bootstrap: cold start from scratch (pre-release validation).
//   0. Clean all build artifacts
//   1. Build dev VM image from scratch
//   2-5. Same as above
//   6. Verify SBOM generation
//
// Takes 30-60 min (warm) or 60-90 min (bootstrap).
func runE2E(args []string) int {
	bootstrap := false
	for _, a := range args {
		if a == "--bootstrap" {
			bootstrap = true
		}
	}

	start := time.Now()
	var steps []struct {
		name string
		fn   func() int
	}

	if bootstrap {
		steps = append(steps,
			struct {
				name string
				fn   func() int
			}{"Clean all artifacts", func() int { return runClean([]string{"--all"}) }},
			struct {
				name string
				fn   func() int
			}{"Build dev VM image (cold bootstrap)", func() int { return vmBuildImage(nil) }},
		)
	}

	steps = append(steps,
		struct {
			name string
			fn   func() int
		}{"Build (codegen + compile)", func() int { return runBuild(nil) }},
		struct {
			name string
			fn   func() int
		}{"Test (required fast gate)", func() int { return runTest(nil) }},
		struct {
			name string
			fn   func() int
		}{"Quality gates", func() int { return runCheck(nil) }},
		struct {
			name string
			fn   func() int
		}{"Build runtime images", func() int { return vmBuildRuntimeImage(nil) }},
		struct {
			name string
			fn   func() int
		}{"Verify runtime bundle", func() int {
			repoRoot, err := repo.Root()
			if err != nil {
				return 1
			}
			bin := filepath.Join(repoRoot, "build", "runtime", "bin", "umbravox")
			if _, err := os.Stat(bin); err != nil {
				log.Fail(tag, "Runtime bundle missing: build/runtime/bin/umbravox")
				return 1
			}
			log.OK(tag, "Runtime bundle present")
			return 0
		}},
		struct {
			name string
			fn   func() int
		}{"Generate SBOM", func() int { return runSBOM(nil) }},
	)

	passed := 0
	failed := 0

	for i, step := range steps {
		log.Info(tag, fmt.Sprintf("[%d/%d] %s...", i+1, len(steps), step.name))
		stepStart := time.Now()
		code := step.fn()
		elapsed := time.Since(stepStart).Round(time.Second)

		if code == 0 {
			log.OK(tag, fmt.Sprintf("[%d/%d] %s — PASS (%s)", i+1, len(steps), step.name, elapsed))
			passed++
		} else {
			log.Fail(tag, fmt.Sprintf("[%d/%d] %s — FAIL (exit %d, %s)", i+1, len(steps), step.name, code, elapsed))
			failed++
			// Stop on first failure — subsequent steps depend on earlier ones
			break
		}
	}

	total := time.Since(start).Round(time.Second)
	fmt.Fprintln(os.Stderr)
	log.Info(tag, fmt.Sprintf("E2E Summary: %d passed, %d failed (%s total)", passed, failed, total))

	if failed > 0 {
		log.Fail(tag, "End-to-end test FAILED")
		return 1
	}
	log.OK(tag, "End-to-end test PASSED")
	return 0
}

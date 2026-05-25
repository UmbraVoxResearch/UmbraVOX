// SPDX-License-Identifier: Apache-2.0
package main

import (
	"fmt"
	"os"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
)

// runE2E handles: uv test e2e
// Full end-to-end pipeline test from clean state. All steps run in VMs.
//
//  1. Build dev VM image (bootstrap)
//  2. Build + codegen in dev VM
//  3. Run test suite in dev VM
//  4. Run quality gates in dev VM
//  5. Build runtime images
//  6. Verify runtime bundle extraction
//  7. Boot Firecracker runtime VM
//  8. Build Signal Server JAR
//
// This is the CI-equivalent full validation. Takes 30-60 minutes.
func runE2E(args []string) int {
	start := time.Now()
	steps := []struct {
		name string
		fn   func() int
	}{
		{"Build dev VM image", func() int { return vmBuildImage(nil) }},
		{"Build (codegen + compile)", func() int { return runBuild(nil) }},
		{"Test (required fast gate)", func() int { return runTest(nil) }},
		{"Quality gates", func() int { return runCheck(nil) }},
		{"Build runtime images", func() int { return vmBuildRuntimeImage(nil) }},
	}

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

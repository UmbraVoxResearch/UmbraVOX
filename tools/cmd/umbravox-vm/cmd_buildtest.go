// SPDX-License-Identifier: Apache-2.0
package main

import (
	"fmt"
	"os"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
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

// runBuild handles: uv build [--docs]
// Codegen always runs before build to ensure generated files match specs.
func runBuild(args []string) int {
	docs := false
	for _, a := range args {
		if a == "--docs" {
			docs = true
		}
	}

	var cmd string
	if docs {
		cmd = "cabal haddock --haddock-html 2>&1"
	} else {
		// Always run codegen before build to ensure generated C/Haskell/FFI
		// files are in sync with .spec sources. This provides assurance that
		// the generated code matches the specifications.
		cmd = "cabal run codegen && cabal build all --enable-tests 2>&1 | tail -20"
	}

	return execInVM(cmd, qemu.ProfileDev, 30*time.Minute)
}

// runTest handles: uv test [SUITE] [--list]
func runTest(args []string) int {
	if len(args) > 0 && (args[0] == "--list" || args[0] == "-l") {
		fmt.Println("Available test suites:")
		for name := range testSuites {
			fmt.Printf("  %s\n", name)
		}
		fmt.Println("  ephemeral  (builds fresh image, tests, discards)")
		fmt.Println("\nUsage: ./uv test [SUITE]")
		fmt.Println("No suite argument runs the required fast gate.")
		return 0
	}

	if len(args) == 0 {
		// Default: required fast gate
		cmd := "cabal build all --enable-tests && cabal test umbravox-test --test-options='required' 2>&1"
		return execInVM(cmd, qemu.ProfileDev, 25*time.Minute)
	}

	suite := args[0]

	if suite == "ephemeral" {
		return runTestEphemeral()
	}

	opts, ok := testSuites[suite]
	if !ok {
		fmt.Fprintf(os.Stderr, "Unknown test suite: %s\nRun './uv test --list' for available suites.\n", suite)
		return 2
	}

	timeout := 25 * time.Minute
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

// runVerify handles: uv verify
func runVerify() int {
	cmd := "cabal build all --enable-tests && cabal run fstar-verify 2>&1"
	return execInVM(cmd, qemu.ProfileDev, 60*time.Minute)
}

// runAll handles: uv (no args) — build + test + check
func runAll() int {
	// Build + test in VM
	cmd := `cabal build all --enable-tests 2>&1 | tail -20 && \
cabal test umbravox-test --test-options='required' 2>&1`
	if code := execInVM(cmd, qemu.ProfileDev, 30*time.Minute); code != 0 {
		return code
	}
	// Host-side checks
	return runCheck(nil)
}

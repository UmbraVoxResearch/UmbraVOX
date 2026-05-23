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
	"runtime"
)

const usage = `vm-build-test — UmbraVOX universal VM build+test runner

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

func main() {
	flag.Usage = func() { fmt.Fprint(os.Stderr, usage) }
	flag.Parse()

	args := flag.Args()
	if len(args) == 0 {
		flag.Usage()
		os.Exit(2)
	}

	subcmd := args[0]
	switch subcmd {
	case "run":
		fs := flag.NewFlagSet("run", flag.ExitOnError)
		workDir := fs.String("work-dir", "", "source directory (default: auto-detect)")
		outputDir := fs.String("output-dir", "/output", "results output directory")
		cabalJobs := fs.Int("cabal-jobs", runtime.NumCPU(), "parallel cabal build jobs")
		skipTest := fs.Bool("skip-test", false, "skip test execution (build only)")
		timeout := fs.String("timeout", "60m", "overall timeout")
		fs.Parse(args[1:])

		fmt.Println("[vm-build-test] run: not yet implemented")
		fmt.Printf("[vm-build-test] platform:   %s/%s\n", runtime.GOOS, runtime.GOARCH)
		if *workDir != "" {
			fmt.Printf("[vm-build-test] work-dir:   %s\n", *workDir)
		} else {
			fmt.Println("[vm-build-test] work-dir:   (auto-detect)")
		}
		fmt.Printf("[vm-build-test] output-dir: %s\n", *outputDir)
		fmt.Printf("[vm-build-test] cabal-jobs: %d\n", *cabalJobs)
		fmt.Printf("[vm-build-test] skip-test:  %v\n", *skipTest)
		fmt.Printf("[vm-build-test] timeout:    %s\n", *timeout)
		fmt.Println("[vm-build-test] This will execute:")
		fmt.Println("[vm-build-test]   1. cabal update")
		fmt.Println("[vm-build-test]   2. cabal build all")
		if !*skipTest {
			fmt.Println("[vm-build-test]   3. cabal test umbravox-test")
		}

	case "status":
		fs := flag.NewFlagSet("status", flag.ExitOnError)
		outputDir := fs.String("output-dir", "/output", "results directory")
		fs.Parse(args[1:])

		fmt.Println("[vm-build-test] status: not yet implemented")
		fmt.Printf("[vm-build-test] output-dir: %s\n", *outputDir)

	case "help", "-h", "--help":
		flag.Usage()

	default:
		fmt.Fprintf(os.Stderr, "vm-build-test: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		os.Exit(2)
	}
}

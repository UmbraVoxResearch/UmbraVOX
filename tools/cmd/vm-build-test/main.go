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
)

const usage = `vm-build-test — UmbraVOX universal VM build+test runner

Usage:
  vm-build-test <subcommand> [flags]

Subcommands:
  run       Run build+test sequence in the VM guest
  status    Show last run results

Flags for 'run':
  --work-dir <path>    Source directory (default: /work/umbravox or ~/umbravox)

This runs inside a QEMU VM guest. The platform setup script must install
GHC and cabal-install before invoking this tool.

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
		runFS := flag.NewFlagSet("run", flag.ExitOnError)
		workDir := runFS.String("work-dir", "", "source directory (default: auto-detect)")
		runFS.Parse(args[1:])

		fmt.Println("[vm-build-test] run: not yet implemented")
		if *workDir != "" {
			fmt.Printf("[vm-build-test] work-dir: %s\n", *workDir)
		}
		fmt.Println("[vm-build-test] This will execute:")
		fmt.Println("[vm-build-test]   1. cabal update")
		fmt.Println("[vm-build-test]   2. cabal build all")
		fmt.Println("[vm-build-test]   3. cabal test umbravox-test")
	case "status":
		fmt.Println("[vm-build-test] status: not yet implemented")
	case "help", "-h", "--help":
		flag.Usage()
	default:
		fmt.Fprintf(os.Stderr, "vm-build-test: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		os.Exit(2)
	}
}

// Command vm-smoke runs the UmbraVOX isolated VM smoke pipeline.
//
// This is the Go replacement for scripts/vm-smoke-run.sh. The smoke
// pipeline runs INSIDE the NixOS QEMU guest and exercises build, test,
// verification, complexity checks, and release artifact generation.
//
// Usage:
//
//	vm-smoke run             Run the full smoke pipeline in-guest
//	vm-smoke status          Show last smoke run results
//
// Requires: runs inside a NixOS QEMU guest with dev tools pre-installed.
package main

import (
	"flag"
	"fmt"
	"os"
)

const usage = `vm-smoke — UmbraVOX isolated VM smoke pipeline

Usage:
  vm-smoke <subcommand>

Subcommands:
  run       Run the full smoke pipeline (build, test, verify, complexity,
            license, format-check, release-linux)
  status    Show results from the last smoke run

This tool runs INSIDE the NixOS QEMU guest. The source tree is expected
on /dev/vdb (ext2, read-only). All dev tools are pre-installed via the
NixOS VM image. No network access is needed or available.
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
		fmt.Println("[vm-smoke] run: not yet implemented")
		fmt.Println("[vm-smoke] This will execute the full smoke pipeline:")
		fmt.Println("[vm-smoke]   1. cabal build all --enable-tests")
		fmt.Println("[vm-smoke]   2. umbravox-test (required)")
		fmt.Println("[vm-smoke]   3. F* verification")
		fmt.Println("[vm-smoke]   4. Complexity check")
		fmt.Println("[vm-smoke]   5. License check")
		fmt.Println("[vm-smoke]   6. Format check")
		fmt.Println("[vm-smoke]   7. Release artifact generation")
	case "status":
		fmt.Println("[vm-smoke] status: not yet implemented")
		fmt.Println("[vm-smoke] This will display the last smoke run results.")
	case "help", "-h", "--help":
		flag.Usage()
	default:
		fmt.Fprintf(os.Stderr, "vm-smoke: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		os.Exit(2)
	}
}

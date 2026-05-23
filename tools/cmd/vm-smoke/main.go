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
  vm-smoke <subcommand> [flags]

Subcommands:
  run       Run the full smoke pipeline (build, test, verify, complexity,
            license, format-check, release-linux)
  status    Show results from the last smoke run

Flags for 'run':
  --work-dir <path>      Source directory (default: /work/umbravox)
  --output-dir <path>    Results output directory (default: /output)
  --skip <stages>        Comma-separated stages to skip
                         (build,test,verify,complexity,license,format,release)
  --only <stages>        Run only these stages (comma-separated)
  --verbose              Verbose output for each stage

Flags for 'status':
  --output-dir <path>    Results directory (default: /output)
  --json                 Output results as JSON

This tool runs INSIDE the NixOS QEMU guest. The source tree is expected
on /dev/vdb (ext2, read-only). All dev tools are pre-installed via the
NixOS VM image. No network access is needed or available.

Smoke pipeline stages:
  1. build       cabal build all --enable-tests
  2. test        cabal test umbravox-test (required gate)
  3. verify      F* verification
  4. complexity  Complexity metrics check
  5. license     License header check
  6. format      Source formatting check
  7. release     Release artifact generation (linux bundle)
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
		workDir := fs.String("work-dir", "/work/umbravox", "source directory")
		outputDir := fs.String("output-dir", "/output", "results output directory")
		skip := fs.String("skip", "", "comma-separated stages to skip")
		only := fs.String("only", "", "run only these stages (comma-separated)")
		verbose := fs.Bool("verbose", false, "verbose output for each stage")
		fs.Parse(args[1:])

		fmt.Println("[vm-smoke] run: not yet implemented")
		fmt.Printf("[vm-smoke] work-dir:   %s\n", *workDir)
		fmt.Printf("[vm-smoke] output-dir: %s\n", *outputDir)
		if *skip != "" {
			fmt.Printf("[vm-smoke] skip:       %s\n", *skip)
		}
		if *only != "" {
			fmt.Printf("[vm-smoke] only:       %s\n", *only)
		}
		if *verbose {
			fmt.Println("[vm-smoke] verbose:    true")
		}
		fmt.Println("[vm-smoke] This will execute the full smoke pipeline:")
		fmt.Println("[vm-smoke]   1. cabal build all --enable-tests")
		fmt.Println("[vm-smoke]   2. umbravox-test (required)")
		fmt.Println("[vm-smoke]   3. F* verification")
		fmt.Println("[vm-smoke]   4. Complexity check")
		fmt.Println("[vm-smoke]   5. License check")
		fmt.Println("[vm-smoke]   6. Format check")
		fmt.Println("[vm-smoke]   7. Release artifact generation")

	case "status":
		fs := flag.NewFlagSet("status", flag.ExitOnError)
		outputDir := fs.String("output-dir", "/output", "results directory")
		jsonOut := fs.Bool("json", false, "output results as JSON")
		fs.Parse(args[1:])

		fmt.Println("[vm-smoke] status: not yet implemented")
		fmt.Printf("[vm-smoke] output-dir: %s\n", *outputDir)
		if *jsonOut {
			fmt.Println("[vm-smoke] format:     JSON")
		}
		fmt.Println("[vm-smoke] This will display the last smoke run results.")

	case "help", "-h", "--help":
		flag.Usage()

	default:
		fmt.Fprintf(os.Stderr, "vm-smoke: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		os.Exit(2)
	}
}

// Command release manages the UmbraVOX release pipeline.
//
// Usage:
//
//	release check       Run pre-release checks (reproducibility, deps, versions)
//	release sign        Sign release artifacts (GPG/minisign)
//	release package     Build platform-specific release packages
//	release smoke       Run release smoke tests (AppImage, Linux, microVM, QEMU)
//	release lane        Run a release lane (firecracker, qemu, readiness-*)
//	release gate        Run release gate assurance checks
//
// This replaces the scripts/release-*.sh and scripts/pre-release-check.sh family.
package main

import (
	"flag"
	"fmt"
	"os"
)

const usage = `release — UmbraVOX release pipeline

Usage:
  release <subcommand> [flags]

Subcommands:
  check      Run pre-release checks (reproducibility, deps, versions)
  sign       Sign release artifacts (GPG/minisign)
  package    Build platform-specific release packages
  smoke      Run release smoke tests
  lane       Run a release lane
  gate       Run release gate assurance checks

Subcommand details:

  release smoke <target>
    Targets: appimage, linux, microvm, qemu-profile

  release lane <name>
    Lanes: firecracker, qemu, readiness-all, readiness-bsd,
           readiness-lib, readiness-linux-arm64, readiness-linux-x86_64,
           readiness-macos, readiness-windows

  release package [--platform <name>]
    Platforms: linux-x86_64, linux-arm64, macos, windows, freebsd, openbsd

  release gate
    Runs the full gate assurance pipeline.
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
	case "check":
		fmt.Println("[release] check: not yet implemented")
		fmt.Println("[release] This will run pre-release checks:")
		fmt.Println("[release]   - Reproducibility verification")
		fmt.Println("[release]   - Dependency audit")
		fmt.Println("[release]   - Version consistency")
	case "sign":
		fmt.Println("[release] sign: not yet implemented")
		fmt.Println("[release] This will sign release artifacts with GPG/minisign.")
	case "package":
		pkgFS := flag.NewFlagSet("package", flag.ExitOnError)
		platform := pkgFS.String("platform", "", "target platform")
		pkgFS.Parse(args[1:])

		fmt.Println("[release] package: not yet implemented")
		if *platform != "" {
			fmt.Printf("[release] platform: %s\n", *platform)
		}
		fmt.Println("[release] This will build platform-specific release packages.")
	case "smoke":
		target := "all"
		if len(args) > 1 {
			target = args[1]
		}
		fmt.Printf("[release] smoke %s: not yet implemented\n", target)
		fmt.Println("[release] Smoke targets: appimage, linux, microvm, qemu-profile")
	case "lane":
		if len(args) < 2 {
			fmt.Fprintln(os.Stderr, "[release] lane requires a lane name")
			fmt.Fprintln(os.Stderr, "[release] Lanes: firecracker, qemu, readiness-all, readiness-bsd,")
			fmt.Fprintln(os.Stderr, "[release]        readiness-lib, readiness-linux-arm64, readiness-linux-x86_64,")
			fmt.Fprintln(os.Stderr, "[release]        readiness-macos, readiness-windows")
			os.Exit(2)
		}
		lane := args[1]
		fmt.Printf("[release] lane %s: not yet implemented\n", lane)
	case "gate":
		fmt.Println("[release] gate: not yet implemented")
		fmt.Println("[release] This will run the full gate assurance pipeline.")
	case "help", "-h", "--help":
		flag.Usage()
	default:
		fmt.Fprintf(os.Stderr, "release: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		os.Exit(2)
	}
}

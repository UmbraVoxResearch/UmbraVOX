// Command vm-platform runs platform-specific VM setup for UmbraVOX
// cross-platform testing.
//
// This replaces the scripts/vm-*-setup.sh family:
//   - vm-freebsd-setup.sh
//   - vm-openbsd-setup.sh
//   - vm-netbsd-setup.sh
//   - vm-dragonfly-setup.sh (DragonFlyBSD)
//   - vm-illumos-setup.sh   (OmniOS/SmartOS/OpenIndiana)
//
// Usage:
//
//	vm-platform freebsd [--version 14.1]    Set up FreeBSD VM
//	vm-platform openbsd [--version 7.5]     Set up OpenBSD VM
//	vm-platform netbsd [--version 10.0]     Set up NetBSD VM
//	vm-platform dragonfly                   Set up DragonFlyBSD VM
//	vm-platform illumos [--dist omnios]     Set up illumos VM (OmniOS/SmartOS/OpenIndiana)
package main

import (
	"flag"
	"fmt"
	"os"
)

const usage = `vm-platform — UmbraVOX cross-platform VM setup

Usage:
  vm-platform <platform> [flags]

Platforms:
  freebsd      Set up FreeBSD VM for build+test
  openbsd      Set up OpenBSD VM for build+test
  netbsd       Set up NetBSD VM for build+test
  dragonfly    Set up DragonFlyBSD VM for build+test
  illumos      Set up illumos VM (OmniOS/SmartOS/OpenIndiana)

Flags:
  --version <ver>    OS version to install (platform-specific default)
  --dist <name>      Distribution variant (illumos only: omnios, smartos, openindiana)
  --work-dir <path>  Guest work directory (default: /work/umbravox)

Each platform setup:
  1. Downloads or locates the platform VM image
  2. Boots the VM with QEMU
  3. Installs GHC and cabal-install via the platform package manager
  4. Copies the source tree into the guest
  5. Runs vm-build-test for the platform
`

func main() {
	version := flag.String("version", "", "OS version")
	dist := flag.String("dist", "", "distribution variant (illumos only)")
	workDir := flag.String("work-dir", "/work/umbravox", "guest work directory")

	flag.Usage = func() { fmt.Fprint(os.Stderr, usage) }
	flag.Parse()

	args := flag.Args()
	if len(args) == 0 {
		flag.Usage()
		os.Exit(2)
	}

	platform := args[0]
	switch platform {
	case "freebsd":
		fmt.Println("[vm-platform] freebsd: not yet implemented")
		if *version != "" {
			fmt.Printf("[vm-platform] version: %s\n", *version)
		}
		fmt.Printf("[vm-platform] work-dir: %s\n", *workDir)
		fmt.Println("[vm-platform] This will set up a FreeBSD VM with GHC and cabal-install.")
	case "openbsd":
		fmt.Println("[vm-platform] openbsd: not yet implemented")
		if *version != "" {
			fmt.Printf("[vm-platform] version: %s\n", *version)
		}
		fmt.Printf("[vm-platform] work-dir: %s\n", *workDir)
		fmt.Println("[vm-platform] This will set up an OpenBSD VM with GHC and cabal-install.")
	case "netbsd":
		fmt.Println("[vm-platform] netbsd: not yet implemented")
		if *version != "" {
			fmt.Printf("[vm-platform] version: %s\n", *version)
		}
		fmt.Printf("[vm-platform] work-dir: %s\n", *workDir)
		fmt.Println("[vm-platform] This will set up a NetBSD VM with GHC and cabal-install.")
	case "dragonfly":
		fmt.Println("[vm-platform] dragonfly: not yet implemented")
		if *version != "" {
			fmt.Printf("[vm-platform] version: %s\n", *version)
		}
		fmt.Printf("[vm-platform] work-dir: %s\n", *workDir)
		fmt.Println("[vm-platform] This will set up a DragonFlyBSD VM with GHC and cabal-install.")
	case "illumos":
		fmt.Println("[vm-platform] illumos: not yet implemented")
		if *version != "" {
			fmt.Printf("[vm-platform] version: %s\n", *version)
		}
		if *dist != "" {
			fmt.Printf("[vm-platform] dist: %s\n", *dist)
		} else {
			fmt.Println("[vm-platform] dist: omnios (default)")
		}
		fmt.Printf("[vm-platform] work-dir: %s\n", *workDir)
		fmt.Println("[vm-platform] This will set up an illumos VM (OmniOS/SmartOS/OpenIndiana)")
		fmt.Println("[vm-platform] with GHC and cabal-install.")
	case "help", "-h", "--help":
		flag.Usage()
	default:
		fmt.Fprintf(os.Stderr, "vm-platform: unknown platform %q\n\n", platform)
		flag.Usage()
		os.Exit(2)
	}
}

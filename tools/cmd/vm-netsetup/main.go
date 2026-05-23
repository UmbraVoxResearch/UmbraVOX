// Command vm-netsetup creates or tears down TAP+bridge infrastructure
// for dual-LAN integration testing.
//
// This is the Go replacement for scripts/vm-network-setup.sh.
//
// Usage:
//
//	sudo vm-netsetup setup [--agents N]    Create dual-LAN bridges and TAP devices
//	sudo vm-netsetup teardown              Remove bridges and TAP devices
//	vm-netsetup status                     Show current bridge status
//
// Requires: CAP_NET_ADMIN (sudo) for setup and teardown.
package main

import (
	"flag"
	"fmt"
	"os"
)

const usage = `vm-netsetup — UmbraVOX VM network setup (TAP+bridge)

Usage:
  vm-netsetup <subcommand> [flags]

Subcommands:
  setup      Create dual-LAN bridge infrastructure (requires sudo)
  teardown   Remove bridges and TAP devices (requires sudo)
  status     Show current bridge status

Flags for 'setup':
  --agents <n>    Number of agents (default: 6)

Network topology:
  LAN A: br-umbravox-a (10.0.42.0/24)
  LAN B: br-umbravox-b (10.0.43.0/24)
  IP forwarding enabled between bridges.
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
	case "setup":
		setupFS := flag.NewFlagSet("setup", flag.ExitOnError)
		agents := setupFS.Int("agents", 6, "number of agents")
		setupFS.Parse(args[1:])

		fmt.Println("[vm-netsetup] setup: not yet implemented")
		fmt.Printf("[vm-netsetup] agents: %d\n", *agents)
		fmt.Println("[vm-netsetup] This will create:")
		fmt.Println("[vm-netsetup]   - br-umbravox-a (10.0.42.0/24)")
		fmt.Println("[vm-netsetup]   - br-umbravox-b (10.0.43.0/24)")
		fmt.Printf("[vm-netsetup]   - %d TAP devices\n", *agents)
	case "teardown":
		fmt.Println("[vm-netsetup] teardown: not yet implemented")
		fmt.Println("[vm-netsetup] This will remove all TAP devices and bridges.")
	case "status":
		fmt.Println("[vm-netsetup] status: not yet implemented")
		fmt.Println("[vm-netsetup] This will show current bridge status.")
	case "help", "-h", "--help":
		flag.Usage()
	default:
		fmt.Fprintf(os.Stderr, "vm-netsetup: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		os.Exit(2)
	}
}

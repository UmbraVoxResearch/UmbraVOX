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
  status     Show current bridge status (no sudo needed)

Flags for 'setup':
  --agents <n>          Number of agents per LAN (default: 6)
  --lan-a-subnet <cidr> LAN A subnet (default: 10.0.42.0/24)
  --lan-b-subnet <cidr> LAN B subnet (default: 10.0.43.0/24)
  --bridge-a <name>     LAN A bridge name (default: br-umbravox-a)
  --bridge-b <name>     LAN B bridge name (default: br-umbravox-b)
  --enable-forward      Enable IP forwarding between bridges (default: true)

Flags for 'teardown':
  --bridge-a <name>     LAN A bridge name (default: br-umbravox-a)
  --bridge-b <name>     LAN B bridge name (default: br-umbravox-b)

Flags for 'status':
  --json                Output as JSON

Network topology:
  LAN A: br-umbravox-a (10.0.42.0/24) — agents 1..N
  LAN B: br-umbravox-b (10.0.43.0/24) — agents 1..N
  Each agent gets a TAP device on one or both LANs.
  IP forwarding is enabled between bridges for cross-LAN tests.
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
		fs := flag.NewFlagSet("setup", flag.ExitOnError)
		agents := fs.Int("agents", 6, "number of agents per LAN")
		lanA := fs.String("lan-a-subnet", "10.0.42.0/24", "LAN A subnet")
		lanB := fs.String("lan-b-subnet", "10.0.43.0/24", "LAN B subnet")
		bridgeA := fs.String("bridge-a", "br-umbravox-a", "LAN A bridge name")
		bridgeB := fs.String("bridge-b", "br-umbravox-b", "LAN B bridge name")
		enableFwd := fs.Bool("enable-forward", true, "enable IP forwarding between bridges")
		fs.Parse(args[1:])

		if os.Geteuid() != 0 {
			fmt.Fprintln(os.Stderr, "[vm-netsetup] error: setup requires root (use sudo)")
			os.Exit(2)
		}

		fmt.Println("[vm-netsetup] setup: not yet implemented")
		fmt.Printf("[vm-netsetup] agents:         %d\n", *agents)
		fmt.Printf("[vm-netsetup] LAN A:          %s (%s)\n", *bridgeA, *lanA)
		fmt.Printf("[vm-netsetup] LAN B:          %s (%s)\n", *bridgeB, *lanB)
		fmt.Printf("[vm-netsetup] IP forwarding:  %v\n", *enableFwd)
		fmt.Println("[vm-netsetup] This will create:")
		fmt.Printf("[vm-netsetup]   - %s (%s)\n", *bridgeA, *lanA)
		fmt.Printf("[vm-netsetup]   - %s (%s)\n", *bridgeB, *lanB)
		fmt.Printf("[vm-netsetup]   - %d TAP devices per LAN\n", *agents)

	case "teardown":
		fs := flag.NewFlagSet("teardown", flag.ExitOnError)
		bridgeA := fs.String("bridge-a", "br-umbravox-a", "LAN A bridge name")
		bridgeB := fs.String("bridge-b", "br-umbravox-b", "LAN B bridge name")
		fs.Parse(args[1:])

		if os.Geteuid() != 0 {
			fmt.Fprintln(os.Stderr, "[vm-netsetup] error: teardown requires root (use sudo)")
			os.Exit(2)
		}

		fmt.Println("[vm-netsetup] teardown: not yet implemented")
		fmt.Printf("[vm-netsetup] Bridges: %s, %s\n", *bridgeA, *bridgeB)
		fmt.Println("[vm-netsetup] This will remove all TAP devices and bridges.")

	case "status":
		fs := flag.NewFlagSet("status", flag.ExitOnError)
		jsonOut := fs.Bool("json", false, "output as JSON")
		fs.Parse(args[1:])

		fmt.Println("[vm-netsetup] status: not yet implemented")
		if *jsonOut {
			fmt.Println("[vm-netsetup] format: JSON")
		}
		fmt.Println("[vm-netsetup] This will show current bridge status.")

	case "help", "-h", "--help":
		flag.Usage()

	default:
		fmt.Fprintf(os.Stderr, "vm-netsetup: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		os.Exit(2)
	}
}

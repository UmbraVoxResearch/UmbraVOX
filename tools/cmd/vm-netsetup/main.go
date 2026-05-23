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
	"os/exec"
	"os/user"
	"strconv"
	"strings"
)

const usageText = `vm-netsetup — UmbraVOX VM network setup (TAP+bridge)

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

func logNet(msg string) { fmt.Printf("[NET] %s\n", msg) }

func main() {
	flag.Usage = func() { fmt.Fprint(os.Stderr, usageText) }
	flag.Parse()

	args := flag.Args()
	if len(args) == 0 {
		flag.Usage()
		os.Exit(2)
	}

	os.Exit(dispatch(args))
}

func dispatch(args []string) int {
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
			return 2
		}
		return runSetup(*agents, *lanA, *lanB, *bridgeA, *bridgeB, *enableFwd)

	case "teardown":
		fs := flag.NewFlagSet("teardown", flag.ExitOnError)
		bridgeA := fs.String("bridge-a", "br-umbravox-a", "LAN A bridge name")
		bridgeB := fs.String("bridge-b", "br-umbravox-b", "LAN B bridge name")
		fs.Parse(args[1:])

		if os.Geteuid() != 0 {
			fmt.Fprintln(os.Stderr, "[vm-netsetup] error: teardown requires root (use sudo)")
			return 2
		}
		return runTeardown(*bridgeA, *bridgeB)

	case "status":
		fs := flag.NewFlagSet("status", flag.ExitOnError)
		jsonOut := fs.Bool("json", false, "output as JSON")
		fs.Parse(args[1:])
		return runStatus("br-umbravox-a", "br-umbravox-b", *jsonOut)

	case "help", "-h", "--help":
		flag.Usage()
		return 0

	default:
		fmt.Fprintf(os.Stderr, "vm-netsetup: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		return 2
	}
}

// subnetBase extracts the first three octets from a CIDR like "10.0.42.0/24".
func subnetBase(cidr string) string {
	ip := strings.SplitN(cidr, "/", 2)[0]
	parts := strings.Split(ip, ".")
	if len(parts) < 3 {
		return ip
	}
	return strings.Join(parts[:3], ".")
}

// ipCmd runs "ip" with the given args, ignoring errors (idempotent ops).
func ipCmd(args ...string) error {
	cmd := exec.Command("ip", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// getLoginUser returns the user who invoked sudo (SUDO_USER) or the
// result of logname, for use as the TAP device owner.
func getLoginUser() string {
	if u := os.Getenv("SUDO_USER"); u != "" {
		return u
	}
	out, err := exec.Command("logname").Output()
	if err == nil {
		return strings.TrimSpace(string(out))
	}
	if u, err := user.Current(); err == nil {
		return u.Username
	}
	return "root"
}

func runSetup(agents int, lanA, lanB, bridgeA, bridgeB string, enableFwd bool) int {
	half := agents / 2
	if half < 1 {
		half = 1
	}

	baseA := subnetBase(lanA)
	baseB := subnetBase(lanB)
	tapUser := getLoginUser()

	logNet("creating dual-LAN bridge infrastructure...")

	// Create bridges
	ipCmd("link", "add", bridgeA, "type", "bridge")
	ipCmd("link", "add", bridgeB, "type", "bridge")
	ipCmd("link", "set", bridgeA, "up")
	ipCmd("link", "set", bridgeB, "up")
	ipCmd("addr", "add", baseA+".1/24", "dev", bridgeA)
	ipCmd("addr", "add", baseB+".1/24", "dev", bridgeB)

	// Enable IP forwarding
	if enableFwd {
		exec.Command("sysctl", "-w", "net.ipv4.ip_forward=1").Run()
	}

	// Create TAP devices for LAN A
	for i := 0; i < half; i++ {
		tap := fmt.Sprintf("tap-a%d", i)
		ipCmd("tuntap", "add", tap, "mode", "tap", "user", tapUser)
		ipCmd("link", "set", tap, "up")
		ipCmd("link", "set", tap, "master", bridgeA)
		fmt.Printf("  LAN A: %s -> %s (agent %d: %s.%d)\n", tap, bridgeA, i, baseA, 10+i)
	}

	// Create TAP devices for LAN B
	for i := 0; i < half; i++ {
		j := half + i
		tap := fmt.Sprintf("tap-b%d", i)
		ipCmd("tuntap", "add", tap, "mode", "tap", "user", tapUser)
		ipCmd("link", "set", tap, "up")
		ipCmd("link", "set", tap, "master", bridgeB)
		fmt.Printf("  LAN B: %s -> %s (agent %d: %s.%d)\n", tap, bridgeB, j, baseB, 10+i)
	}

	logNet("dual-LAN bridges ready")
	fmt.Printf("  LAN A: %s (%s)\n", bridgeA, lanA)
	fmt.Printf("  LAN B: %s (%s)\n", bridgeB, lanB)
	fwdStr := "enabled"
	if !enableFwd {
		fwdStr = "disabled"
	}
	fmt.Printf("  IP forwarding: %s\n", fwdStr)

	return 0
}

func runTeardown(bridgeA, bridgeB string) int {
	logNet("tearing down bridge infrastructure...")

	// Find and remove TAP devices
	out, err := exec.Command("ip", "-o", "link", "show", "type", "tun").Output()
	if err == nil {
		for _, line := range strings.Split(string(out), "\n") {
			// Look for tap-a* or tap-b* devices
			for _, prefix := range []string{"tap-a", "tap-b"} {
				idx := strings.Index(line, prefix)
				if idx < 0 {
					continue
				}
				// Extract the tap name (tap-aN or tap-bN)
				rest := line[idx:]
				fields := strings.Fields(rest)
				if len(fields) > 0 {
					tapName := strings.TrimSuffix(fields[0], ":")
					// Only delete if it matches tap-[ab]N pattern
					if isValidTapName(tapName) {
						ipCmd("link", "del", tapName)
						fmt.Printf("  removed: %s\n", tapName)
					}
				}
			}
		}
	}

	// Remove bridges
	for _, br := range []string{bridgeA, bridgeB} {
		ipCmd("link", "set", br, "down")
		ipCmd("link", "del", br)
		fmt.Printf("  removed: %s\n", br)
	}

	logNet("teardown complete")
	return 0
}

// isValidTapName checks if a name matches tap-[ab]N pattern.
func isValidTapName(name string) bool {
	if !strings.HasPrefix(name, "tap-a") && !strings.HasPrefix(name, "tap-b") {
		return false
	}
	numPart := name[5:]
	if numPart == "" {
		return false
	}
	_, err := strconv.Atoi(numPart)
	return err == nil
}

func runStatus(bridgeA, bridgeB string, jsonOut bool) int {
	if jsonOut {
		fmt.Println("{")
		bridges := []string{bridgeA, bridgeB}
		for i, br := range bridges {
			state := getBridgeState(br)
			addr := getBridgeAddr(br)
			members := getBridgeMembers(br)
			comma := ","
			if i == len(bridges)-1 {
				comma = ""
			}
			fmt.Printf("  %q: {\"state\": %q, \"addr\": %q, \"members\": [", br, state, addr)
			for j, m := range members {
				if j > 0 {
					fmt.Print(", ")
				}
				fmt.Printf("%q", m)
			}
			fmt.Printf("]}%s\n", comma)
		}
		fwd := getIPForward()
		fmt.Printf("  \"ip_forward\": %q\n", fwd)
		fmt.Println("}")
		return 0
	}

	logNet("bridge status:")
	for _, br := range []string{bridgeA, bridgeB} {
		// Check if bridge exists
		if err := exec.Command("ip", "link", "show", br).Run(); err != nil {
			fmt.Printf("  %s: not found\n", br)
			continue
		}
		addr := getBridgeAddr(br)
		state := getBridgeState(br)
		fmt.Printf("  %s: %s (%s)\n", br, state, addr)

		// Show member interfaces
		out, err := exec.Command("bridge", "link", "show", "master", br).Output()
		if err == nil {
			for _, line := range strings.Split(strings.TrimSpace(string(out)), "\n") {
				if line != "" {
					fmt.Printf("    %s\n", line)
				}
			}
		}
	}
	fmt.Println()
	fmt.Printf("  IP forwarding: %s\n", getIPForward())

	return 0
}

func getBridgeState(br string) string {
	out, err := exec.Command("ip", "-o", "link", "show", br).Output()
	if err != nil {
		return "not found"
	}
	line := string(out)
	idx := strings.Index(line, "state ")
	if idx < 0 {
		return "unknown"
	}
	rest := line[idx+6:]
	fields := strings.Fields(rest)
	if len(fields) > 0 {
		return fields[0]
	}
	return "unknown"
}

func getBridgeAddr(br string) string {
	out, err := exec.Command("ip", "-4", "addr", "show", br).Output()
	if err != nil {
		return "no IP"
	}
	for _, line := range strings.Split(string(out), "\n") {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "inet ") {
			fields := strings.Fields(line)
			if len(fields) >= 2 {
				return fields[1]
			}
		}
	}
	return "no IP"
}

func getBridgeMembers(br string) []string {
	out, err := exec.Command("bridge", "link", "show", "master", br).Output()
	if err != nil {
		return nil
	}
	var members []string
	for _, line := range strings.Split(strings.TrimSpace(string(out)), "\n") {
		line = strings.TrimSpace(line)
		if line != "" {
			members = append(members, line)
		}
	}
	return members
}

func getIPForward() string {
	data, err := os.ReadFile("/proc/sys/net/ipv4/ip_forward")
	if err != nil {
		return "unknown"
	}
	return strings.TrimSpace(string(data))
}

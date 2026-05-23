// Command vm-agent runs the UmbraVOX integration test agent inside a
// minimal NixOS test VM.
//
// This is the Go replacement for scripts/vm-integration-agent.sh.
// The agent extracts the release bundle, configures networking, runs
// scenario-based tests, and reports results.
//
// Usage:
//
//	vm-agent readiness    Verify bundle integrity and network config (default)
//	vm-agent exchange     Start listener, discover peers, verify connectivity
//	vm-agent flood        Send many messages to connected peers (future)
//
// Requires: runs inside a minimal NixOS test VM with the release bundle
// on /dev/vdb (ext2, read-only) along with agent.env configuration.
package main

import (
	"flag"
	"fmt"
	"os"
)

const usage = `vm-agent — UmbraVOX integration test agent

Usage:
  vm-agent <scenario> [flags]

Scenarios:
  readiness    Verify bundle integrity and network config (default)
  exchange     Start listener, discover peers, verify connectivity
  flood        Send many messages to connected peers (stub)

Flags:
  --port <n>       Agent listen port (default: 7853)
  --ip <addr>      Agent IP address
  --peers <list>   Comma-separated peer list (host:port,host:port)
  --timeout <s>    Scenario timeout in seconds (default: 60)
  --bundle-sha256  Expected SHA-256 of the release bundle

This runs inside a minimal NixOS test VM. The release bundle and
agent.env must be present on /dev/vdb.
`

func main() {
	port := flag.Int("port", 7853, "agent listen port")
	ip := flag.String("ip", "", "agent IP address")
	peers := flag.String("peers", "", "comma-separated peer list")
	timeout := flag.Int("timeout", 60, "scenario timeout in seconds")
	bundleSHA := flag.String("bundle-sha256", "", "expected SHA-256 of the release bundle")

	flag.Usage = func() { fmt.Fprint(os.Stderr, usage) }
	flag.Parse()

	args := flag.Args()
	scenario := "readiness"
	if len(args) > 0 {
		scenario = args[0]
	}

	switch scenario {
	case "readiness":
		fmt.Println("[vm-agent] readiness: not yet implemented")
		fmt.Printf("[vm-agent] port=%d ip=%s peers=%s timeout=%d\n", *port, *ip, *peers, *timeout)
		if *bundleSHA != "" {
			fmt.Printf("[vm-agent] bundle-sha256=%s\n", *bundleSHA)
		}
		fmt.Println("[vm-agent] This will verify bundle integrity and network config.")
	case "exchange":
		fmt.Println("[vm-agent] exchange: not yet implemented")
		fmt.Printf("[vm-agent] port=%d ip=%s peers=%s timeout=%d\n", *port, *ip, *peers, *timeout)
		fmt.Println("[vm-agent] This will start a listener, discover peers, and verify connectivity.")
	case "flood":
		fmt.Println("[vm-agent] flood: not yet implemented")
		fmt.Println("[vm-agent] This will send many messages to connected peers.")
	case "help", "-h", "--help":
		flag.Usage()
	default:
		fmt.Fprintf(os.Stderr, "vm-agent: unknown scenario %q\n\n", scenario)
		flag.Usage()
		os.Exit(2)
	}
}

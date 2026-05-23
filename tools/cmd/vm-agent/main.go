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
  vm-agent [flags] <scenario>

Scenarios:
  readiness    Verify bundle integrity and network config (default)
  exchange     Start listener, discover peers, verify connectivity
  flood        Send many messages to connected peers (stub)

Flags:
  --port <n>            Agent listen port (default: 7853)
  --ip <addr>           Agent IP address (required for exchange/flood)
  --peers <list>        Comma-separated peer list (host:port,host:port)
  --timeout <s>         Scenario timeout in seconds (default: 60)
  --bundle-sha256 <h>   Expected SHA-256 of the release bundle
  --bundle-path <path>  Path to release bundle (default: /mnt/src/umbravox-*.tar.gz)
  --output-dir <path>   Results output directory (default: /output)
  --agent-id <id>       Agent identifier for multi-agent tests (default: auto)

This runs inside a minimal NixOS test VM. The release bundle and
agent.env must be present on /dev/vdb.

Exit codes:
  0   Scenario passed
  1   Scenario failed
  2   Configuration error
`

func main() {
	port := flag.Int("port", 7853, "agent listen port")
	ip := flag.String("ip", "", "agent IP address")
	peers := flag.String("peers", "", "comma-separated peer list (host:port,...)")
	timeout := flag.Int("timeout", 60, "scenario timeout in seconds")
	bundleSHA := flag.String("bundle-sha256", "", "expected SHA-256 of the release bundle")
	bundlePath := flag.String("bundle-path", "", "path to release bundle (default: auto-detect)")
	outputDir := flag.String("output-dir", "/output", "results output directory")
	agentID := flag.String("agent-id", "", "agent identifier for multi-agent tests")

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
		fmt.Printf("[vm-agent] port=%d ip=%q peers=%q timeout=%d\n", *port, *ip, *peers, *timeout)
		fmt.Printf("[vm-agent] output-dir=%s\n", *outputDir)
		if *bundleSHA != "" {
			fmt.Printf("[vm-agent] bundle-sha256=%s\n", *bundleSHA)
		}
		if *bundlePath != "" {
			fmt.Printf("[vm-agent] bundle-path=%s\n", *bundlePath)
		}
		if *agentID != "" {
			fmt.Printf("[vm-agent] agent-id=%s\n", *agentID)
		}
		fmt.Println("[vm-agent] This will:")
		fmt.Println("[vm-agent]   1. Locate and verify the release bundle checksum")
		fmt.Println("[vm-agent]   2. Extract the bundle to a temp directory")
		fmt.Println("[vm-agent]   3. Verify the binary runs (--version)")
		fmt.Println("[vm-agent]   4. Check network interface configuration")
		fmt.Println("[vm-agent]   5. Report PASS/FAIL to output directory")

	case "exchange":
		if *ip == "" {
			fmt.Fprintln(os.Stderr, "[vm-agent] error: --ip is required for exchange scenario")
			os.Exit(2)
		}
		fmt.Println("[vm-agent] exchange: not yet implemented")
		fmt.Printf("[vm-agent] port=%d ip=%s peers=%q timeout=%d\n", *port, *ip, *peers, *timeout)
		fmt.Printf("[vm-agent] agent-id=%s output-dir=%s\n", *agentID, *outputDir)
		fmt.Println("[vm-agent] This will:")
		fmt.Println("[vm-agent]   1. Start listener on the specified port")
		fmt.Println("[vm-agent]   2. Discover peers via the provided list")
		fmt.Println("[vm-agent]   3. Exchange test messages with each peer")
		fmt.Println("[vm-agent]   4. Verify message integrity (post-quantum crypto)")
		fmt.Println("[vm-agent]   5. Report connectivity results")

	case "flood":
		fmt.Println("[vm-agent] flood: not yet implemented")
		fmt.Printf("[vm-agent] port=%d ip=%s peers=%q timeout=%d\n", *port, *ip, *peers, *timeout)
		fmt.Println("[vm-agent] This will send many messages to connected peers.")
		fmt.Println("[vm-agent] (stub -- not yet designed)")

	case "help", "-h", "--help":
		flag.Usage()

	default:
		fmt.Fprintf(os.Stderr, "vm-agent: unknown scenario %q\n\n", scenario)
		flag.Usage()
		os.Exit(2)
	}
}

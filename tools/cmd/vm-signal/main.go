// Command vm-signal manages the UmbraVOX Signal-Server VM lifecycle.
//
// This is the Go replacement for scripts/vm-signal-server-run.sh.
//
// Usage:
//
//	vm-signal build-jar     Stage 1: boot build VM with network, run Maven, output JAR
//	vm-signal interactive   Stage 2: boot runtime VM with services
//	vm-signal check         Stage 2: boot runtime VM, health-check, exit
//
// Requires: qemu-system-x86_64, /dev/kvm
package main

import (
	"flag"
	"fmt"
	"os"
)

const usage = `vm-signal — UmbraVOX Signal-Server VM runner

Usage:
  vm-signal <subcommand> [flags]

Subcommands:
  build-jar      Stage 1: boot build VM with network, run Maven, output JAR
  interactive    Stage 2: boot runtime VM with services (interactive shell)
  check          Stage 2: boot runtime VM, health-check, exit

Flags for 'build-jar':
  --network-policy <path>   Network policy file (default: vm-network-policy.conf)
  --output-dir <path>       JAR output directory (default: build/signal-server-jar)
  --timeout <duration>      Build timeout (default: 30m)

Flags for 'check':
  --timeout <duration>      Health-check timeout (default: 120s)
  --endpoint <url>          Health endpoint URL (default: http://localhost:8080/health)

Requires: qemu-system-x86_64, /dev/kvm
The VM images must be pre-built via the appropriate Makefile targets.
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
	case "build-jar":
		fs := flag.NewFlagSet("build-jar", flag.ExitOnError)
		networkPolicy := fs.String("network-policy", "vm-network-policy.conf", "network policy file")
		outputDir := fs.String("output-dir", "build/signal-server-jar", "JAR output directory")
		timeout := fs.String("timeout", "30m", "build timeout")
		fs.Parse(args[1:])

		fmt.Println("[vm-signal] build-jar: not yet implemented")
		fmt.Printf("[vm-signal] network-policy: %s\n", *networkPolicy)
		fmt.Printf("[vm-signal] output-dir:     %s\n", *outputDir)
		fmt.Printf("[vm-signal] timeout:        %s\n", *timeout)
		fmt.Println("[vm-signal] This will boot a build VM with network access,")
		fmt.Println("[vm-signal] run Maven to build the Signal-Server JAR, and")
		fmt.Println("[vm-signal] output the result to the specified directory.")

	case "interactive":
		fs := flag.NewFlagSet("interactive", flag.ExitOnError)
		memory := fs.Int("memory", 4096, "VM memory in MB")
		cores := fs.Int("cores", 2, "VM CPU cores")
		fs.Parse(args[1:])

		fmt.Println("[vm-signal] interactive: not yet implemented")
		fmt.Printf("[vm-signal] memory: %dMB, cores: %d\n", *memory, *cores)
		fmt.Println("[vm-signal] This will boot the Signal-Server runtime VM")
		fmt.Println("[vm-signal] with an interactive serial console.")

	case "check":
		fs := flag.NewFlagSet("check", flag.ExitOnError)
		timeout := fs.String("timeout", "120s", "health-check timeout")
		endpoint := fs.String("endpoint", "http://localhost:8080/health", "health endpoint URL")
		fs.Parse(args[1:])

		fmt.Println("[vm-signal] check: not yet implemented")
		fmt.Printf("[vm-signal] timeout:  %s\n", *timeout)
		fmt.Printf("[vm-signal] endpoint: %s\n", *endpoint)
		fmt.Println("[vm-signal] This will boot the Signal-Server runtime VM,")
		fmt.Println("[vm-signal] run health checks, and exit.")

	case "help", "-h", "--help":
		flag.Usage()

	default:
		fmt.Fprintf(os.Stderr, "vm-signal: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		os.Exit(2)
	}
}

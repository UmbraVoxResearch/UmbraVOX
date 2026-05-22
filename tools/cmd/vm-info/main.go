// Command vm-info prints UmbraVOX VM configuration information.
//
// This is a proof-of-concept binary for the Go tools scaffold.
// It demonstrates that the module compiles, the shared packages are
// importable, and the build pipeline produces a working binary.
//
// Usage:
//
//	vm-info                      # print default VM config
//	vm-info -policy <path>       # also parse a network policy file
package main

import (
	"flag"
	"fmt"
	"os"
	"runtime"
	"strings"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/netpol"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/ninep"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
)

func main() {
	policyPath := flag.String("policy", "", "path to vm-network-policy.conf")
	flag.Parse()

	cfg := qemu.DefaultConfig()

	fmt.Println("UmbraVOX VM Configuration (Go tools scaffold)")
	fmt.Println("==============================================")
	fmt.Printf("Host OS/Arch:  %s/%s\n", runtime.GOOS, runtime.GOARCH)
	fmt.Printf("Host CPUs:     %d\n", runtime.NumCPU())
	fmt.Printf("VM Machine:    %s\n", cfg.Machine)
	fmt.Printf("VM CPU Model:  %s\n", cfg.CPUModel)
	fmt.Printf("VM Cores:      %d\n", cfg.SMP)
	fmt.Printf("VM Memory:     %d MB\n", cfg.MemoryMB)
	fmt.Printf("VM Network:    %s\n", cfg.NetArgs)
	fmt.Printf("VM Display:    serial (nographic)\n")

	// Show default output share config
	share := ninep.DefaultOutputShare("/tmp/example-output")
	fmt.Printf("9p Share:      %s\n", share.VirtFSArg())

	// Show generated QEMU args
	args := cfg.Args()
	fmt.Printf("QEMU Args:     qemu-system-x86_64 %s\n", strings.Join(args, " "))

	// Parse network policy if provided
	if *policyPath != "" {
		fmt.Printf("\nNetwork Policy: %s\n", *policyPath)
		policy, err := netpol.ParseFile(*policyPath)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("  ALLOW rules: %d\n", len(policy.Rules))
		for _, r := range policy.Rules {
			fmt.Printf("    ALLOW %s %s %s\n", r.Protocol, r.Destination, r.Port)
		}
		netArgs, err := policy.QEMUNetArgs()
		if err != nil {
			fmt.Printf("  QEMU net:    ERROR: %v\n", err)
		} else {
			fmt.Printf("  QEMU net:    %s\n", netArgs)
		}
	}
}

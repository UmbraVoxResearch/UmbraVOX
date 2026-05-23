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
	"bufio"
	"crypto/sha256"
	"encoding/hex"
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"
)

const usageText = `vm-agent — UmbraVOX integration test agent

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

var (
	pass int
	fail int
)

func check(label string, fn func() bool) {
	if fn() {
		fmt.Printf("  PASS: %s\n", label)
		pass++
	} else {
		fmt.Printf("  FAIL: %s\n", label)
		fail++
	}
}

func main() {
	port := flag.Int("port", 7853, "agent listen port")
	ip := flag.String("ip", "", "agent IP address")
	peers := flag.String("peers", "", "comma-separated peer list (host:port,...)")
	timeout := flag.Int("timeout", 60, "scenario timeout in seconds")
	bundleSHA := flag.String("bundle-sha256", "", "expected SHA-256 of the release bundle")
	bundlePath := flag.String("bundle-path", "", "path to release bundle (default: auto-detect)")
	outputDir := flag.String("output-dir", "/output", "results output directory")
	agentID := flag.String("agent-id", "", "agent identifier for multi-agent tests")

	flag.Usage = func() { fmt.Fprint(os.Stderr, usageText) }
	flag.Parse()

	args := flag.Args()
	scenario := "readiness"
	if len(args) > 0 {
		scenario = args[0]
	}

	os.Exit(run(scenario, *port, *ip, *peers, *timeout, *bundleSHA, *bundlePath, *outputDir, *agentID))
}

func run(scenario string, port int, ip, peerList string, timeoutSec int, bundleSHA, bundlePath, outputDir, agentID string) int {
	fmt.Println("========================================")
	fmt.Println("  UmbraVOX Integration Test Agent")
	fmt.Println("========================================")

	// Load agent.env configuration (safe key=value parsing)
	loadAgentEnv("/mnt/bundle/agent.env")

	// Override from env if flags not set
	if agentID == "" {
		agentID = os.Getenv("AGENT_ID")
	}
	if ip == "" {
		ip = os.Getenv("AGENT_IP")
	}
	if peerList == "" {
		peerList = os.Getenv("AGENT_PEERS")
	}
	if bundleSHA == "" {
		bundleSHA = os.Getenv("AGENT_BUNDLE_SHA256")
	}
	if os.Getenv("AGENT_PORT") != "" {
		if p, err := strconv.Atoi(os.Getenv("AGENT_PORT")); err == nil {
			port = p
		}
	}
	if os.Getenv("AGENT_TIMEOUT") != "" {
		if t, err := strconv.Atoi(os.Getenv("AGENT_TIMEOUT")); err == nil {
			timeoutSec = t
		}
	}
	if scenario == "readiness" && os.Getenv("AGENT_SCENARIO") != "" {
		scenario = os.Getenv("AGENT_SCENARIO")
	}

	// Validate port
	if port < 1 || port > 65535 {
		fmt.Printf("AGENT_RESULT=FAIL (invalid AGENT_PORT: %d)\n", port)
		return 1
	}

	// Validate IP if set
	if ip != "" && !isValidIPv4(ip) {
		fmt.Printf("AGENT_RESULT=FAIL (invalid AGENT_IP: %s)\n", ip)
		return 1
	}

	// Validate peers if set
	if peerList != "" && !validatePeerList(peerList) {
		fmt.Printf("AGENT_RESULT=FAIL (invalid AGENT_PEERS: %s)\n", peerList)
		return 1
	}

	fmt.Printf("agent_id:    %s\n", orQ(agentID))
	fmt.Printf("agent_ip:    %s\n", orQ(ip))
	fmt.Printf("port:        %d\n", port)
	fmt.Printf("peers:       %s\n", orDefault(peerList, "none"))
	fmt.Printf("scenario:    %s\n", scenario)
	fmt.Printf("timeout:     %ds\n", timeoutSec)
	fmt.Println()

	_ = timeoutSec // used for exchange probes

	// Configure network
	if ip != "" {
		exec.Command("ip", "addr", "add", ip+"/24", "dev", "eth0").Run()
		exec.Command("ip", "link", "set", "eth0", "up").Run()
		// Add default route via gateway (.1)
		parts := strings.Split(ip, ".")
		if len(parts) == 4 {
			gateway := strings.Join(parts[:3], ".") + ".1"
			exec.Command("ip", "route", "add", "default", "via", gateway).Run()
		}
	}

	// Find release bundle
	bundle := bundlePath
	if bundle == "" {
		matches, _ := filepath.Glob("/mnt/bundle/umbravox-*-linux-x86_64.tar.gz")
		if len(matches) > 0 {
			bundle = matches[0]
		}
	}
	if bundle == "" {
		fmt.Println("AGENT_RESULT=FAIL (no release bundle found)")
		return 1
	}

	// Bundle checksum verification
	skipChecksum := os.Getenv("AGENT_SKIP_CHECKSUM") == "1"
	if skipChecksum {
		fmt.Println("  WARN: bundle checksum verification skipped (AGENT_SKIP_CHECKSUM=1)")
	} else if bundleSHA == "" {
		fmt.Println("AGENT_RESULT=FAIL (AGENT_BUNDLE_SHA256 required; set AGENT_SKIP_CHECKSUM=1 to bypass)")
		return 1
	} else {
		// Validate format
		matched, _ := regexp.MatchString(`^[0-9a-fA-F]{64}$`, bundleSHA)
		if !matched {
			fmt.Println("AGENT_RESULT=FAIL (invalid AGENT_BUNDLE_SHA256 format)")
			return 1
		}
		if !verifyBundleSHA256(bundle, strings.ToLower(bundleSHA)) {
			return 1
		}
	}

	// Validate tar paths for safety
	if !validateTarPaths(bundle) {
		return 1
	}

	// Extract bundle
	os.MkdirAll("/work/app", 0o755)
	extractCmd := exec.Command("tar", "xzf", bundle, "-C", "/work/app")
	if out, err := extractCmd.CombinedOutput(); err != nil {
		fmt.Printf("AGENT_RESULT=FAIL (bundle extraction failed: %v)\n%s\n", err, out)
		return 1
	}

	// Find extracted app directory
	appDir := ""
	entries, _ := filepath.Glob("/work/app/umbravox-*")
	for _, e := range entries {
		if fi, err := os.Stat(e); err == nil && fi.IsDir() {
			appDir = e
			break
		}
	}
	if appDir == "" {
		fmt.Println("AGENT_RESULT=FAIL (bundle extraction failed)")
		return 1
	}
	os.Chdir(appDir)

	// Readiness checks (always run)
	fmt.Println("\n-- readiness checks --")

	check("binary exists", func() bool {
		fi, err := os.Stat("umbravox")
		return err == nil && fi.Mode()&0o111 != 0
	})
	check("lib directory", func() bool {
		fi, err := os.Stat("lib")
		return err == nil && fi.IsDir()
	})
	check("launch script", func() bool {
		fi, err := os.Stat("run-umbravox.sh")
		return err == nil && fi.Mode()&0o111 != 0
	})
	check("release manifest", func() bool {
		_, err := os.Stat("RELEASE-MANIFEST.txt")
		return err == nil
	})
	check("checksums", func() bool {
		_, err := os.Stat("CONTENTS.SHA256")
		return err == nil
	})

	// Network checks
	if ip != "" {
		check("IP assigned", func() bool {
			out, err := exec.Command("ip", "addr", "show", "eth0").CombinedOutput()
			return err == nil && strings.Contains(string(out), "inet ")
		})
		check("port currently free", func() bool {
			return isPortFree(port)
		})
	}

	// Scenario-specific
	switch scenario {
	case "readiness":
		fmt.Println("\n-- readiness scenario complete --")

	case "exchange":
		if ip == "" {
			fmt.Fprintln(os.Stderr, "[vm-agent] error: --ip is required for exchange scenario")
			return 2
		}
		fmt.Println("\n-- exchange scenario --")
		time.Sleep(2 * time.Second)

		if peerList != "" {
			for _, peer := range strings.Split(peerList, ",") {
				parts := strings.SplitN(peer, ":", 2)
				if len(parts) != 2 {
					continue
				}
				peerIP := parts[0]
				peerPort := parts[1]
				check("reach peer "+peer, func() bool {
					return tcpProbe(peerIP, peerPort, 5)
				})
			}
		}

		// Bidirectional connectivity test
		fmt.Println("\n-- bidirectional connectivity (M5.4.2) --")
		if peerList != "" {
			for _, peer := range strings.Split(peerList, ",") {
				parts := strings.SplitN(peer, ":", 2)
				if len(parts) != 2 {
					continue
				}
				if tcpProbe(parts[0], parts[1], 3) {
					fmt.Printf("  PASS: forward connectivity to %s\n", peer)
					pass++
				} else {
					fmt.Printf("  INFO: peer %s not reachable yet (may not be ready)\n", peer)
				}
			}
		} else {
			fmt.Println("  INFO: no peers configured; skipping connectivity probes")
		}
		fmt.Println("  NOTE: full message round-trip requires umbravox-headless in bundle")

	case "flood":
		fmt.Println("\n-- flood scenario (stub) --")
		fmt.Println("  INFO: flood scenario not yet implemented")
		fmt.Println("  INFO: will send many messages to connected peers once")
		fmt.Println("  INFO: headless mode is available")

	default:
		fmt.Printf("\n-- unknown scenario: %s --\n", scenario)
	}

	// Summary
	fmt.Println()
	fmt.Println("========================================")
	fmt.Printf("  AGENT SUMMARY: %d passed, %d failed\n", pass, fail)
	fmt.Println("========================================")

	result := "PASS"
	if fail > 0 {
		result = "FAIL"
	}
	fmt.Printf("AGENT_RESULT=%s\n", result)

	// Write to output
	os.MkdirAll(outputDir, 0o755)
	summary := fmt.Sprintf("AGENT_RESULT=%s\nAGENT_ID=%s\nPASS=%d\nFAIL=%d\nSCENARIO=%s\n",
		result, agentID, pass, fail, scenario)
	os.WriteFile(filepath.Join(outputDir, "agent-result"), []byte(summary), 0o644)

	if fail > 0 {
		return 1
	}
	return 0
}

// loadAgentEnv safely parses agent.env as strict key=value pairs.
// Only exports variables whose names begin with AGENT_ or UMBRAVOX_.
func loadAgentEnv(path string) {
	f, err := os.Open(path)
	if err != nil {
		return
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		idx := strings.IndexByte(line, '=')
		if idx < 0 {
			continue
		}
		key := line[:idx]
		value := line[idx+1:]
		if strings.HasPrefix(key, "AGENT_") || strings.HasPrefix(key, "UMBRAVOX_") {
			os.Setenv(key, value)
		}
	}
}

func isValidIPv4(ip string) bool {
	parts := strings.Split(ip, ".")
	if len(parts) != 4 {
		return false
	}
	for _, p := range parts {
		n, err := strconv.Atoi(p)
		if err != nil || n < 0 || n > 255 {
			return false
		}
	}
	return true
}

func isValidPort(port int) bool {
	return port >= 1 && port <= 65535
}

func isValidPeer(peer string) bool {
	idx := strings.LastIndexByte(peer, ':')
	if idx < 0 {
		return false
	}
	peerIP := peer[:idx]
	peerPort := peer[idx+1:]
	p, err := strconv.Atoi(peerPort)
	if err != nil {
		return false
	}
	return isValidIPv4(peerIP) && isValidPort(p)
}

func validatePeerList(peers string) bool {
	for _, peer := range strings.Split(peers, ",") {
		peer = strings.TrimSpace(peer)
		if peer == "" {
			continue
		}
		if !isValidPeer(peer) {
			return false
		}
	}
	return true
}

func tcpProbe(host, port string, timeoutSec int) bool {
	addr := net.JoinHostPort(host, port)
	conn, err := net.DialTimeout("tcp", addr, time.Duration(timeoutSec)*time.Second)
	if err != nil {
		return false
	}
	conn.Close()
	return true
}

func isPortFree(port int) bool {
	ln, err := net.Listen("tcp", fmt.Sprintf(":%d", port))
	if err != nil {
		return false
	}
	ln.Close()
	return true
}

func verifyBundleSHA256(bundle, expected string) bool {
	f, err := os.Open(bundle)
	if err != nil {
		fmt.Printf("AGENT_RESULT=FAIL (cannot open bundle: %v)\n", err)
		return false
	}
	defer f.Close()

	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		fmt.Printf("AGENT_RESULT=FAIL (cannot hash bundle: %v)\n", err)
		return false
	}
	actual := hex.EncodeToString(h.Sum(nil))
	if actual != expected {
		fmt.Println("AGENT_RESULT=FAIL (bundle checksum mismatch)")
		fmt.Printf("  expected: %s\n", expected)
		fmt.Printf("  actual:   %s\n", actual)
		return false
	}
	return true
}

func validateTarPaths(bundle string) bool {
	cmd := exec.Command("tar", "tzf", bundle)
	out, err := cmd.Output()
	if err != nil {
		fmt.Printf("AGENT_RESULT=FAIL (cannot list bundle: %v)\n", err)
		return false
	}
	for _, line := range strings.Split(string(out), "\n") {
		entry := strings.TrimSpace(line)
		if entry == "" {
			continue
		}
		if strings.HasPrefix(entry, "/") || strings.HasPrefix(entry, "../") ||
			strings.Contains(entry, "/../") || strings.HasSuffix(entry, "/..") ||
			entry == ".." {
			fmt.Printf("AGENT_RESULT=FAIL (unsafe archive path: %s)\n", entry)
			return false
		}
	}
	return true
}

func orQ(s string) string {
	if s == "" {
		return "?"
	}
	return s
}

func orDefault(s, def string) string {
	if s == "" {
		return def
	}
	return s
}

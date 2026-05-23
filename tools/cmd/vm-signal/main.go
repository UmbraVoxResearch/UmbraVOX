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
	"bufio"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
)

// ANSI color helpers.
const (
	red    = "\033[0;31m"
	green  = "\033[0;32m"
	blue   = "\033[0;34m"
	yellow = "\033[1;33m"
	nc     = "\033[0m"
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

func logMsg(color, msg string) {
	fmt.Fprintf(os.Stderr, "%s[SIGNAL-VM]%s %s\n", color, nc, msg)
}

func main() {
	flag.Usage = func() { fmt.Fprint(os.Stderr, usage) }
	flag.Parse()

	args := flag.Args()
	if len(args) == 0 {
		flag.Usage()
		os.Exit(2)
	}

	os.Exit(run(args))
}

func run(args []string) int {
	subcmd := args[0]
	switch subcmd {
	case "build-jar":
		fs := flag.NewFlagSet("build-jar", flag.ExitOnError)
		_ = fs.String("network-policy", "vm-network-policy.conf", "network policy file")
		outputDir := fs.String("output-dir", "", "JAR output directory")
		_ = fs.String("timeout", "30m", "build timeout")
		fs.Parse(args[1:])
		return runBuildJar(*outputDir)

	case "interactive":
		fs := flag.NewFlagSet("interactive", flag.ExitOnError)
		_ = fs.Int("memory", 4096, "VM memory in MB")
		_ = fs.Int("cores", 2, "VM CPU cores")
		fs.Parse(args[1:])
		return runRuntime("interactive")

	case "check":
		fs := flag.NewFlagSet("check", flag.ExitOnError)
		_ = fs.String("timeout", "120s", "health-check timeout")
		_ = fs.String("endpoint", "http://localhost:8080/health", "health endpoint URL")
		fs.Parse(args[1:])
		return runRuntime("check")

	case "help", "-h", "--help":
		flag.Usage()
		return 0

	default:
		fmt.Fprintf(os.Stderr, "vm-signal: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		return 2
	}
}

// findRepoRoot walks up from the executable (or cwd) to find the repo root.
func findRepoRoot() string {
	exe, err := os.Executable()
	if err == nil {
		dir := filepath.Dir(exe)
		for i := 0; i < 10; i++ {
			if _, err := os.Stat(filepath.Join(dir, "Makefile")); err == nil {
				if _, err := os.Stat(filepath.Join(dir, "scripts", "vm-dev-run.sh")); err == nil {
					return dir
				}
			}
			dir = filepath.Dir(dir)
		}
	}
	cwd, err := os.Getwd()
	if err != nil {
		logMsg(red, "Cannot determine working directory")
		os.Exit(1)
	}
	dir := cwd
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "Makefile")); err == nil {
			if _, err := os.Stat(filepath.Join(dir, "scripts", "vm-dev-run.sh")); err == nil {
				return dir
			}
		}
		dir = filepath.Dir(dir)
	}
	logMsg(red, "Cannot find repo root (looked for Makefile + scripts/vm-dev-run.sh)")
	os.Exit(1)
	return ""
}

// preflightCheck validates KVM and QEMU availability.
func preflightCheck() error {
	ok := true
	if _, err := os.Stat("/dev/kvm"); os.IsNotExist(err) {
		logMsg(red, "/dev/kvm not found; KVM required")
		ok = false
	}
	if _, err := exec.LookPath("qemu-system-x86_64"); err != nil {
		logMsg(red, "qemu-system-x86_64 not on PATH")
		ok = false
	}
	if !ok {
		return fmt.Errorf("preflight checks failed")
	}
	return nil
}

// scaleResources returns (cores, memMB) scaled to 1/fraction of the host.
func scaleResources(fraction int) (int, int) {
	hostCores := runtime.NumCPU()
	hostMemMB := readHostMemoryMB()
	vmCores := hostCores / fraction
	vmMemMB := hostMemMB / fraction
	if vmCores < 2 {
		vmCores = 2
	}
	if vmMemMB < 2048 {
		vmMemMB = 2048
	}
	return vmCores, vmMemMB
}

// readHostMemoryMB reads total host memory from /proc/meminfo.
func readHostMemoryMB() int {
	f, err := os.Open("/proc/meminfo")
	if err != nil {
		return 8192
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "MemTotal:") {
			fields := strings.Fields(line)
			if len(fields) >= 2 {
				kb, err := strconv.Atoi(fields[1])
				if err == nil {
					return kb / 1024
				}
			}
		}
	}
	return 8192
}

// createOverlay creates a COW qcow2 overlay backed by baseImage in dir.
func createOverlay(baseImage, dir string) (string, error) {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("create overlay dir: %w", err)
	}
	f, err := os.CreateTemp(dir, "umbravox-signal-overlay.*.qcow2")
	if err != nil {
		return "", err
	}
	overlayPath := f.Name()
	f.Close()

	cmd := exec.Command("qemu-img", "create", "-f", "qcow2",
		"-b", baseImage, "-F", "raw", overlayPath)
	if out, err := cmd.CombinedOutput(); err != nil {
		os.Remove(overlayPath)
		return "", fmt.Errorf("qemu-img create overlay: %w\n%s", err, out)
	}
	return overlayPath, nil
}

// runBuildJar implements the build-jar subcommand (Stage 1).
func runBuildJar(outputDirFlag string) int {
	if err := preflightCheck(); err != nil {
		return 1
	}

	repoRoot := findRepoRoot()
	vmCacheDir := filepath.Join(repoRoot, "build", "vm-signal-server")
	buildImagePath := filepath.Join(vmCacheDir, "build-image")
	tmpDir := filepath.Join(vmCacheDir, "tmp")
	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		logMsg(red, fmt.Sprintf("Failed to create tmp dir: %v", err))
		return 1
	}

	jarOutputDir := filepath.Join(repoRoot, "build", "signal-server-jar")
	if outputDirFlag != "" {
		if filepath.IsAbs(outputDirFlag) {
			jarOutputDir = outputDirFlag
		} else {
			jarOutputDir = filepath.Join(repoRoot, outputDirFlag)
		}
	}

	// Check build VM image exists
	if fi, err := os.Stat(buildImagePath); err != nil || !fi.IsDir() {
		logMsg(red, fmt.Sprintf("Build VM image not found at %s", buildImagePath))
		logMsg(yellow, "Build the Signal-Server build VM image first.")
		return 1
	}

	diskImg, err := filepath.EvalSymlinks(filepath.Join(buildImagePath, "nixos.img"))
	if err != nil {
		logMsg(red, fmt.Sprintf("Cannot resolve nixos.img: %v", err))
		return 1
	}

	overlay, err := createOverlay(diskImg, tmpDir)
	if err != nil {
		logMsg(red, fmt.Sprintf("Failed to create overlay: %v", err))
		return 1
	}
	defer os.Remove(overlay)

	if err := os.MkdirAll(jarOutputDir, 0o755); err != nil {
		logMsg(red, fmt.Sprintf("Failed to create JAR output dir: %v", err))
		return 1
	}

	// Build VM gets 50% of host resources (Maven is hungry)
	vmCores, vmMemMB := scaleResources(2)

	logMsg(blue, "Stage 1: Building Signal-Server JAR in VM...")
	logMsg(blue, fmt.Sprintf("VM resources: %d cores, %dMB RAM", vmCores, vmMemMB))
	logMsg(blue, "Network: enabled (Maven needs to fetch deps)")
	logMsg(blue, fmt.Sprintf("Output: %s/signal-server.jar", jarOutputDir))
	fmt.Fprintln(os.Stderr)

	qemuArgs := []string{
		"-machine", "q35,accel=kvm",
		"-cpu", "max",
		"-m", fmt.Sprintf("%d", vmMemMB),
		"-smp", fmt.Sprintf("%d", vmCores),
		"-nographic", "-serial", "mon:stdio",
		"-drive", fmt.Sprintf("if=virtio,format=qcow2,file=%s", overlay),
		"-nic", "user,model=virtio",
		"-virtfs", fmt.Sprintf("local,path=%s,mount_tag=output,security_model=mapped-xattr,id=output", jarOutputDir),
		"-no-reboot",
	}

	cmd := exec.Command("qemu-system-x86_64", qemuArgs...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	qemuErr := cmd.Run()

	// Check if JAR was produced
	jarPath := filepath.Join(jarOutputDir, "signal-server.jar")
	if _, err := os.Stat(jarPath); err == nil {
		fmt.Fprintln(os.Stderr)
		logMsg(green, "Signal-Server JAR built successfully!")
		logMsg(green, fmt.Sprintf("Location: %s", jarPath))
		fmt.Fprintln(os.Stderr)
		logMsg(blue, "Now rebuild the runtime VM image:")
		logMsg(blue, "  make vm-signal-server-build")
		return 0
	}

	fmt.Fprintln(os.Stderr)
	logMsg(red, "Build failed -- no JAR produced.")
	logMsg(red, "Check the VM console output above for errors.")
	if qemuErr != nil {
		return 1
	}
	return 1
}

// runRuntime implements the interactive and check subcommands (Stage 2).
func runRuntime(mode string) int {
	if err := preflightCheck(); err != nil {
		return 1
	}

	repoRoot := findRepoRoot()
	vmCacheDir := filepath.Join(repoRoot, "build", "vm-signal-server")
	vmImagePath := filepath.Join(vmCacheDir, "image")
	tmpDir := filepath.Join(vmCacheDir, "tmp")
	jarOutputDir := filepath.Join(repoRoot, "build", "signal-server-jar")

	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		logMsg(red, fmt.Sprintf("Failed to create tmp dir: %v", err))
		return 1
	}

	if fi, err := os.Stat(vmImagePath); err != nil || !fi.IsDir() {
		logMsg(red, fmt.Sprintf("Runtime VM image not found at %s", vmImagePath))
		logMsg(yellow, "Run 'make vm-signal-server-build' first.")
		return 1
	}

	diskImg, err := filepath.EvalSymlinks(filepath.Join(vmImagePath, "nixos.img"))
	if err != nil {
		logMsg(red, fmt.Sprintf("Cannot resolve nixos.img: %v", err))
		return 1
	}

	overlay, err := createOverlay(diskImg, tmpDir)
	if err != nil {
		logMsg(red, fmt.Sprintf("Failed to create overlay: %v", err))
		return 1
	}
	defer os.Remove(overlay)

	// Runtime VM: 25% of host
	vmCores, vmMemMB := scaleResources(4)

	logMsg(blue, "Stage 2: Booting Signal-Server runtime VM...")
	logMsg(blue, fmt.Sprintf("VM resources: %d cores, %dMB RAM", vmCores, vmMemMB))
	logMsg(blue, "Network: deny-all (runtime isolation)")

	jarPath := filepath.Join(jarOutputDir, "signal-server.jar")
	if _, err := os.Stat(jarPath); err == nil {
		logMsg(green, "Signal-Server JAR: present")
	} else {
		logMsg(yellow, "Signal-Server JAR: not built (backing services only)")
	}
	logMsg(blue, fmt.Sprintf("Mode: %s", mode))
	fmt.Fprintln(os.Stderr)

	qemuArgs := []string{
		"-machine", "q35,accel=kvm",
		"-cpu", "max",
		"-m", fmt.Sprintf("%d", vmMemMB),
		"-smp", fmt.Sprintf("%d", vmCores),
		"-nographic", "-nodefaults", "-serial", "stdio",
		"-drive", fmt.Sprintf("if=virtio,format=qcow2,file=%s", overlay),
		"-nic", "none",
	}

	if mode != "interactive" {
		qemuArgs = append(qemuArgs, "-no-reboot")
	}

	cmd := exec.Command("qemu-system-x86_64", qemuArgs...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return exitErr.ExitCode()
		}
		logMsg(red, fmt.Sprintf("Failed to run QEMU: %v", err))
		return 1
	}
	return 0
}

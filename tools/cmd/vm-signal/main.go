// Command vm-signal manages the UmbraVOX Signal-Server VM lifecycle.
//
// This is the Go replacement for scripts/vm-signal-server-run.sh.
//
// Usage:
//
//	vm-signal build-jar     Stage 1: boot build VM with network, run Maven, output JAR
//	vm-signal interactive   Stage 2: boot runtime VM with services
//	vm-signal check         Stage 2: boot runtime VM, health-check, exit
//	vm-signal check-health  Stage 2: host-side health endpoint verification
//
// Requires: qemu-system-x86_64, /dev/kvm
package main

import (
	"context"
	"flag"
	"fmt"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/vmctl"
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
  extract-hash   Extract FOD hash from failed build log and patch nix file
  interactive    Stage 2: boot runtime VM with services (interactive shell)
  check          Stage 2: boot runtime VM, health-check, exit
  check-health   Stage 2: host-side health endpoint verification

Flags for 'build-jar':
  --network-policy <path>   Network policy file (reserved, not yet implemented)
  --output-dir <path>       JAR output directory (default: build/signal-server-jar)
  --timeout <duration>      Kill QEMU after this duration (default: 30m)

Flags for 'extract-hash':
  --log <path>              Build log file (default: build/signal-server-build.log)
  --nix-file <path>         Nix file to patch (default: nix/signal-server-build.nix)
  --dry-run                 Print the hash but do not patch the file

Flags for 'check-health':
  --host <host>             Health check host (default: localhost)
  --port <port>             Health check port (default: 8081)
  --retries <n>             Max retries (default: 30)

Flags for 'check':
  --timeout <duration>      Health-check timeout (default: 120s)
  --endpoint <url>          Health endpoint URL (default: http://localhost:8081/healthcheck)

Requires: qemu-system-x86_64, /dev/kvm
The VM images must be pre-built via './uv vm signal build-jar'.
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
		// TODO: --network-policy is reserved for future per-build egress filtering;
		// currently all build VMs use the same user-mode NAT NIC.
		_ = fs.String("network-policy", "conf/vm-network-policy.conf", "network policy file (reserved, not yet implemented)")
		outputDir := fs.String("output-dir", "", "JAR output directory")
		timeoutStr := fs.String("timeout", "30m", "build timeout")
		fs.Parse(args[1:])
		timeout, err := time.ParseDuration(*timeoutStr)
		if err != nil {
			fmt.Fprintf(os.Stderr, "vm-signal: invalid --timeout %q: %v\n", *timeoutStr, err)
			return 2
		}
		return runBuildJar(*outputDir, timeout)

	case "extract-hash":
		fs := flag.NewFlagSet("extract-hash", flag.ExitOnError)
		logFile := fs.String("log", "", "build log file")
		nixFile := fs.String("nix-file", "", "nix file to patch")
		dryRun := fs.Bool("dry-run", false, "print hash without patching")
		fs.Parse(args[1:])
		return runExtractHash(*logFile, *nixFile, *dryRun)

	case "interactive":
		fs := flag.NewFlagSet("interactive", flag.ExitOnError)
		memory := fs.Int("memory", 0, "VM memory in MB (0 = auto-scale to 25% of host)")
		cores := fs.Int("cores", 0, "VM CPU cores (0 = auto-scale to 25% of host)")
		fs.Parse(args[1:])
		return runRuntime("interactive", *memory, *cores, 0, "")

	case "check":
		fs := flag.NewFlagSet("check", flag.ExitOnError)
		timeoutStr := fs.String("timeout", "120s", "health-check timeout")
		endpoint := fs.String("endpoint", "http://localhost:8081/healthcheck", "health endpoint URL")
		fs.Parse(args[1:])
		timeout, err := time.ParseDuration(*timeoutStr)
		if err != nil {
			fmt.Fprintf(os.Stderr, "vm-signal: invalid --timeout %q: %v\n", *timeoutStr, err)
			return 2
		}
		return runRuntime("check", 0, 0, timeout, *endpoint)

	case "check-health":
		fs := flag.NewFlagSet("check-health", flag.ExitOnError)
		host := fs.String("host", "localhost", "health check host")
		port := fs.Int("port", 8081, "health check port")
		retries := fs.Int("retries", 30, "max retries")
		fs.Parse(args[1:])
		if err := runCheckHealth(*host, *port, *retries); err != nil {
			logMsg(red, err.Error())
			return 1
		}
		return 0

	case "help", "-h", "--help":
		flag.Usage()
		return 0

	default:
		fmt.Fprintf(os.Stderr, "vm-signal: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		return 2
	}
}

// preflightCheck validates KVM and QEMU availability.
// Delegates to vmctl.PreflightQEMU.
func preflightCheck() error {
	return vmctl.PreflightQEMU()
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

// resolveJarOutputDir returns the absolute path for the JAR output directory.
func resolveJarOutputDir(repoRoot, outputDirFlag string) string {
	if outputDirFlag == "" {
		return filepath.Join(repoRoot, "build", "signal-server-jar")
	}
	if filepath.IsAbs(outputDirFlag) {
		return outputDirFlag
	}
	return filepath.Join(repoRoot, outputDirFlag)
}

// findNixBuild locates the nix-build binary on PATH or at the default Nix
// profile location. Returns an error if not found.
func findNixBuild() (string, error) {
	nixBuild, err := exec.LookPath("nix-build")
	if err == nil {
		return nixBuild, nil
	}
	nixBuild = "/nix/var/nix/profiles/default/bin/nix-build"
	if _, err := os.Stat(nixBuild); err != nil {
		return "", fmt.Errorf("nix-build not found on PATH")
	}
	return nixBuild, nil
}

// parseSandboxBuildDir extracts UMBRAVOX_NIX_SANDBOX_BUILD_DIR from the
// nix-vm-build-config.sh output, returning fallback if not found.
func parseSandboxBuildDir(cfgOutput, fallback string) string {
	for _, line := range strings.Split(cfgOutput, "\n") {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "export ") {
			line = strings.TrimPrefix(line, "export ")
		}
		if strings.HasPrefix(line, "UMBRAVOX_NIX_SANDBOX_BUILD_DIR=") {
			val := strings.TrimPrefix(line, "UMBRAVOX_NIX_SANDBOX_BUILD_DIR=")
			val = strings.Trim(val, "\"'")
			if val != "" {
				return val
			}
		}
	}
	return fallback
}

// ensureBuildImage builds the signal-server build VM image via nix-build if it
// does not already exist at buildImagePath.
func ensureBuildImage(repoRoot, buildImagePath, tmpDir string) error {
	if fi, err := os.Stat(buildImagePath); err == nil && fi.IsDir() {
		return nil
	}

	logMsg(yellow, "Build VM image not found, building now...")
	os.RemoveAll(buildImagePath)

	cfgScript := filepath.Join(repoRoot, "scripts", "nix-vm-build-config.sh")
	if info, err := os.Stat(cfgScript); err == nil && info.Mode()&0o111 == 0 {
		os.Chmod(cfgScript, 0o755)
	}

	nixBuild, err := findNixBuild()
	if err != nil {
		logMsg(yellow, "Install Nix or add /nix/var/nix/profiles/default/bin to PATH.")
		return err
	}

	cfgCmd := exec.Command(cfgScript, "shell")
	cfgOut, err := cfgCmd.Output()
	if err != nil {
		return fmt.Errorf("failed to run nix-vm-build-config.sh: %w", err)
	}
	sandboxBuildDir := parseSandboxBuildDir(string(cfgOut), tmpDir+"/sandbox")

	os.MkdirAll(tmpDir, 0o755)

	logMsg(blue, fmt.Sprintf("Using TMPDIR=%s", tmpDir))
	logMsg(blue, "Building via nix (this may take several minutes)...")

	nixCmd := exec.Command(nixBuild,
		filepath.Join(repoRoot, "nix", "vm-signal-server.nix"),
		"-A", "buildVm",
		"--option", "build-dir", tmpDir,
		"--option", "sandbox-build-dir", sandboxBuildDir,
		"-o", buildImagePath)
	nixCmd.Stdout = os.Stdout
	nixCmd.Stderr = os.Stderr
	nixCmd.Env = append(os.Environ(), "TMPDIR="+tmpDir)
	if err := nixCmd.Run(); err != nil {
		return fmt.Errorf("nix-build failed: %w", err)
	}
	return nil
}

// checkJarResult reports success or failure based on whether the JAR file was
// produced, returning 0 on success.
func checkJarResult(jarOutputDir string, qemuErr error) int {
	jarPath := filepath.Join(jarOutputDir, "signal-server.jar")
	if _, err := os.Stat(jarPath); err == nil {
		fmt.Fprintln(os.Stderr)
		logMsg(green, "Signal-Server JAR built successfully!")
		logMsg(green, fmt.Sprintf("Location: %s", jarPath))
		fmt.Fprintln(os.Stderr)
		logMsg(blue, "Now rebuild the runtime VM image:")
		logMsg(blue, "  ./uv vm signal build-jar")
		return 0
	}

	fmt.Fprintln(os.Stderr)
	logMsg(red, "Build failed -- no JAR produced.")
	logMsg(red, "Check the VM console output above for errors.")
	return 1
}

// runBuildJar implements the build-jar subcommand (Stage 1).
func runBuildJar(outputDirFlag string, timeout time.Duration) int {
	if err := preflightCheck(); err != nil {
		return 1
	}

	repoRoot, err := repo.Root()
	if err != nil {
		logMsg(red, fmt.Sprintf("Cannot find repo root: %v", err))
		return 1
	}
	vmCacheDir := filepath.Join(repoRoot, "build", "vm-signal-server")
	buildImagePath := filepath.Join(vmCacheDir, "build-image")
	tmpDir := filepath.Join(vmCacheDir, "tmp")
	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		logMsg(red, fmt.Sprintf("Failed to create tmp dir: %v", err))
		return 1
	}

	jarOutputDir := resolveJarOutputDir(repoRoot, outputDirFlag)

	if err := ensureBuildImage(repoRoot, buildImagePath, tmpDir); err != nil {
		logMsg(red, err.Error())
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
	vmCores, vmMemMB := qemu.ScaleToHost(qemu.ProfileDev)

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

	var cmd *exec.Cmd
	if timeout > 0 {
		ctx, cancel := context.WithTimeout(context.Background(), timeout)
		defer cancel()
		cmd = exec.CommandContext(ctx, "qemu-system-x86_64", qemuArgs...)
	} else {
		cmd = exec.Command("qemu-system-x86_64", qemuArgs...)
	}
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	qemuErr := cmd.Run()
	return checkJarResult(jarOutputDir, qemuErr)
}

// runRuntime implements the interactive and check subcommands (Stage 2).
// memoryMB and cores override auto-scaling when non-zero.
// timeout sets a deadline on the QEMU process (check mode only; 0 = no deadline).
// endpoint overrides the health-check URL logged at startup (check mode only).
func runRuntime(mode string, memoryMB, cores int, timeout time.Duration, endpoint string) int {
	if err := preflightCheck(); err != nil {
		return 1
	}

	repoRoot, err := repo.Root()
	if err != nil {
		logMsg(red, fmt.Sprintf("Cannot find repo root: %v", err))
		return 1
	}
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
		logMsg(yellow, "Run './uv vm signal build-jar' first.")
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

	// Runtime VM: 25% of host by default; flags override individual values.
	vmCores, vmMemMB := qemu.ScaleToHost(qemu.ProfileRuntime)
	if cores > 0 {
		vmCores = cores
	}
	if memoryMB > 0 {
		vmMemMB = memoryMB
	}

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
	if mode == "check" {
		if endpoint == "" {
			endpoint = "http://localhost:8081/healthcheck"
		}
		logMsg(blue, fmt.Sprintf("Health endpoint: %s", endpoint))
		if timeout > 0 {
			logMsg(blue, fmt.Sprintf("Timeout: %s", timeout))
		}
	}
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

	var cmd *exec.Cmd
	if timeout > 0 {
		ctx, cancel := context.WithTimeout(context.Background(), timeout)
		defer cancel()
		cmd = exec.CommandContext(ctx, "qemu-system-x86_64", qemuArgs...)
	} else {
		cmd = exec.Command("qemu-system-x86_64", qemuArgs...)
	}
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

// runCheckHealth performs a host-side HTTP health check against the Signal-Server
// admin endpoint, retrying until success or max retries exhausted.
func runCheckHealth(host string, port int, maxRetries int) error {
	client := &http.Client{Timeout: 5 * time.Second}
	url := fmt.Sprintf("http://%s:%d/healthcheck", host, port)

	logMsg(blue, fmt.Sprintf("Checking Signal-Server health at %s:%d...", host, port))

	for i := 0; i < maxRetries; i++ {
		resp, err := client.Get(url)
		if err == nil {
			resp.Body.Close()
			if resp.StatusCode == http.StatusOK {
				logMsg(green, "Health check passed")
				return nil
			}
		}
		time.Sleep(2 * time.Second)
	}
	return fmt.Errorf("health check failed after %d retries (%s:%d)", maxRetries, host, port)
}

// runExtractHash parses a build log for the Nix FOD hash mismatch error,
// extracts the correct hash, and patches nix/signal-server-build.nix.
//
// This implements the same logic as the archived Makefile target
// vm-signal-server-hash (M19.4.5).
func runExtractHash(logFileFlag, nixFileFlag string, dryRun bool) int {
	repoRoot, err := repo.Root()
	if err != nil {
		logMsg(red, fmt.Sprintf("Cannot find repo root: %v", err))
		return 1
	}

	logFile := logFileFlag
	if logFile == "" {
		logFile = filepath.Join(repoRoot, "build", "signal-server-build.log")
	} else if !filepath.IsAbs(logFile) {
		logFile = filepath.Join(repoRoot, logFile)
	}

	nixFile := nixFileFlag
	if nixFile == "" {
		nixFile = filepath.Join(repoRoot, "nix", "signal-server-build.nix")
	} else if !filepath.IsAbs(nixFile) {
		nixFile = filepath.Join(repoRoot, nixFile)
	}

	// Read the build log
	data, err := os.ReadFile(logFile)
	if err != nil {
		logMsg(red, fmt.Sprintf("Build log not found: %s", logFile))
		logMsg(yellow, "Run the build first and capture output:")
		logMsg(yellow, "  ./uv vm signal build-jar 2>&1 | tee build/signal-server-build.log")
		return 1
	}

	// Extract the "got: sha256-..." hash from the Nix error output.
	// Nix prints lines like:
	//   got:    sha256-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=
	re := regexp.MustCompile(`got:\s+(sha256-[A-Za-z0-9+/]+=*)`)
	matches := re.FindAllStringSubmatch(string(data), -1)
	if len(matches) == 0 {
		logMsg(red, fmt.Sprintf("No 'got: sha256-...' hash found in %s", logFile))
		logMsg(yellow, "The build may not have reached the hash mismatch stage.")
		logMsg(yellow, "Look for 'hash mismatch in fixed-output derivation' in the log.")
		return 1
	}

	// Use the last match (in case there are multiple FOD steps)
	gotHash := matches[len(matches)-1][1]
	logMsg(green, fmt.Sprintf("Extracted FOD hash: %s", gotHash))

	if dryRun {
		logMsg(blue, "Dry run: not patching file.")
		fmt.Println(gotHash)
		return 0
	}

	// Read the nix file and replace the outputHash value
	nixData, err := os.ReadFile(nixFile)
	if err != nil {
		logMsg(red, fmt.Sprintf("Cannot read nix file: %s", nixFile))
		return 1
	}

	hashRe := regexp.MustCompile(`outputHash = "sha256-[A-Za-z0-9+/=]*";`)
	if !hashRe.Match(nixData) {
		logMsg(red, "Cannot find outputHash line in nix file")
		return 1
	}

	replacement := fmt.Sprintf(`outputHash = "%s";`, gotHash)
	newData := hashRe.ReplaceAll(nixData, []byte(replacement))

	if err := os.WriteFile(nixFile, newData, 0o644); err != nil {
		logMsg(red, fmt.Sprintf("Failed to write nix file: %v", err))
		return 1
	}

	logMsg(green, fmt.Sprintf("Updated %s with FOD hash", nixFile))
	logMsg(blue, "Next step: ./uv vm signal build-jar  (should succeed now)")
	return 0
}

// runBuildJarV2 is the vmctl-based replacement for runBuildJar.
// It constructs a VMSpec and delegates all QEMU argument assembly to
// QEMUHypervisor.Boot, eliminating the hand-rolled arg slice in runBuildJar.
func runBuildJarV2(outputDirFlag string) int {
	if err := preflightCheck(); err != nil {
		return 1
	}

	repoRoot, err := repo.Root()
	if err != nil {
		logMsg(red, fmt.Sprintf("Cannot find repo root: %v", err))
		return 1
	}
	vmCacheDir := filepath.Join(repoRoot, "build", "vm-signal-server")
	buildImagePath := filepath.Join(vmCacheDir, "build-image")
	tmpDir := filepath.Join(vmCacheDir, "tmp")
	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		logMsg(red, fmt.Sprintf("Failed to create tmp dir: %v", err))
		return 1
	}

	jarOutputDir := resolveJarOutputDir(repoRoot, outputDirFlag)

	if err := ensureBuildImage(repoRoot, buildImagePath, tmpDir); err != nil {
		logMsg(red, err.Error())
		return 1
	}

	diskImg, err := filepath.EvalSymlinks(filepath.Join(buildImagePath, "nixos.img"))
	if err != nil {
		logMsg(red, fmt.Sprintf("Cannot resolve nixos.img: %v", err))
		return 1
	}

	dm := vmctl.DiskManager{}
	overlay, err := dm.CreateOverlay(diskImg, tmpDir)
	if err != nil {
		logMsg(red, fmt.Sprintf("Failed to create overlay: %v", err))
		return 1
	}
	defer overlay.Remove()

	if err := os.MkdirAll(jarOutputDir, 0o755); err != nil {
		logMsg(red, fmt.Sprintf("Failed to create JAR output dir: %v", err))
		return 1
	}

	logMsg(blue, "Stage 1 (v2): Building Signal-Server JAR in VM...")
	logMsg(blue, "Network: enabled (Maven needs to fetch deps)")
	logMsg(blue, fmt.Sprintf("Output: %s/signal-server.jar", jarOutputDir))
	fmt.Fprintln(os.Stderr)

	spec := &vmctl.VMSpec{
		BaseImage: vmctl.ImageRef{
			Path:   overlay.Path,
			Format: vmctl.DiskFormatQCOW2,
		},
		Shares: []vmctl.ShareSpec{
			{
				HostPath: jarOutputDir,
				MountTag: "output",
				ID:       "output",
			},
		},
		Network:  vmctl.NetworkSpec{Mode: vmctl.NetworkUserMode},
		Resources: vmctl.Resources{Fraction: 50},
		Timeout:  30 * time.Minute,
		NoReboot: true,
	}

	ctx := context.Background()
	if spec.Timeout > 0 {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, spec.Timeout)
		defer cancel()
	}

	hv := vmctl.QEMUHypervisor{}
	result, hvErr := hv.Boot(ctx, spec, tmpDir)
	if hvErr != nil {
		logMsg(red, fmt.Sprintf("VM boot failed: %v", hvErr))
		return 1
	}
	_ = result
	return checkJarResult(jarOutputDir, hvErr)
}

// runRuntimeV2 is the vmctl-based replacement for runRuntime.
// It constructs a VMSpec and delegates all QEMU argument assembly to
// QEMUHypervisor.Boot, eliminating the hand-rolled arg slice in runRuntime.
func runRuntimeV2(mode string) int {
	if err := preflightCheck(); err != nil {
		return 1
	}

	repoRoot, err := repo.Root()
	if err != nil {
		logMsg(red, fmt.Sprintf("Cannot find repo root: %v", err))
		return 1
	}
	vmCacheDir := filepath.Join(repoRoot, "build", "vm-signal-server")
	vmImagePath := filepath.Join(vmCacheDir, "image")
	tmpDir := filepath.Join(vmCacheDir, "tmp")

	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		logMsg(red, fmt.Sprintf("Failed to create tmp dir: %v", err))
		return 1
	}

	if fi, err := os.Stat(vmImagePath); err != nil || !fi.IsDir() {
		logMsg(red, fmt.Sprintf("Runtime VM image not found at %s", vmImagePath))
		logMsg(yellow, "Run './uv vm signal build-jar' first.")
		return 1
	}

	diskImg, err := filepath.EvalSymlinks(filepath.Join(vmImagePath, "nixos.img"))
	if err != nil {
		logMsg(red, fmt.Sprintf("Cannot resolve nixos.img: %v", err))
		return 1
	}

	dm := vmctl.DiskManager{}
	overlay, err := dm.CreateOverlay(diskImg, tmpDir)
	if err != nil {
		logMsg(red, fmt.Sprintf("Failed to create overlay: %v", err))
		return 1
	}
	defer overlay.Remove()

	logMsg(blue, "Stage 2 (v2): Booting Signal-Server runtime VM...")
	logMsg(blue, "Network: deny-all (runtime isolation)")
	logMsg(blue, fmt.Sprintf("Mode: %s", mode))
	fmt.Fprintln(os.Stderr)

	spec := &vmctl.VMSpec{
		BaseImage: vmctl.ImageRef{
			Path:   overlay.Path,
			Format: vmctl.DiskFormatQCOW2,
		},
		Network:   vmctl.NetworkSpec{Mode: vmctl.NetworkNone},
		Resources: vmctl.Resources{Fraction: 25},
		NoReboot:  mode != "interactive",
	}

	ctx := context.Background()
	hv := vmctl.QEMUHypervisor{}
	result, hvErr := hv.Boot(ctx, spec, tmpDir)
	if hvErr != nil {
		logMsg(red, fmt.Sprintf("VM boot failed: %v", hvErr))
		return 1
	}
	return result.ExitCode
}

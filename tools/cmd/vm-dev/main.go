// Command vm-dev boots the UmbraVOX NixOS development VM with the source tree
// mounted, then either opens an interactive shell, a GUI console, or runs a
// single command and exits.
//
// This is the Go replacement for scripts/vm-dev-run.sh.
//
// Usage:
//
//	vm-dev                           # interactive dev shell (serial console)
//	vm-dev dev                       # alias for interactive
//	vm-dev interactive               # interactive dev shell (serial console)
//	vm-dev gui                       # interactive dev shell (QEMU VGA window)
//	vm-dev exec "cabal build all"    # run command, exit
//	vm-dev smoke                     # run full smoke pipeline, exit
//
// Requires: qemu-system-x86_64, genext2fs, /dev/kvm
// The VM image must be pre-built via `make vm-image-build`.
package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"strings"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/disk"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/netpol"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/ninep"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
)

// ANSI color helpers.
const (
	red    = "\033[0;31m"
	green  = "\033[0;32m"
	blue   = "\033[0;34m"
	yellow = "\033[1;33m"
	nc     = "\033[0m"
)

func logMsg(color, msg string) {
	fmt.Fprintf(os.Stderr, "%s[VM-DEV]%s %s\n", color, nc, msg)
}

func main() {
	os.Exit(run())
}

func run() int {
	mode, vmCmd := parseArgs()

	repoRoot := findRepoRoot()
	vmCacheDir := filepath.Join(repoRoot, "build", "vm")
	vmImagePath := filepath.Join(vmCacheDir, "image")

	// ── Preflight ────────────────────────────────────────────────────
	if err := preflightCheck(vmImagePath); err != nil {
		return 1
	}

	// ── Resolve base disk image ──────────────────────────────────────
	diskImg, err := filepath.EvalSymlinks(filepath.Join(vmImagePath, "nixos.img"))
	if err != nil {
		logMsg(red, fmt.Sprintf("Cannot resolve nixos.img: %v", err))
		return 1
	}

	// ── Create source disk ───────────────────────────────────────────
	srcDisk, err := createSourceDisk(repoRoot, mode, vmCmd)
	if err != nil {
		logMsg(red, fmt.Sprintf("Failed to create source disk: %v", err))
		return 1
	}
	defer os.Remove(srcDisk)

	// ── Create COW overlay ───────────────────────────────────────────
	logMsg(blue, "Creating COW overlay...")
	overlay, err := disk.CreateOverlay(diskImg, "")
	if err != nil {
		logMsg(red, fmt.Sprintf("Failed to create overlay: %v", err))
		return 1
	}
	defer overlay.Remove()

	// ── Persistent build cache disk ──────────────────────────────────
	cacheDisk := filepath.Join(vmCacheDir, "build-cache.qcow2")
	if err := disk.EnsureCacheDisk(cacheDisk, "4G"); err != nil {
		logMsg(red, fmt.Sprintf("Failed to create cache disk: %v", err))
		return 1
	}

	// ── Shared output directory (host <-> guest via 9p) ──────────────
	outputDir := filepath.Join(repoRoot, "build", "vm-output")
	if err := os.MkdirAll(outputDir, 0o755); err != nil {
		logMsg(red, fmt.Sprintf("Failed to create output dir: %v", err))
		return 1
	}

	vmStatusFile := filepath.Join(outputDir, "vm-exec-status")
	if mode != "interactive" && mode != "gui" {
		os.Remove(vmStatusFile)
	}

	// ── Auto-scale VM resources ──────────────────────────────────────
	hostCores := runtime.NumCPU()
	hostMemMB := readHostMemoryMB()

	vmCores := hostCores / 2
	vmMemMB := hostMemMB / 2

	// Enforce minimums: 25% of host or absolute floor
	if minCores := hostCores / 4; vmCores < minCores {
		vmCores = minCores
	}
	if minMem := hostMemMB / 4; vmMemMB < minMem {
		vmMemMB = minMem
	}
	if vmCores < 2 {
		vmCores = 2
	}
	if vmMemMB < 2048 {
		vmMemMB = 2048
	}

	// ── Network policy ───────────────────────────────────────────────
	policyFile := filepath.Join(repoRoot, "vm-network-policy.conf")
	policy, err := netpol.ParseFile(policyFile)
	if err != nil {
		logMsg(red, fmt.Sprintf("Failed to parse network policy: %v", err))
		return 1
	}
	netArgs, err := policy.QEMUNetArgs()
	if err != nil {
		logMsg(red, fmt.Sprintf("Network policy error: %v", err))
		return 1
	}

	logMsg(blue, fmt.Sprintf("VM resources: %d cores, %dMB RAM (host: %d cores, %dMB)",
		vmCores, vmMemMB, hostCores, hostMemMB))
	logMsg(blue, fmt.Sprintf("Network: %s", netArgs))

	// ── Build QEMU config ────────────────────────────────────────────
	outputShare := ninep.DefaultOutputShare(outputDir)

	cfg := qemu.Config{
		Machine:  "q35,accel=kvm",
		CPUModel: "max",
		MemoryMB: vmMemMB,
		SMP:      vmCores,
		Drives: []qemu.Drive{
			{Interface: "virtio", Format: qemu.FormatQCOW2, File: overlay.Path},
			{Interface: "virtio", Format: qemu.FormatRaw, File: srcDisk, ReadOnly: true},
			{Interface: "virtio", Format: qemu.FormatQCOW2, File: cacheDisk},
		},
		VirtFS: []qemu.VirtFS{
			{
				LocalPath:     outputShare.LocalPath,
				MountTag:      outputShare.MountTag,
				SecurityModel: string(outputShare.SecurityModel),
				ID:            outputShare.ID,
			},
		},
		NetArgs: netArgs,
	}

	if mode == "gui" {
		cfg.Display = qemu.DisplayGTK
		logMsg(blue, "Display: GUI (QEMU VGA window)")
	} else {
		cfg.Display = qemu.DisplayNone
	}

	if mode != "interactive" && mode != "gui" {
		cfg.NoReboot = true
	}

	// ── Boot ─────────────────────────────────────────────────────────
	logMsg(blue, "Booting NixOS development VM...")
	logMsg(blue, fmt.Sprintf("Mode: %s", mode))
	if vmCmd != "" {
		logMsg(blue, fmt.Sprintf("Command: %s", vmCmd))
	}
	fmt.Fprintln(os.Stderr)

	qemuArgs := cfg.Args()
	cmd := exec.Command("qemu-system-x86_64", qemuArgs...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	qemuErr := cmd.Run()
	qemuExit := 0
	if qemuErr != nil {
		if exitErr, ok := qemuErr.(*exec.ExitError); ok {
			qemuExit = exitErr.ExitCode()
		} else {
			logMsg(red, fmt.Sprintf("Failed to run QEMU: %v", qemuErr))
			qemuExit = 1
		}
	}

	// ── Interpret guest exit status (exec mode) ──────────────────────
	if mode != "interactive" && mode != "gui" {
		data, err := os.ReadFile(vmStatusFile)
		if err == nil {
			raw := strings.TrimSpace(strings.SplitN(string(data), "\n", 2)[0])
			if matched, _ := regexp.MatchString(`^[0-9]+$`, raw); matched {
				status, _ := strconv.Atoi(raw)
				if status == 0 {
					logMsg(green, "Guest command completed successfully.")
				} else {
					logMsg(red, fmt.Sprintf("Guest command failed with exit %d.", status))
				}
				return status
			}
			logMsg(yellow, fmt.Sprintf("Invalid vm-exec-status payload: '%s'; falling back to QEMU exit.", raw))
		} else {
			logMsg(yellow, "Missing vm-exec-status marker; falling back to QEMU exit.")
		}
		return qemuExit
	}

	return 0
}

// smokeCmd is the full smoke pipeline command executed in smoke mode.
const smokeCmd = `cabal build all --enable-tests && cabal test umbravox-test`

// parseArgs extracts mode and optional command from os.Args.
func parseArgs() (mode, vmCmd string) {
	args := os.Args[1:]
	if len(args) == 0 {
		return "interactive", ""
	}
	mode = args[0]
	switch mode {
	case "dev":
		return "interactive", ""
	case "interactive", "gui":
		return mode, ""
	case "exec":
		if len(args) < 2 {
			fmt.Fprintf(os.Stderr, "Usage: %s exec \"command\"\n", os.Args[0])
			os.Exit(2)
		}
		return "exec", args[1]
	case "smoke":
		return "exec", smokeCmd
	case "help", "-h", "--help":
		printUsage()
		os.Exit(0)
		return "", ""
	default:
		fmt.Fprintf(os.Stderr, "Unknown mode: %s\n", mode)
		printUsage()
		os.Exit(2)
		return "", "" // unreachable
	}
}

func printUsage() {
	fmt.Fprintf(os.Stderr, `Usage: %s <mode> [args]

Modes:
  dev, interactive   Interactive dev shell via serial console (default)
  gui                Interactive dev shell via QEMU VGA window
  exec "cmd"         Run a command in the VM, then exit
  smoke              Run the full smoke pipeline (build + test), then exit

Requires: qemu-system-x86_64, genext2fs, /dev/kvm
The VM image must be pre-built via 'make vm-image-build'.
`, os.Args[0])
}

// findRepoRoot walks up from the executable (or cwd) to find the repo root.
func findRepoRoot() string {
	// Prefer the directory containing go.mod's parent (tools/ is a subdir).
	// In practice we find the repo root by looking for scripts/vm-dev-run.sh.
	exe, err := os.Executable()
	if err == nil {
		// Walk up from exe directory
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

	// Fallback: walk up from cwd
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
	return "" // unreachable
}

// preflightCheck validates KVM, QEMU, genext2fs, and VM image availability.
func preflightCheck(vmImagePath string) error {
	ok := true

	if _, err := os.Stat("/dev/kvm"); os.IsNotExist(err) {
		logMsg(red, "/dev/kvm not found; KVM required")
		ok = false
	}
	if _, err := exec.LookPath("qemu-system-x86_64"); err != nil {
		logMsg(red, "qemu-system-x86_64 not on PATH")
		ok = false
	}
	if _, err := exec.LookPath("genext2fs"); err != nil {
		logMsg(red, "genext2fs not on PATH")
		ok = false
	}
	if fi, err := os.Stat(vmImagePath); err != nil || !fi.IsDir() {
		logMsg(red, fmt.Sprintf("VM image not found at %s", vmImagePath))
		logMsg(yellow, "Run 'make vm-image-build' first.")
		ok = false
	}

	if !ok {
		return fmt.Errorf("preflight checks failed")
	}
	return nil
}

// readHostMemoryMB reads total host memory from /proc/meminfo.
// Returns 8192 as a fallback.
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

// createSourceDisk exports the worktree into an ext2 disk image.
func createSourceDisk(repoRoot, mode, vmCmd string) (string, error) {
	// Create temp file for the ext2 disk
	diskFile, err := os.CreateTemp("", fmt.Sprintf("umbravox-vm-dev-source.%d.*.ext2", os.Getpid()))
	if err != nil {
		return "", fmt.Errorf("create source disk temp file: %w", err)
	}
	diskPath := diskFile.Name()
	diskFile.Close()
	os.Remove(diskPath) // genext2fs creates it

	// Create temp dir for source export
	srcDir, err := os.MkdirTemp("", "umbravox-vm-dev-src.")
	if err != nil {
		return "", fmt.Errorf("create source temp dir: %w", err)
	}
	defer os.RemoveAll(srcDir)

	logMsg(blue, "Exporting source tree (current worktree)...")

	// Export live worktree excluding large/generated directories and VCS metadata.
	// Uses tar to replicate the shell script behavior.
	tarCreate := exec.Command("tar",
		"-C", repoRoot,
		"--exclude=.git",
		"--exclude=dist-newstyle",
		"--exclude=build",
		"--exclude=result",
		"-cf", "-", ".")
	tarExtract := exec.Command("tar", "-xf", "-", "-C", srcDir)

	pipe, err := tarCreate.StdoutPipe()
	if err != nil {
		return "", fmt.Errorf("tar pipe: %w", err)
	}
	tarExtract.Stdin = pipe

	if err := tarCreate.Start(); err != nil {
		return "", fmt.Errorf("tar create start: %w", err)
	}
	if err := tarExtract.Start(); err != nil {
		return "", fmt.Errorf("tar extract start: %w", err)
	}
	if err := tarCreate.Wait(); err != nil {
		return "", fmt.Errorf("tar create: %w", err)
	}
	if err := tarExtract.Wait(); err != nil {
		return "", fmt.Errorf("tar extract: %w", err)
	}

	// Optionally copy dist-newstyle to preserve build cache
	if os.Getenv("UMBRAVOX_VM_CACHE") == "1" {
		distDir := filepath.Join(repoRoot, "dist-newstyle")
		if fi, err := os.Stat(distDir); err == nil && fi.IsDir() {
			logMsg(blue, "Including dist-newstyle build cache...")
			cpCmd := exec.Command("cp", "-a", distDir, filepath.Join(srcDir, "dist-newstyle"))
			if out, err := cpCmd.CombinedOutput(); err != nil {
				return "", fmt.Errorf("copy dist-newstyle: %w\n%s", err, out)
			}
		}
	}

	// Generate and embed init script
	initScript := generateInitScript(mode, vmCmd)
	initPath := filepath.Join(srcDir, ".vm-init.sh")
	if err := os.WriteFile(initPath, []byte(initScript), 0o755); err != nil {
		return "", fmt.Errorf("write init script: %w", err)
	}

	// For exec mode, write the command to a separate file so the init script
	// reads it at runtime (avoids shell metacharacter issues).
	if mode == "exec" && vmCmd != "" {
		execCmdPath := filepath.Join(srcDir, ".vm-exec-cmd")
		if err := os.WriteFile(execCmdPath, []byte(vmCmd+"\n"), 0o644); err != nil {
			return "", fmt.Errorf("write exec command file: %w", err)
		}
	}

	logMsg(blue, "Creating source disk...")
	genCmd := exec.Command("genext2fs", "-b", "1048576", "-d", srcDir, diskPath)
	if out, err := genCmd.CombinedOutput(); err != nil {
		return "", fmt.Errorf("genext2fs: %w\n%s", err, out)
	}

	return diskPath, nil
}

// generateInitScript produces the in-guest init script content.
func generateInitScript(mode, cmd string) string {
	var b strings.Builder

	// Common preamble (runs in all modes)
	b.WriteString(`#!/usr/bin/env bash
set -euo pipefail

export HOME=/root
export PATH="/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH"

# Offline cabal config
mkdir -p /root/.cabal
cat > /root/.cabal/config << 'CABALEOF'
offline: True
nix: False
CABALEOF

# Mount source disk
mkdir -p /mnt/src
if ! mountpoint -q /mnt/src 2>/dev/null; then
    mount -o ro /dev/vdb /mnt/src 2>/dev/null || true
fi

# Mount shared output directory (host <-> guest via 9p)
mkdir -p /output
mount -t 9p -o trans=virtio,version=9p2000.L output /output 2>/dev/null || true

# Mount persistent build cache (vdc)
mkdir -p /cache
if [ -b /dev/vdc ]; then
    # Format on first use, then mount
    if ! blkid /dev/vdc >/dev/null 2>&1; then
        mkfs.ext4 -q /dev/vdc
    fi
    mount /dev/vdc /cache 2>/dev/null || true
    mkdir -p /cache/dist-newstyle /cache/.cabal-store
fi

# Copy to writable workspace (idempotent marker prevents duplicate syncs)
if [ ! -f /work/umbravox/.vm-source-ready ]; then
    mkdir -p /work/umbravox
    cp -a /mnt/src/. /work/umbravox/
    touch /work/umbravox/.vm-source-ready
fi

# Link build cache if available
if [ -d /cache/dist-newstyle ]; then
    rm -rf /work/umbravox/dist-newstyle
    ln -s /cache/dist-newstyle /work/umbravox/dist-newstyle
fi
if [ -d /cache/.cabal-store ]; then
    mkdir -p /root/.cabal
    ln -sf /cache/.cabal-store /root/.cabal/store
fi

cd /work/umbravox
export UMBRAVOX_ROOT=/work/umbravox
export UMBRAVOX_VM=1
export PATH="/work/umbravox/scripts:$PATH"
unset LD_LIBRARY_PATH 2>/dev/null || true

# Set FSTAR_HOME
FSTAR_EXE_PATH=$(command -v fstar.exe 2>/dev/null || true)
if [ -n "$FSTAR_EXE_PATH" ]; then
    FSTAR_REAL=$(readlink -f "$FSTAR_EXE_PATH")
    export FSTAR_HOME=$(dirname "$(dirname "$FSTAR_REAL")")
fi

# Set COQPATH for Coq packages (stdlib, coqprime, bignums)
# Use the system profile which merges all package paths.
if [ -d /run/current-system/sw/lib/coq ]; then
    COQPATH_DIRS=""
    for d in /run/current-system/sw/lib/coq/*/user-contrib; do
        [ -d "$d" ] && COQPATH_DIRS="$COQPATH_DIRS:$d"
    done
    [ -n "$COQPATH_DIRS" ] && export COQPATH="${COQPATH_DIRS#:}"
fi
`)

	switch {
	case mode == "interactive" || mode == "gui":
		// Banner (shared by interactive and gui)
		b.WriteString(`
echo ""
echo -e "\033[35m  ╦ ╦╔╦╗╔╗ ╦═╗╔═╗╦  ╦╔═╗═╗ ╦\033[0m"
echo -e "\033[35m  ║ ║║║║╠╩╗╠╦╝╠═╣╚╗╔╝║ ║╔╩╦╝\033[0m"
echo -e "\033[35m  ╚═╝╩ ╩╚═╝╩╚═╩ ╩ ╚╝ ╚═╝╩ ╚═\033[0m"
echo -e "\033[33m  Post-Quantum Encrypted Messaging\033[0m"
echo ""
echo -e "  \033[1;32m[ VM DEVELOPMENT SHELL ]\033[0m  \033[90m(NixOS QEMU Guest)\033[0m"
echo ""
echo "  Source: /work/umbravox"
echo "  Kernel: $(uname -r)"
echo "  GHC:    $(ghc --numeric-version 2>/dev/null || echo N/A)"
echo "  Cabal:  $(cabal --numeric-version 2>/dev/null || echo N/A)"
echo "  F*:     $(fstar.exe --version 2>/dev/null | head -1 | sed 's/F\* //' || echo N/A)"
echo "  Z3:     $(z3 --version 2>/dev/null | sed 's/Z3 version //' || echo N/A)"
echo ""
echo "  Commands:"
echo "    cabal build all                Build everything"
echo "    cabal test umbravox-test       Run tests"
echo "    make verify                    F* verification"
echo "    exit / Ctrl-D                  Shut down VM"
echo ""
echo "  NOTE: Source is copied to /work/umbravox (writable tmpfs)."
echo "  Changes inside the VM are NOT synced back to the host."
echo ""
`)
		if mode == "gui" {
			b.WriteString(`cat > /etc/profile.d/umbravox-dev.sh << 'PROFILEEOF'
export HOME=/root
export UMBRAVOX_ROOT=/work/umbravox
export PATH="/work/umbravox/scripts:/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH"
unset LD_LIBRARY_PATH 2>/dev/null || true
cd /work/umbravox 2>/dev/null || true
PROFILEEOF

echo ""
echo "  GUI mode: log in on the VGA console (root, no password)."
echo "  The workspace at /work/umbravox is ready."
echo ""
`)
		} else {
			b.WriteString(`# Drop into interactive shell (serial console mode)
exec /bin/bash --login
`)
		}

	default:
		// Exec mode: read command from .vm-exec-cmd file (written alongside
		// this init script on the source disk). This avoids any shell
		// metacharacter issues from embedding the command directly.
		b.WriteString(`
# Read the command written by the host into .vm-exec-cmd on the source disk.
VM_EXEC_CMD="$(cat /mnt/src/.vm-exec-cmd 2>/dev/null || true)"

echo ""
echo "========================================"
echo "  UmbraVOX VM: $VM_EXEC_CMD"
echo "========================================"
echo ""

# Run the command and preserve its exit status.
set +e
eval "$VM_EXEC_CMD"
STATUS=$?
set -e

# Persist command status to host-visible shared output.
if [ -d /output ]; then
    printf "%s\n" "$STATUS" > /output/vm-exec-status 2>/dev/null || true
    sync 2>/dev/null || true
fi

echo ""
if [ $STATUS -eq 0 ]; then
    echo "VM_DEV_RESULT=PASS"
else
    echo "VM_DEV_RESULT=FAIL (exit $STATUS)"
fi

# Power off after command completes
systemctl poweroff || true
exit $STATUS
`)
	}

	return b.String()
}

// SPDX-License-Identifier: Apache-2.0
package main

import (
	"bufio"
	"compress/gzip"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/disk"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/download"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/netproxy"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/ninep"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
	"github.com/UmbraVoxResearch/vmctl"
)

const (
	builderVersionDefault = "builder-v0.5.0"
	scratchSize           = "100G"
	minFreeSpaceGB        = 35
)

var (
	builderVersion = builderVersionDefault
	builderBaseURL = "https://github.com/UmbraVoxResearch/UmbraVOX/releases/download/" + builderVersionDefault
)

func init() {
	if v := os.Getenv("UMBRAVOX_BUILDER_VERSION"); v != "" {
		builderVersion = v
		builderBaseURL = "https://github.com/UmbraVoxResearch/UmbraVOX/releases/download/" + v
	}
}

// runVM handles: uv vm <action>
func runVM(args []string) int {
	if len(args) == 0 {
		printVMHelp()
		return 0
	}

	action := args[0]
	rest := args[1:]

	switch action {
	case "build-image":
		return vmBuildImage(rest)
	case "build-runtime-image":
		return vmBuildRuntimeImage(rest)
	case "clean-image":
		return vmCleanImage()
	case "smoke":
		return vmSmoke(rest)
	case "seed":
		// Legacy — redirect to appropriate new command
		log.Warn(tag, "Seed commands removed. Use './uv vm build-image' instead.")
		return 1
	case "signal":
		return vmSignal(rest)
	case "integration":
		return vmIntegration(rest)
	case "info":
		return vmInfo()
	default:
		fmt.Fprintf(os.Stderr, "Unknown vm action: %s\n", action)
		printVMHelp()
		return 2
	}
}

func printVMHelp() {
	fmt.Print(`Usage: ./uv vm <action>

Actions:
  build-image [--on-host]    Build NixOS VM image
  build-runtime-image [--on-host] Build lightweight runtime VM images
  clean-image                Remove cached VM image
  smoke [TARGET]             Platform smoke (freebsd, openbsd, netbsd, illumos, dragonfly, arm64, release)
  signal build-jar|update|test|run|health Signal Server VM
  integration [--dual-lan]   Multi-VM integration test
  info                       VM config diagnostics
`)
}

func vmBuildImage(args []string) int {
	onHost := false
	for _, a := range args {
		if a == "--on-host" {
			onHost = true
		}
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	if onHost {
		return vmBuildImageOnHost(repoRoot)
	}

	// ── Preflight: disk space check ───────────────────────────────
	freeBytes := availableDiskSpace(repoRoot)
	freeGB := freeBytes / (1024 * 1024 * 1024)
	if freeGB < minFreeSpaceGB {
		log.Fail(tag, fmt.Sprintf("Insufficient disk space: %dGB free, need %dGB", freeGB, minFreeSpaceGB))
		log.Info(tag, "Free space with: ./uv vm clean-image, ./uv clean --all, or nix-collect-garbage -d")
		return 1
	}

	// ── Ensure builder image ──────────────────────────────────────
	vmCacheDir := filepath.Join(repoRoot, "build", "vm")
	builderDir := filepath.Join(vmCacheDir, "builder-image")
	builderImg := filepath.Join(builderDir, "nixos.img")

	if code := downloadOrBuildBuilder(repoRoot, builderDir, builderImg); code != 0 {
		return code
	}

	return bootBuilderVM(repoRoot, builderImg)
}

// vmBuildImageOnHost builds the VM image directly on the host via nix-build.
func vmBuildImageOnHost(repoRoot string) int {
	log.Info(tag, "Building VM image on host (writes to /nix/store)...")
	b := &vmctl.NixBuild{
		File:    filepath.Join(repoRoot, "nix", "vm-image.nix"),
		Attr:    "qemu",
		OutLink: filepath.Join(repoRoot, "build", "vm", "image"),
		Stdout:  os.Stdout,
		Stderr:  os.Stderr,
	}
	if err := b.Build(); err != nil {
		log.Fail(tag, err.Error())
		return 1
	}
	log.OK(tag, "VM image built successfully.")
	return 0
}

// vmBuildRuntimeImage builds lightweight runtime VM images.
// Default: builds inside the builder VM (same as dev image).
// --on-host: builds directly on the host via nix-build.
func vmBuildRuntimeImage(args []string) int {
	onHost := false
	for _, a := range args {
		if a == "--on-host" {
			onHost = true
		}
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	if onHost {
		return vmBuildRuntimeImageOnHost(repoRoot)
	}

	// Build runtime images via nix-build on the host.
	// The runtime image is small (~1.3GB) so this is fast.
	// TODO: support building inside the builder VM for fully host-isolated builds.
	return vmBuildRuntimeImageOnHost(repoRoot)
}

// vmBuildRuntimeImageOnHost builds runtime images directly on the host.
func vmBuildRuntimeImageOnHost(repoRoot string) int {
	runtimeNix := filepath.Join(repoRoot, "nix", "vm-runtime.nix")
	vmDir := filepath.Join(repoRoot, "build", "vm")

	// ── Firecracker bundle (rootfs + kernel) ─────────────────────
	fcOutDir := filepath.Join(vmDir, "runtime-image")
	os.RemoveAll(fcOutDir) // clean stale symlink/dir before nix-build
	log.Info(tag, "Building Firecracker runtime image on host...")
	cmd := exec.Command("nix-build", runtimeNix, "-A", "firecracker", "-o", fcOutDir)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		log.Fail(tag, fmt.Sprintf("nix-build firecracker failed: %v", err))
		return 1
	}

	// ── QEMU image ───────────────────────────────────────────────
	qemuOutDir := filepath.Join(vmDir, "runtime-qemu-image")
	os.RemoveAll(qemuOutDir) // clean stale symlink/dir before nix-build
	log.Info(tag, "Building QEMU runtime image on host...")
	cmd = exec.Command("nix-build", runtimeNix, "-A", "qemu", "-o", qemuOutDir)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		log.Fail(tag, fmt.Sprintf("nix-build qemu failed: %v", err))
		return 1
	}

	fcSize := pathSize(fcOutDir)
	qemuSize := pathSize(qemuOutDir)
	log.OK(tag, "Runtime images built successfully")
	log.Info(tag, fmt.Sprintf("  Firecracker: %s (%s)", fcOutDir, formatSize(fcSize)))
	log.Info(tag, fmt.Sprintf("  QEMU:        %s (%s)", qemuOutDir, formatSize(qemuSize)))
	return 0
}

// downloadOrBuildBuilder ensures the builder image exists, downloading or
// building it if necessary. Returns 0 on success.
func downloadOrBuildBuilder(repoRoot, builderDir, builderImg string) int {
	vmCacheDir := filepath.Dir(builderDir)
	if err := os.MkdirAll(vmCacheDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create cache dir: %v", err))
		return 1
	}

	if _, err := os.Stat(builderImg); os.IsNotExist(err) {
		// Try download, fall back to local build
		if code := ensureBuilderImage(repoRoot, builderDir, builderImg); code != 0 {
			return code
		}
	}
	// Verify image exists after ensure
	if _, err := os.Stat(builderImg); os.IsNotExist(err) {
		log.Fail(tag, "Builder image not available after download/build attempt")
		return 1
	}
	return 0
}

// builderDisks holds the disk and overlay paths created for a builder VM boot.
type builderDisks struct {
	srcDisk    string
	scratchDisk string
	overlay    *disk.Overlay
}

// prepareBuilderDisks creates the source disk, scratch disk, and COW overlay
// needed to boot the builder VM. The caller must clean up via cleanup().
func prepareBuilderDisks(repoRoot, diskImg, tmpDir string) (*builderDisks, func(), error) {
	vmCacheDir := filepath.Join(repoRoot, "build", "vm")

	log.Info(tag, "Creating source disk (with .git for flake eval)...")
	srcDisk, err := createBuilderSourceDisk(repoRoot, tmpDir)
	if err != nil {
		return nil, nil, fmt.Errorf("failed to create source disk: %w", err)
	}

	scratchDisk := filepath.Join(vmCacheDir, "nix-cache.qcow2")
	if err := disk.EnsureCacheDisk(scratchDisk, scratchSize); err != nil {
		os.Remove(srcDisk)
		return nil, nil, fmt.Errorf("failed to create scratch disk: %w", err)
	}

	log.Info(tag, "Creating COW overlay...")
	overlay, err := disk.CreateOverlay(diskImg, tmpDir)
	if err != nil {
		os.Remove(srcDisk)
		return nil, nil, fmt.Errorf("failed to create overlay: %w", err)
	}

	cleanup := func() {
		os.Remove(srcDisk)
		overlay.Remove()
	}
	return &builderDisks{srcDisk: srcDisk, scratchDisk: scratchDisk, overlay: overlay}, cleanup, nil
}

// setupNetworkFilter initialises the network policy filter for the builder VM.
// If no policy file exists or the allowlist is empty, the filter is not started
// and a nil stop function is returned.
func setupNetworkFilter(repoRoot, tmpDir string) (func(), error) {
	policyFile := filepath.Join(repoRoot, "conf/vm-builder-network-policy.conf")
	allowed, err := netproxy.ParseAllowlist(policyFile)
	if err != nil {
		log.Warn(tag, fmt.Sprintf("No network policy file, using unrestricted: %v", err))
		return nil, nil
	}
	if len(allowed) == 0 {
		return nil, nil
	}

	socketPath := filepath.Join(tmpDir, fmt.Sprintf("uv-proxy.%d.sock", os.Getpid()))
	filter := &netproxy.Filter{
		SocketPath: socketPath,
		Allowed:    allowed,
	}
	if err := filter.Start(); err != nil {
		return nil, fmt.Errorf("failed to start network filter: %w", err)
	}
	log.Info(tag, fmt.Sprintf("Network filter: allowing %d destination(s)", len(allowed)))
	return filter.Stop, nil
}

// checkBuilderResult reads the builder status file and returns an error if the
// build failed or no status was written.
func checkBuilderResult(statusFile string) error {
	statusData, err := os.ReadFile(statusFile)
	if err != nil {
		return fmt.Errorf("builder VM did not produce a status file")
	}
	status := strings.TrimSpace(string(statusData))
	if status != "0" {
		return fmt.Errorf("builder VM failed (status %s)", status)
	}
	return nil
}

// decompressBuilderImage decompresses the zstd-compressed VM image from
// outputDir into the final image directory.
func decompressBuilderImage(outputDir, vmCacheDir string) error {
	compressedImg := filepath.Join(outputDir, "nixos.img.zst")
	finalDir := filepath.Join(vmCacheDir, "image")
	if err := os.MkdirAll(finalDir, 0o755); err != nil {
		return fmt.Errorf("failed to create image dir: %w", err)
	}
	finalImg := filepath.Join(finalDir, "nixos.img")

	log.Info(tag, "Decompressing VM image...")
	zstdCmd := exec.Command("zstd", "-d", compressedImg, "-o", finalImg, "--force")
	if out, err := zstdCmd.CombinedOutput(); err != nil {
		return fmt.Errorf("failed to decompress: %w\n%s", err, out)
	}
	os.Remove(compressedImg)
	log.OK(tag, fmt.Sprintf("VM image ready at %s", finalDir))
	return nil
}

// bootBuilderVM prepares disks, boots the builder VM via vmctl.QEMUHypervisor,
// and extracts the resulting VM image. Returns 0 on success.
func bootBuilderVM(repoRoot, builderImg string) int {
	vmCacheDir := filepath.Join(repoRoot, "build", "vm")
	builderDir := filepath.Dir(builderImg)

	if err := repo.Preflight(builderDir, false); err != nil {
		return 1
	}

	diskImg, err := filepath.EvalSymlinks(builderImg)
	if err != nil {
		diskImg = builderImg
	}

	tmpDir := filepath.Join(vmCacheDir, "tmp")
	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create temp dir: %v", err))
		return 1
	}

	disks, diskCleanup, err := prepareBuilderDisks(repoRoot, diskImg, tmpDir)
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}
	defer diskCleanup()

	// ── Output directory ──────────────────────────────────────────
	outputDir := filepath.Join(repoRoot, "build", "vm-output")
	if err := os.MkdirAll(outputDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create output dir: %v", err))
		return 1
	}
	statusFile := filepath.Join(outputDir, "builder-status")
	os.Remove(statusFile)
	os.Remove(filepath.Join(outputDir, "nixos.img.zst"))

	// ── Network filter ────────────────────────────────────────────
	stopFilter, err := setupNetworkFilter(repoRoot, tmpDir)
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}
	if stopFilter != nil {
		defer stopFilter()
	}

	// ── VMSpec ────────────────────────────────────────────────────
	outputShare := ninep.DefaultOutputShare(outputDir)
	spec := &vmctl.VMSpec{
		Hypervisor: vmctl.HypervisorQEMU,
		BaseImage: vmctl.ImageRef{
			Path:   disks.overlay.Path,
			Format: vmctl.DiskFormatQCOW2,
		},
		Disks: []vmctl.DiskSpec{
			{Path: disks.srcDisk, Format: vmctl.DiskFormatRaw, ReadOnly: true, Interface: "virtio"},
			{Path: disks.scratchDisk, Format: vmctl.DiskFormatQCOW2, Interface: "virtio"},
		},
		Shares: []vmctl.ShareSpec{{
			HostPath:      outputShare.LocalPath,
			MountTag:      outputShare.MountTag,
			SecurityModel: string(outputShare.SecurityModel),
			ID:            outputShare.ID,
		}},
		// User-mode networking: builder needs access to cache.nixos.org.
		Network: vmctl.NetworkSpec{RawArgs: "-nic user,model=virtio"},
		Resources: vmctl.Resources{
			Fraction: 75, // ProfileBuild
			MinCores: 2,
			MinMemMB: 2048,
		},
		Timeout:    2 * time.Hour,
		NoReboot:   true,
		StatusFile: statusFile,
	}

	res := vmctl.ResolveResources(spec.Resources)
	log.Info(tag, fmt.Sprintf("VM: %d cores, %dMB RAM", res.Cores, res.MemoryMB))
	log.Info(tag, "Building dev VM image inside builder VM (vmctl)...")
	fmt.Fprintln(os.Stderr)

	// ── Boot ──────────────────────────────────────────────────────
	ctx, cancel := context.WithTimeout(context.Background(), spec.Timeout)
	defer cancel()

	result, bootErr := (&vmctl.QEMUHypervisor{}).Boot(ctx, spec, tmpDir)
	if bootErr != nil {
		log.Fail(tag, fmt.Sprintf("QEMU error: %v", bootErr))
		return 1
	}
	if result.ExitCode != 0 {
		log.Fail(tag, fmt.Sprintf("QEMU exited with %d", result.ExitCode))
		return 1
	}

	// ── Check result ──────────────────────────────────────────────
	if err := checkBuilderResult(statusFile); err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	// ── Decompress result ─────────────────────────────────────────
	if err := decompressBuilderImage(outputDir, vmCacheDir); err != nil {
		log.Fail(tag, err.Error())
		return 1
	}
	return 0
}


// ensureBuilderImage downloads the builder image or prompts to build locally.
func ensureBuilderImage(repoRoot, builderDir, builderImg string) int {
	log.Info(tag, "Builder VM image not found. Downloading from GitHub release...")

	if err := os.MkdirAll(builderDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create builder dir: %v", err))
		return 1
	}

	// Download SHA-256 sidecar
	hashURL := builderBaseURL + "/umbravox-builder.img.gz.sha256"
	hashFile := filepath.Join(builderDir, "builder.sha256")
	if err := download.FetchFile(hashURL, hashFile, ""); err != nil {
		log.Warn(tag, fmt.Sprintf("Download failed: %v", err))
		return fallbackLocalBuild(repoRoot, builderDir)
	}

	hashData, err := os.ReadFile(hashFile)
	if err != nil {
		log.Warn(tag, "Failed to read hash file")
		return fallbackLocalBuild(repoRoot, builderDir)
	}
	expectedHash := strings.Fields(strings.TrimSpace(string(hashData)))[0]

	// Download compressed image
	imgURL := builderBaseURL + "/umbravox-builder.img.gz"
	gzFile := filepath.Join(builderDir, "builder.img.gz")
	log.Info(tag, fmt.Sprintf("Downloading builder image from %s ...", builderVersion))
	if err := download.FetchFile(imgURL, gzFile, expectedHash); err != nil {
		log.Warn(tag, fmt.Sprintf("Download or verification failed: %v", err))
		os.Remove(gzFile)
		return fallbackLocalBuild(repoRoot, builderDir)
	}

	// Decompress
	log.Info(tag, "Decompressing builder image...")
	if err := decompressGzip(gzFile, builderImg); err != nil {
		log.Fail(tag, fmt.Sprintf("Decompression failed: %v", err))
		return 1
	}
	os.Remove(gzFile)
	os.Remove(hashFile)

	log.OK(tag, "Builder image ready.")
	return 0
}

func fallbackLocalBuild(repoRoot, builderDir string) int {
	log.Info(tag, "Falling back to local nix-build (writes ~3GB to host /nix/store)...")
	outPath := filepath.Join(builderDir, "result")
	os.RemoveAll(outPath)
	cmd := exec.Command("nix-build",
		filepath.Join(repoRoot, "nix", "vm-builder.nix"),
		"-o", outPath)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		log.Fail(tag, fmt.Sprintf("Local builder build failed: %v", err))
		return 1
	}
	// Symlink result/nixos.img to expected path
	srcImg := filepath.Join(outPath, "nixos.img")
	dstImg := filepath.Join(builderDir, "nixos.img")
	if _, err := os.Stat(srcImg); err == nil {
		if err := os.Remove(dstImg); err != nil && !os.IsNotExist(err) {
			log.Fail(tag, fmt.Sprintf("Failed to remove existing symlink: %v", err))
			return 1
		}
		if err := os.Symlink(srcImg, dstImg); err != nil {
			log.Fail(tag, fmt.Sprintf("Failed to create symlink: %v", err))
			return 1
		}
	}
	log.OK(tag, "Builder image built locally.")
	return 0
}

// createBuilderSourceDisk creates a source disk including .git for flake eval.
func createBuilderSourceDisk(repoRoot, tmpDir string) (string, error) {
	diskFile, err := os.CreateTemp(tmpDir, fmt.Sprintf("uv-builder-src.%d.*.ext2", os.Getpid()))
	if err != nil {
		return "", fmt.Errorf("create temp file: %w", err)
	}
	diskPath := diskFile.Name()
	diskFile.Close()
	os.Remove(diskPath)

	srcDir, err := os.MkdirTemp(tmpDir, "uv-builder-src.")
	if err != nil {
		return "", fmt.Errorf("create temp dir: %w", err)
	}
	defer os.RemoveAll(srcDir)

	// Export worktree INCLUDING .git (needed for flake eval)
	tarCreate := exec.Command("tar",
		"-C", repoRoot,
		"--exclude=dist-newstyle",
		"--exclude=build",
		"--exclude=result",
		"-cf", "-", ".")
	tarExtract := exec.Command("tar", "-xf", "-", "-C", srcDir, "--no-same-owner", "--no-same-permissions")

	pipe, err := tarCreate.StdoutPipe()
	if err != nil {
		return "", fmt.Errorf("tar pipe: %w", err)
	}
	tarExtract.Stdin = pipe
	if err := tarCreate.Start(); err != nil {
		return "", fmt.Errorf("tar create: %w", err)
	}
	if err := tarExtract.Start(); err != nil {
		return "", fmt.Errorf("tar extract: %w", err)
	}
	if err := tarCreate.Wait(); err != nil {
		return "", fmt.Errorf("tar create: %w", err)
	}
	if err := tarExtract.Wait(); err != nil {
		return "", fmt.Errorf("tar extract: %w", err)
	}

	// Build ext2 image — larger block count for .git
	genCmd := exec.Command("genext2fs", "-b", "2097152", "-d", srcDir, diskPath)
	if out, err := genCmd.CombinedOutput(); err != nil {
		return "", fmt.Errorf("genext2fs: %w\n%s", err, out)
	}

	return diskPath, nil
}

func decompressGzip(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()

	gz, err := gzip.NewReader(in)
	if err != nil {
		return err
	}
	defer gz.Close()

	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()

	if _, err := io.Copy(out, gz); err != nil {
		os.Remove(dst)
		return err
	}
	return nil
}

func vmCleanImage() int {
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	vmDir := filepath.Join(repoRoot, "build", "vm")
	freed := uint64(0)

	for _, name := range []string{"image", "builder-image", "nix-cache.qcow2", "tmp"} {
		p := filepath.Join(vmDir, name)
		freed += pathSize(p)
		os.RemoveAll(p)
	}

	// Also clean output dir
	outDir := filepath.Join(repoRoot, "build", "vm-output")
	freed += pathSize(outDir)
	os.RemoveAll(outDir)

	log.OK(tag, fmt.Sprintf("VM images + cache removed (freed ~%dGB)", freed/(1024*1024*1024)))
	log.Info(tag, "Run 'nix-collect-garbage -d' to also free /nix/store space")
	return 0
}

// availableDiskSpace returns free bytes on the filesystem containing path.
func availableDiskSpace(path string) uint64 {
	var stat syscall.Statfs_t
	if err := syscall.Statfs(path, &stat); err != nil {
		return 0
	}
	return stat.Bavail * uint64(stat.Bsize)
}

// pathSize returns the on-disk size of a file or directory in bytes.
func pathSize(path string) uint64 {
	fi, err := os.Stat(path)
	if err != nil {
		return 0
	}
	if !fi.IsDir() {
		return uint64(fi.Size())
	}
	var total uint64
	filepath.Walk(path, func(_ string, info os.FileInfo, _ error) error {
		if info != nil {
			total += uint64(info.Size())
		}
		return nil
	})
	return total
}

// formatSize returns a human-readable size string.
func formatSize(bytes uint64) string {
	const gb = 1024 * 1024 * 1024
	const mb = 1024 * 1024
	if bytes >= gb {
		return fmt.Sprintf("%.1fGB", float64(bytes)/float64(gb))
	}
	return fmt.Sprintf("%.0fMB", float64(bytes)/float64(mb))
}

func vmSmoke(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ./uv vm smoke <target>")
		fmt.Fprintln(os.Stderr, "Targets: freebsd, openbsd, netbsd, illumos, dragonfly, arm64, release")
		return 2
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	target := args[0]
	scriptMap := map[string]string{
		"freebsd":   "scripts/vm-freebsd-setup.sh",
		"openbsd":   "scripts/vm-openbsd-setup.sh",
		"netbsd":    "scripts/vm-netbsd-setup.sh",
		"illumos":   "scripts/vm-illumos-setup.sh",
		"dragonfly": "scripts/vm-dragonfly-setup.sh",
	}

	if script, ok := scriptMap[target]; ok {
		log.Info(tag, fmt.Sprintf("Running %s platform smoke test...", target))
		cmd := exec.Command("bash", filepath.Join(repoRoot, script))
		cmd.Dir = repoRoot
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		cmd.Stdin = os.Stdin
		if err := cmd.Run(); err != nil {
			log.Fail(tag, fmt.Sprintf("%s smoke test failed: %v", target, err))
			return 1
		}
		log.OK(tag, fmt.Sprintf("%s smoke test passed.", target))
		return 0
	}

	if target == "arm64" {
		log.Info(tag, "Running arm64 platform smoke test...")
		cmd := exec.Command("bash", filepath.Join(repoRoot, "scripts/vm-arm64-setup.sh"))
		cmd.Dir = repoRoot
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			log.Fail(tag, fmt.Sprintf("arm64 smoke test failed: %v", err))
			return 1
		}
		return 0
	}

	if target == "release" {
		return vmSmokeRelease(args[1:])
	}

	fmt.Fprintf(os.Stderr, "Unknown smoke target: %s\n", target)
	return 2
}

func vmSignal(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ./uv vm signal build-jar|update|test|run|health")
		return 2
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	signalBin := filepath.Join(repoRoot, "build", "tools", "vm-signal")
	if _, err := os.Stat(signalBin); os.IsNotExist(err) {
		log.Info(tag, "Building vm-signal tool...")
		buildCmd := exec.Command("go", "build", "-trimpath", "-ldflags=-s -w", "-o", signalBin, "./cmd/vm-signal/")
		buildCmd.Dir = filepath.Join(repoRoot, "tools")
		buildCmd.Env = append(os.Environ(),
			"GOMODCACHE="+filepath.Join(repoRoot, "build", "go", "mod"),
			"GOCACHE="+filepath.Join(repoRoot, "build", "go", "cache"))
		if out, err := buildCmd.CombinedOutput(); err != nil {
			log.Fail(tag, fmt.Sprintf("Failed to build vm-signal: %v\n%s", err, out))
			return 1
		}
	}

	signalCmd := ""
	switch args[0] {
	case "build-jar":
		signalCmd = "build-jar"
	case "extract-hash":
		signalCmd = "extract-hash"
	case "run":
		signalCmd = "interactive"
	case "health":
		signalCmd = "check-health"
	case "test":
		testBin := filepath.Join(repoRoot, "build", "tools", "signal-test")
		if _, err := os.Stat(testBin); os.IsNotExist(err) {
			log.Info(tag, "Building signal-test tool...")
			buildCmd := exec.Command("go", "build", "-trimpath", "-ldflags=-s -w", "-o", testBin, "./cmd/signal-test/")
			buildCmd.Dir = filepath.Join(repoRoot, "tools")
			buildCmd.Env = append(os.Environ(),
				"GOMODCACHE="+filepath.Join(repoRoot, "build", "go", "mod"),
				"GOCACHE="+filepath.Join(repoRoot, "build", "go", "cache"))
			if out, err := buildCmd.CombinedOutput(); err != nil {
				log.Fail(tag, fmt.Sprintf("Failed to build signal-test: %v\n%s", err, out))
				return 1
			}
		}
		cmd := exec.Command(testBin, args[1:]...)
		cmd.Dir = repoRoot
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		cmd.Stdin = os.Stdin
		if err := cmd.Run(); err != nil {
			if exitErr, ok := err.(*exec.ExitError); ok {
				return exitErr.ExitCode()
			}
			log.Fail(tag, fmt.Sprintf("signal-test failed: %v", err))
			return 1
		}
		return 0
	case "update":
		return vmSignalUpdate(repoRoot)
	default:
		fmt.Fprintf(os.Stderr, "Unknown signal action: %s\n", args[0])
		return 2
	}

	cmd := exec.Command(signalBin, signalCmd)
	cmd.Dir = repoRoot
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin
	if err := cmd.Run(); err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return exitErr.ExitCode()
		}
		log.Fail(tag, fmt.Sprintf("vm-signal %s failed: %v", signalCmd, err))
		return 1
	}
	return 0
}

func vmIntegration(args []string) int {
	agents := "3"
	dualLan := false
	for _, a := range args {
		if a == "--dual-lan" {
			dualLan = true
			agents = "6"
		}
	}

	cmd := fmt.Sprintf("cabal run umbravox -- vm-integration-test --agents=%s", agents)
	if dualLan {
		cmd += " --dual-lan"
	}
	return execInVM(cmd, qemu.ProfileBuild, 60*time.Minute)
}

func vmInfo() int {
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	vmDir := filepath.Join(repoRoot, "build", "vm")
	vmImagePath := filepath.Join(vmDir, "image")
	builderPath := filepath.Join(vmDir, "builder-image")
	scratchPath := filepath.Join(vmDir, "nix-cache.qcow2")

	hasImage := false
	if fi, err := os.Stat(vmImagePath); err == nil && fi.IsDir() {
		hasImage = true
	}
	hasBuilder := false
	if _, err := os.Stat(filepath.Join(builderPath, "nixos.img")); err == nil {
		hasBuilder = true
	}

	cores, mem := qemu.ScaleToHost(qemu.ProfileDev)
	freeBytes := availableDiskSpace(repoRoot)

	fmt.Printf("Repository:    %s\n", repoRoot)
	fmt.Printf("Dev VM image:  %v", hasImage)
	if hasImage {
		fmt.Printf(" (%s)", formatSize(pathSize(vmImagePath)))
	}
	fmt.Println()
	fmt.Printf("Builder image: %v", hasBuilder)
	if hasBuilder {
		fmt.Printf(" (%s)", formatSize(pathSize(builderPath)))
	}
	fmt.Println()
	fmt.Printf("Scratch disk:  ")
	if fi, err := os.Stat(scratchPath); err == nil {
		fmt.Printf("%s\n", formatSize(uint64(fi.Size())))
	} else {
		fmt.Println("none")
	}
	fmt.Printf("Disk free:     %s\n", formatSize(freeBytes))
	fmt.Printf("VM profile:    dev (%d cores, %dMB RAM)\n", cores, mem)
	fmt.Printf("KVM:           ")
	if _, err := os.Stat("/dev/kvm"); err == nil {
		fmt.Println("available")
	} else {
		fmt.Println("NOT available")
	}
	return 0
}

// signalServerConfig represents nix/signal-server.json.
type signalServerConfig struct {
	Version         string `json:"version"`
	Tag             string `json:"tag"`
	Owner           string `json:"owner"`
	Repo            string `json:"repo"`
	SHA256          string `json:"sha256"`
	JDK             string `json:"jdk"`
	MavenVersion    string `json:"mavenVersion"`
	OutputTimestamp string `json:"outputTimestamp"`
}

// githubTag represents a tag from the GitHub API.
type githubTag struct {
	Name string `json:"name"`
}

// vmSignalUpdate prompts with recent Signal-Server tags, lets the user
// pick one, computes the source hash, and updates signal-server.json.
func vmSignalUpdate(repoRoot string) int {
	configPath := filepath.Join(repoRoot, "nix", "signal-server.json")

	data, err := os.ReadFile(configPath)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot read %s: %v", configPath, err))
		return 1
	}
	var cfg signalServerConfig
	if err := json.Unmarshal(data, &cfg); err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot parse %s: %v", configPath, err))
		return 1
	}

	// Fetch recent tags from GitHub API
	log.Info(tag, fmt.Sprintf("Fetching recent tags from %s/%s...", cfg.Owner, cfg.Repo))
	apiURL := fmt.Sprintf("https://api.github.com/repos/%s/%s/tags?per_page=10", cfg.Owner, cfg.Repo)
	client := &http.Client{Timeout: 15 * time.Second}
	resp, err := client.Get(apiURL)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("GitHub API error: %v", err))
		return 1
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		log.Fail(tag, fmt.Sprintf("GitHub API returned %d", resp.StatusCode))
		return 1
	}

	var tags []githubTag
	if err := json.NewDecoder(resp.Body).Decode(&tags); err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot parse tags: %v", err))
		return 1
	}

	if len(tags) == 0 {
		log.Fail(tag, "No tags found")
		return 1
	}

	// Display tags
	fmt.Printf("\nCurrent: %s\n\n", cfg.Tag)
	fmt.Println("Available Signal-Server versions:")
	limit := 10
	if len(tags) < limit {
		limit = len(tags)
	}
	for i := 0; i < limit; i++ {
		marker := "  "
		if tags[i].Name == cfg.Tag {
			marker = "* "
		}
		fmt.Printf("  %s[%d] %s\n", marker, i+1, tags[i].Name)
	}

	// Prompt
	fmt.Printf("\nSelect version (1-%d) or Enter to keep current: ", limit)
	reader := bufio.NewReader(os.Stdin)
	input, _ := reader.ReadString('\n')
	input = strings.TrimSpace(input)

	if input == "" {
		fmt.Println("Keeping current version.")
		return 0
	}

	var sel int
	if _, err := fmt.Sscanf(input, "%d", &sel); err != nil {
		log.Fail(tag, "Invalid selection")
		return 1
	}
	if sel < 1 || sel > limit {
		log.Fail(tag, fmt.Sprintf("Selection out of range (1-%d)", limit))
		return 1
	}

	selected := tags[sel-1]
	if selected.Name == cfg.Tag {
		fmt.Println("Already on this version.")
		return 0
	}

	// Compute source hash
	log.Info(tag, fmt.Sprintf("Computing source hash for %s...", selected.Name))
	tarURL := fmt.Sprintf("https://github.com/%s/%s/archive/refs/tags/%s.tar.gz",
		cfg.Owner, cfg.Repo, selected.Name)
	hashCmd := exec.Command("nix-prefetch-url", "--unpack", tarURL)
	hashOut, err := hashCmd.Output()
	if err != nil {
		log.Fail(tag, fmt.Sprintf("nix-prefetch-url failed: %v", err))
		return 1
	}
	newHash := strings.TrimSpace(string(hashOut))

	// Extract version from tag (strip leading "v")
	newVersion := strings.TrimPrefix(selected.Name, "v")

	// Update config
	cfg.Tag = selected.Name
	cfg.Version = newVersion
	cfg.SHA256 = newHash
	if len(newVersion) >= 8 {
		datePart := newVersion[:8]
		cfg.OutputTimestamp = fmt.Sprintf("%s-%s-%sT00:00:00Z",
			datePart[:4], datePart[4:6], datePart[6:8])
	}

	outData, err := json.MarshalIndent(cfg, "", "  ")
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot marshal config: %v", err))
		return 1
	}
	outData = append(outData, '\n')
	if err := os.WriteFile(configPath, outData, 0o644); err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot write %s: %v", configPath, err))
		return 1
	}

	log.OK(tag, fmt.Sprintf("Updated signal-server.json to %s", selected.Name))
	fmt.Printf("  Tag:       %s\n", cfg.Tag)
	fmt.Printf("  Version:   %s\n", cfg.Version)
	fmt.Printf("  SHA256:    %s\n", cfg.SHA256)
	fmt.Printf("  Timestamp: %s\n", cfg.OutputTimestamp)
	fmt.Printf("\nRun './uv vm signal build-jar' to build with the new version.\n")
	return 0
}

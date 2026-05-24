// SPDX-License-Identifier: Apache-2.0
package main

import (
	"compress/gzip"
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/disk"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/download"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/netproxy"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/ninep"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
)

const (
	builderVersion = "builder-2026.05.24"
	builderBaseURL = "https://github.com/UmbraVoxResearch/UmbraVOX/releases/download/" + builderVersion
	scratchSize    = "100G"
)

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
  clean-image                Remove cached VM image
  smoke [TARGET]             Platform smoke (freebsd, openbsd, netbsd, illumos, dragonfly, arm64)
  signal build-jar|run|health Signal Server VM
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
		log.Info(tag, "Building VM image on host (writes to /nix/store)...")
		cmd := exec.Command("nix-build", filepath.Join(repoRoot, "nix", "vm-image.nix"),
			"-o", filepath.Join(repoRoot, "build", "vm", "image"))
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			log.Fail(tag, fmt.Sprintf("nix-build failed: %v", err))
			return 1
		}
		log.OK(tag, "VM image built successfully.")
		return 0
	}

	// ── Ensure builder image ──────────────────────────────────────
	vmCacheDir := filepath.Join(repoRoot, "build", "vm")
	if err := os.MkdirAll(vmCacheDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create cache dir: %v", err))
		return 1
	}

	builderDir := filepath.Join(vmCacheDir, "builder-image")
	builderImg := filepath.Join(builderDir, "nixos.img")

	if _, err := os.Stat(builderImg); os.IsNotExist(err) {
		if code := ensureBuilderImage(repoRoot, builderDir, builderImg); code != 0 {
			return code
		}
	}

	// ── Preflight ─────────────────────────────────────────────────
	if err := repo.Preflight(builderDir, false); err != nil {
		return 1
	}

	diskImg, err := filepath.EvalSymlinks(builderImg)
	if err != nil {
		// If not a symlink, use directly
		diskImg = builderImg
	}

	// ── Create source disk (includes .git) ────────────────────────
	log.Info(tag, "Creating source disk (with .git for flake eval)...")
	tmpDir := filepath.Join(vmCacheDir, "tmp")
	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create temp dir: %v", err))
		return 1
	}

	srcDisk, err := createBuilderSourceDisk(repoRoot, tmpDir)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create source disk: %v", err))
		return 1
	}
	defer os.Remove(srcDisk)

	// ── Scratch disk (100GB, thin-provisioned) ────────────────────
	scratchDisk := filepath.Join(vmCacheDir, "nix-cache.qcow2")
	if err := disk.EnsureCacheDisk(scratchDisk, scratchSize); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create scratch disk: %v", err))
		return 1
	}

	// ── COW overlay ───────────────────────────────────────────────
	log.Info(tag, "Creating COW overlay...")
	overlay, err := disk.CreateOverlay(diskImg, tmpDir)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create overlay: %v", err))
		return 1
	}
	defer overlay.Remove()

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
	policyFile := filepath.Join(repoRoot, "vm-builder-network-policy.conf")
	allowed, err := netproxy.ParseAllowlist(policyFile)
	if err != nil {
		log.Warn(tag, fmt.Sprintf("No network policy file, using unrestricted: %v", err))
		allowed = nil
	}

	socketPath := filepath.Join(tmpDir, fmt.Sprintf("uv-proxy.%d.sock", os.Getpid()))
	filter := &netproxy.Filter{
		SocketPath: socketPath,
		Allowed:    allowed,
	}

	useFilter := len(allowed) > 0
	if useFilter {
		if err := filter.Start(); err != nil {
			log.Fail(tag, fmt.Sprintf("Failed to start network filter: %v", err))
			return 1
		}
		defer filter.Stop()
		log.Info(tag, fmt.Sprintf("Network filter: allowing %d destination(s)", len(allowed)))
	}

	// ── QEMU config ───────────────────────────────────────────────
	outputShare := ninep.DefaultOutputShare(outputDir)
	cfg := qemu.ProfileConfig(qemu.ProfileBuild)
	cfg.Drives = []qemu.Drive{
		{Interface: "virtio", Format: qemu.FormatQCOW2, File: overlay.Path},
		{Interface: "virtio", Format: qemu.FormatRaw, File: srcDisk, ReadOnly: true},
		{Interface: "virtio", Format: qemu.FormatQCOW2, File: scratchDisk},
	}
	cfg.VirtFS = []qemu.VirtFS{{
		LocalPath:     outputShare.LocalPath,
		MountTag:      outputShare.MountTag,
		SecurityModel: string(outputShare.SecurityModel),
		ID:            outputShare.ID,
	}}

	if useFilter {
		cfg.NetArgs = fmt.Sprintf("-nic user,model=virtio,restrict=on,guestfwd=tcp:10.0.2.100:443-unix:%s", socketPath)
	} else {
		cfg.NetArgs = "-nic user,model=virtio"
	}

	cfg.NoReboot = true
	cfg.Timeout = 2 * time.Hour

	log.Info(tag, fmt.Sprintf("VM: %d cores, %dMB RAM", cfg.SMP, cfg.MemoryMB))
	log.Info(tag, "Building dev VM image inside builder VM...")
	fmt.Fprintln(os.Stderr)

	// ── Boot ──────────────────────────────────────────────────────
	ctx, cancel := context.WithTimeout(context.Background(), cfg.Timeout)
	defer cancel()
	qemuCmd := exec.CommandContext(ctx, "qemu-system-x86_64", cfg.Args()...)
	qemuCmd.Stdin = os.Stdin
	qemuCmd.Stdout = os.Stdout
	qemuCmd.Stderr = os.Stderr

	qemuErr := qemuCmd.Run()
	if qemuErr != nil {
		if exitErr, ok := qemuErr.(*exec.ExitError); ok {
			log.Fail(tag, fmt.Sprintf("QEMU exited with %d", exitErr.ExitCode()))
		} else {
			log.Fail(tag, fmt.Sprintf("QEMU error: %v", qemuErr))
		}
	}

	// ── Check result ──────────────────────────────────────────────
	statusData, err := os.ReadFile(statusFile)
	if err != nil {
		log.Fail(tag, "Builder VM did not produce a status file.")
		return 1
	}
	status := strings.TrimSpace(string(statusData))
	if status != "0" {
		log.Fail(tag, fmt.Sprintf("Builder VM failed (status %s)", status))
		return 1
	}

	// ── Decompress result ─────────────────────────────────────────
	compressedImg := filepath.Join(outputDir, "nixos.img.zst")
	finalDir := filepath.Join(vmCacheDir, "image")
	if err := os.MkdirAll(finalDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create image dir: %v", err))
		return 1
	}
	finalImg := filepath.Join(finalDir, "nixos.img")

	log.Info(tag, "Decompressing VM image...")
	zstdCmd := exec.Command("zstd", "-d", compressedImg, "-o", finalImg, "--force")
	if out, err := zstdCmd.CombinedOutput(); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to decompress: %v\n%s", err, out))
		return 1
	}
	os.Remove(compressedImg)

	log.OK(tag, fmt.Sprintf("VM image ready at %s", finalDir))
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
		os.Remove(dstImg)
		os.Symlink(srcImg, dstImg)
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
	tarExtract := exec.Command("tar", "-xf", "-", "-C", srcDir)

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
	for _, dir := range []string{"image", "builder-image"} {
		p := filepath.Join(repoRoot, "build", "vm", dir)
		os.RemoveAll(p)
	}
	log.OK(tag, "VM images removed.")
	return 0
}

func vmSmoke(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ./uv vm smoke <target>")
		fmt.Fprintln(os.Stderr, "Targets: freebsd, openbsd, netbsd, illumos, dragonfly, arm64")
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
		cmd := exec.Command("bash", "-c",
			fmt.Sprintf("cd %s && bash scripts/vm-arm64-setup.sh", repoRoot))
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			log.Fail(tag, fmt.Sprintf("arm64 smoke test failed: %v", err))
			return 1
		}
		return 0
	}

	fmt.Fprintf(os.Stderr, "Unknown smoke target: %s\n", target)
	return 2
}

func vmSignal(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ./uv vm signal build-jar|run|health")
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
		buildCmd := exec.Command("go", "build", "-o", signalBin, "./cmd/vm-signal/")
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
	case "run":
		signalCmd = "interactive"
	case "health":
		signalCmd = "check-health"
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

	vmImagePath := filepath.Join(repoRoot, "build", "vm", "image")
	builderPath := filepath.Join(repoRoot, "build", "vm", "builder-image")

	hasImage := false
	if fi, err := os.Stat(vmImagePath); err == nil && fi.IsDir() {
		hasImage = true
	}
	hasBuilder := false
	if _, err := os.Stat(filepath.Join(builderPath, "nixos.img")); err == nil {
		hasBuilder = true
	}

	cores, mem := qemu.ScaleToHost(qemu.ProfileDev)
	fmt.Printf("Repository:    %s\n", repoRoot)
	fmt.Printf("Dev VM image:  %v\n", hasImage)
	fmt.Printf("Builder image: %v\n", hasBuilder)
	fmt.Printf("VM profile:    dev (%d cores, %dMB RAM)\n", cores, mem)
	fmt.Printf("KVM:           ")
	if _, err := os.Stat("/dev/kvm"); err == nil {
		fmt.Println("available")
	} else {
		fmt.Println("NOT available")
	}
	return 0
}

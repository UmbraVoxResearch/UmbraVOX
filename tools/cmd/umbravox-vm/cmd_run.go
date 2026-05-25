// SPDX-License-Identifier: Apache-2.0
package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/disk"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/firecracker"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/netpol"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/ninep"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/vmctl"
)

// runRun handles: uv run [gui|tui|headless] [--port PORT]
//
//	uv run          → tui (default: Firecracker microVM, serial console)
//	uv run tui      → Firecracker microVM, serial console in terminal
//	uv run headless → Firecracker microVM, headless daemon mode
//	uv run gui      → lightweight QEMU runtime with VGA display
func runRun(args []string) int {
	mode := "tui" // DEFAULT IS NOW TUI
	port := ""

	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "gui":
			mode = "gui"
		case "tui":
			mode = "tui"
		case "headless":
			mode = "headless"
		case "--port":
			if i+1 < len(args) {
				port = args[i+1]
				i++
			}
		}
	}

	if mode == "gui" {
		return runRunQEMU(port)
	}
	// tui and headless use Firecracker
	return runRunFirecracker(mode, port)
}

// canDisplayGUI checks whether a GUI window can be opened.
func canDisplayGUI() bool {
	// X11/Wayland: DISPLAY or WAYLAND_DISPLAY must be set
	if os.Getenv("DISPLAY") != "" || os.Getenv("WAYLAND_DISPLAY") != "" {
		return true
	}
	// SSH sessions, containers, headless servers → no GUI
	return false
}

// runRunFirecracker boots the runtime bundle in a Firecracker microVM.
// mode is "tui" (serial console) or "headless" (daemon).
func runRunFirecracker(mode, port string) int {
	if err := vmctl.PreflightFirecracker(); err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	// 1. Check runtime bundle
	runtimeBin := filepath.Join(repoRoot, "build", "runtime", "bin", "umbravox")
	if _, err := os.Stat(runtimeBin); err != nil {
		log.Fail(tag, "No runtime bundle. Run ./uv build first.")
		return 1
	}

	// 2. Check / auto-build Firecracker runtime image (bundle: kernel + initrd + rootfs)
	runtimeImageDir := filepath.Join(repoRoot, "build", "vm", "runtime-image")
	if _, err := os.Stat(runtimeImageDir); err != nil {
		log.Info(tag, "Firecracker runtime image not found; building from nix/vm-runtime.nix...")
		nixBuild := exec.Command("nix-build",
			filepath.Join(repoRoot, "nix", "vm-runtime.nix"),
			"-A", "firecracker",
			"-o", runtimeImageDir,
		)
		nixBuild.Stdout = os.Stderr
		nixBuild.Stderr = os.Stderr
		if err := nixBuild.Run(); err != nil {
			log.Fail(tag, fmt.Sprintf("Failed to build Firecracker runtime image: %v", err))
			return 1
		}
	}

	// 3. Create app disk from build/runtime/
	bundleDir := filepath.Join(repoRoot, "build", "runtime")
	tmpDir := filepath.Join(repoRoot, "build", "vm", "tmp")
	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create tmp dir: %v", err))
		return 1
	}
	appDiskPath := filepath.Join(tmpDir, "app-disk.ext2")
	if err := firecracker.CreateAppDisk(bundleDir, appDiskPath); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create app disk: %v", err))
		return 1
	}
	defer os.Remove(appDiskPath)

	// 4. Decompress rootfs to writable location (bundle stores zstd-compressed;
	// Firecracker needs uncompressed + writable, unlike QEMU which uses qcow2 overlays)
	rootfsSrc := filepath.Join(runtimeImageDir, "rootfs.ext4.zst")
	rootfsPath := filepath.Join(tmpDir, "runtime-rootfs.ext4")
	log.Info(tag, "Decompressing runtime rootfs...")
	zstdCmd := exec.Command("zstd", "-d", "-f", rootfsSrc, "-o", rootfsPath)
	if out, err := zstdCmd.CombinedOutput(); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to decompress rootfs: %v\n%s", err, out))
		return 1
	}
	defer os.Remove(rootfsPath)

	// 5. Build Firecracker config with scaled resources
	kernelPath := filepath.Join(runtimeImageDir, "vmlinux")
	initrdPath := filepath.Join(runtimeImageDir, "initrd")

	cores, memMB := firecracker.ScaleToHost()

	// NixOS needs init= pointing to the system closure (normally set by GRUB).
	// The bundle includes an init-path file with the correct nix store path.
	initPathFile := filepath.Join(runtimeImageDir, "init-path")
	initPathBytes, err := os.ReadFile(initPathFile)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot read init-path from runtime image: %v", err))
		return 1
	}
	kernelArgs := fmt.Sprintf("init=%s ro", string(initPathBytes))
	if mode == "tui" {
		kernelArgs = "console=ttyS0 " + kernelArgs
	}
	if port != "" {
		kernelArgs += " umbravox.port=" + port
	}

	cfg := firecracker.Config{
		KernelPath:    kernelPath,
		InitrdPath:    initrdPath,
		RootfsPath:    rootfsPath,
		AppDiskPath:   appDiskPath,
		VcpuCount:     cores,
		MemSizeMB:     memMB,
		KernelArgs:    kernelArgs,
		SerialConsole: mode == "tui",
	}

	log.Info(tag, fmt.Sprintf("Firecracker VM: %d cores, %dMB RAM | mode: %s", cores, memMB, mode))
	fmt.Fprintln(os.Stderr)

	// 5. Boot and return exit code
	exitCode, err := firecracker.Boot(cfg)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Firecracker error: %v", err))
		return 1
	}
	return exitCode
}

// runRunFirecrackerV2 boots the runtime bundle in a Firecracker microVM using
// the vmctl package (FirecrackerHypervisor) instead of calling firecracker
// directly. It is a parallel path to runRunFirecracker and does not replace it.
func runRunFirecrackerV2(mode, port string) int {
	if err := vmctl.PreflightFirecracker(); err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	// 1. Check runtime bundle exists.
	runtimeBin := filepath.Join(repoRoot, "build", "runtime", "bin", "umbravox")
	if _, err := os.Stat(runtimeBin); err != nil {
		log.Fail(tag, "No runtime bundle. Run ./uv build first.")
		return 1
	}

	// 2. Check / auto-build Firecracker runtime image.
	runtimeImageDir := filepath.Join(repoRoot, "build", "vm", "runtime-image")
	if _, err := os.Stat(runtimeImageDir); err != nil {
		log.Info(tag, "Firecracker runtime image not found; building from nix/vm-runtime.nix...")
		nixBuild := exec.Command("nix-build",
			filepath.Join(repoRoot, "nix", "vm-runtime.nix"),
			"-A", "firecracker",
			"-o", runtimeImageDir,
		)
		nixBuild.Stdout = os.Stderr
		nixBuild.Stderr = os.Stderr
		if err := nixBuild.Run(); err != nil {
			log.Fail(tag, fmt.Sprintf("Failed to build Firecracker runtime image: %v", err))
			return 1
		}
	}

	// 3. Create app disk from build/runtime/ using vmctl.DiskManager.
	bundleDir := filepath.Join(repoRoot, "build", "runtime")
	tmpDir := filepath.Join(repoRoot, "build", "vm", "tmp")
	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create tmp dir: %v", err))
		return 1
	}
	appDiskPath := filepath.Join(tmpDir, "app-disk-v2.ext2")
	dm := vmctl.DiskManager{}
	if err := dm.CreateAppDisk(bundleDir, appDiskPath); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create app disk: %v", err))
		return 1
	}
	defer os.Remove(appDiskPath)

	// 4. Read init-path for kernel args.
	initPathFile := filepath.Join(runtimeImageDir, "init-path")
	initPathBytes, err := os.ReadFile(initPathFile)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot read init-path from runtime image: %v", err))
		return 1
	}
	kernelArgs := fmt.Sprintf("console=ttyS0 init=%s ro", string(initPathBytes))
	if mode != "tui" {
		// headless: drop console=ttyS0 so there is no serial I/O on stdio.
		kernelArgs = fmt.Sprintf("init=%s ro", string(initPathBytes))
	}
	if port != "" {
		kernelArgs += " umbravox.port=" + port
	}

	// 5. Construct VMSpec with 25% host resources.
	spec := &vmctl.VMSpec{
		Hypervisor: vmctl.HypervisorFirecracker,
		Resources: vmctl.Resources{
			Fraction: 25,
		},
		BaseImage: vmctl.ImageRef{
			Path:   filepath.Join(runtimeImageDir, "rootfs.ext4.zst"),
			Format: vmctl.DiskFormatExt4,
		},
		Disks: []vmctl.DiskSpec{
			{
				Path:     appDiskPath,
				Format:   vmctl.DiskFormatExt2,
				ReadOnly: true,
			},
		},
		Boot: &vmctl.BootSpec{
			KernelPath: filepath.Join(runtimeImageDir, "vmlinux"),
			InitrdPath: filepath.Join(runtimeImageDir, "initrd"),
			KernelArgs: kernelArgs,
		},
	}

	log.Info(tag, fmt.Sprintf("Firecracker V2 VM: 25%% host resources | mode: %s", mode))
	fmt.Fprintln(os.Stderr)

	// 6. Boot via FirecrackerHypervisor.
	hyp := &vmctl.FirecrackerHypervisor{Logger: nil}
	result, err := hyp.Boot(context.Background(), spec, tmpDir)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Firecracker error: %v", err))
		return 1
	}
	return result.ExitCode
}

// runRunQEMU boots the runtime bundle in a lightweight QEMU VM with VGA display.
func runRunQEMU(port string) int {
	if !canDisplayGUI() {
		log.Fail(tag, "No display available (DISPLAY/WAYLAND_DISPLAY not set). Use 'uv run tui' for terminal mode.")
		return 1
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	// 1. Check runtime bundle
	runtimeBin := filepath.Join(repoRoot, "build", "runtime", "bin", "umbravox")
	if _, err := os.Stat(runtimeBin); err != nil {
		log.Fail(tag, "No runtime bundle. Run ./uv build first.")
		return 1
	}

	// 2. Check / auto-build QEMU runtime image
	runtimeImageDir := filepath.Join(repoRoot, "build", "vm", "runtime-qemu-image")
	if _, err := os.Stat(runtimeImageDir); err != nil {
		log.Info(tag, "QEMU runtime image not found; building from nix/vm-runtime.nix...")
		nixBuild := exec.Command("nix-build",
			filepath.Join(repoRoot, "nix", "vm-runtime.nix"),
			"-A", "qemu",
			"-o", runtimeImageDir,
		)
		nixBuild.Stdout = os.Stderr
		nixBuild.Stderr = os.Stderr
		if err := nixBuild.Run(); err != nil {
			log.Fail(tag, fmt.Sprintf("Failed to build QEMU runtime image: %v", err))
			return 1
		}
	}

	// 3. Create app disk from build/runtime/
	bundleDir := filepath.Join(repoRoot, "build", "runtime")
	tmpDir := filepath.Join(repoRoot, "build", "vm", "tmp")
	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create tmp dir: %v", err))
		return 1
	}
	appDiskPath := filepath.Join(tmpDir, "app-disk-qemu.ext2")
	if err := firecracker.CreateAppDisk(bundleDir, appDiskPath); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create app disk: %v", err))
		return 1
	}
	defer os.Remove(appDiskPath)

	// 4. Resolve base image and create overlay
	diskImg, err := filepath.EvalSymlinks(filepath.Join(runtimeImageDir, "nixos.img"))
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot resolve runtime QEMU image: %v", err))
		return 1
	}

	overlay, err := disk.CreateOverlay(diskImg, tmpDir)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create overlay: %v", err))
		return 1
	}
	defer overlay.Remove()

	// 5. Build QEMU config with ProfileRuntime (25% resources)
	outputDir := filepath.Join(repoRoot, "build", "vm-output")
	os.MkdirAll(outputDir, 0o755)

	// Runtime VM: networking with policy restrictions
	policyFile := filepath.Join(repoRoot, "conf/vm-network-policy.conf")
	policy, err := netpol.ParseFile(policyFile)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to parse network policy: %v", err))
		return 1
	}
	netArgs, err := policy.QEMUNetArgs()
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Network policy error: %v", err))
		return 1
	}

	outputShare := ninep.DefaultOutputShare(outputDir)
	cfg := qemu.ProfileConfig(qemu.ProfileRuntime)
	cfg.NetArgs = netArgs
	cfg.Drives = []qemu.Drive{
		{Interface: "virtio", Format: qemu.FormatQCOW2, File: overlay.Path},
		{Interface: "virtio", Format: qemu.FormatRaw, File: appDiskPath, ReadOnly: true},
	}
	cfg.VirtFS = []qemu.VirtFS{{
		LocalPath:     outputShare.LocalPath,
		MountTag:      outputShare.MountTag,
		SecurityModel: string(outputShare.SecurityModel),
		ID:            outputShare.ID,
	}}
	cfg.Display = qemu.DisplayGTK

	if port != "" {
		// Forward host port to guest via QEMU user-mode networking if needed.
		if cfg.NetArgs == "-nic none" {
			cfg.NetArgs = fmt.Sprintf("-nic user,model=virtio,hostfwd=tcp::%s-:%s", port, port)
		}
	}

	log.Info(tag, fmt.Sprintf("VM: %d cores, %dMB RAM | GUI (QEMU VGA window, runtime image)", cfg.SMP, cfg.MemoryMB))
	fmt.Fprintln(os.Stderr)

	qemuCmd := exec.Command("qemu-system-x86_64", cfg.Args()...)
	qemuCmd.Stdin = os.Stdin
	qemuCmd.Stdout = os.Stdout
	qemuCmd.Stderr = os.Stderr

	if err := qemuCmd.Run(); err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return exitErr.ExitCode()
		}
		log.Fail(tag, fmt.Sprintf("QEMU error: %v", err))
		return 1
	}
	return 0
}

// runRunQEMUV2 boots the runtime bundle in a lightweight QEMU VM with VGA
// display using QEMUHypervisor.Boot from the vmctl package.
// It is a parallel path to runRunQEMU and does not replace it.
func runRunQEMUV2(port string) int {
	if !canDisplayGUI() {
		log.Fail(tag, "No display available (DISPLAY/WAYLAND_DISPLAY not set). Use 'uv run tui' for terminal mode.")
		return 1
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	// 1. Check runtime bundle
	runtimeBin := filepath.Join(repoRoot, "build", "runtime", "bin", "umbravox")
	if _, err := os.Stat(runtimeBin); err != nil {
		log.Fail(tag, "No runtime bundle. Run ./uv build first.")
		return 1
	}

	// 2. Check / auto-build QEMU runtime image
	runtimeImageDir := filepath.Join(repoRoot, "build", "vm", "runtime-qemu-image")
	if _, err := os.Stat(runtimeImageDir); err != nil {
		log.Info(tag, "QEMU runtime image not found; building from nix/vm-runtime.nix...")
		nixBuild := exec.Command("nix-build",
			filepath.Join(repoRoot, "nix", "vm-runtime.nix"),
			"-A", "qemu",
			"-o", runtimeImageDir,
		)
		nixBuild.Stdout = os.Stderr
		nixBuild.Stderr = os.Stderr
		if err := nixBuild.Run(); err != nil {
			log.Fail(tag, fmt.Sprintf("Failed to build QEMU runtime image: %v", err))
			return 1
		}
	}

	// 3. Create app disk from build/runtime/
	bundleDir := filepath.Join(repoRoot, "build", "runtime")
	tmpDir := filepath.Join(repoRoot, "build", "vm", "tmp")
	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create tmp dir: %v", err))
		return 1
	}
	appDiskPath := filepath.Join(tmpDir, "app-disk-qemu-v2.ext2")
	if err := firecracker.CreateAppDisk(bundleDir, appDiskPath); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create app disk: %v", err))
		return 1
	}
	defer os.Remove(appDiskPath)

	// 4. Resolve base image and create qcow2 overlay
	diskImg, err := filepath.EvalSymlinks(filepath.Join(runtimeImageDir, "nixos.img"))
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot resolve runtime QEMU image: %v", err))
		return 1
	}
	overlay, err := disk.CreateOverlay(diskImg, tmpDir)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create overlay: %v", err))
		return 1
	}
	defer overlay.Remove()

	// 5. Resolve network from policy file
	outputDir := filepath.Join(repoRoot, "build", "vm-output")
	os.MkdirAll(outputDir, 0o755) //nolint:errcheck

	policyFile := filepath.Join(repoRoot, "conf/vm-network-policy.conf")
	policy, err := netpol.ParseFile(policyFile)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to parse network policy: %v", err))
		return 1
	}
	netArgs, err := policy.QEMUNetArgs()
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Network policy error: %v", err))
		return 1
	}

	// Optionally forward a host port when policy provides no networking.
	if port != "" && netArgs == "-nic none" {
		netArgs = fmt.Sprintf("-nic user,model=virtio,hostfwd=tcp::%s-:%s", port, port)
	}

	// 6. Build output share for guest-written results
	outputShare := ninep.DefaultOutputShare(outputDir)

	// 7. Construct VMSpec: 25% host resources, GTK display, runtime image
	spec := &vmctl.VMSpec{
		Hypervisor: vmctl.HypervisorQEMU,
		Resources:  vmctl.Resources{Fraction: 25},
		BaseImage: vmctl.ImageRef{
			Path:   overlay.Path,
			Format: vmctl.DiskFormatQCOW2,
		},
		Disks: []vmctl.DiskSpec{
			{
				Path:      appDiskPath,
				Format:    vmctl.DiskFormatExt2,
				ReadOnly:  true,
				Interface: "virtio",
			},
		},
		Shares: []vmctl.ShareSpec{
			{
				HostPath:      outputShare.LocalPath,
				MountTag:      outputShare.MountTag,
				SecurityModel: string(outputShare.SecurityModel),
				ID:            outputShare.ID,
			},
		},
		Network: vmctl.NetworkSpec{RawArgs: netArgs},
		Display: vmctl.DisplayGTK,
	}

	res := vmctl.ResolveResources(spec.Resources)
	log.Info(tag, fmt.Sprintf("VM: %d cores, %dMB RAM | GUI (QEMU VGA window, runtime image, vmctl)", res.Cores, res.MemoryMB))
	fmt.Fprintln(os.Stderr)

	// 8. Boot via QEMUHypervisor
	result, err := (&vmctl.QEMUHypervisor{}).Boot(context.Background(), spec, tmpDir)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("QEMU error: %v", err))
		return 1
	}
	return result.ExitCode
}

// copyFile copies src to dst, creating dst with write permissions.
func copyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()
	if _, err := io.Copy(out, in); err != nil {
		return err
	}
	return out.Chmod(0o644)
}

// SPDX-License-Identifier: Apache-2.0
package main

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/disk"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/firecracker"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/netpol"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/ninep"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
	"github.com/UmbraVoxResearch/vmctl"
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

	// All modes use lightweight QEMU runtime image.
	// Firecracker deferred due to genext2fs ext2 permission issues.
	// GUI mode: QEMU with VGA display. TUI/headless: QEMU with serial console.
	return runRunQEMUAll(mode, port)
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

// runRunFirecracker boots the runtime bundle in a Firecracker microVM using
// the vmctl package (FirecrackerHypervisor).
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
				ReadOnly: false, // rw so runtime can chmod +x (genext2fs strips execute bits)
			},
		},
		Boot: &vmctl.BootSpec{
			KernelPath: filepath.Join(runtimeImageDir, "vmlinux"),
			InitrdPath: filepath.Join(runtimeImageDir, "initrd"),
			KernelArgs: kernelArgs,
		},
	}

	log.Info(tag, fmt.Sprintf("Firecracker VM: 25%% host resources | mode: %s", mode))
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

// runRunQEMU boots the runtime bundle in a lightweight QEMU VM with VGA
// display using QEMUHypervisor.Boot from the vmctl package.
// runRunQEMUAll runs the app in the lightweight QEMU runtime image.
// mode: "gui" (VGA display), "tui" (serial console), "headless" (serial, no input)
func runRunQEMUAll(mode, port string) int {
	useGUI := mode == "gui" && canDisplayGUI()
	if mode == "gui" && !canDisplayGUI() {
		log.Info(tag, "No display available, falling back to TUI (serial console)")
	}
	return runRunQEMUInner(useGUI, port)
}

func runRunQEMU(port string) int {
	return runRunQEMUInner(true, port)
}

func runRunQEMUInner(useGUI bool, port string) int {

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

	// 7. Construct VMSpec: load from vm-defs/runtime-qemu.yaml, fall back to 25%.
	resources := vmctl.Resources{Fraction: 25}
	if def, defErr := loadVMDef(repoRoot, "runtime-qemu"); defErr != nil {
		log.Fail(tag, fmt.Sprintf("failed to load vm-def: %v", defErr))
		return 1
	} else if def != nil {
		resources = def.Resources
	}
	spec := &vmctl.VMSpec{
		Hypervisor: vmctl.HypervisorQEMU,
		Resources:  resources,
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
	}
	if useGUI {
		spec.Display = vmctl.DisplayGTK
	}

	res := vmctl.ResolveResources(spec.Resources)
	displayLabel := "serial console"
	if useGUI {
		displayLabel = "GUI (QEMU VGA window)"
	}
	log.Info(tag, fmt.Sprintf("VM: %d cores, %dMB RAM | %s (runtime image, vmctl)", res.Cores, res.MemoryMB, displayLabel))
	fmt.Fprintln(os.Stderr)

	// 8. Boot via QEMUHypervisor
	result, err := (&vmctl.QEMUHypervisor{}).Boot(context.Background(), spec, tmpDir)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("QEMU error: %v", err))
		return 1
	}
	return result.ExitCode
}

// SPDX-License-Identifier: Apache-2.0
package main

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/disk"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/netpol"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/ninep"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/vmctl"
)

// runDevLegacy handles: uv dev [--gui] — original QEMU-direct implementation, kept for reference.
func runDevLegacy(args []string) int {
	gui := false
	for _, a := range args {
		if a == "--gui" || a == "gui" {
			gui = true
		}
	}

	mode := "interactive"
	if gui {
		mode = "gui"
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	vmCacheDir := filepath.Join(repoRoot, "build", "vm")
	vmImagePath := filepath.Join(vmCacheDir, "image")

	if err := repo.Preflight(vmImagePath, true); err != nil {
		return 1
	}

	diskImg, err := filepath.EvalSymlinks(filepath.Join(vmImagePath, "nixos.img"))
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot resolve nixos.img: %v", err))
		return 1
	}

	// Source disk with interactive init script
	tmpDir := filepath.Join(vmCacheDir, "tmp")
	initScript := generateInitScript(mode, "")
	srcDisk, err := disk.CreateSourceDisk(repoRoot, tmpDir, initScript, "")
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create source disk: %v", err))
		return 1
	}
	defer os.Remove(srcDisk)

	// COW overlay
	overlayDir := filepath.Join(vmCacheDir, "tmp")
	if err := os.MkdirAll(overlayDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create overlay dir: %v", err))
		return 1
	}
	overlay, err := disk.CreateOverlay(diskImg, overlayDir)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create overlay: %v", err))
		return 1
	}
	defer overlay.Remove()

	// Build cache
	cacheDisk := filepath.Join(vmCacheDir, "build-cache.qcow2")
	if err := disk.EnsureCacheDisk(cacheDisk, "4G"); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create cache disk: %v", err))
		return 1
	}

	// Output dir
	outputDir := filepath.Join(repoRoot, "build", "vm-output")
	if err := os.MkdirAll(outputDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create output dir: %v", err))
		return 1
	}

	// Network
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

	// QEMU config
	outputShare := ninep.DefaultOutputShare(outputDir)
	cfg := qemu.ProfileConfig(qemu.ProfileDev)
	cfg.Drives = []qemu.Drive{
		{Interface: "virtio", Format: qemu.FormatQCOW2, File: overlay.Path},
		{Interface: "virtio", Format: qemu.FormatRaw, File: srcDisk, ReadOnly: true},
		{Interface: "virtio", Format: qemu.FormatQCOW2, File: cacheDisk},
	}
	cfg.VirtFS = []qemu.VirtFS{{
		LocalPath:     outputShare.LocalPath,
		MountTag:      outputShare.MountTag,
		SecurityModel: string(outputShare.SecurityModel),
		ID:            outputShare.ID,
	}}
	cfg.NetArgs = netArgs

	if gui {
		cfg.Display = qemu.DisplayGTK
		log.Info(tag, "Display: GUI (QEMU VGA window)")
	}

	log.Info(tag, fmt.Sprintf("VM: %d cores, %dMB RAM | mode: %s", cfg.SMP, cfg.MemoryMB, mode))
	log.Info(tag, "Booting NixOS development VM...")
	fmt.Fprintln(os.Stderr)

	// Boot
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

// runDev handles: uv dev [--gui] — vmctl-based implementation.
// It constructs a VMSpec and delegates to QEMUHypervisor.Boot instead of
// manually assembling QEMU arguments. runDevLegacy is kept intact as the original path.
func runDev(args []string) int {
	gui := false
	for _, a := range args {
		if a == "--gui" || a == "gui" {
			gui = true
		}
	}

	mode := "interactive"
	if gui {
		mode = "gui"
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	vmCacheDir := filepath.Join(repoRoot, "build", "vm")
	vmImagePath := filepath.Join(vmCacheDir, "image")

	if err := repo.Preflight(vmImagePath, true); err != nil {
		return 1
	}

	diskImg, err := filepath.EvalSymlinks(filepath.Join(vmImagePath, "nixos.img"))
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot resolve nixos.img: %v", err))
		return 1
	}

	// Source disk with interactive init script.
	tmpDir := filepath.Join(vmCacheDir, "tmp")
	initScript := generateInitScript(mode, "")
	srcDisk, err := disk.CreateSourceDisk(repoRoot, tmpDir, initScript, "")
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create source disk: %v", err))
		return 1
	}
	defer os.Remove(srcDisk)

	// COW overlay.
	overlayDir := filepath.Join(vmCacheDir, "tmp")
	if err := os.MkdirAll(overlayDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create overlay dir: %v", err))
		return 1
	}
	overlay, err := disk.CreateOverlay(diskImg, overlayDir)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create overlay: %v", err))
		return 1
	}
	defer overlay.Remove()

	// Persistent build cache.
	cacheDisk := filepath.Join(vmCacheDir, "build-cache.qcow2")
	if err := disk.EnsureCacheDisk(cacheDisk, "4G"); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create cache disk: %v", err))
		return 1
	}

	// Output dir (9p share).
	outputDir := filepath.Join(repoRoot, "build", "vm-output")
	if err := os.MkdirAll(outputDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create output dir: %v", err))
		return 1
	}

	// Network policy.
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

	display := vmctl.DisplayNone
	if gui {
		display = vmctl.DisplayGTK
		log.Info(tag, "Display: GUI (QEMU VGA window)")
	}

	spec := &vmctl.VMSpec{
		Hypervisor: vmctl.HypervisorQEMU,
		Resources:  profileToResources(qemu.ProfileDev),
		BaseImage: vmctl.ImageRef{
			Path:   overlay.Path,
			Format: vmctl.DiskFormatQCOW2,
		},
		Disks: []vmctl.DiskSpec{
			{Path: srcDisk, Format: vmctl.DiskFormatRaw, ReadOnly: true, Interface: "virtio"},
			{Path: cacheDisk, Format: vmctl.DiskFormatQCOW2, Interface: "virtio"},
		},
		Shares: []vmctl.ShareSpec{{
			HostPath:      outputShare.LocalPath,
			MountTag:      outputShare.MountTag,
			SecurityModel: string(outputShare.SecurityModel),
			ID:            outputShare.ID,
		}},
		Network:  vmctl.NetworkSpec{RawArgs: netArgs},
		Display:  display,
		NoReboot: false,
	}

	log.Info(tag, fmt.Sprintf("VM: ProfileDev (50%% host) | mode: %s", mode))
	log.Info(tag, "Booting NixOS development VM (vmctl)...")
	fmt.Fprintln(os.Stderr)

	hyp := &vmctl.QEMUHypervisor{Logger: nil}
	result, err := hyp.Boot(context.Background(), spec, tmpDir)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("vmctl boot failed: %v", err))
		return 1
	}
	return result.ExitCode
}
